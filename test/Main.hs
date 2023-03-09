{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Data.Int
import Data.Primitive.ByteArray
import Data.Word
import Test.Tasty
import Test.Tasty.Golden
import Prelude hiding (readFile)
import Data.Bytes (Bytes)
import Data.Bytes.Parser (Parser)

import qualified ArrowSchema
import qualified Test.Tasty.Golden.Advanced as Advanced
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Primitive as PM
import qualified Data.Builder.Catenable.Bytes as Catenable
import qualified Flatbuffers.Builder as FB
import qualified Data.Bytes.Parser as Parser
import qualified Data.Bytes as Bytes
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy.Char8 as LBC8
import qualified Data.Bytes.Parser.Latin as Latin
import qualified GHC.Exts as Exts

import qualified MonsterA
import qualified MonsterB

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests = testGroup "tests"
  [ testGroup "MonsterA"
    [ goldenHex "001" "golden/monster-a-001.hex.txt" $
        Catenable.run (FB.encode (MonsterA.encode MonsterA.Monster{mana=0x0123,health=0x6cba}))
    ]
  , testGroup "MonsterB"
    [ goldenHex "001" "golden/monster-b-001.hex.txt" $
        Catenable.run (FB.encode (MonsterB.encode MonsterB.Monster{name="sully",mana=216,health=43}))
    ]
  , testGroup "ArrowSchema"
    [ goldenHex "001" "golden/arrow-schema-001.hex.txt"
      $ Catenable.run $ FB.encode $ ArrowSchema.encode $ ArrowSchema.Schema
        { endianness = 1
        , fields = Exts.fromList
          [ ArrowSchema.Field
            { name = "active"
            , nullable = True
            , type_ = ArrowSchema.Bool
            , dictionary = ()
            , children = mempty
            }
          ]
        }
    , goldenHex "002" "golden/arrow-schema-002.hex.txt"
      $ Catenable.run $ FB.encode $ ArrowSchema.encode $ ArrowSchema.Schema
        { endianness = 0
        , fields = Exts.fromList
          [ ArrowSchema.Field
            { name = "recently_added_post"
            , nullable = True
            , type_ = ArrowSchema.Bool
            , dictionary = ()
            , children = mempty
            }
          , ArrowSchema.Field
            { name = "likes"
            , nullable = True
            , type_ = ArrowSchema.Int ArrowSchema.TableInt{bitWidth=64,isSigned=True}
            , dictionary = ()
            , children = mempty
            }
          ]
        }
    ]
  ]

-- | Compare a given string against the golden file's contents.
goldenHex
  :: TestName -- ^ test name
  -> FilePath -- ^ path to the «golden» file (the file that contains correct output)
  -> Chunks.Chunks -- ^ action that returns a string
  -> TestTree -- ^ the test verifies that the returned string is the same as the golden file contents
goldenHex name ref act = Advanced.goldenTest
  name
  ((maybe (fail "expected output malformed") pure . cleanExpectedOutput) =<< Bytes.readFile ref)
  (pure (Chunks.concatU act))
  cmp
  upd
  where
  cmp x y = simpleCmp msg x y
    where
    msg =
      "Test output was different from:\n"
      <>
      ref
      <>
      "\n"
      -- "\nIt was:\n"
      -- <>
      -- unpackUtf8 (truncateLargeOutput sizeCutoff y)
  upd bytes = createDirectoriesAndWriteFile ref
    $ LBC8.pack
    $ injectSpaces
    $ BC8.unpack
    $ Base16.encode
    $ Bytes.toByteString (Bytes.fromByteArray bytes)

injectSpaces :: String -> String
injectSpaces (w0 : w1 : w2 : w3 : w4 : w5 : w6 : w7 : w8 : w9 : w10 : w11 : w12 : w13 : w14 : w15 : zs) =
  w0 : w1 : ' ' : w2 : w3 : ' ' : w4 : w5 : ' ' : w6 : w7 : ' ' : w8 : w9 : ' ' : w10 : w11 : ' ' : w12 : w13 : ' ' : w14 : w15 : '\n' : injectSpaces zs
injectSpaces (x : y : zs) = x : y : ' ' : injectSpaces zs
injectSpaces [] = []
injectSpaces _ = error "injectSpaces: expected an even number of characters"

simpleCmp :: Eq a => String -> a -> a -> IO (Maybe String)
simpleCmp e x y =
  return $ if x == y then Nothing else Just e

cleanExpectedOutput :: Bytes -> Maybe ByteArray
cleanExpectedOutput =
    decodeSpacedHex
  . Bytes.intercalate (Bytes.singleton 0x20)
  . fmap (Bytes.takeWhile (/= 0x23))
  . Bytes.split 0x0A
  . Bytes.dropWhileEnd (==0x20)
  . Bytes.dropWhile (==0x20)

-- | Decode a byte sequence that looks like this:
--
-- > cd 0a bf ea 09 ...
--
-- There must be one or more space between each two-character representation
-- of an octet.
decodeSpacedHex :: Bytes -> Maybe ByteArray
decodeSpacedHex !b = Parser.parseBytesMaybe
  ( do let len = Bytes.length b
       dst <- Parser.effect (PM.newByteArray (len + 1))
       Parser.effect (PM.setByteArray dst 0 len (0 :: Word8))
       Latin.skipChar ' '
       parserSpacedHex dst 0
  ) b

parserSpacedHex :: MutableByteArray s -> Int -> Parser () s ByteArray
parserSpacedHex !dst !ix = do
  w <- Latin.hexFixedWord8 ()
  Parser.effect (PM.writeByteArray dst ix w)
  Parser.isEndOfInput >>= \case
    False -> do
      Latin.skipChar1 () ' '
      Parser.isEndOfInput >>= \case
        True -> Parser.effect $ do
          PM.shrinkMutableByteArray dst (ix + 1)
          PM.unsafeFreezeByteArray dst
        False -> parserSpacedHex dst (ix + 1)
    True -> Parser.effect $ do
      PM.shrinkMutableByteArray dst (ix + 1)
      PM.unsafeFreezeByteArray dst
