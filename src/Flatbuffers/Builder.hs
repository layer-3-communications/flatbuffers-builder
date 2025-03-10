{-# language DerivingStrategies #-}
{-# language LambdaCase #-}

module Flatbuffers.Builder
  ( -- * Types
    Field(..)
  , Array(..)
  , Object(..)
  , Union(..)
    -- * Encoding
  , encode
    -- * Field Helpers
  , text
  , unsigned16
  , signed32
  , signed64
  , boolean
  , union
  , objects
  , structs
  , absent
  ) where

import Control.Monad.ST.Run (runByteArrayST)
import Data.Builder.Catenable.Bytes (Builder)
import Data.Bytes (Bytes)
import Data.Functor.Identity (Identity(Identity),runIdentity)
import Data.Int
import Data.IntMap (IntMap)
import Data.Kind (Type)
import Data.Monoid (Sum(Sum),getSum)
import Data.Primitive (ByteArray(ByteArray),PrimArray(PrimArray),SmallArray)
import Data.Text (Text)
import Data.Foldable (foldlM)
import Data.Word

import qualified Data.Builder.Catenable.Bytes as Builder
import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Utf8 as Utf8
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import qualified Data.Primitive as PM
import qualified Data.Primitive.ByteArray.LittleEndian as LE
import qualified Data.Primitive.Contiguous as C
import qualified GHC.Exts as Exts

-- Not exported
singletonByteArray :: Word8 -> ByteArray
singletonByteArray !w = runByteArrayST $ do
  dst <- PM.newByteArray 1
  PM.writeByteArray dst 0 w
  PM.unsafeFreezeByteArray dst

-- | Encode a boolean as a field.
boolean :: Bool -> Field
boolean !b = FieldPrimitive $ runByteArrayST $ do
  dst <- PM.newByteArray 1
  LE.writeByteArray dst 0 (if b then 0x01 :: Word8 else 0x00 :: Word8)
  PM.unsafeFreezeByteArray dst

-- | Encode a 16-bit unsigned word as a field.
unsigned16 :: Word16 -> Field
unsigned16 !w = FieldPrimitive $ runByteArrayST $ do
  dst <- PM.newByteArray 2
  LE.writeByteArray dst 0 w
  PM.unsafeFreezeByteArray dst

-- | Encode a 32-bit signed word as a field.
signed32 :: Int32 -> Field
signed32 !w = FieldPrimitive $ runByteArrayST $ do
  dst <- PM.newByteArray 4
  LE.writeByteArray dst 0 w
  PM.unsafeFreezeByteArray dst

-- | Encode a 64-bit signed word as a field.
signed64 :: Int64 -> Field
signed64 !w = FieldPrimitive $ runByteArrayST $ do
  dst <- PM.newByteArray 8
  LE.writeByteArray dst 0 w
  PM.unsafeFreezeByteArray dst

-- | Encode text as a field.
text :: Text -> Field
text = FieldString

-- | Use a tagged union as a field.
union :: Union -> Field 
union = FieldUnion

-- | Use an array of objects as a field.
objects :: SmallArray Object -> Field
objects = FieldArray . ArrayObject

-- | Use an array of objects as a field.
structs ::
     Int -- numbers of structs
  -> ByteArray -- pre-serialized structs
  -> Field 
structs !n = FieldArray . ArrayPrimitive n

-- | Indicates that a field is missing.
absent :: Field
absent = FieldAbsent

data Field
  = FieldAbsent
    -- ^ Indicates that the field is missing.
  | FieldPrimitive 
    -- ^ Fixed-width, primitive type. Alignment is inferred from
    -- the length of the payload. Alignments based on length:
    -- 
    -- * 1: 1
    -- * 2: 2
    -- * 3: 4
    -- * 4: 4
    -- * 5: 8
    -- * 6: 8
    -- * 7: 8
    -- * 8+: 8
    !ByteArray
  | FieldUnion !Union
  | FieldArray !Array
  | FieldObject !Object
  | FieldString {-# UNPACK #-} !Text

-- | An object
newtype Object = Object (SmallArray Field)

-- | A tagged union
data Union = Union
  { tag :: !Word8
  , object :: !Object
  }

-- | An array. Flatbuffers serializes arrays of objects differently
-- from arrays of primitive values, but this library lumps all of these
-- together anyway.
--
-- This type might need to have an @ArrayArray@ data constructor as well,
-- but this is not needed at the moment. Open an issue on the issue tracker
-- if this is important.
data Array
  = ArrayPrimitive 
      !Int
        -- ^ Number of elements, not the same as the length of the byte array.
        -- This number multiplied by the element size should equal the length
        -- of the byte array.
        --
        -- To keep this simple, these are pre-serialized. This lets the library
        -- avoid picking up dependencies on any array libraries (beyond @primitive@).
      !ByteArray
  | ArrayObject !(SmallArray Object)

-- Internal type. Not exported.
data Section
  = SectionBoxedArray
      !(PrimArray Id) -- pointers to tables
  | SectionPrimitiveArray -- the alignment of this is weird
      !Int -- number of elements
      !ByteArray -- payload
  | SectionString {-# UNPACK #-} !Text
  | SectionVirtualTable -- size of virtual table can be discerned
      !Int -- table size. if this is >= 2^16, we cannot encode, does not need to divide 8 evenly 
      !(PrimArray Word16) -- pointers into table, zero means field missing
  | SectionTable
      !Id -- virtual table id
      !Int -- Size of this table, including the vtable id. This must be divisible by 8.
      !(SmallArray (Padded FinalOutput))

type Id = Int

-- When we encode a table, we turn it into this first and then
-- build the vtable based on this.
data IntermediateOutput
  = IntermediatePointer !Id -- pointer to section id, size is 4 bytes
  | IntermediateRaw {-# UNPACK #-} !ByteArray
  | IntermediateAbsent

-- Missing fields do not have a payload in the table
data FinalOutput
  = FinalPointer !Id -- pointer to section id, size is 4 bytes
  | FinalRaw {-# UNPACK #-} !ByteArray

-- Padding is added to the right (positions greater than the end of
-- the chunk).
data Padded a = Padded
  !Int -- total size, must be >= size(a)
  !a
  deriving stock (Functor)

data R a = R !(IntMap Section) !Id a
  deriving stock (Functor)

newtype M :: Type -> Type where
  M :: (IntMap Section -> Id -> R a) -> M a

deriving stock instance Functor M

instance Applicative M where
  pure = pureM
  m1 <*> m2 = m1 `bindM` \x -> m2 `bindM` \y -> pureM (x y)

instance Monad M where
  (>>=) = bindM

pureM :: a -> M a
pureM r = M (\m i -> R m i r)

bindM :: M a -> (a -> M b) -> M b
bindM (M f) g = M
  (\a b -> case f a b of
    R a' b' r -> case g r of
      M h -> h a' b'
  )

data Acc = Acc
  !Int -- total number bytes used to encode everything
  !Builder -- everything encoded so far, len(bldr) = total
  !(IntMap Int) -- how many bytes away is each section from the end

-- | Encode a root object as a catenable builder.
encode :: Object -> Builder
encode obj =
  -- Implementation note: We take advantage of the fact that tables cannot
  -- refer to anything (tables, vtables, arrays) with IDs lower than their
  -- own. If we ever start consolidating identical vtables, this property
  -- will no longer hold. We could just do all the vtables first and put
  -- them at the end.
  -- Also, the root object always gets ID 0.
  let M f = encodeObject obj
      R sections _ _ = f IntMap.empty 0
      Acc _ result _ = IntMap.foldrWithKey'
        (\sectionId section (Acc total bldr distances) ->
          let bytes = encodeSectionToBytes distances total section
              len = PM.sizeofByteArray bytes
              newTotal = total + len
           in Acc newTotal (Builder.Cons (Bytes.fromByteArray bytes) bldr) (IntMap.insert sectionId (newTotal - sectionOffset section) distances)
        ) (Acc 0 Builder.Empty IntMap.empty) sections
   in Builder.Cons rootTableOffsetBytes result

-- For primitive arrays, we have to keep the payload 8-byte aligned.
-- But the length is only represented with 4 bytes. So, we write the
-- length not at the beginning of the array but 4 bytes past the
-- beginning.
sectionOffset :: Section -> Int
sectionOffset = \case
  SectionPrimitiveArray{} -> 4
  _ -> 0

rootTableOffsetBytes :: Bytes
rootTableOffsetBytes = Bytes.fromByteArray $ runByteArrayST $ do
  dst <- PM.newByteArray 8
  PM.setByteArray dst 0 8 (0x00 :: Word8)
  PM.writeByteArray dst 0 (0x08 :: Word8) -- Look at offset 0x08 for the root table
  PM.unsafeFreezeByteArray dst

-- This must pad everything out to 8-byte alignment by putting
-- zeroes at the end. Whenever we encode a section, we assume that
-- we begin 8-byte aligned.
encodeSectionToBytes ::
     IntMap Int -- distances of sections from the end of the serialized output
  -> Int -- total size of everything that comes after this section
  -> Section
  -> ByteArray
encodeSectionToBytes distances !totalSz = \case
  SectionString str -> runByteArrayST $ do
    let b = Utf8.fromText str
    let sz = 8 * div (7 + (Bytes.length b + 4 + 1)) 8
    dst <- PM.newByteArray sz
    PM.setByteArray dst 0 sz (0x00 :: Word8)
    LE.writeByteArray dst 0 (fromIntegral (Bytes.length b) :: Word32)
    Bytes.unsafeCopy dst 4 b
    PM.unsafeFreezeByteArray dst
  SectionPrimitiveArray count payload -> runByteArrayST $ do
    let arrByteSizeRoundedUp = 8 * div (7 + PM.sizeofByteArray payload) 8
    let totalSize = 8 + arrByteSizeRoundedUp
    dst <- PM.newByteArray totalSize
    PM.setByteArray dst 0 totalSize (0x00 :: Word8)
    LE.writeByteArray dst 1 (fromIntegral count :: Word32)
    PM.copyByteArray dst 8 payload 0 (PM.sizeofByteArray payload)
    PM.unsafeFreezeByteArray dst
  SectionBoxedArray idents -> runByteArrayST $ do
    let arrElementSize = PM.sizeofPrimArray idents
    let arrByteSizeRoundedUp = 8 * div (7 + (4 + (4 * arrElementSize))) 8
    let arrDistance = arrByteSizeRoundedUp + totalSz
    dst <- PM.newByteArray arrByteSizeRoundedUp
    PM.setByteArray dst 0 arrByteSizeRoundedUp (0x00 :: Word8)
    LE.writeByteArray dst 0 (fromIntegral arrElementSize :: Word32)
    !_ <- C.foldlM'
      (\ix ident -> case IntMap.lookup ident distances of
        Nothing -> errorWithoutStackTrace "Flatbuffers.Builder.encodeSectionToBytes: missing table in array"
        Just tableDistance -> do
          LE.writeByteArray dst ix (fromIntegral ((arrDistance - ix*4) - tableDistance) :: Int32)
          pure (ix + 1)
      ) (1 :: Int) idents
    PM.unsafeFreezeByteArray dst
  SectionTable vtableId tableSize payload -> runByteArrayST $ do
    let tableDistance = tableSize + totalSz
    dst <- PM.newByteArray tableSize
    PM.setByteArray dst 0 tableSize (0x00 :: Word8)
    case IntMap.lookup vtableId distances of
      Nothing -> errorWithoutStackTrace "Flatbuffers.Builder.encodeSectionToBytes: missing vtable"
      Just vtableDistance -> do
        LE.writeByteArray dst 0 (fromIntegral (vtableDistance - tableDistance) :: Int32)
        let go !ix !position !fieldDistance = if ix < PM.sizeofSmallArray payload
              then do
                let Padded fieldSz output = PM.indexSmallArray payload ix
                case output of
                  FinalRaw bytes -> if PM.sizeofByteArray bytes > fieldSz
                    then errorWithoutStackTrace ("Flatbuffers.Builder.encodeSectionToBytes: bad field size (" ++ show (PM.sizeofByteArray bytes) ++ " and " ++ show fieldSz)
                    else PM.copyByteArray dst position bytes 0 (PM.sizeofByteArray bytes)
                  FinalPointer ident -> case IntMap.lookup ident distances of
                    Nothing -> errorWithoutStackTrace "Flatbuffers.Builder.encodeSectionToBytes: missing table id"
                    Just distance -> case mod position 4 of
                      0 -> LE.writeByteArray dst (div position 4) (fromIntegral (fieldDistance - distance) :: Word32)
                      _ -> errorWithoutStackTrace "Flatbuffers.Builder.encodeSectionToBytes: unaligned table id position"
                go (ix + 1) (position + fieldSz) (fieldDistance - fieldSz)
              else PM.unsafeFreezeByteArray dst
        -- Here, we rely on having 4 bytes of padding after the vtable id.
        go 0 8 (tableDistance - 8)
  SectionVirtualTable tableSize offsets -> runByteArrayST $ do
    -- Add 2 entries for the beginning, round up to nearest multiple
    -- of 4.
    let vtableSize = 2 + PM.sizeofPrimArray offsets
    let vtableSizeRoundedUp = 4 * div (3 + vtableSize) 4
    dst <- PM.newPrimArray vtableSizeRoundedUp
    PM.setPrimArray dst 0 vtableSizeRoundedUp (0x0000 :: Word16)
    -- Note: this only works on little-endian machines. Sorry.
    PM.writePrimArray dst 0 ((2 * fromIntegral vtableSize) :: Word16)
    PM.writePrimArray dst 1 (fromIntegral tableSize :: Word16)
    PM.copyPrimArray dst 2 offsets 0 (PM.sizeofPrimArray offsets)
    PrimArray output <- PM.unsafeFreezePrimArray dst
    pure (ByteArray output)

encodeFieldOnto :: [IntermediateOutput] -> Field -> M [IntermediateOutput]
encodeFieldOnto !acc = \case
  FieldAbsent -> pure (IntermediateAbsent : acc)
  FieldPrimitive b -> pure (IntermediateRaw b : acc)
  FieldObject obj -> do
    enc <- encodeObject obj
    pure (IntermediatePointer enc : acc)
  FieldUnion Union{tag,object} -> do
    enc <- encodeObject object
    pure (IntermediatePointer enc : IntermediateRaw (singletonByteArray tag) : acc)
  FieldString str -> do
    enc <- encodeString str
    pure (IntermediatePointer enc : acc)
  FieldArray arr -> do
    enc <- encodeArray arr
    pure (IntermediatePointer enc : acc)

encodeArray :: Array -> M Id
encodeArray = \case
  ArrayObject objs -> do
    arrId <- freshId
    objIds <- C.traverse encodeObject objs
    push arrId (SectionBoxedArray objIds)
    pure arrId
  ArrayPrimitive count payload -> do
    arrId <- freshId
    push arrId (SectionPrimitiveArray count payload)
    pure arrId

encodeString :: Text -> M Id
encodeString !str = do
  stringId <- freshId
  push stringId (SectionString str)
  pure stringId

encodeObject :: Object -> M Id
encodeObject (Object values) = do
  tableId <- freshId
  vtableId <- freshId
  chunksReversed <- foldlM (\acc field -> encodeFieldOnto acc field) [] values
  let chunks = Exts.fromList (List.reverse chunksReversed)
  let (finals,offsets) = arrangeChunks chunks
  -- Add 8 to the beginning for the reference to the vtable offset plus
  -- four bytes of padding. It ought to be possible to pack this
  -- better, but I'm just trying to do the most simple thing
  -- possible at the moment.
  -- We actually rely on this padding elsewhere, so be careful if
  -- you ever change this.
  let tableSize = 8 + getSum (C.foldMap' (\(Padded sz _) -> Sum sz) finals) :: Int
  push vtableId (SectionVirtualTable tableSize offsets)
  push tableId (SectionTable vtableId tableSize finals)
  pure tableId

-- Postcondition: len(arg) = len(offsets)
-- Postcondition: len(paddedFinal) <= len(arg)
arrangeChunks :: SmallArray IntermediateOutput -> (SmallArray (Padded FinalOutput), PrimArray Word16)
arrangeChunks !xs =
  let padded = fmap naivePad xs
      (_,offsets) = runIdentity $ C.mapAccumLM'
        (\off (Padded sz output) -> case mod sz 8 of
          0 -> case output of
            IntermediateAbsent -> Identity (off + sz, 0 :: Word16)
            _ -> Identity (off + sz, fromIntegral @Int @Word16 off)
          _ -> errorWithoutStackTrace "Flatbuffers.Builder.arrangeChunks: mistake"
        ) (8 :: Int) padded
        -- By starting with 8, we are making an assumption about the vtable id
        -- being padded.
      paddedFinal = C.map' (fmap intermediateToFinalNaive) padded
   in (paddedFinal,offsets)

intermediateToFinalNaive :: IntermediateOutput -> FinalOutput
intermediateToFinalNaive = \case
  IntermediatePointer x -> FinalPointer x
  IntermediateRaw x -> FinalRaw x
  IntermediateAbsent -> FinalRaw mempty

-- This causes all absent fields to be represented by 8 bytes.
-- This is not a good way to serialize, but it is simple.
naivePad :: IntermediateOutput -> Padded IntermediateOutput
naivePad c = case c of
  IntermediateAbsent -> Padded 8 c
  IntermediatePointer{} -> Padded 8 c
  IntermediateRaw !b -> let sz = PM.sizeofByteArray b in if sz <= 8
    then Padded 8 c
    else Padded (8 * div (sz + 7) 8) c

freshId :: M Id
{-# inline freshId #-}
freshId = M (\m i -> R m (i + 1) i)

push :: Id -> Section -> M ()
{-# inline push #-}
push ident section = M (\m i -> R (IntMap.insert ident section m) i ())
