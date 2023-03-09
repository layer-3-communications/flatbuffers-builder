{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}

module ArrowSchema
  ( Field(..)
  , Type(..)
  , Schema(..)
  , TableInt(..)
  , encode
  ) where

import Data.Word (Word16)
import Data.Int (Int32)
import Data.Primitive (SmallArray)
import Data.Text (Text)

import qualified Flatbuffers.Builder as B
import qualified GHC.Exts as Exts

-- | Corresponding schema at @schema/arrow-schema.fbs@:
data Schema = Schema
  { endianness :: !Word16
  , fields :: !(SmallArray Field)
  }

data Field = Field
  { name :: !Text
  , nullable :: !Bool
  , type_ :: Type
  , dictionary :: () -- omitting this for now. add it later
  , children :: !(SmallArray Field)
  }

data Type
  = Null
  | Int TableInt
  | Bool

data TableInt = TableInt
  { bitWidth :: !Int32
  , isSigned :: !Bool
  }

encode :: Schema -> B.Object
encode Schema{endianness,fields} = B.Object $ Exts.fromList
  [ B.unsigned16 endianness
  , B.objects (fmap encodeField fields)
  ]

encodeField :: Field -> B.Object
encodeField Field{name,nullable,type_,children} = B.Object $ Exts.fromList
  [ B.text name
  , B.boolean nullable
  , B.union (encodeType type_)
  , B.absent
  , B.objects (fmap encodeField children)
  ]

encodeTableInt :: TableInt -> B.Object
encodeTableInt TableInt{bitWidth,isSigned} = B.Object $ Exts.fromList
  [ B.signed32 bitWidth
  , B.boolean isSigned
  ]

encodeType :: Type -> B.Union
encodeType = \case
  Null -> B.Union{tag=1,object=B.Object mempty}
  Int table -> B.Union{tag=2,object=encodeTableInt table}
  Bool -> B.Union{tag=6,object=B.Object mempty}
