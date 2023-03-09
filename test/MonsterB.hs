module MonsterB
  ( Monster(..)
  , encode
  ) where

import Data.Word (Word16)
import Data.Text (Text)

import qualified Flatbuffers.Builder as B
import qualified GHC.Exts as Exts

-- | Corresponding schema at @schema/monster-b.fbs@:
--
-- > table Monster {
-- >   name:string;
-- >   mana:short;
-- >   health:short;
-- > }
data Monster = Monster
  { name :: !Text
  , mana :: !Word16
  , health :: !Word16
  }

encode :: Monster -> B.Object
encode Monster{name,mana,health} = B.Object $ Exts.fromList
  [ B.text name
  , B.unsigned16 mana
  , B.unsigned16 health
  ]

