module MonsterA
  ( Monster(..)
  , encode
  ) where

import Data.Word (Word16)

import qualified Flatbuffers.Builder as B
import qualified GHC.Exts as Exts

-- | Corresponding schema at @schema/monster-a.fbs@:
--
-- > table Monster {
-- >   mana:short;
-- >   health:short;
-- > }
data Monster = Monster
  { mana :: !Word16
  , health :: !Word16
  }

encode :: Monster -> B.Object
encode Monster{mana,health} = B.Object $ Exts.fromList
  [ B.unsigned16 mana
  , B.unsigned16 health
  ]
