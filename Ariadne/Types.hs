module Ariadne.Types where

import Language.Haskell.Names
import Language.Haskell.Exts.Annotated
import qualified Data.Map as Map

data NameLevel = TypeLevel | ValueLevel
  deriving (Ord, Eq, Show, Enum, Bounded)

-- | Global name index records the correspondence between global names and
-- their definition sites. Not every available global name is necessarily
-- in the index.
type GlobalNameIndex = Map.Map (OrigName, NameLevel) SrcLoc
