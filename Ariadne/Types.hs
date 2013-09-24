module Ariadne.Types where

import Language.Haskell.Names
import Language.Haskell.Exts.Annotated
import qualified Data.Map as Map

data NameLevel = TypeLevel | ValueLevel
  deriving (Ord, Eq, Show, Enum, Bounded)

type Index = Map.Map (OrigName, NameLevel) SrcLoc
