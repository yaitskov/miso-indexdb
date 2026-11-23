module Miso.IndexDb.DbName where

import Data.String (IsString)
import Miso.IndexDb.Cb
import Prelude (Eq, Ord, Show)

newtype DbName = DbName { unDbName :: MisoString }
  deriving newtype (Eq, Ord, Show, IsString, ToJSVal, FromJSVal)
