{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.List
import Miso.IndexDb
import Miso.Promise
import Control.Monad
import Miso.Promise.Demo
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.IO.Class

old_main :: IO ()
old_main = do
  dateType <- jsg "Date"
  d <- new dateType ()
  ds <- d # "toString" $ ()
  dss <- fromJSValUnchecked ds
  putStrLn dss
  indexDb <- jsg "indexedDB"
  indexDbOk <- not <$> liftA2 (||) (isNull indexDb) (isUndefined indexDb)
  putStrLn $ "indexedDb is available: " <> show indexDbOk
  dbs :: [String] <- fromJSValUnchecked =<< await =<< (indexDb # "databases" $ ())
  putStrLn $ "Databases: " <> intercalate ", " dbs


deleteAllDbs :: PromiseM m => Factory -> m ()
deleteAllDbs idb = do
  dbNames <- listDatabaseNames idb
  consoleLog $ "DB found: " <> ms (show (length dbNames))
  forM_ dbNames $ \dbi -> do
    consoleLog $ "Deleting DB " <> dbi.name
    () <- await =<< deleteDatabase idb dbi.name
    consoleLog $ "DB " <> dbi.name <> " has been deleted"

asm :: MonadIO m => String -> (t -> Bool) -> IO t -> PropertyM m ()
asm msg p mv = (liftIO mv) >>= \v -> assertWith (p v) msg

isolationTest :: IO ()
isolationTest =
  quickCheck do
    withNumTests 1 do
      monadicIO do
        asm "No DBs after deleteAllDbs" ([] ==) do
          runPromiseT do
            deleteAllDbs =<< factory
            factory >>= listDatabaseNames
        asm "openDb & closeDb creates db" (elem "db1" . fmap (\x -> x.name)) do
          runPromiseT do
            openDb "db1" >>= closeDb
            factory >>= listDatabaseNames
        asm "No DBs after deleteAllDbs2" ([] ==) do
          runPromiseT do
            f <- factory
            deleteAllDbs f
            listDatabaseNames f

main :: IO ()
main = do
  isolationTest