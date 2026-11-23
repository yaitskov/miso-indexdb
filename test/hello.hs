{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Miso.Cb
import Miso.IndexDb
import Miso.Promise.Demo
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

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


deleteAllDbs :: CbM m => Factory -> m ()
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
          runCbT do
            deleteAllDbs =<< factory
            factory >>= listDatabaseNames
        asm "openDb & closeDb creates db" (elem "db1" . fmap (\x -> x.name)) do
          runCbT do
            openDb "db1" >>= closeDb
            factory >>= listDatabaseNames
        asm "No DBs after deleteAllDbs2" ([] ==) do
          runCbT do
            f <- factory
            deleteAllDbs f
            listDatabaseNames f

-- foo :: MisoString -> IO Expectation
-- foo dbName = runCbT do
--   -- pure $ dbName `shouldBe` "e"
--   closeDb =<< openDb (dbName)
--   f <- factory
--   dbNames <- fmap (\x -> x.name) <$> listDatabaseNames f
--   () <- await =<< deleteDatabase f dbName
--   pure $ dbNames `shouldContain` [dbName]

parallelTests :: IO ()
parallelTests = hspec do
  describe "IndexDb parallel" do
    it "openDb creates db if missing" do
        property $ \(dbName':: String) ->
          dbName' /= [] ==> idempotentIOProperty do
            let dbName = ms dbName'
            runCbT do
              closeDb =<< openDb (dbName)
              f <- factory
              dbNames <- fmap (\x -> x.name) <$> listDatabaseNames f
              () <- await =<< deleteDatabase f dbName
              pure $ dbNames `shouldContain` [dbName]

main :: IO ()
main = do
  isolationTest
  parallelTests