module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Demo
import Miso.IndexDb
import OrphanInstances ()
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
    consoleLog $ "Deleting DB " <> unDbName dbi.name
    () <- await =<< deleteDatabase idb dbi.name
    consoleLog $ "DB " <> unDbName dbi.name <> " has been deleted"

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

parallelTests :: IO ()
parallelTests = hspec do
  describe "IndexDb parallel" do
    describe "Factory" do
      it "openDb creates db if missing" do
          property $ \(dbName :: DbName) ->
            idempotentIOProperty do
              runCbT do
                f <- factory
                existingDbs <- fmap (\x -> x.name) <$> listDatabaseNames f
                if dbName `elem` existingDbs then
                  pure $ pendingWith $
                    "DB name collision " <> fromMisoString (unDbName dbName)
                else do
                  closeDb =<< openDb dbName
                  dbNames <- fmap (\x -> x.name) <$> listDatabaseNames f
                  () <- await =<< deleteDatabase f dbName
                  pure $ dbNames `shouldContain` [dbName]
    -- describe "DB" do
    --   it "getDbName returns DB name" do
    --     propert $ \(dbName' :: String) -> do


main :: IO ()
main = do
  isolationTest
  parallelTests