{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main,
  )
where

import Data.List
import Miso
-- import Miso.DSL
-- import GHC.Wasm.Prim
-- import Test.QuickCheck
-- import Test.QuickCheck.Monadic

-- prop_factor :: Positive Integer -> Property
-- prop_factor (Positive n) = monadicIO $ do
--   factors <- run (factor n)
--   assert (product factors == n)
-- echo main | wasm32-wasi-cabal --verbose repl browser -v0
-- start browser in async but with delay
-- echo main | wasm32-wasi-cabal --verbose repl browser --repl-options=-fghci-browser -v0
main :: IO ()
main = do
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
-- main = quickCheck $ \s -> s == fromJSString (toJSString s)
