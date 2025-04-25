{-# LANGUAGE OverloadedStrings, BlockArguments #-}
module Main (main) where

import Data.Aeson
import Data.Aeson.Types
import Data.Bifunctor
import Data.String
import Data.Vector hiding ((++), map)
import Statistics.Distribution.StudentT
import Statistics.Test.StudentT
import Statistics.Test.Types
import Statistics.Types
import System.Environment

data BenchTimes = BenchTimes (Vector Double)
  deriving Show

data BenchResult = BenchResult { name :: String, result :: TestResult, test :: Test StudentT }
  deriving Show

instance FromJSON BenchTimes where
  parseJSON v = (BenchTimes . fromList) <$>
    withObject "results" (\o ->
      explicitParseField
        (withArray "first" (\arr -> withObject "times" (\t -> explicitParseField (listParser parseJSON) t "times") (arr ! 0)))
        o
        "results"
    )
    v

resultFiles :: [FilePath]
resultFiles =
  [ "asymptotics.stt.json"
  , "conv_eval.stt.json"
  , "stlc.stt.json"
  -- , "stlc100k.stt.json" -- DNF
  -- , "stlc10k.stt.json" -- DNF
  , "stlc5k.stt.json"
  , "stlc_lessimpl.stt.json"
  -- , "stlc_lessimpl10k.stt.json" -- DNF
  , "stlc_lessimpl5k.stt.json"
  , "stlc_small.stt.json"
  , "stlc_small10k.stt.json"
  , "stlc_small5k.stt.json"
  ]

analyzeBench :: FilePath -> FilePath -> FilePath -> IO (Either String BenchResult)
analyzeBench unmodDir modDir resFile = do
  unmodContent <- readFile (unmodDir ++ "/" ++ resFile)
  modContent <- readFile (modDir ++ "/" ++ resFile)
  return $ first ((resFile ++ ": ") ++) do
    (BenchTimes unmodTimes) <- eitherDecode (fromString unmodContent)
    (BenchTimes modTimes) <- eitherDecode (fromString modContent)
    let test = BGreater -- We want to know if the modified smalltt is slower than the unmodified
    let pValue = mkPValue 0.01 -- Significance level of 0.01
    case welchTTest test unmodTimes modTimes of
      Nothing -> Left "welchTTest error"
      Just res -> return (BenchResult resFile (isSignificant (testSignificance res) res) res)

-- unmodified, modified directories
dataAnalysis :: FilePath -> FilePath -> IO (Either String [BenchResult])
dataAnalysis unmodDir modDir = sequenceA <$> sequenceA (map (analyzeBench unmodDir modDir) resultFiles)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [unmodDir, modDir] -> do
      res <- dataAnalysis unmodDir modDir
      putStrLn (show res)
    _ -> usage

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn ("usage: " ++ progName ++ " <unmodified directory> <modified directory>")
