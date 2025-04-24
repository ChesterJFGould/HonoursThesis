{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson
import Data.Aeson.Types
import Data.String
import Data.Vector
import Statistics.Test.StudentT
import System.Environment

data BenchTimes = BenchTimes (Vector Double)
  deriving Show

parseObjectPath :: (Value -> Parser a) -> [Key] -> Value -> Parser a
parseObjectPath p [] v = p v
parseObjectPath p (k : ks) v = withObject (show k) (\o -> explicitParseField (parseObjectPath p ks) o k) v

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
  , "stlc100k.stt.json"
  , "stlc10k.stt.json"
  , "stlc5k.stt.json"
  , "stlc_lessimpl.stt.json"
  , "stlc_lessimpl10k.stt.json"
  , "stlc_lessimpl5k.stt.json"
  , "stlc_small.stt.json"
  , "stlc_small10k.stt.json"
  , "stlc_small5k.stt.json"
  ]


-- unmodified, modified directories
dataAnalysis :: FilePath -> FilePath -> Maybe a

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      contents <- readFile path
      let
        x :: Either String BenchTimes
        x = eitherDecode (fromString contents) 
      putStrLn (show x)
    _ -> usage

usage :: IO ()
usage = putStrLn "usage"
