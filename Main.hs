module Main where

import System.Console.Haskeline
import System.Environment
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M
import Text.JSON.Yocto
import Data.Map.Strict hiding (map, filter)
import Data.Ratio

main :: IO ()
main = do
  _ <- getArgs
  let inputFunc = getInputLine
  runInputT mySettings $ withInterrupt $ loop inputFunc (0 :: Int)
  where
    loop inputFunc n = do
      minput <- handleInterrupt (return (Just "interrupted"))
        $ inputFunc (show n ++ ":")
      case minput of
        Nothing -> return ()
        Just s -> do
          outputStrLn ("line " ++ show n ++ ":" ++ s)
          loop inputFunc (n+1)

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) wordList


wordList :: [String]
wordList = ["one.twoa", "one.twob", "one.twoc"]

mySettings :: Settings IO
mySettings = Settings {
  historyFile = Just "myhist"
  , complete = completeWord Nothing " \t" $ return . searchFunc
  -- , complete = completeQuotedWord (Just '\\') (" \t" $ return . searchFunc) completeWord
  , autoAddHistory = True
  }

-- buildTree :: Value -> [[String]]
-- buildTree (Array vs) = buildTree <$> vs
-- buildTree (Object m) = M.toList $ M.map buildTree m

testthing = do testtext <- readFile "test.json"
               print $ decode testtext
