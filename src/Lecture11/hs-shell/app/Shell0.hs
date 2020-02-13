module Shell0 where

import Data.List.Split
import System.Directory
import Control.Monad

main :: IO ()
main = prompt

prompt :: IO ()
prompt = do
  dir <- getCurrentDirectory
  putStr (last (splitOn "/" dir) ++ " $ ")
  tok <- liftM (splitOn " ") getLine
  print tok
  prompt
