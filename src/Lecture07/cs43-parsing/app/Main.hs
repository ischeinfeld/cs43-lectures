module Main where

import Calculator (calculator)

main :: IO ()
main = do
  input <- getLine
  case input of
    [] -> return ()
    _  -> (putStrLn $ calculator input) >> main

