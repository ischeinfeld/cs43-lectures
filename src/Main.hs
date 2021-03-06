module Main where

import Data.Char
import Control.Monad

import Data.IORef
import Data.Array.IO

-- type Program a = State Stack a = Stack -> (a, Stack)
-- type IO a = State RealWorld a = RealWorld -> (a, RealWorld)
--
-- `Program a` was an effect on a stack eventually returning a value
-- `IO a` is an IO effect eventually returning a value
--

main :: IO ()
main = do
  x <- getLine
  let v = read x
  (fib v)
  return ()

io0 :: IO ()
io0 = forever $ getLine >>= putStrLn

io1 :: IO ()
io1 = do
  putStr "enter a number: "
  x <- getLine
  when (and $ fmap isDigit x) (comment $ read x)

comment :: Int -> IO ()
comment x
  | even x = putStrLn "great number"
  | otherwise = return ()




io3 :: IO ()
io3 = do
  ref <- newIORef (0 :: Int)
  replicateM_ 100 $ modifyIORef ref (+ 1) -- sequence_ $ replicate n ...
  readIORef ref >>= print

fib :: Int -> IO Int
fib 0 = return 0
fib 1 = return 1
fib n = do
  ref1 <- newIORef (0 :: Int)
  ref2 <- newIORef (1 :: Int)
  replicateM_ (n - 1) $ do
    x <- readIORef ref1
    y <- readIORef ref2
    writeIORef ref1 y
    writeIORef ref2 $ x + y
  res <- readIORef ref2
  print res
  return res
