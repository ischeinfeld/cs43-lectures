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

main :: IO () -- RealWorld -> ((), RealWorld)
main = putStrLn "hello world" -- :t ?

main' = getLine >>= putStrLn
main'' = forever $ getLine >>= putStrLn

-- HOOGLE forever, type and implementation

io1 :: IO ()
io1 = do
  putStrLn "Hi, what's your name?" 
  x <- getLine -- :t ?
  putStrLn $ "Nice to meet you " ++ x ++ "!"

io2 :: IO ()
io2 = do
  putStr "Please enter a number: "
  x <- getLine
  when (and $ fmap isDigit x) (comment $ read x)

comment :: Int -> IO ()
comment n
  | even n = putStrLn "Wow, an even number!"
  | otherwise = return ()
  

-- Memory

io3 :: IO ()
io3 = do
  ref <- newIORef (0 :: Int)
  replicateM_ 100 $ modifyIORef ref (+1)
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
  

-- Why doesn't passing two functions the same IORef and cause problems

io4 :: IO ()
io4 = do
  arr <- newArray (1,10) 37 :: IO (IOArray Int Int)
  a <- readArray arr 1
  writeArray arr 1 64
  b <- readArray arr 1 
  print (a,b)
