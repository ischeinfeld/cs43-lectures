{-# LANGUAGE InstanceSigs #-} -- allows writing type signatures in instances
{-# LANGUAGE DeriveFunctor #-} -- allows deriving Functor where possible

module Lecture11 where

import Control.Applicative
import Control.Monad
import Data.List

-- Do notation

thing1 = do
  Just 5 -- Just 5 >>
  Just 6

-- add Nothing above to show behaviour of >>

thing2 = do
  x <- [1,2,3]  -- [1,2,3] >>= \x ->
  y <- [4,5]
  return (x,y) -- reminder, what is return

thing3 = do
  (x, y) <- thing2 -- thing2 >>= \(x,y) -> 
  return x


-- introduce idea of stateful computation
-- not allowed in Haskell, but we can model it
-- think of similar? : Parser
-- Parser has String state

newtype Parser a = Parser { getParser :: String -> (a, String) }

-- generalizes to

newtype State s a = State { runState :: s -> (a, s) }

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)


instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f t = State $ \s -> let (a, s') = runState t s in (f a, s')

instance Applicative (State s) where
  pure :: a -> State s a
  pure x = State $ \s -> (x, s)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  tf <*> tx = State $ \s -> let (f, s') = runState tf s
                                (x, s'') = runState tx s'
                            in  (f x, s'')

instance Monad (State s) where
  return = pure

  (>>=) :: State s a -> (a -> State s b) -> State s b
  tx >>= f = State $ \s -> let (x, s') = runState tx s
                           in runState (f x) s'


modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put $ f s

gets :: (s -> a) -> State s a
gets f = do
  s <- get
  return $ f s

data Stack = Stack [Int] deriving (Eq, Show)

initStack :: Stack
initStack = Stack []

push :: Int -> State Stack ()
push x = modify $ \(Stack xs) -> Stack (x:xs)

-- > runState (push 4 >> push 2) initStack

pop :: State Stack Int
pop = State $ \(Stack (x:xs)) -> (x, Stack xs)

-- > runState (push 4 >> pop) initState

peek :: State Stack Int
peek = gets $ \(Stack (x:xs)) -> x

-- > runState (push 4 >> peek) initState

type Program a = State Stack a

runProgram :: Program a -> a
runProgram p = fst $ runState p initStack

-- runProgram (push 4 >> peek)

fib :: Int -> Program Int
fib 0 = return 0
fib 1 = return 1
fib n = do
  push 0
  push 1
  sequence_ $ replicate (n - 1) $ do -- replace w/ replicateM_
    x <- pop
    y <- pop
    push y
    push x
    push $ x + y
  pop

-- If we only export runProgram, program primitives, then the Program type is
-- completely abstract, i.e. we cannot access any internal representation
