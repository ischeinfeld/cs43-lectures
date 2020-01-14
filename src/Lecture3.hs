module Lecture3 where

data List a = Empty | Cons a (List a)
-- deriving (Show)  -- delete later

bools :: List Bool
bools = Cons True $ Cons False Empty

ints :: List Int
ints = Cons 1 $ Cons 2 $ Cons 4 Empty

names :: List String
names = Cons "Kylan" $ Cons "Pablo" $ Cons "Milind" Empty


mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Cons x xs) = Cons (f x) $ mapList f xs

-- > mapList not bools

filterList :: (a -> Bool) -> List a -> List a
filterList _ Empty = Empty
filterList p (Cons x xs)
  | p x = Cons x $ filterList p xs
  | otherwise = filterList p xs

-- > filterList (> 1) ints


-- Motivation
sumIntList :: List Int -> Int
sumIntList Empty = 0
sumIntList (Cons x xs) = x + sumIntList xs

prodIntList :: List Int -> Int
prodIntList Empty = 1
prodIntList (Cons x xs) = x * prodIntList xs

foldIntList :: (Int -> Int -> Int) -> Int -> List Int -> Int
foldIntList _ acc Empty = acc
foldIntList f acc (Cons x xs) = f x $ foldIntList f acc xs


-- Can we make the type more general? Comment out type above,
-- rename function to foldList

foldList _ acc Empty = acc
foldList f acc (Cons x xs) = f x $ foldList f acc xs

-- Look up type sig on HOOGLE, find foldr

-- > :t foldr
-- > foldr (+) 0 [1..10]
-- > foldr (*) 1 [1..10]
-- > foldr (\x st -> show x ++ "," ++ st) "" [1..10]
-- > foldr (:) [] [1..10]
-- > :t (:)

-- > :t foldr
-- > :t foldl
-- Show picture: https://wiki.haskell.org/Fold#Examples

-- > foldl (+) 0 [1..10]
-- > foldl (:) [] [1..10]
-- > foldl (flip (:)) [] [1..10] -- HOOGLE flip by type


-----------------------------
-- PART 2: Typeclasses
-----------------------------

-- > ints
-- > show ints
-- - deriving (Show) from IntList

{- Built-in

class Show a where
  show :: a -> String

-}


instance Show a => Show (List a) where
  show Empty = "[]"
  show (Cons x xs) = "[" ++ show x ++ showVals xs ++ "]"
    where showVals = foldList (\x str -> "," ++ show x ++ str) ""
          

-- make point-free

-- > bools
-- > ints
-- > names


data Tree a = End | Node a (Tree a) (Tree a)
  deriving (Show)

tree :: Tree Int
tree = (Node 0 (Node (-10) (Node (-11) End End)
                               (Node (-9) End End))
                 (Node 10 End
                            (Node 11 End End)))



-- data RoseTree a = RoseTree a [RoseTree a]

data RoseTree a = RoseTree { val :: a , branches :: [RoseTree a] }
  -- deriving (Show) -- delete later


-- > :t val
-- > :t branches

rt :: RoseTree Int
rt = RoseTree 0 [ RoseTree 1 [ RoseTree 2 []
                             , RoseTree 3 []
                             ]
                , RoseTree 4 [ RoseTree 5 []
                             , RoseTree 6 []
                             , RoseTree 7 []
                             ]
                ]               

-- leave out tail on first implementation

instance Show a => Show (RoseTree a) where
  show rt = tail $ showLevel 0 rt
    where indent i = replicate (2*i) ' ' ++ "|-"
          showLevel i (RoseTree x rts) =
            ("\n"
             ++ indent i 
             ++ show x 
             ++ foldr (\rt st -> showLevel (i+1) rt ++ st) "" rts)


----------------------------------
-- Built-in typeclasses
---------------------------------

threeEq a b c = (a == b) && (b == c)

-- > threeEq 1 1 1
-- > threeEq 1 1 2
-- > :t threeEq
-- > :t (==)

-- > :info Eq

{-

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

  x /= y = not (x == y)
  x == y = not (x /= y)

-}

-- > ints == Empty
-- + deriving (Eq)
-- - deriving (Eq)

-- first leave out Eq constraint

instance Eq a => Eq (List a) where
  (==) Empty Empty = True
  (==) (Cons x xs) (Cons y ys) = (x == y) && (xs == ys)
  (==) _ _ = False

instance Eq a => Eq (Tree a) where
  (==) End End = True
  (==) (Node x xleft xright) (Node y yleft yright) =
    (x == y) && (xleft == yleft) && (xright == yright)

-- talk about derived instances, show docs for Eq
-- talk about laws


-- > :t (>)
-- > :i Ord

{-
 
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a

  -- ...

-}

-- > :t compare
-- > :i Ordering
-- > compare 1 2

instance Ord a => Ord (List a) where
  compare Empty Empty = EQ
  compare Empty _     = LT
  compare _ Empty     = GT
  compare (Cons x xs) (Cons y ys) = case compare x y of
    EQ -> compare xs ys
    comp -> comp


list1 = Cons 1 $ Cons 2 $ Cons 3 Empty
list2 = Cons 1 $ Cons 3 Empty

-- > compare list1 list2
-- > compare [1,2,3] [1,3]


-- Ord for Tree
-- + Ord for Tree


tree1 = Node 1 (Node 2 End End) (Node 3 End End)
tree2 = Node 1 (Node 2 End End) (Node 4 End End)

-- - Ord for Tree

traverseDF :: Tree a -> [a]
traverseDF End = []
traverseDF (Node x left right) =
  x : (traverseDF left) ++ (traverseDF right)


traverseBF :: Tree a -> [a]
traverseBF tree = tbf [tree]
  where
    tbf [] = []
    tbf xs = map nodeValue xs ++ tbf (concat (map leftAndRightNodes xs))

    nodeValue (Node a _ _) = a

    leftAndRightNodes (Node _ End End) = []
    leftAndRightNodes (Node _ End b)   = [b]
    leftAndRightNodes (Node _ a End)   = [a]
    leftAndRightNodes (Node _ a b)     = [a,b]

instance Ord a => Ord (Tree a) where
  compare t1 t2 = compare (traverseDF t1) (traverseDF t2)
