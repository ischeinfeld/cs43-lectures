{- This file demonstrates the canonical example of dependent types,
 - vectors (implemented as lists) with their lengths encoded at the type level.
 - First note that Idris has built-in natural numbers, defined as follows:

 data Nat = Z | S Nat

 - A Nat is either 0 (Z) or the successor of a Nat, (i.e. N + 1) -}

data Vect : Nat -> Type -> Type where
  Nil  : Vect Z elem
  (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem

-- Hints for interactive editing
%name Vect xs,ys,zs,ws

{- Let's break this down. We have a type constructor
 -   Vect : Nat -> Type -> Type
 - In Idris, since types are values, type constructors are just functions!
 - This type constructor takes a value of type `Nat` and a value of type `Type`
 - and constructs the type (Vect N Type) of a length N vector of type Type.

 - We also have two term constructors, `Nil` and `(::)`.
 -   Nil : Vect z elem
 - constructs a vector of length `0` with element type `elem` for any type `elem`.
 -   (::) : (x : elem) -> (xs : Vect len elem) -> Vect (S len) elem
 - takes an element and a Vector of length `len`, and returns a vector of
 - length `len + 1`. Note that it does so by construction, i.e. this is where
 - we *define* how the length of a vector is calculated. -}

v1 : Vect 0 Int
v1 = [] -- [] is a built-in synonym for Nil

v2 : Vect 3 Int
v2 = [1,2,3] -- list syntax is overloaded for any type with constructors Nil, ::

-- The following code will throw a compile-time error
--   v3 : Vect 4 Int
--   v3 : [1,2,3]


{- Crucial to the defintion above is the fact that we can use functions to
 - compute types. For example, `Vect (len + 1) elem` uses addition to compute
 - the resulting type. We can write functions whose output types are computed
 - from their input types.

 - Let us write an append function on vectors. -}

append : Vect n a -> Vect m a -> Vect (n + m) a
append xs ys = ?hole

{- Here `?hole` is a hole, which is a valid piece of syntax that Idris can assign
 - a type. In the Idris editor mode (see https://atom.io/packages/language-idris),
 - the shortcut Ctrl-Alt-t shows the type of the value under the cursor as well
 - as the types of all the available values in scope. On the hole above, this yields

 0 m : Nat
 0 a : Type
 0 n : Nat
   ys : Vect m a
   xs : Vect n a
-------------------------------------
hole : Vect (plus n m) a

 - Ctrl-Alt-c on a function argument case splits that argument. Doing so on `xs`:
 -}

append2 : Vect n a -> Vect m a -> Vect (n + m) a
append2 [] ys = ?base2       -- base case
append2 (x :: xs) ys = ?rec2 -- recursive case

{- Inspecting ?base2, we see

 0 m : Nat
 0 a : Type
   ys : Vect m a
 0 n : Nat
-------------------------------------
base2 : Vect m a

- We require a `Vect m a` and have a `Vect m a`, ys.

- Ctrl-Alt-s searches the types and values in scope for a solution to the program,
- i.e. a term with the type of the hole. Running serach on the two holes above
- automatically generates the following code. -}

append3 : Vect n a -> Vect m a -> Vect (n + m) a
append3 [] ys = ys
append3 (x :: xs) ys = x :: append3 xs ys

{- This is amazing! The compiler just discovered our code for us! Note that all
 - this does is replace our usual reasoning "see what fits the type and its
 - probably right," and will thus not always yield the correct program. However,
 - the extra expressivity of dependent types does often allow correct program
 - generation, just via a simple search.

 - Let us consider another example: -}

zipWith : (f : a -> b -> c) -> (xs : Vect n a) -> (ys : Vect n b) -> Vect n c
zipWith f xs ys = ?zipWith_rhs

{- Here, both input vectors are guarenteed to be the same length. This gives
 - the compiler a lot of useful information to help us write our program.
 - First, Ctrl-Alt-c to case split on `xs` yields the following: -}

zipWith2 : (f : a -> b -> c) -> (xs : Vect n a) -> (ys : Vect n b) -> Vect n c
zipWith2 f [] ys = ?zipWith2_rhs_1
zipWith2 f (x :: xs) ys = ?zipWith2_rhs_2

{- Subsequently case splitting again on ys in both the base
 - and recursive case only yields the one possibility for each, since in the
 - base case `ys` is guarenteed to be `[]` and in the recursive case `ys` is
 - guarenteed to be of the form `(y :: ys)`. -}

zipWith3 : (f : a -> b -> c) -> (xs : Vect n a) -> (ys : Vect n b) -> Vect n c
zipWith3 f [] [] = ?zipWith_base
zipWith3 f (x :: xs) (y :: ys) = ?zipWith_rec

{- Searching w/ Ctrl-Alt-s on the two holes above immediately yields the solution! -}

zipWith4 : (f : a -> b -> c) -> (xs : Vect n a) -> (ys : Vect n b) -> Vect n c
zipWith4 f [] [] = []
zipWith4 f (x :: xs) (y :: ys) = f x y :: zipWith4 f xs ys
