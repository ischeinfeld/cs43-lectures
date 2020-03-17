import Data.Vect

{- Dependent types allow parts of a value's type to depend on that value. Another
 - common example of a dependent type is a dependent tuple or pair, where the
 - type of the second value depends on the first value.

 - A dependent pair is defined as follows:

     data DPair : (a : Type) -> (P : a -> Type) -> Type where
         MkDPair : {P : a -> Type} -> (x : a) -> P x -> DPair a P

 - Unpacking the types above gives a good intuition for dependent types.
 - `DPair` constructs the type of the dependent tuple from a Type for the first
 - value and a function from the first value giving the type of the second.
 - `MkDPair` takes the function constructing the type as an implicit argument,
 - and constructs the pair from the first value and the second value, where the
 - first value `x` has type `a` and the second value has type `P x` -}

vec : DPair Nat (\n => Vect n Int) -- Idris anon. function syntax is `\x => result`
vec = MkDPair 2 [3, 4]

{- Idris has syntactic sugar for dependent pairs, where `vec` above can
 - equivalently be written as follows. Here, a pair of type `(T1 ** T2)`
 - contains values of those types, and T2 can depend on the value of T1. -}


vec2 : (n : Nat ** Vect n Int) -- Vect n Int depends on the value n : Nat
vec2 = (2 ** [3, 4])           -- the first element is the length of the second

{- We can even leave out information in the type *and* in the values, which
 - the compiler can infer. -}

vec3 : (n ** Vect n Int)  -- n must be a Nat
vec3 = (_ ** [3, 4])      -- the first value must be the length of the second
