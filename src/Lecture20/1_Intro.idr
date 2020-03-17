{- These files given an introductory look at programming in the (currently
 - pre-alpha) release of Idris2, a dependently typed programming language.
 - They can be run by installing Idris2 (not trivial at the moment), and
 - the interactive type-driven development features described are available
 - through the Idris Atom plugin. Not all features of dependently typed
 - programmming are touched on, most significantly equality proofs are not
 - discussed.
 -
 - While Haskell makes the claim "all values have types" Idris and other
 - dependently typed languages add the claim that "values are types and
 - types are values." -}

x : Int -- x has type Int (note the symbols : and :: are reversed from Haskell)
x = 4

y : Type -- y has type Type
y = Int  -- y's value is a type

z : Type -- the value Type has type Type as well.
z = Type

{- For proofs in our system to be sound, we should not have Type : Type
 - as this leads to paradoxes. We should instead have universes or levels
 - of types, i.e. Type : Type1, Type1 : Type2. This already exists in other
 - dependently typed languages and will eventually exist in Idris2. Don't
 - worry about this for now, as it will not be important to the examples.

 - To understand dependent types, we will first revisit polymorphism. What
 - are polymorphic values? They are functions on types. -}

-- Consider the polymorphic identity function
id : a -> a
id x = x

{- id2 below makes explicit in the type signature what is implicit above, that
 - it takes an initial {implicit} type argument and returns a monomorphic
 - identity function on that type. Since the value is implicit, Idris can
 - infer it from the other arguments. -}

id2 : {a : Type} -> a -> a -- takes a value `a` of type `Type`
id2 x = x                  -- and a value `x` of type `a`.

id3 : Int -> Int
id3 = id2 { a = Int } -- you can pass implicit arguments explicitly

{- We could also write an identity function with an explicit type parameter -}

id4 : (a : Type) -> a -> a
id4 a x = x

{- These are already examples of a "dependent type", where part of the type
 - (the type `a` of the second argument in this instance) depends on the
 - *value* `a` passed (possibly implicitly) as the first argument. -}
