{- Example (modified) by Chris Done,
 - from: https://gist.github.com/chrisdone/672efcd784528b7d0b7e17ad9c115292

 - This files gives an example of a dependently typed printf function. printf
 - takes a format string as its first argument, for example:
      "This is a format string with an int %d and a string %s."
 - Our printf has the following type:
      printf : (s : String) -> interpFormat ( formatString s )
 - Here, the type of the expression `printf s` for a format string `s` is calculated
 - from the value of the format string. `interpFormat ( formatString s)` is a function
 - whose number of arguments and their types are determined by the format string's
 - value. Thus, a compiler error is thrown if the wrong arguments are passed
 - for a given format string. -}

%default total -- turns on totality checking for all functions

{- Idris can try and check that functions are total, i.e. that they terminate.
 - This is too hard a problem in general, but Idris can check totality for the
 - specific class of recursive functions which shrink toward a base case on
 - every call. -}

-- Formatting AST.
data Format
  = FInt Format
  | FString Format
  | FOther Char Format
  | FEnd

-- Parse the format string (list of characters) into an AST.
-- Example: "%d,%s"  →   (FInt (FOther ',' (FString FEnd)))
format : List Char -> Format
format ('%' :: 'd' :: cs ) = FInt ( format cs )
format ('%' :: 's' :: cs ) = FString ( format cs )
format ( c :: cs )         = FOther c ( format cs )
format []                  = FEnd

-- Convenience function to unpack a string into a list of chars, then
-- run format on it. Note that in Idris strings are not lists of characters.
formatString : String -> Format
formatString s = format ( unpack s )

-- Convert a format AST into a type.
-- Example: FInt (FOther ',' (FString FEnd))   →   Int -> String -> String
interpFormat : Format -> Type
interpFormat (FInt f)     = Int -> interpFormat f
interpFormat (FString f)  = String -> interpFormat f
interpFormat (FOther _ f) = interpFormat f
interpFormat FEnd         = String

-- Dependently-typed part: turn a formatting AST into a well-typed
-- function accepting n arguments.
-- Example:
--      toFunction (FInt (FString FEnd))
--    →
--      \a i s => a ++ (show i) ++ s
{-
-}
toFunction : (fmt : Format) -> String -> interpFormat fmt
toFunction ( FInt f ) a     = \i => toFunction f ( a ++ show i )
toFunction ( FString f ) a  = \s => toFunction f ( a ++ s )
toFunction ( FOther c f ) a = toFunction f ( a ++ pack [c])
toFunction FEnd a           = a

-- Dependently-typed part: turn a formatting string into a well-typed
-- function accepting n arguments.
-- Example: printf "%d%s" → \i s => (show i) ++ s
printf : (s : String) -> interpFormat ( formatString s )
printf s = toFunction ( formatString s ) ""

{- Now we can see printf in action. The following typechecks: -}

output : String
output = printf "My name is %s, and I am %d years old" "Isaac" 22

{- The following does not: -}

outputBad : String
outputBad = printf "My name is %s, and I am %d years old" 22 "Isaac"

{- Indeed, the compiler error is:

 Mismatch between: String and Int

 -}
