{- Another feature of the Idris2 type system, which comes from the underlying
 - Quantitative Type Theory, are quantities. Each value in a type can be annotated
 - as follows:
 -   0 - Argument erased at runtime, i.e. is only available at type-checking
 -   1 - Argument must be used exactly once each time the function is called.
       - No annotation means the argument can be used any number of times
 - The full inferred type of `id` below is shown in `id2`. -}

id : a -> a
id x = x

id2 : {0 a : Type} -> (1 x : a) -> a -- implicit eraced argument,
id2 x = x

{- We can see the effect of quantities (although not yet why they are useful)
 - through the following example. Consider the following definition. -}

duplicate : (1 x : a) -> (a, a)
duplicate x = ?hole

{- Inspecting ?hole above yields

 0 a : Type
 1 x : a
 -------------------------------------
 hole : (a, a)

 - Thus, idris is telling us we cannot use `a` in a term (only a type) and
 - we can use `x` exactly once. Say we do use it once:

-}

duplicate2 : (1 x : a) -> (a, a)
duplicate2 x = (?hole2, x)

{- On the hole `?hole2` above, we get the following. Note how the quantity of `x`
 - has changed from 1 to 0, since it has been used once.

 0 a : Type
 0 x : a
 -------------------------------------
 hole2 : a

 - Thus, this function is impossible to write without removing the quanity
 - constraint. We cannot use `x` twice.
 -}

duplicate3 : (x : a) -> (a, a)
duplicate3 x = (x, x)

{- Watch the following talk by the creator of Idris, starting at 26:15 for an
 - example of how quantities could be useful in practice. Or watch the whole
 - video, it gives a bunch of cool examples and should be accessible after
 - reading these notes.

 - https://www.youtube.com/watch?v=DRq2NgeFcO0}
