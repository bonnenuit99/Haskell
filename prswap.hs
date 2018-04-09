{-
  Write a function prswap that swaps the elements of a list on a pair-wise basis. That is, the first and second elements are swapped, the third and fourth are swapped, etc. Assume the list has an even number of elements but is possibly empty.

Minnies-MacBook-Pro:HaskellCode minniew$ ghci
GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
Prelude> :set editor nano
Prelude> :edit prswap.hs
Ok, no modules loaded.
Prelude> :load prswap.hs
[1 of 1] Compiling Main             ( prswap.hs, interpreted )
Ok, one module loaded.
*Main> prswap[1..6]
[2,1,4,3,6,5]
*Main> prswap "abcd"
"badc"
*Main> prswap [False, True, True, False]
[True,False,False,True]
*Main> prswap []
[]
*Main> :t prswap
prswap :: [a] -> [a]
-}


prswap :: [a] -> [a]

prswap [] = []
prswap (x : y : xs) = y: x: prswap(xs)
