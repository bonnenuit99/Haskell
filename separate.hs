{-
  Consider a function separate s that returns a 2-tuple with the digits and non-digits in the string s separated, 
  with the initial order maintained:
 
 Minnies-MacBook-Pro:HaskellCode minniew$ ghci
  GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
  Prelude> :load separate.hs
  [1 of 1] Compiling Main             ( separate.hs, interpreted )
  Ok, one module loaded.
  *Main> separate "July 4, 1776"
  ("41776","July , ")
  *Main> separate "Problem 7: (10 points)"
  ("710","Problem : ( points)")
  
  Here is a partial implementation of separate, using foldr:
    separate s = foldr f ([], []) s
  The task on this problem is to write the folding function f. Use isDigit to test for a digit.
-}


import Data.Char
f :: Char -> ([Char], [Char]) -> ([Char], [Char])
separate :: [Char] -> ([Char], [Char])

f z (x, y)
    | isDigit z      = (z : x, y)
    | otherwise      = (x, z : y)

separate s = foldr f ([], []) s
