{-
  Write a function rotabc that changes a's to b's, b's to c's and c's to a's in a string. Only lowercase letters are affected.
Prelude> :edit rotabc.hs
[1 of 1] Compiling Main             ( rotabc.hs, interpreted )
Ok, one module loaded.
*Main> rotabc "abc"
"bca"
*Main> rotabc "test"
"test"
*Main> rotabc "aaaa"
"bbbb"
*Main> rotabc "ababab"
"bcbcbc"
*Main> rotabc "cabinet"
"abcinet"
*Main> :t rotabc
rotabc :: [Char] -> [Char]
-}



rotabc :: [Char] -> [Char]

rotabc [] = []
rotabc (x:xs)
       |x == 'a'    = ['b'] ++ rotabc(xs)
       |x == 'b'    = ['c'] ++ rotabc(xs)
       |x == 'c'    = ['a'] ++ rotabc(xs)
       |otherwise   = x : rotabc(xs)
