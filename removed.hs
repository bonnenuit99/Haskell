{-
  Write a function removedup lt that removes the duplicate elements from a list lt.
  Prelude> :load removedup.hs
  [1 of 1] Compiling Main             ( removedup.hs, interpreted )
  Ok, one module loaded.
  *Main> removedup[1,2,3,2,4]
  [1,3,2,4] 
-}



removedup [] = []
removedup (x : xs)
          | member x xs   = removedup xs
          | otherwise     = x : removedup xs

member x [] = False
member x (y : ys)
       | x == y     = True
       | otherwise  = member x ys
