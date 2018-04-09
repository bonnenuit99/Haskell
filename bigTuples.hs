{-
  bigTuples max tuples produces a list of the 2-tuples in tuples whose sum is larger than max,
  i.e., for a tuple (a, b), a + b > max.
  Prelude> :edit bigTuples.hs
[1 of 1] Compiling Main             ( bigTuples.hs, interpreted )
Ok, one module loaded.
*Main> bigTuples 10 [(9, 3), (3, 4), (10, 20)]
[(9,3),(10,20)]
*Main> bigTuples 25 [(9, 3), (3, 4), (10, 20)]
[(10,20)]
*Main> bigTuples 100 [(9, 3), (3, 4), (10, 20)]
[]
*Main> bigTuples 1 []
[]
*Main> take 5 $ bigTuples 1000 $ zip [1..] [1..]
[(501,501),(502,502),(503,503),(504,504),(505,505)]
*Main> :t bigTuples
bigTuples :: (Ord t, Num t) => t -> [(t, t)] -> [(t, t)]
*Main> 
-}


igTuples :: (Num t, Ord t) => t -> [(t, t)] -> [(t, t)]

bigTuples n [] = []
bigTuples n ((x, y) : xs)
          | x + y > n       = (x, y) : bigTuples n xs
          | otherwise       = bigTuples n xs
