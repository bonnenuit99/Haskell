prswap :: [a] -> [a]

prswap [] = []
prswap (x : y : xs) = y: x: prswap(xs)
