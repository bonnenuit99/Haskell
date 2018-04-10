--Question 1
{-
Write a Haskell function sublist lt that computes all sublists of a list lt.
e.g. >sublist [1,2,3]
     [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

The order of elements in the sublist does not matter. For example, the answer [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]] is also correct.
-}


sublist::[a]->[[a]]
sublist[] = [[]]
sublist(x:xs) = (map(\xs->x:xs)(sublist xs)) ++ (sublist xs)
{-
 This map function means make the x as a head of all the lists in sublist xs, then append
 sublist xs to the former list. Finally, get sublist of (x:xs).
-}



--Question 2
{-
Write a Haskell function delete k lt that removes every kth element of a list lt.
e.g. > delete 2 [3,4,5,6,7,8,9]
           [3,5,7,9]
-}


delete k lt
	| k <= 0     = undefined
	| lt == []   = []
	| otherwise  = take (k-1) lt ++ delete k (drop k lt)
{-
  take: Int -> [a] -> [a]  take n elements from the front of a list
  drop: Int -> [a] -> [a]  drop n elements from the front of a list
  To take k-1 elements from the front of the list, then do recursion in the list which drops 
  k elements from the front of the list. In this way we can remove every kth element of a list.
-}



--Question 3
{-
Define a function replic lt that replicates each element in lt into a list. If the element is in the kth position of lt, the resulting list contains k copies of the same element. You must define this function using the higher-order function of “map”
e.g. >replic [2,3,4,7,6]
         [[2],[3,3],[4,4,4], [7,7,7,7], [6,6,6,6,6]]
-}


replic lt = map copy 1 lt
        where
                map copy _ [] = []
                map copy k (x:xs) = (copy k x) : map copy (k + 1) xs
                copy k x
                        | k == 0     = []
                        | otherwise  = x: copy (k - 1) x
 


--Question 4
{-
In class, we introduced function “reverse” to reverse the elements of a list, and function “head” as returning the first element of the list. Define a function “liste” to return the last element of a non-empty list based on “reverse”, “head” and possible function composition operator (.). For instance
e.g. >laste [2,3,4,7,6]
       6

In your definition, you must not explicitly give an argument to “laste”. For instance, definition in the form of “laste xs = …” is not acceptable, whereas definition in the form of “laste  = …” is OK.
-}


laste = head.reverse
-- Returning the last element of a non-empty list means returning the first element of the reversed list.



--Question 6
{-
Given the following definition of the propositional formula:

data Formula
        = Atom Bool			-- atomic formula
        | And Formula Formula		-- f /\ f
        | Or Formula Formula		-- f \/ f
        | Not Formula			-- not(f)

(1) Write a Haskell function collect_atoms f that computes all atomic formulas of a propositional formula f. 

e.g. >collect_atoms (And (OR (Atom True) (Atom False)) (Not (Atom False)) )
     [Atom True, Atom False, Atom False]

(Note that your hug environment may not display the atomic formula as above if the display of a Formula (i.e. “show”) is not defined. You do not need to worry about it. It is OK as long as your “collect_atoms” indeed returns a list as above.)

(2) Write a Haskell function eval f to evaluate term f according to standard definitions of propositional logic. 

e.g. >eval (And (OR (Atom True) (Atom False)) (Not (Atom False)) )
     True
-}


data Formula
	= Atom Bool
	| And Formula Formula
	| Or Formula Formula
	| Not Formula

{-
  "show" is a built-in Haskell function to convert values to strings.
  Define show is to ensure that hug environment can display the atomic formula on screen.
-} 
instance Show Formula
	 where
		show (Atom a) = "Atom " ++ show a
		show (And a b) = "And (" ++ show a ++ ") (" ++ show b ++ ")"
		show (Or a b) = "Or (" ++ show a ++ ") (" ++ show b ++ ")"
		show (Not a) = "Not (" ++ show a ++ ")" 

-- (1)
collect_atoms(Atom x) = [Atom x]
collect_atoms(And a b) = collect_atoms a ++ collect_atoms b
collect_atoms(Or a b) = collect_atoms a ++ collect_atoms b
collect_atoms(Not a) = collect_atoms a

-- (2)
eval(Atom a) = a
eval(And a b) = eval a && eval b
eval(Or a b) = eval a || eval b
eval(Not a) = not(eval a)
