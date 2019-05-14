import Data.Char

import Data.List 
import Debug.Trace

lineTest1 = [(1,'*'),(2,'x'),(3,'x'),(2,'*')];
lineTest2 = [(1,'*'),(2,'*'),(3,'*'),(2,'*')];

solveLine  (size, xs)
                    |(lineLengthSum(xs)==size) = line(xs)
                    |(lineLengthSum(xs)>size) = error("Too long")
                    
insertIntoSimple :: (Int, [[Char]]) -> [[Char]]
insertIntoSimple (_,[]) = [];
insertIntoSimple (0,xs) = xs;
--insertIntoSimple (spaces, xs) = 


lineSolver [] = [];
lineSolver (xs) = [""]++line(xs)++[""]

line :: [(Integer,Char)] -> [[Char]]
--line (xs) | trace ("line " ++ show xs) False = undefined
line [] = [""];
line [x] = [lineHelper(fst x, snd x)];
line (x:y:xs) = line [x] ++ colorEqInsert2((snd x),(snd y)) ++ line (y:xs);

colorEqInsert2 (a,b) = if (a==b) then [" "] else [""]

--generates strings with length of first arg from char from second
lineHelper :: (Integer,Char) -> [Char]
lineHelper (0,_) = ""
lineHelper (count,letter) = letter : lineHelper(count-1, letter)

lineLengthSum [] = 0;
lineLengthSum [x] = fst(x);
lineLengthSum (x:y:xs) = fst(x) + colorEq(snd(x),snd(y)) + lineLengthSum(y:xs);
colorEq (a,b) = if (a==b) then 1 else 0


--creates tree with all space combinations
data Tree a = Empty | Node a [Tree a]

generateNodes :: Integer -> Integer -> [Tree [Char]]
generateNodes 0 _ = [Empty]
generateNodes 1 m = [Node (addSpaces m []) [Empty]]
generateNodes n m = [Node (addSpaces k []) (generateNodes (n-1) (m-k)) | k <- [0..m]] 
 
addSpaces :: Integer -> [Char] -> [Char]
addSpaces 0 x = ""
addSpaces m x = x ++ " " ++ addSpaces (m - 1) x    

generateSpaceCombinations :: [Tree [Char]] -> [[[Char]]] -> [[[Char]]]
generateSpaceCombinations [] [] = []
generateSpaceCombinations [] z = z
generateSpaceCombinations [Empty] [] = []
generateSpaceCombinations [Empty] z = z
generateSpaceCombinations ((Node a b):c) [] = if length c == 0 then generateSpaceCombinations b [[a]] else generateSpaceCombinations b [[a]] ++ generateSpaceCombinations c []
generateSpaceCombinations ((Node a b):c) [z] = if length c == 0 then generateSpaceCombinations b [(z ++ [a])] else generateSpaceCombinations b [(z ++ [a])] ++ generateSpaceCombinations c [z]