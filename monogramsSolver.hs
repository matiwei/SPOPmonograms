import Data.Char

import Data.List 
import Debug.Trace

lineTest1 :: [(Int, Char)]
lineTest1 = [(1,'*'),(2,'x'),(3,'x'),(2,'*')];
lineTest2 :: [(Int, Char)]
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

line :: [(Int,Char)] -> [[Char]]
--line (xs) | trace ("line " ++ show xs) False = undefined
line [] = [""];
line [x] = [lineHelper(fst x, snd x)];
line (x:y:xs) = line [x] ++ colorEqInsert2((snd x),(snd y)) ++ line (y:xs);

colorEqInsert2 (a,b) = if (a==b) then [" "] else [""]

--generates strings with length of first arg from char from second
lineHelper :: (Int,Char) -> [Char]
lineHelper (0,_) = ""
lineHelper (count,letter) = letter : lineHelper(count-1, letter)

lineLengthSum [] = 0;
lineLengthSum [x] = fst(x);
lineLengthSum (x:y:xs) = fst(x) + colorEq(snd(x),snd(y)) + lineLengthSum(y:xs);
colorEq (a,b) = if (a==b) then 1 else 0


--creates tree with all space combinations
data Tree a = Empty | Node a [Tree a]

generateNodes :: Int -> Int -> [Tree [Char]]
generateNodes 0 _ = [Empty]
generateNodes 1 m = [Node (addSpaces m []) [Empty]]
generateNodes n m = [Node (addSpaces k []) (generateNodes (n-1) (m-k)) | k <- [0..m]] 
 
addSpaces :: Int -> [Char] -> [Char]
addSpaces 0 x = ""
addSpaces m x = x ++ " " ++ addSpaces (m - 1) x    

generateSpaceCombinations :: [Tree [Char]] -> [[[Char]]] -> [[[Char]]]
generateSpaceCombinations [] [] = []
generateSpaceCombinations [] z = z
generateSpaceCombinations [Empty] [] = []
generateSpaceCombinations [Empty] z = z
generateSpaceCombinations ((Node a b):c) [] = if length c == 0 then generateSpaceCombinations b [[a]] else generateSpaceCombinations b [[a]] ++ generateSpaceCombinations c []
generateSpaceCombinations ((Node a b):c) [z] = if length c == 0 then generateSpaceCombinations b [(z ++ [a])] else generateSpaceCombinations b [(z ++ [a])] ++ generateSpaceCombinations c [z]

createSpaceList :: Int -> Int -> [[[Char]]]
createSpaceList a b = generateSpaceCombinations (generateNodes a b) []

--create all line or column combinations
generateAllLineCombinations :: [[Char]] -> [[[Char]]]-> [[[Char]]]
generateAllLineCombinations x [] = []
generateAllLineCombinations x (y:ys) = [combineLineWithSpaces x y 0] ++ generateAllLineCombinations x ys

combineLineWithSpaces :: [[Char]] -> [[Char]] -> Int -> [[Char]]
combineLineWithSpaces [] _ _ = []
combineLineWithSpaces (x:xs) (y:ys) z   | odd z = [addSpacesToEvenIndexes x y z] ++ combineLineWithSpaces xs ys (z + 1)
                                        | otherwise = [addSpacesToEvenIndexes x y z] ++ combineLineWithSpaces xs (y:ys) (z + 1)       

addSpacesToEvenIndexes :: [Char] -> [Char] -> Int -> [Char]
addSpacesToEvenIndexes x y a    | odd a = x 
                                | otherwise = x ++ y

computeInterspacesToFill :: [[Char]] -> Int
computeInterspacesToFill a = div (length a - 1) 2 + 1 

generateLineAndSpaces :: [(Int,Char)] -> Int -> [[[Char]]]
generateLineAndSpaces [] _ = []
generateLineAndSpaces pattern size = generateAllLineCombinations a (createSpaceList (computeInterspacesToFill a) b)
    where   a = lineSolver pattern
            b = size - (lineLengthSum pattern)
            

--Instruction: Use "generateLineAndSpaces pattern size" to generate all possible combinations of line and spaces