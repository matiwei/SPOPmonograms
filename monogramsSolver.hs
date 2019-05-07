import Data.Char
import Debug.Observe
import Debug.Trace

lineTest1 = [(1,'*'),(2,'x'),(3,'x'),(2,'*')];
lineTest2 = [(1,'*'),(2,'*'),(3,'*'),(2,'*')];

solveLine  (size, xs)
                    |(lineLengthSum(xs)==size) = line(xs)
                    |(lineLengthSum(xs)>size) = error("Too long")
                    
insertIntoSimple :: (Int, [[Char]]) -> [[Char]]
insertIntoSimple (_,[]) = [];
insertIntoSimple (0,xs) = xs;
insertIntoSimple (spaces, xs) = 

insertHelper (pos, left)

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