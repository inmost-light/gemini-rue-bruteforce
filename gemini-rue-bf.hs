import Data.Bits (xor, testBit, setBit)
import Data.List (elemIndex, minimumBy)
import Data.Maybe (fromJust)
import Data.Function (on)

type Goal = Int
type Node = Int
type Solution = [Node]
type SolutionIndices = [Int]

bitsToNode:: [Int] -> Node
bitsToNode = foldl setBit 0 . map (subtract 1)
  
nodes :: [Node]
nodes = map bitsToNode [
  [1,2,4], [1,2,3,4,5], [2,3,6], [1,2,4,7], [2,5,6,8],
  [3,5,6,8,9], [4,7,8,10], [5,6,7,8], [6,9], [7,10]
  ]
        
allGoals :: [[Goal]]
allGoals = map (map bitsToNode) [
  [ [2,3,4,7,10], [3,6,7,8,10] ],
  [ [1,2,3,4,5,6,7,8,9,10] ]
  ]

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subs ++ map (x:) subs where subs = subsets xs

xors :: [Int] -> Int
xors = foldl xor 0

shortestSolutionIndices :: [Node] -> [Goal] -> SolutionIndices
shortestSolutionIndices nodes goals =
  solutionIndices nodes shortestSolution
  where shortestSolution = minimumBy (compare `on` length) $ solutions nodes goals
  
solutions :: [Node] -> [Goal] -> [Solution]
solutions nodes goals = [s | s <- subsets nodes, xors s `elem` goals]

solutionIndices :: [Node] -> Solution -> SolutionIndices
solutionIndices nodes solution = [1 + (fromJust $ n `elemIndex` nodes) | n <- solution]

main = print $ map (shortestSolutionIndices nodes) allGoals
