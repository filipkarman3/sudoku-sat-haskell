{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.List (delete, sort, transpose)
import Data.Set (fromList, toList)
import qualified SAT.Mios as SAT

import qualified Data.Map as Map

import Prelude hiding (lookup)

main :: IO ()
main = do
    contents <- readFile "data/sudoku.txt"
    let clues  = map words . lines $ contents
    let outraw = calc . format $ clues
    let (expr, VarMap i vm) = unformat outraw

    sol <- SAT.solveSAT (SAT.CNFDescription i (length expr) "") expr 

    -- print sol

    -- print clues
    -- mapM_ print $ cleanSudokuSolution sol vm

    putStrLn $ showSudoku clues sol vm
    -- result <- satSolve expr
    -- mapM_ print expr
    -- writeFile "data/out.txt" (unlines . map (++ " 0") . map unwords $ out)
    -- writeFile "data/out.txt" . unlines $ map show expr

showSudoku :: [[String]] -> [Int] -> Map.Map BV Int -> String
showSudoku clues sol m = showSudoku' sol'' where
    sol' = cleanSudokuSolution sol m
    sol'' = reverse . sort $ clues' ++ sol'
    clues' = map clueReader clues

cleanSudokuSolution :: [Int] -> Map.Map BV Int -> [BV]
cleanSudokuSolution sol m = Map.foldrWithKey f [] m where
    f :: BV -> Int -> [BV] -> [BV]
    f k v l
        |  v `elem` sol && not (isNeg k) = k:l
        | -v `elem` sol &&      isNeg k  = neg k:l
        | otherwise                      = l

showSudoku' :: [BV] -> String
showSudoku' l = (foldr f "" l') ++ " |" where
    f :: BV -> String -> String
    f (Var x y n) s
        | x == 0 && y == 0         = "\n| "   ++ show (n+1) ++ s
        | y `rem` 3 == 0 && x == 0 = " |\n" ++ breakline ++ "\n| " ++ show (n+1) ++ s
        | x == 0                   = " |\n| " ++ show (n+1) ++ s
        | x `rem` 3 == 0           = " | "    ++ show (n+1) ++ s
        | otherwise = " " ++ show (n+1) ++ s
        -- | y == 0 && x == 0         = s ++ "| " ++ show (Var x y (n+1))
        -- | x `rem` 3 == 0 && y == 0 = s ++ " |\n" ++ breakline ++ "\n| " ++ show (Var x y (n+1))
        -- | y == 0                   = s ++ " |\n| " ++ show (Var x y (n+1))
        -- | y `rem` 3 == 0           = s ++ " | " ++ show (Var x y (n+1))
        -- | otherwise                = s ++ " " ++ show (Var x y (n+1))
    f (Neg _) _ = ""
    
    breakline = concat (replicate 3 "|-------") ++"|"
    
    l' = concat [reverse $ makeSubList y' l | y'<-[0..8]]
    
    makeSubList y' l = filter (\(Var x y n) -> y' == y) l

isNeg :: BV -> Bool
isNeg (Neg _) = True
isNeg _       = False

removeDuplicates :: (Eq a, Ord a) => [a] -> [a]
removeDuplicates = toList . fromList

calc :: [BV] -> CNF
calc x = removeDuplicates $ applyRule x sudokucnf


data BV = Var Int Int Int | Neg BV deriving (Show, Eq, Ord)
type DJN = [BV]
type CNF = [DJN]

sudokucnf :: CNF
sudokucnf = everyTileSomeNum
    ++ noTileManyNums
    ++ everyRowEveryNum
    ++ everyRowNoDupeNum
    ++ everyColEveryNum
    ++ everyColNoDupeNum
    ++ everySquareEveryNum
    ++ everySquareNoDupeNum

everyTileSomeNum :: CNF
everyTileSomeNum = [ clause x y | x <- [0..8], y <- [0..8] ] where
    clause x y = [ Var x y n | n <- [0..8] ]

noTileManyNums :: CNF
noTileManyNums = [ clause x y n m | x <- [0..8], y <- [0..8], n <- [0..8], m <- [0..8], n<m ] where
    clause x y n m = [Neg (Var x y n), Neg (Var x y m)]

everyRowEveryNum :: CNF
everyRowEveryNum = [ clause y n | y <- [0..8], n <- [0..8] ] where
    clause y n = [ Var x y n | x <- [0..8] ]

everyRowNoDupeNum :: CNF
everyRowNoDupeNum = [ clause y n x1 x2 | y <- [0..8], n <- [0..8], x1 <- [0..8], x2 <- [0..8], x1<x2 ] where
    clause y n x1 x2 = [Neg (Var x1 y n), Neg (Var x2 y n)]

everyColEveryNum :: CNF
everyColEveryNum = [ clause x n | x <- [0..8], n <- [0..8] ] where
    clause x n = [ Var x y n | y <- [0..8] ]

everyColNoDupeNum :: CNF
everyColNoDupeNum = [ clause x n y1 y2 | x <- [0..8], n <- [0..8], y1 <- [0..8], y2 <- [0..8], y1<y2 ] where
    clause x n y1 y2 = [Neg (Var x y1 n), Neg (Var x y2 n)]

everySquareEveryNum :: CNF
everySquareEveryNum = [ clause x' y' n | x' <- [0..2], y' <- [0..2], n <- [0..8] ] where
    clause x' y' n = [ Var x y n | x<-[x'*3..x'*3+2], y<-[y'*3..y'*3+2] ]

{-
the majority of the following condition is alr checked for
consider:
| X A B |
| Y _ _ |
| Z _ _ |
X, Y and Z must be diff since a column can't contain 2 nums
Similarly, X, A and B must be diff
This only leaves tiles that are not on the same rows and columns
-}
everySquareNoDupeNum :: CNF
everySquareNoDupeNum = [ clause x1 y1 x2 y2 n |
        x' <- [0..2], y' <- [0..2], n <- [0..8],
        x1 <- [x'*3..x'*3+2], y1 <- [y'*3..y'*3+2],
        x2 <- [x'*3..x'*3+2], y2 <- [y'*3..y'*3+2],
        x1 /= x2, y1 /= y2,
        x1 < x2,  y1 < y2 ] where
    clause x1 y1 x2 y2 n = [Neg (Var x1 y1 n), Neg (Var x2 y2 n)]

format :: [[String]] -> [BV]
format s = varGenerator $ map clueReader s

clueReader :: [String] -> BV
clueReader [x,y,n] = Var (read x) (read y) (read n-1)

-- given that tile x,y has colour n, it cannot have colour m, m/=n
-- hence can create the variable x,y,m and set it to false
varGenerator :: [BV] -> [BV]
varGenerator vs = foldr (\x l -> varGenerator' x++l) [] vs

varGenerator' :: BV -> [BV]
varGenerator' (Var x y n) = (Var x y n) : [Neg (Var x y m) | m <- [0..8], m/=n]


-- iterate over cnf
-- if bv in djn: remove djn
-- if neg bv in djn: remove neg bv from djn

-- Aforementioned is wrong, should be 4 rules:
-- if x in djn, remove djn
-- if -x in djn, remove -x from djn
applyRule :: [BV] -> CNF -> CNF
applyRule vs cnf = foldr f cnf vs where
    f :: BV -> CNF -> CNF
    f v cnf' = applyRuleToCnf v cnf'

-- foldr :: (a -> b -> b) -> b -> [a] -> b

applyRuleToCnf :: BV -> CNF -> CNF
applyRuleToCnf v cnf = foldr f [] cnf where
    f :: DJN -> CNF -> CNF
    f djn cnf' = case applyRuleToDjn v djn of
        Just djn' -> djn':cnf'
        Nothing   -> delete djn cnf'

applyRuleToDjn :: BV -> DJN -> Maybe DJN
applyRuleToDjn v djn
    | v `elem` djn     = Nothing
    | neg v `elem` djn = Just $ delete (neg v) djn
    | otherwise        = Just djn

{-
 = case (v `elem` djn, v) of
    (True, Var x y n) -> Nothing
    (True, Neg v')    -> Just $ delete (Neg v') djn
    (False, _)        -> Just djn
-}

neg :: BV -> BV
neg (Var x y n) = Neg (Var x y n)
neg (Neg v)     = v

data VarMap = VarMap Int (Map.Map BV Int)

-- unformat cnf = map showdjn cnf where
--     showdjn = map show
unformat :: CNF -> ([[Int]], VarMap)
unformat cnf = foldWith f [] (VarMap 1 Map.empty) cnf where
    f :: DJN -> [[Int]] -> VarMap -> ([[Int]], VarMap)
    f djn s m = let (s', m') = unformatDjn djn m in (s':s, m')

foldWith :: (a -> b -> c -> (b, c)) -> b -> c -> [a] -> (b, c)
foldWith f b c []     = (b, c)
foldWith f b c (a:as) = foldWith f b' c' as where
    (b', c') = f a b c

unformatDjn :: DJN -> VarMap -> ([Int], VarMap)
unformatDjn djn m = foldWith f [] m djn where
    f :: BV -> [Int] -> VarMap -> ([Int], VarMap)
    f v l m = let (v', m') = unformatVar v m in (v':l, m')

unformatVar :: BV -> VarMap -> (Int, VarMap)
unformatVar v m = case v of
    Var x y z -> lookup (Var x y z) m
    Neg v     -> let (s, m') = unformatVar v m in (- s, m')

lookup :: BV -> VarMap -> (Int, VarMap)
lookup v (VarMap i m) = case Map.lookup v m of
    Just s  -> (s,  VarMap i m)
    Nothing -> (i, VarMap (i+1) m') where
        m' = Map.insert v i m

printcnf :: [[String]] -> IO ()
printcnf cnf = forM print cnf

printcnf' :: CNF -> IO ()
printcnf' cnf = forM (print . show) cnf

forM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
forM f xs = foldr (\x io -> f x >> io) (pure mempty) xs