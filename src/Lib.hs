{-# LANGUAGE LambdaCase, ViewPatterns, TypeApplications #-}

module Lib where
import Data.List
import Control.Monad.Logic
import Control.Monad.Logic.Class
import qualified Data.Array as A
import Control.Monad
import System.IO
import Control.Parallel
import Control.Parallel.Strategies
type Board = A.Array (Int,Int) (Maybe Bool)

board :: Board
board = A.listArray ((0,0),(14,14)) $ replicate (15^2) Nothing


width :: Board -> Int
width b = succ $ fst $ snd $ A.bounds b

height :: Board -> Int
height b = succ $ snd $ snd $ A.bounds b

rs :: [Restriction]
rs = 
    [ Restriction [1] Vertical 0
    , Restriction [3] Vertical 1
    , Restriction [5] Vertical 2
    , Restriction [3] Vertical 3
    , Restriction [1] Vertical 4
    , Restriction [1] Horizontal 0
    , Restriction [3] Horizontal 1
    , Restriction [5] Horizontal 2
    , Restriction [3] Horizontal 3
    , Restriction [1] Horizontal 4
    ]


-- rs' = mkRestricitons 
--         [ [2,2]
--         , [2,2]
--         , [1,1,1,1]
--         ,[1,4]
--         ,[4]
--         ,[1]
--         ,[3]
--         ,[3,3,1]
--         ,[3,6]
--         ,[3,6]]
        
--         [[4]
--         ,[4]
--         ,[4,4]
--         ,[2]
--         ,[1,2]
--         ,[3]
--         ,[3,3]
--         ,[3,3]
--         ,[5,2]
--         ,[2,2,3]]
rs'' = mkRestricitons
        [ [3,1,1,3]
        , [3,1,3,3,1]
        , [3,1,5,3]
        , [2,6,1]
        , [1,6,2]
        , [3,1,3]
        , [1,2,2]
        , [1,1,2,1]
        , [1,1,2,1]
        , [3,1,4]
        , [1,1,3]
        , [1,2,3]
        , [2,5]
        , [5,5]
        ,[3,3]

        ]
        [ [4,2,2]
        , [5,3,2]
        , [3,2]
        , [1,3,1]
        , [3,1,1,3]
        , [10]
        , [9]
        , [5,1]
        , [4]
        , [2,1]
        , [4,3]
        , [2,1,3]
        , [3,1,7]
        , [1,3,5]
        , [7,5]

        ]


mkRestricitons :: [[Int]] -> [[Int]] -> [Restriction]
mkRestricitons horizontal vertical = v ++ h
    where
        v = map (\(i,x) -> Restriction x Vertical i) $ zip [0..] vertical
        h = map (\(i,x) -> Restriction x  Horizontal i) $ zip [length horizontal -1, length horizontal -2..]  horizontal





ppRow :: Board -> Int -> String
ppRow b x = intersperse ' ' [case a of Nothing -> '_'; Just True -> 'X'; Just False -> '_' | ((_,i),a) <- A.assocs b, i == x]

pp :: Board -> IO ()
pp b = do
    hPutStrLn stdout $ concat $ intersperse "\n" $ map (ppRow b) [14,13..0]
    

data Direction = Vertical | Horizontal deriving Eq

data Restriction = Restriction [Int] Direction Int

data Game = Game 
        { _board :: Board
        , _restrictions :: [Restriction]
        }

allPossibilities :: [Int] -> Int -> [[Bool]]
allPossibilities seq size = map (take size) $ allPossibilities' seq size
    where            
        allPossibilities' [] size = pure $ replicate size False
        allPossibilities' (sequence:sequences) size = do
            start <- [0..size-sequence]
            let part = (replicate start False) ++ (replicate sequence True) ++ [False]
            map (part++) $ allPossibilities' sequences (size - (start + sequence + 1))


restrictionForRow :: Game -> Int -> Maybe Restriction
restrictionForRow (Game _ restrictions) row = find (\r@(Restriction _ d x) -> d == Horizontal && x == row) restrictions

restrictionForColumn :: Game -> Int -> Maybe Restriction
restrictionForColumn (Game _ restrictions) col = find (\r@(Restriction _ d x) -> d == Vertical && x == col) restrictions



matching :: [Bool] -> [Maybe Bool] -> Bool
matching as bs = all id [a==b | (a, Just b) <- zip as bs]


checkRestriction :: Restriction -> [Maybe Bool] -> Int -> Bool
checkRestriction (Restriction sequences _ _) cells size = any (flip matching cells) $ allPossibilities sequences size

getCells :: Board -> Direction -> Int -> [Maybe Bool]
getCells _board Vertical x = reverse $ map snd $ sortOn fst [(y',a) | ((x',y'),a) <- A.assocs _board, x' == x]
getCells _board Horizontal x = map snd $ sortOn fst [(x',a) | ((x',y'),a) <- A.assocs _board, y' == x]

checkGame :: Game -> Bool
checkGame (Game _board _restrictions) = all (\r@(Restriction _ dir x) -> checkRestriction r (getCells _board dir x) (case dir of Vertical -> height _board; Horizontal -> width _board)) _restrictions 
        

solve :: Game -> Board
solve (Game _b _rs) = do
    observe $ (go _b $ A.indices _b)
    where 
        go :: Board -> [(Int,Int)] -> Logic Board
        go b [] = return b
        go b (ix:ixs) = do
            let values = filter (\i -> checkGame (Game i _rs)) $ map (\i -> b A.// [(ix, Just i)]) [True, False]--[LogicT (\x y -> x True y), LogicT (\x y -> x False y)]
            case values of
                [] -> mzero
                [x] -> go x ixs
                [x,y] -> let r1 = go x ixs
                             r2 = go y ixs
                         in interleave r1 r2

