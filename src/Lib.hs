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
import System.IO.Unsafe
import Control.Applicative
import Data.IORef
import Control.Exception
import Data.Foldable
import qualified Data.Map as M
type Board = A.Array (Int,Int) (Maybe Bool)

board :: Board
board = A.listArray ((0,0),(24,24)) $ replicate (25^2) Nothing


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
-- rs'' = mkRestricitons
--         [ [3,1,1,3]
--         , [3,1,3,3,1]
--         , [3,1,5,3]
--         , [2,6,1]
--         , [1,6,2]
--         , [3,1,3]
--         , [1,2,2]
--         , [1,1,2,1]
--         , [1,1,2,1]
--         , [3,1,4]
--         , [1,1,3]
--         , [1,2,3]
--         , [2,5]
--         , [5,5]
--         ,[3,3]

--         ]
--         [ [4,2,2]
--         , [5,3,2]
--         , [3,2]
--         , [1,3,1]
--         , [3,1,1,3]
--         , [10]
--         , [9]
--         , [5,1]
--         , [4]
--         , [2,1]
--         , [4,3]
--         , [2,1,3]
--         , [3,1,7]
--         , [1,3,5]
--         , [7,5]

--         ]
rs'' = mkRestricitons
    [[6]
    ,[6]
    ,[4]
    ,[1]
    ,[10]
    ,[4,10]
    ,[4,1,1,1]
    ,[4,1,1,1]
    ,[2,1,1,1]
    ,[2,1,1,1]
    ,[12,4]
    ,[1,9,1,4]
    ,[2,9,8]
    ,[2,9,8]
    ,[2,1,1,8]
    ,[1,1,1,8]
    ,[19]
    ,[1,1,2]
    ,[14,3,3]
    ,[3,3,2,5,2]
    ,[3,3,1,3,8]
    ,[3,5,5,7]
    ,[3,2,2,2,2,3,3]
    ,[5,5,5]
    ,[3,3,3]]
    [[2]
    ,[3]
    ,[3,4]
    ,[5,3]
    ,[1,4,3]
    ,[7,1,5]
    ,[3,4,1,1,2,2]
    ,[9,1,1,5]
    ,[2,9,1,2,3]
    ,[2,3,4,1,3]
    ,[3,4,1,2,3]
    ,[3,4,1,1,5]
    ,[2,4,1,1,2,2]
    ,[2,7,1,5]
    ,[2,2,1,1,2,3]
    ,[1,13,3]
    ,[2,5,1,3]
    ,[2,6,5]
    ,[2,5,7]
    ,[13,4,2]
    ,[2,7,7]
    ,[2,8,5]
    ,[15,3]
    ,[2,2]
    ,[2]
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
    hPutStrLn stdout $ concat $ intersperse "\n" $ map (ppRow b) $ let rcount = snd (snd $ A.bounds b) in [rcount,rcount-1..0]
    

data Direction = Vertical | Horizontal deriving (Eq,Show)

data Restriction = Restriction [Int] Direction Int 
                    deriving (Show)

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





certainCellsForRestriction :: Board -> Restriction -> M.Map (Int,Int) Bool
certainCellsForRestriction b (Restriction seq dir x) = 
    let possibilities = allPossibilities seq (case dir of Vertical -> height b; Horizontal -> width b)  
        matchingPossibilities = filter (flip matching (getCells b dir x)) possibilities
        dicts = map M.fromList $ map (case dir of Vertical -> zip (zip (repeat x) [height b - 1, height b - 2..]); Horizontal -> zip (zip [0..] (repeat x))) matchingPossibilities
        subtractDict :: (Ord a, Eq b) => M.Map a b -> M.Map a b -> M.Map a b
        subtractDict a b = M.filterWithKey (\k v-> M.lookup k b == Just v) a
    in if null matchingPossibilities then mempty else foldr1 subtractDict dicts



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
        

solve :: Game -> IO Board
solve (Game _b _rs) = do
    observeT $ (go _b)
    where 
        go :: Board -> LogicT IO Board
        go b    | all (/=Nothing) $ A.elems b = return b  
                | otherwise = do
                    liftIO (progressBar b)
                    
                    v <- foldl interleave mempty $ sequence [fillCertainCells, dfs] b
                    go v
        

        progressBar :: Board -> IO ()
        progressBar b = do
            let n = length $ filter (\(i,v) -> v /= Nothing) $ A.assocs b
            let successLength = floor (40.0 * ((\(x,y) -> fromIntegral @Int @Double n / fromIntegral @Int @Double (x*y)) $ snd $ A.bounds _b))
        
            liftIO $ putStrLn (replicate successLength '=' ++ replicate (40-successLength) '_')
            

        dfs :: Board -> LogicT IO Board
        dfs b = do            
            (ix:_) <- return $ map (\(k,_) -> k) $ filter (\(k,v) -> v == Nothing) $ A.assocs b            
            v <- msum [pure True, pure False]            
            let newBoard = b A.// [(ix, Just v)]
            guard (checkGame (Game newBoard _rs))
            return newBoard

        fillCertainCells :: Board -> LogicT IO Board
        fillCertainCells b = do
            let certainCells = (map (\(a,b) -> (a,Just b)) $ M.toList $ foldl1 M.union $ map (certainCellsForRestriction b) _rs)
            guard (any (\(i,v) -> b A.! i == Nothing) certainCells)
            return $ (b A.// certainCells)
            

