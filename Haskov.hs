module Haskov where

import System.Random    
import Data.Matrix (Matrix, extendTo, setElem, getElem)
import qualified Data.Matrix as Mat
import Data.Map.Strict (Map, (!), keys)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec

-- HaskovMatrix Type --
data HaskovMatrix a = HaskovMatrix { hmap :: Map a Int
                                     , hmatrix :: Matrix Double }
                                     
instance (Show a, Ord a) => Show (HaskovMatrix a) where 
    show haskov = show $ toList haskov 

-- Query --
lookUp :: (Ord a) => a -> a -> HaskovMatrix a -> Maybe Double
lookUp i j (HaskovMatrix hmap hmatrix)
    | hasI && hasJ = Just (getElem (hmap ! i) (hmap ! j) hmatrix)
    | otherwise    = Nothing
    where
        hasI = Map.member i hmap
        hasJ = Map.member j hmap

size :: (Ord a) => HaskovMatrix a -> Int
size (HaskovMatrix hmap hmatrix) = Map.size hmap

null :: (Ord a) => HaskovMatrix a -> Bool
null (HaskovMatrix hmap hmatrix) = Map.null hmap 

member :: (Ord a) => a -> a -> HaskovMatrix a -> Bool
member i j (HaskovMatrix hmap hmatrix)
    | (Map.member i hmap) && (Map.member j hmap) = True
    | otherwise = False

notMember :: (Ord a) => a -> a -> HaskovMatrix a -> Bool
notMember i j haskov = not (member i j haskov)

states :: (Ord a) => HaskovMatrix a -> [a]
states (HaskovMatrix hmap hmatrix) = keys hmap

-- Construction --         
empty :: HaskovMatrix a
empty = HaskovMatrix (Map.empty) (Mat.zero 0 0)

singleton :: (Ord a) => a -> Double -> HaskovMatrix a
singleton a n = 
    HaskovMatrix (Map.singleton a 1) (Mat.matrix 1 1 (\(i,j) -> n))

-- insert --
insert :: (Ord a) => a -> a -> Double -> HaskovMatrix a -> HaskovMatrix a
insert i j n (HaskovMatrix hmap hmatrix) =
    let newMap = hmapInsert i j hmap
    in HaskovMatrix (newMap) (hmatrixInsert i j n hmatrix newMap)

-- Chains -- 
walk :: (Ord a) => HaskovMatrix a -> Int -> IO [a]
walk haskov n = do 
    gen <- getStdGen
    let rand = randomR (0, (size haskov) - 1) gen :: (Int, StdGen)
        start = Vec.fromList (states haskov) !? (fst rand)
    return (walker haskov n start (snd rand))

walker :: (Ord a) => HaskovMatrix a -> Int -> Maybe a -> StdGen -> [a]
walker _ _ Nothing _ = []
walker (HaskovMatrix hmap hmatrix) 0 s gen = []
walker (HaskovMatrix hmap hmatrix) n s gen = []
    --let rand = randomR

-- Lists --    
toList :: (Ord a) => HaskovMatrix a -> [((a, a), Double)]
toList haskov =
    let pairs = rowColPairs haskov
        values = map (rowColValue haskov) pairs
    in zip pairs values

fromList :: (Ord a) => [((a, a), Double)] -> HaskovMatrix a
fromList [] = empty
fromList tuples = foldl (\acc x -> insert (fst . fst $ x) (snd . fst $ x) (snd x) acc) empty tuples
        
-- Helper Functions --
hmapInsert :: (Ord a) => a -> a -> Map a Int -> Map a Int
hmapInsert i j hmap
    | hasI && (not hasJ) = Map.insert j (mapSize+1) hmap 
    | (not hasI) && hasJ = Map.insert i (mapSize+1) hmap
    | (not hasI) && (not hasJ) = 
        Map.insert j (mapSize+1) (Map.insert i (mapSize+1) hmap)
    | otherwise = hmap
    where
        mapSize = Map.size hmap
        hasI = Map.member i hmap
        hasJ = Map.member j hmap
    
hmatrixInsert :: (Ord a) => a -> a -> Double -> Matrix Double -> Map a Int -> Matrix Double
hmatrixInsert i j n hmatrix hmap
    | mapSize > matrixSize =
        setElem n (hmap ! i, hmap ! j) (extendTo 0 mapSize mapSize hmatrix)
    | otherwise = setElem n (hmap ! i, hmap ! j) hmatrix
    where
        mapSize = Map.size hmap
        matrixSize = Mat.nrows hmatrix

showPairs :: (Ord a, Show a) => HaskovMatrix a -> String
showPairs haskov = show . rowColPairs $ haskov

rowColValue :: (Ord a) => HaskovMatrix a -> (a, a) -> Double
rowColValue (HaskovMatrix hmap hmatrix) (i, j) =
    getElem (hmap ! i) (hmap ! j) hmatrix 

rowColPairs :: (Ord a) => HaskovMatrix a -> [(a, a)]
rowColPairs (HaskovMatrix hmap hmatrix) = 
    [(i, j) | i <- (keys hmap), j <- (keys hmap)]
    