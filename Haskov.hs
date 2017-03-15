module Haskov where
    
import Data.Matrix (Matrix, extendTo, setElem, getElem)
import qualified Data.Matrix as Matrix
import Data.Map.Strict (Map, (!), member, keys)
import qualified Data.Map.Strict as Map

data HaskovMatrix a = HaskovMatrix { hmap :: Map a Int
                                     , hmatrix :: Matrix Float }
                                     
instance (Show a, Ord a) => Show (HaskovMatrix a) where 
    show haskov = show $ toList haskov 
        
empty :: HaskovMatrix a
empty = HaskovMatrix (Map.empty) (Matrix.zero 0 0)

singleton :: (Ord a) => a -> Float -> HaskovMatrix a
singleton a n = 
    HaskovMatrix (Map.singleton a 1) (Matrix.matrix 1 1 (\(i,j) -> n))

insert :: (Ord a) => a -> a -> Float -> HaskovMatrix a -> HaskovMatrix a
insert i j n (HaskovMatrix hmap hmatrix) =
    let newMap = hmapInsert i j hmap
    in HaskovMatrix (newMap) (hmatrixInsert i j n hmatrix newMap)
    
lookUp :: (Ord a) => a -> a -> HaskovMatrix a -> Maybe Float
lookUp i j (HaskovMatrix hmap hmatrix)
    | hasI && hasJ = Just (getElem (hmap ! i) (hmap ! j) hmatrix)
    | otherwise    = Nothing
    where
        hasI = member i hmap
        hasJ = member j hmap

--walk :: (Ord a) => HaskovMatrix a -> [a]
--walk (HaskovMatrix hmap hmatrix) = 
    
toList :: (Ord a) => HaskovMatrix a -> [((a, a), Float)]
toList haskov =
    let pairs = rowColPairs haskov
        values = map (rowColValue haskov) pairs
    in zip pairs values

fromList :: (Ord a) => [((a, a), Float)] -> HaskovMatrix a
fromList _ = empty
        
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
        hasI = member i hmap
        hasJ = member j hmap
    
hmatrixInsert :: (Ord a) => a -> a -> Float -> Matrix Float -> Map a Int -> Matrix Float
hmatrixInsert i j n hmatrix hmap
    | mapSize > matrixSize =
        setElem n (hmap ! i, hmap ! j) (extendTo 0 mapSize mapSize hmatrix)
    | otherwise = setElem n (hmap ! i, hmap ! j) hmatrix
    where
        mapSize = Map.size hmap
        matrixSize = Matrix.nrows hmatrix

showPairs :: (Ord a, Show a) => HaskovMatrix a -> String
showPairs haskov = show . rowColPairs $ haskov

rowColValue :: (Ord a) => HaskovMatrix a -> (a, a) -> Float
rowColValue (HaskovMatrix hmap hmatrix) (i, j) =
    getElem (hmap ! i) (hmap ! j) hmatrix 

rowColPairs :: (Ord a) => HaskovMatrix a -> [(a, a)]
rowColPairs (HaskovMatrix hmap hmatrix) = 
    [(i, j) | i <- (keys hmap), j <- (keys hmap)]
    