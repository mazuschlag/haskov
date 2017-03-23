module Haskov where

import System.Random    
import Data.Matrix (Matrix, extendTo, setElem, getElem)
import qualified Data.Matrix as Mat
import Data.Map.Strict (Map, (!), keys)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec

-- Markov Type --
data Markov a = Markov { hmap :: Map a Int
                                     , hmatrix :: Matrix Double }
                                     
instance (Show a, Ord a) => Show (Markov a) where 
    show haskov = show $ toList haskov 

-- Query --
lookUp :: (Ord a) => a -> a -> Markov a -> Maybe Double
lookUp i j (Markov hmap hmatrix)
    | hasI && hasJ = Just (getElem (hmap ! i) (hmap ! j) hmatrix)
    | otherwise    = Nothing
    where
        hasI = Map.member i hmap
        hasJ = Map.member j hmap

size :: (Ord a) => Markov a -> Int
size (Markov hmap hmatrix) = Map.size hmap

null :: (Ord a) => Markov a -> Bool
null (Markov hmap hmatrix) = Map.null hmap 

member :: (Ord a) => a -> a -> Markov a -> Bool
member i j (Markov hmap hmatrix)
    | (Map.member i hmap) && (Map.member j hmap) = True
    | otherwise = False

notMember :: (Ord a) => a -> a -> Markov a -> Bool
notMember i j haskov = not (member i j haskov)

states :: (Ord a) => Markov a -> [a]
states (Markov hmap hmatrix) = keys hmap

-- Construction --         
empty :: Markov a
empty = Markov (Map.empty) (Mat.zero 0 0)

singleton :: (Ord a) => a -> Double -> Markov a
singleton a n = 
    Markov (Map.singleton a 1) (Mat.matrix 1 1 (\(i,j) -> n))

-- insert --
insert :: (Ord a) => a -> a -> Double -> Markov a -> Markov a
insert i j n (Markov hmap hmatrix) =
    let newMap = hmapInsert i j hmap
    in Markov (newMap) (hmatrixInsert i j n hmatrix newMap)

-- Chains -- 
walk :: (Ord a) => Markov a -> Int -> IO [a]
walk haskov n = do 
    gen <- getStdGen
    let rand = randomR (0, (size haskov) - 1) gen :: (Int, StdGen)
        start = Vec.fromList (states haskov) !? (fst rand)
    return (walker haskov n start (snd rand))

walker :: (Ord a) => Markov a -> Int -> Maybe a -> StdGen -> [a]
walker _ _ Nothing _ = []
walker _ 0 _ _ = []
walker (Markov hmap hmatrix) n (Just s) gen
    | (Vec.foldl (+) 0 row) == 0 = []
    | otherwise = choice : walker (Markov hmap hmatrix) (n-1) (Just choice) (snd rand) 
    where 
        rand = random gen
        row = Mat.getRow (hmap ! s) hmatrix
        choice = keys hmap !! randomStep row (fst rand) 0.0 0
        
randomStep :: Vector Double -> Double -> Double -> Int -> Int
randomStep vec rand total i 
    | newTotal < rand = randomStep vec rand newTotal newI
    | otherwise       = i
    where
        newTotal = total + (vec Vec.! i)
        newI = (i + 1) `mod` (Vec.length vec)

-- Lists --    
toList :: (Ord a) => Markov a -> [((a, a), Double)]
toList haskov =
    let pairs = rowColPairs haskov
        values = map (rowColValue haskov) pairs
    in zip pairs values

fromList :: (Ord a) => [((a, a), Double)] -> Markov a
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
    | otherwise            = setElem n (hmap ! i, hmap ! j) hmatrix
    where
        mapSize = Map.size hmap
        matrixSize = Mat.nrows hmatrix

showPairs :: (Ord a, Show a) => Markov a -> String
showPairs haskov = show . rowColPairs $ haskov

rowColValue :: (Ord a) => Markov a -> (a, a) -> Double
rowColValue (Markov hmap hmatrix) (i, j) =
    getElem (hmap ! i) (hmap ! j) hmatrix 

rowColPairs :: (Ord a) => Markov a -> [(a, a)]
rowColPairs (Markov hmap hmatrix) = 
    [(i, j) | i <- (keys hmap), j <- (keys hmap)]
    