module Haskov where

import System.Random
import Data.Map.Strict (Map, keys)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec
import qualified Numeric.Container as Con
import Numeric.LinearAlgebra.Data (Matrix)
import qualified Numeric.LinearAlgebra.Data as Lin
-- Markov Type --
data Markov a = Markov { hmap :: Map a Int
                                     , hmatrix :: Matrix Double }
                                     
--instance (Show a, Ord a) => Show (Markov a) where 
--    show haskov = show $ toList haskov 

-- Query --
lookUp :: (Ord a) => a -> a -> Markov a -> Maybe Double
lookUp i j (Markov hmap hmatrix)
    | hasI && hasJ = Just (hmatrix Lin.! (hmap Map.! i) Lin.! (hmap Map.! j))
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
empty = Markov (Map.empty) (Lin.matrix 0 [])

-- Lists --
fromList :: (Ord a) => [(a, [Double])] -> Markov a
fromList list = 
    let hmap = tohmap (fst . unzip $ list) 0 
        hmatrix = Lin.fromLists . snd . unzip $ list
    in  Markov hmap hmatrix
    where 
        tohmap [] n = Map.empty
        tohmap (i:sublist) n = Map.insert i n (tohmap sublist (n+1))

toList :: (Ord a) => Markov a -> [(a, [Double])]
toList (Markov hmap hmatrix) = 
    let k = keys hmap
        d = Lin.toLists hmatrix
    in zip k d
        
-- Chains -- 
walk :: (Ord a) => Markov a -> Int -> IO [a]
walk haskov n = do 
    gen <- getStdGen
    let rand = randomR (0, (size haskov) - 1) gen :: (Int, StdGen)
        start = Vec.fromList (states haskov) !? (fst rand)
    return (steps haskov n start (snd rand))

steps :: (Ord a) => Markov a -> Int -> Maybe a -> StdGen -> [a]
steps _ _ Nothing _ = []
steps _ 0 _ _ = []
steps (Markov hmap hmatrix) n (Just s) gen
    | (Con.sumElements row) == 0 = []
    | otherwise = choice : steps (Markov hmap hmatrix) (n-1) (Just choice) (snd rand) 
    where 
        rand = random gen
        row = hmatrix Lin.? [hmap Map.! s]
        choice = keys hmap !! randomStep row (fst rand) 0.0 0
        
randomStep :: Matrix Double -> Double -> Double -> Int -> Int
randomStep row rand total j
    | newTotal < rand = randomStep row rand newTotal newJ
    | otherwise       = j
    where
        newTotal = total + (row Lin.! 0 Lin.! j)
        newJ = (j + 1) `mod` (Lin.cols row)
{-    
Steady State: 
    1. m = number of states (keys of hmap)
    2. P = hmatrix
    3. Q = tanspose (Identity matrix size m - P), Q[0,0] += machine epsilon
    4. e = array of zeros size m, where last element is 1.0
    5. x = solve sparse linear system of Q and e (Ax = b), divided by the sum of its values
    6. match x values to corresponding states (keys)
    Note - Numeric.LinearAlgebra seems necessary
-} 

    
    