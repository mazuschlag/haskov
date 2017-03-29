module Haskov where

import System.Random
import Data.Map.Strict (Map, keys)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vec
import Numeric.Container (sub, scale)
import qualified Numeric.Container as Con
import Numeric.LinearAlgebra.Data (Matrix, matrix, col, ident, tr)
import qualified Numeric.LinearAlgebra.Data as Lin
import Numeric.LinearAlgebra.Algorithms (linearSolve)
import qualified Numeric.LinearAlgebra.Algorithms as Alg
import Numeric.LinearAlgebra.HMatrix (sumElements)
import qualified Numeric.LinearAlgebra.HMatrix as Hma

-- Markov Type --
data Markov a = Markov { hmap :: Map a Int
                                     , hmatrix :: Matrix Double }
                                     
instance (Show a, Ord a) => Show (Markov a) where 
    show haskov = "fromList " ++ (show $ toList haskov) 

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
empty = Markov (Map.empty) (matrix 0 [])

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
    return (steps haskov (tr . steady $ haskov) n gen)
    
steps :: (Ord a) => Markov a -> Matrix Double -> Int -> StdGen -> [a]
steps _ _ 0 _ = []
steps (Markov hmap hmatrix) row n gen
    | (sumElements row) == 0 = []
    | otherwise = choice : steps (Markov hmap hmatrix) newRow (n-1) (snd rand) 
    where 
        rand = random gen
        choice = keys hmap !! randomStep row (fst rand) 0.0 0
        newRow = hmatrix Lin.? [(hmap Map.! choice)]
        
randomStep :: Matrix Double -> Double -> Double -> Int -> Int
randomStep row rand total j
    | newTotal < rand = randomStep row rand newTotal newJ
    | otherwise       = j
    where
        newTotal = total + (row Lin.! 0 Lin.! j)
        newJ = (j + 1) `mod` (Lin.cols row)
        
steadyState :: (Ord a) =>  Markov a -> [(a, Double)]
steadyState haskov = zip (states haskov) (concat . Lin.toLists $ steady haskov)
        
steady :: (Ord a) => Markov a -> Matrix Double
steady (Markov hmap hmatrix) =
    let m = length . keys $ hmap -- Number of states
        q = tr $ sub (ident m) hmatrix -- Transpose (I-matrix size m - hmatrix)
        e = col ((replicate (m-1) 0) ++ [1]) -- col of zeros size m, last element is 1.0
        x = linearSolve q e -- Solve linear system of Q and e (Qx = e)
    in  scale (1 / (sumElements x)) x  -- Divide x by sum of elements

-- Machine Epsilon    
machineE :: Double
machineE = until ((== 1) . (+1)) (/2) 1
