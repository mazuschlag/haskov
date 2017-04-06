module Haskov where

import System.Random
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seqq
import Data.Map.Strict (Map, keys, foldlWithKey', mapWithKey)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra (scale, luSolve, luPacked)
import qualified Numeric.LinearAlgebra as Lin
import Numeric.LinearAlgebra.Data (Matrix, matrix, col, cols, ident, tr, accum)
import qualified Numeric.LinearAlgebra.Data as Dat
import Numeric.LinearAlgebra.HMatrix (sumElements, cmap)
import qualified Numeric.LinearAlgebra.HMatrix as Hma

-- Markov Type --
type IndexMap a = Map a Int
type HMatrixMap = Map (Int, Int) Double

data Markov a = Markov {  imap :: IndexMap a
                        , hmap :: HMatrixMap }

instance (Show a, Ord a) => Show (Markov a) where 
    show haskov = show $ toList haskov 

-- Query --

-- Safe query
lookup :: (Ord a) => a -> a -> Markov a -> Maybe Double
lookup i j (Markov imap hmap) 
    | (Map.member i imap) && (Map.member j imap) = Just (hmap Map.! ((imap Map.!i), (imap Map.! j))) 
    | otherwise                                  = Nothing
    
-- Returns number of states
size :: (Ord a) => Markov a -> Int
size (Markov imap hmap) = Map.size imap

-- Returns true if the chain is empty
null :: (Ord a) => Markov a -> Bool
null (Markov imap hmap) = Map.null imap 

-- Returns true if input is a state of the chain
member :: (Ord a) => a -> Markov a -> Bool
member i (Markov imap hmap)
    | (Map.member i imap) = True
    | otherwise = False

-- Returns true if input is not a state of the chain
notMember :: (Ord a) => a -> Markov a -> Bool
notMember i haskov = not (member i haskov)

-- States of the Markov chain 
states :: (Ord a) => Markov a -> [a]
states (Markov imap hmap) = keys imap

statesI :: (Ord a) => Markov a -> IndexMap a
statesI (Markov imap hmap) = imap

-- Construction --

empty :: Markov a
empty = Markov (Map.empty) (Map.empty)

insert :: (Ord a) => a -> a -> Double -> Markov a -> Markov a
insert i j n (Markov imap hmap) 
    | hasI && (not hasJ) = Markov (Map.insert j s imap) (Map.insert (imap Map.! i, s) n hmap) 
    | (not hasI) && hasJ = Markov (Map.insert i s imap) (Map.insert (s, imap Map.! j) n hmap) 
    | (not hasI) && (not hasJ) = Markov (Map.insert j (s+1) (Map.insert i s imap)) (Map.insert (s, (s+1)) n hmap)
    | otherwise          = Markov imap (Map.insert (imap Map.! i, imap Map.! j) n hmap)
    where
        hasI = Map.member i imap
        hasJ = Map.member j imap
        s = Map.size imap
        
-- Lists --
fromList :: (Ord a) => [((a, a), Double)] -> Markov a
fromList list = 
    let imap = foldl' toimap Map.empty list
        hmap = foldl' (tohmap imap) Map.empty list
    in Markov imap hmap

toList :: (Ord a) => Markov a -> [((a, a), Double)]
toList (Markov imap hmap) =
    let rIMap = foldlWithKey' reverseIMap Map.empty imap
    in foldlWithKey' (combineImapHmap rIMap) [] hmap
        
combineImapHmap :: (Ord a) => Map Int a -> [((a, a), Double)] -> (Int, Int) -> Double -> [((a, a), Double)]
combineImapHmap rIMap acc (i, j) d = ((rIMap Map.! i, rIMap Map.! j), d) : acc

reverseIMap :: (Ord a) => Map Int a -> a -> Int -> Map Int a
reverseIMap  m state i = Map.insert i state m

-- Maps --

fromMap :: (Ord a) => Map (a, a) Double -> Markov a
fromMap mapp = 
    let imap = foldlWithKey' toimap' Map.empty mapp
        hmap = foldlWithKey' (tohmap' imap) Map.empty mapp
    in Markov imap hmap

-- Helpers --
-- for fromList
toimap :: (Ord a) => IndexMap a -> ((a, a), Double) -> IndexMap a
toimap acc tuple = 
    if Map.notMember i acc then Map.insert i (Map.size acc) acc else acc
    where i = fst . fst $ tuple

-- For fromMap
toimap' :: (Ord a) => IndexMap a -> (a, a) -> Double -> IndexMap a
toimap' acc tuple n = 
    if Map.notMember i acc then Map.insert i (Map.size acc) acc else acc
    where i = fst tuple

-- For fromList
tohmap :: (Ord a) => IndexMap a -> HMatrixMap -> ((a, a), Double) -> HMatrixMap
tohmap imap acc ((i, j), n) = Map.insert (imap Map.! i, imap Map.! j) n acc

-- For fromMap
tohmap' :: (Ord a) => IndexMap a -> HMatrixMap -> (a, a) -> Double -> HMatrixMap 
tohmap' imap acc (i, j) n = Map.insert (imap Map.! i, imap Map.! j) n acc

-- Matrices --

hmatrix :: (Ord a) => Markov a -> Matrix Double
hmatrix (Markov imap hmap) =
    let m = Map.size imap
        zeros = matrix m (replicate (m*m) 0)
        hlist = Map.toList hmap
    in  Dat.accum zeros (+) hlist

-- Chains -- 

walk :: (Ord a) => Markov a -> Int -> IO [a]
walk haskov n = do 
    gen <- getStdGen
    let mat = hmatrix haskov
        ss = tr . steady $ mat
    return (steps (statesI haskov) mat ss n gen)
    
steps :: (Ord a) => IndexMap a -> Matrix Double -> Matrix Double -> Int -> StdGen -> [a]
steps _ _ _ 0 _ = []
steps imap mat row n gen
    | (sumElements row) == 0 = []
    | otherwise = choice : steps imap mat newRow (n-1) (snd rand) 
    where 
        rand = random gen
        choice = keys imap !! randomStep row (fst rand) 0.0 0
        newRow = mat Dat.? [(imap Map.! choice)]
        
randomStep :: Matrix Double -> Double -> Double -> Int -> Int
randomStep row rand total j
    | newTotal < rand = randomStep row rand newTotal newJ
    | otherwise       = j
    where
        newTotal = total + (row Dat.! 0 Dat.! j)
        newJ = (j + 1) `mod` (cols row)
        
steadyState :: (Ord a) =>  Markov a -> [(a, Double)]
steadyState haskov = zip (states haskov) (concat . Dat.toLists $ steady (hmatrix haskov))
        
steady :: Matrix Double -> Matrix Double
steady mat =
    let m = Dat.rows mat -- Number of states
        q = tr $ ident m - mat -- Transpose (I-matrix size m - hmatrix)
        e = col ((replicate (m-1) 0) ++ [1.0]) -- col of zeros size m, last element is 1.0
        x = luSolve (luPacked q) e -- Solve linear system of Q and e (Qx = e)
    in  scale (1 / (sumElements x)) x  -- Divide x by sum of elements
    
normalize :: Markov a -> Markov a 
normalize (Markov imap hmap) = 
    let ss = foldlWithKey' sumSeq Seqq.empty hmap
    in Markov imap (mapWithKey (calcNorm ss) hmap)
    
calcNorm :: Seq Double -> (Int, Int) -> Double -> Double
calcNorm ss (i, j) n = n / (Seqq.index ss i)

sumSeq :: Seq Double -> (Int, Int) -> Double -> Seq Double
sumSeq s (i, j) n = 
    if Seqq.length s < (i+1) 
    then s |> n
    else Seqq.adjust (+n) i s

norm :: Matrix Double -> Matrix Double
norm row = cmap (/ (sumElements row)) row 

-- Rounding function -- 
roundDouble :: Double -> Int -> Double
roundDouble d n = (fromInteger $ round $ d * (10^n)) / (10.0^^n)

-- Machine Epsilon --

machineE :: Double
machineE = until ((== 1) . (+1)) (/2) 1
