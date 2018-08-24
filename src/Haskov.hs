module Haskov where

import System.Random
import Data.Tuple (swap)
import Data.List (foldl')
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seqq
import Data.Map.Strict (Map, keys, foldlWithKey', foldrWithKey', mapWithKey)
import qualified Data.Map.Strict as Map
import Numeric.LinearAlgebra (scale, luSolve, luPacked)
--import qualified Numeric.LinearAlgebra as Lin
import Numeric.LinearAlgebra.Data (Matrix, matrix, col, cols, ident, tr)
import qualified Numeric.LinearAlgebra.Data as Dat
import Numeric.LinearAlgebra.HMatrix (sumElements, cmap)
import qualified Numeric.LinearAlgebra.HMatrix as Hma

--------------------------------------------------------------------------------

-- Markov Type --
type IndexMap a = Map a Int
type ReverseIndexMap a = Map Int a
type HMatrixMap = Map (Int, Int) Double

data Markov a = Markov {  imap :: IndexMap a
                        , hmap :: HMatrixMap }

instance (Show a, Ord a) => Show (Markov a) where
    show haskov = show $ toList haskov

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- Construction --
empty :: Markov a
empty = Markov (Map.empty) (Map.empty)

insert :: (Ord a) => a -> a -> Double -> Markov a -> Markov a
insert i j d (Markov imap hmap)
    | hasI && (not hasJ) = Markov (Map.insert j s imap) (Map.insert (imap Map.! i, s) d hmap)
    | (not hasI) && hasJ = Markov (Map.insert i s imap) (Map.insert (s, imap Map.! j) d hmap)
    | (not hasI) && (not hasJ) && (i /= j) = Markov (Map.insert j (s+1) (Map.insert i s imap)) (Map.insert (s, (s+1)) d hmap)
    | (not hasI) && (not hasJ) && (i == j) = Markov (Map.insert i s imap) (Map.insert (s, s) d hmap)
    | otherwise          = Markov imap (Map.insert (imap Map.! i, imap Map.! j) d hmap)
    where
        hasI = Map.member i imap
        hasJ = Map.member j imap
        s = Map.size imap

insertWith :: (Ord a) => (Double -> Double -> Double) -> a -> a -> Double -> Markov a -> Markov a
insertWith f i j d (Markov imap hmap)
    | hasI && (not hasJ) = Markov (Map.insert j s imap) (Map.insert (imap Map.! i, s) d hmap)
    | (not hasI) && hasJ = Markov (Map.insert i s imap) (Map.insert (s, imap Map.! j) d hmap)
    | (not hasI) && (not hasJ) && (i /= j) = Markov (Map.insert j (s+1) (Map.insert i s imap)) (Map.insert (s, (s+1)) d hmap)
    | (not hasI) && (not hasJ) && (i == j) = Markov (Map.insert i s imap) (Map.insert (s, s) d hmap)
    | otherwise          = Markov imap (Map.insertWith f (imap Map.! i, imap Map.! j) d hmap)
    where
        hasI = Map.member i imap
        hasJ = Map.member j imap
        s = Map.size imap

--------------------------------------------------------------------------------

-- Lists --
fromList :: (Ord a) => [((a, a), Double)] -> Markov a
fromList list =
    let imap = foldl' toimap Map.empty list
        hmap = foldl' (tohmap imap) Map.empty list
    in Markov imap hmap

toList :: (Ord a) => Markov a -> [((a, a), Double)]
toList (Markov imap hmap) =
    let rIMap = reverseIMap imap
    in foldrWithKey' (combineImapHmap rIMap) [] hmap

--------------------------------------------------------------------------------

-- Maps --
fromMap :: (Ord a) => Map (a, a) Double -> Markov a
fromMap mapp =
    let imap = foldlWithKey' toimap' Map.empty mapp
        hmap = foldlWithKey' (tohmap' imap) Map.empty mapp
    in Markov imap hmap

--------------------------------------------------------------------------------

-- Matrices --
hmatrix :: (Ord a) => Markov a -> Matrix Double
hmatrix (Markov imap hmap) =
    let m = Map.size imap
        zeros = matrix m (replicate (m*m) 0)
        hlist = Map.toList hmap
    in  Dat.accum zeros (+) hlist

--------------------------------------------------------------------------------

-- Chains --

walkFrom :: (Ord a, Show a) => a -> Int -> Markov a -> IO [a]
walkFrom start n haskov = do
    gen <- getStdGen
    let mat = hmatrix haskov
        imap = statesI haskov
        rimap = transMap imap
        nextRow = mat Dat.? [(imap Map.! start)]
    return (start : (steps rimap mat nextRow n gen))

walk :: (Ord a, Show a) => Int -> Markov a -> IO [a]
walk n haskov = do
    gen <- getStdGen
    let mat = hmatrix haskov
        ss = tr . steady $ mat
        imap = statesI haskov
        rimap = transMap imap
    return (steps rimap mat ss n gen)

steps :: (Ord a, Show a) => ReverseIndexMap a -> Matrix Double -> Matrix Double -> Int -> StdGen -> [a]
steps _ _ _ 0 _ = []
steps rimap mat row n gen
    | (sumElements row) == 0 = []
    | otherwise = choice : steps rimap mat newRow (n-1) (snd rand)
    where
        rand = random gen
        index = randomStep row (fst rand) 0.0 0
        choice = rimap Map.! index
        newRow = mat Dat.? [index]

randomStep :: Matrix Double -> Double -> Double -> Int -> Int
randomStep row rand total j
    | newTotal < rand = randomStep row rand newTotal newJ
    | otherwise       = j
    where
        newTotal = total + (row Dat.! 0 Dat.! j)
        newJ = (j + 1) `mod` (cols row)

--------------------------------------------------------------------------------

-- Steady State --
steadyState :: (Ord a) =>  Markov a -> [(a, Double)]
steadyState haskov = zip (states haskov) (concat . Dat.toLists $ steady (hmatrix haskov))

steady :: Matrix Double -> Matrix Double
steady mat =
    let m = Dat.rows mat -- Number of states
        q = tr $ ident m - mat -- Transpose (I-matrix size m - hmatrix)
        e = col ((replicate (m-1) 0) ++ [1.0]) -- col of zeros size m, last element is 1.0
        x = luSolve (luPacked q) e -- Solve linear system of Q and e (Qx = e)
    in  scale (1 / (sumElements x)) x  -- Divide x by sum of elements

--------------------------------------------------------------------------------

-- Normalize --
normalize :: (Ord a) => Markov a -> Markov a
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

--------------------------------------------------------------------------------

---- Mapping --
--map :: (Ord a) => (Double -> Double) -> Markov a -> Markov a
--map f (Markov imap hmap) = Markov imap (fmap f hmap)

--mapWithState :: (Ord a) => (a -> Double -> Double) -> Markov a -> Markov a
--mapWithState f (Markov imap hmap) =
    --let rIMap = reverseIMap imap

--------------------------------------------------------------------------------

-- Delete/Update --
--delete :: (Ord a) => a -> a -> Markov a -> Markov a
--delete i j (Markov imap hmap) =

adjust :: (Ord a) => (Double -> Double) -> a -> a -> Markov a -> Markov a
adjust f i j (Markov imap hmap) =
    if Map.member i imap && Map.member j imap
    then Markov imap (Map.adjust f (imap Map.! i, imap Map.! j) hmap)
    else Markov imap hmap

-- Helper Functions --
-- for fromList
toimap :: (Ord a) => IndexMap a -> ((a, a), Double) -> IndexMap a
toimap acc ((i, j), d)
    | i /= j && Map.notMember i acc && Map.notMember j acc = Map.insert j (Map.size acc+1) (Map.insert i (Map.size acc) acc)
    | Map.notMember i acc = Map.insert i (Map.size acc) acc
    | Map.notMember j acc = Map.insert j (Map.size acc) acc
    | otherwise = acc

-- For fromMap
toimap' :: (Ord a) => IndexMap a -> (a, a) -> Double -> IndexMap a
toimap' acc (i, j) d
    | i /= j && Map.notMember i acc && Map.notMember j acc = Map.insert j (Map.size acc+1) (Map.insert i (Map.size acc) acc)
    | Map.notMember i acc = Map.insert i (Map.size acc) acc
    | Map.notMember j acc = Map.insert j (Map.size acc) acc
    | otherwise = acc

-- For fromList
tohmap :: (Ord a) => IndexMap a -> HMatrixMap -> ((a, a), Double) -> HMatrixMap
tohmap imap acc ((i, j), n) = Map.insert (imap Map.! i, imap Map.! j) n acc

-- For fromMap
tohmap' :: (Ord a) => IndexMap a -> HMatrixMap -> (a, a) -> Double -> HMatrixMap
tohmap' imap acc (i, j) n = Map.insert (imap Map.! i, imap Map.! j) n acc

combineImapHmap :: (Ord a) => Map Int a -> (Int, Int) -> Double -> [((a, a), Double)] -> [((a, a), Double)]
combineImapHmap rIMap (i, j) d acc = ((rIMap Map.! i, rIMap Map.! j), d) : acc

reverseIMap :: (Ord a) => Map a Int -> Map Int a
reverseIMap imap = foldrWithKey' reverseIMap' Map.empty imap
    where reverseIMap' state i m = Map.insert i state m


-- swap map key and index
transMap :: (Ord k, Ord a) => Map k a -> Map a k
transMap = Map.fromList . map swap . Map.toList

-- Machine Epsilon --

machineE :: Double
machineE = until ((== 1) . (+1)) (/2) 1
