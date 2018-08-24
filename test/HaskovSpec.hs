module HaskovSpec where

import Haskov (fromList,imap,hmatrix, walk,steady,steadyState,statesI)

import Test.Hspec
import qualified Data.Set as Set
import qualified Numeric.LinearAlgebra.Data as Dat
import Data.List (intercalate)

import           Control.Monad (unless,when)

testTransitions =
  [ (("A", "B"), 0.3)
  , (("A", "A"), 0.7)
  , (("B", "C"), 1.0)
  , (("C", "A"), 1.0)
  ]


spec :: Spec
spec =

  describe "haskov" $ do

--    describe "steadyState" $ do
--
--      it "should always contain positive numbers" $ do
--        let
--          transitions = [ (("A", "B"), 1.0), (("B", "C"), 1.0), (("C", "A"), 1.0)]
--          haskov = fromList transitions
--          s = steadyState haskov
--        --mapM (\(s, p) -> s `shouldSatisfy` (> 0))
--        print s
--        True `shouldBe` True

    describe "A haskov walk" $ do

    -- could use quickcheck for better tests

      it "should never do invalid transitions when transitions are ordered" $ do
        let
          transitions = [(("A", "B"), 1.0), (("B", "C"), 1.0), (("C", "A"), 1.0)]
          valid = map fst transitions
          markov = fromList transitions
        res <- walk 10 markov
        expectOnlyValidTransitions transitions res

      it "should never do invalid transitions when transtions are not ordered" $ do
        let
          transitions = [ (("C", "A"), 1.0), (("A", "B"), 1.0), (("B", "C"), 1.0)]
          valid = map fst transitions
          haskov = fromList transitions
        res <- walk 3 haskov
        expectOnlyValidTransitions transitions res

--
--    describe "A haskov walkFrom" $ do
--
--      it "starts with the head initial state" $ do
--        let
--          markov = fromList testTransitions
--        res <- walkFrom "B" 10 markov
--        head res `shouldBe` "B"
--
--      it "just some test" $ do
--        let
--          markov = fromList testTransitions
--          index = imap markov
--          matrix = hmatrix markov
--          start = "A"
--        res <- walkFrom "B" 10 markov
--        --putStrLn $ "index: " ++ ( show index)
--        --putStrLn $ "matrix: " ++ ( show matrix)
--        --putStrLn $ "result from " ++ start ++ ": " ++ ( show res)
--        return ()




expectTrue :: HasCallStack => String -> Bool -> Expectation
expectTrue msg b = unless b (expectationFailure msg)

expectOnlyValidTransitions transitions actual = do
  let
    valid = map fst transitions
    actualTransitions = zip actual (drop 1 actual)
    actualUnique = Set.fromList actualTransitions
    invalid = Set.filter (\e -> not $ elem e valid) actualUnique
    len = length invalid
    invalidMsg inv = "invalid transitions encountered: " ++ ( intercalate ", " (Set.toList (Set.map (\(a,b) -> a ++ "->" ++ b) inv)))
  expectTrue (invalidMsg invalid) $ len == 0
