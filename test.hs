module Test where

import qualified Solver as S hiding (main) 

import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.List (intercalate)
import           Data.Time
import           Text.Printf
import           System.Environment
import           Control.Exception (assert)
import           Debug.Trace
import           Data.Typeable

-- | CNF sat instances for testing
oneUnitClause = Set.fromList [Set.fromList [S.Literal 1 True]]
manyUnitClausesUnsat = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False}], 
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]
manyUnitClausesSat = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False}], 
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]
simpleNoUnitClauses = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False}, 
                S.Literal {S.literalVar = 2, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = False}]]
simpleUnitClauseElim = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False},
                S.Literal {S.literalVar = 2, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = False}]]

multipleUnitElims = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False},
                S.Literal {S.literalVar = 2, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = False},
                S.Literal {S.literalVar = 4, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True},
                S.Literal {S.literalVar = 4, S.literalSign = False}],
  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]


test :: Bool -> String -> String 
test expr name =
  if expr then "PASSED: " ++ name else "FAILED: " ++ name 

testCheckUnitElim :: IO ()
testCheckUnitElim = do
  let testCase = S.checkUnitElim (S.Literal 1 True) oneUnitClause
  putStrLn $ test testCase "unit elim with single unit clause"

  let testCase = not $ S.checkUnitElim (S.Literal 1 False) oneUnitClause
  putStrLn $ test testCase "value must match for unit elim"

  let testCase = not $ S.checkUnitElim (S.Literal 2 True) oneUnitClause
  putStrLn $ test testCase "sign must match for unit elim"

  let testCase = S.checkUnitElim  (S.Literal 3 True) manyUnitClausesSat
  putStrLn $ test testCase "can do unit elim with multiple unit clauses"

  let testCase = not $ S.checkUnitElim (S.Literal 1 True) simpleNoUnitClauses
  putStrLn $ test testCase "no unit elim if no unit clauses"

testDoUnitElim :: IO ()
testDoUnitElim = do
  let testCase = S.doUnitElim (S.Literal 1 True) oneUnitClause == 
                  Set.fromList []
  putStrLn $ test testCase "remove single unit clause one clause"

  let testCase = S.doUnitElim (S.Literal 1 False) manyUnitClausesSat == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "removes single unit clause many clauses in sat case"

  let testCase = S.doUnitElim (S.Literal 1 True) manyUnitClausesUnsat == 
                Set.fromList [
                  Set.fromList [], 
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "create empty clause with unit elim in unsat case"

  let testCase = S.doUnitElim (S.Literal 1 True) simpleUnitClauseElim == 
                Set.fromList [ 
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}, 
                                S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "unit elim clauses with same sign, remove from third clause"

  let testCase = S.doUnitElim (S.Literal 3 True) multipleUnitElims == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                                S.Literal {S.literalVar = 4, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True},
                                S.Literal {S.literalVar = 4, S.literalSign = False}]]
  putStrLn $ test testCase "single elim in case where multiple unit elims possible"

testUnitElim :: IO ()
testUnitElim = do
  let initState = S.SATProgress (S.Assignment []) oneUnitClause
  let finalState = S.SATProgress (S.Assignment [(1, True)]) $ 
                Set.fromList []
  let testCase = S.unitElim initState == finalState
  putStrLn $ test testCase "single unit elim and assignment"

  let initState = S.SATProgress (S.Assignment []) multipleUnitElims
  let finalState = S.SATProgress (S.Assignment [(3, True), (1, True)]) $ 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True},
                                S.Literal {S.literalVar = 4, S.literalSign = False}]]
  let testCase = S.unitElim initState == finalState
  putStrLn $ test testCase "multiple unit elims and assignments"

testCheckPureElim :: IO ()
testCheckPureElim = do
  pure ()

testDoPureElim :: IO ()
testDoPureElim = do
  pure ()

testPureElim :: IO ()
testPureElim = do
  pure ()

main :: IO ()
main = do

  putStr "\n"

  testCheckUnitElim
  testDoUnitElim
  testUnitElim

  testCheckPureElim
  testDoPureElim
  testPureElim

  putStr "\n"


