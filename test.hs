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

chainedUnitElims = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False},
                S.Literal {S.literalVar = 2, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = False}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}]]

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

chainedPureElims = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 2, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                S.Literal {S.literalVar = 3, S.literalSign = False}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                S.Literal {S.literalVar = 3, S.literalSign = True}]]

multiplePureElims = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 2, S.literalSign = False}],
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                S.Literal {S.literalVar = 3, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                S.Literal {S.literalVar = 3, S.literalSign = False}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                S.Literal {S.literalVar = 3, S.literalSign = True}]]

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

  let initState = S.SATProgress (S.Assignment []) chainedUnitElims
  let finalState = S.SATProgress (S.Assignment [(2, True), (3, True), (1, True)]) $ 
                Set.fromList []
  let testCase = S.unitElim initState == finalState
  putStrLn $ test testCase "each unit elim allows the next and assignments"


testCheckPureElim :: IO ()
testCheckPureElim = do
  let testCase = S.checkPureElim (S.Literal 1 True) oneUnitClause
  putStrLn $ test testCase "pure elim with single unit clause"

  let testCase = not $ S.checkPureElim (S.Literal 1 False) oneUnitClause
  putStrLn $ test testCase "value must match for pure elim"

  let testCase = not $ S.checkPureElim (S.Literal 2 True) oneUnitClause
  putStrLn $ test testCase "sign must match for pure elim"

  let testCase = S.checkPureElim  (S.Literal 3 True) manyUnitClausesSat
  putStrLn $ test testCase "can do pure elim with multiple unit clauses"

  let testCase = not $ S.checkPureElim  (S.Literal 1 True) manyUnitClausesUnsat
  putStrLn $ test testCase "variable does not always appear with same sign"
  

testDoPureElim :: IO ()
testDoPureElim = do
  let testCase = S.doPureElim (S.Literal 1 True) oneUnitClause == 
                  Set.fromList []
  putStrLn $ test testCase "remove single unit clause one clause pure elim"

  let testCase = S.doPureElim (S.Literal 1 False) manyUnitClausesSat == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "removes single unit clause many clauses in sat case pure elim"

  let testCase = S.doPureElim (S.Literal 2 True) simpleUnitClauseElim == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
                  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                                S.Literal {S.literalVar = 3, S.literalSign = False}]]
  putStrLn $ test testCase "pure elim removes single row with single instances"

  let testCase = S.doPureElim (S.Literal 2 False) multiplePureElims == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True},
                                S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "single elim of -2 in case where multiple pure elims possible"

  let testCase = S.doPureElim (S.Literal 1 True) multiplePureElims == 
                Set.fromList [
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                                S.Literal {S.literalVar = 3, S.literalSign = False}],
                  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = False},
                                S.Literal {S.literalVar = 3, S.literalSign = True}]]
  putStrLn $ test testCase "single elim of 1 in case where multiple pure elims possible"

testPureElim :: IO ()
testPureElim = do

  let initState = S.SATProgress (S.Assignment []) oneUnitClause
  let finalState = S.SATProgress (S.Assignment [(1, True)]) $ 
                Set.fromList []
  let testCase = S.pureElim initState == finalState
  putStrLn $ test testCase "pure elim removes single unit clause and assigns"

  let initState = S.SATProgress (S.Assignment []) multiplePureElims
  let finalState = S.SATProgress (S.Assignment [(2, False), (1, True)]) $ 
                Set.fromList []
  let testCase = S.pureElim initState == finalState
  putStrLn $ test testCase "all rows removed through multiple pure elims"

  let initState = S.SATProgress (S.Assignment []) chainedPureElims
  let finalState = S.SATProgress (S.Assignment [(2, False), (1, True)]) $ 
                Set.fromList []
  let testCase = S.pureElim initState == finalState
  putStrLn $ test testCase "each unit elim allows the next and assignments"

-- | automates testing of solver by ensuring sat examples satisfy the original formula
verifySolve :: S.Result -> S.SATInstance -> Bool
verifySolve result cnf = 
  let S.Assignment assignments = result in
  -- each row of the formula must be satisfied by the result
  Set.foldl (\acc clause -> acc &&
    Set.foldl (\acc lit -> 
      -- for each literal in the formula, there's one literal with that var value in result
      1 == length (filter (\res -> S.literalVar lit == (let (v,_) = res in v)) assignments) 
      &&
      -- each clause has at least one literal contained in the result
      (acc || elem (S.literalVar lit, S.literalSign lit) assignments))
    False clause) True cnf

testSolver :: IO ()
testSolver = do
  let testCase = verifySolve (S.solve oneUnitClause) oneUnitClause
  putStrLn $ test testCase "oneUnitClause valid solution"

  let testCase = S.solve manyUnitClausesUnsat == S.Unsat
  putStrLn $ test testCase "manyUnitClausesUnsat valid solution"

  let testCase = verifySolve (S.solve simpleNoUnitClauses) simpleNoUnitClauses
  putStrLn $ test testCase "simpleNoUnitClauses valid solution"

  let testCase = verifySolve (S.solve simpleUnitClauseElim) simpleUnitClauseElim
  putStrLn $ test testCase "simpleUnitClauseElim valid solution"

  let testCase = verifySolve (S.solve chainedUnitElims) chainedUnitElims
  putStrLn $ test testCase "chainedUnitElims valid solution"

  let testCase = verifySolve (S.solve multipleUnitElims) multipleUnitElims
  putStrLn $ test testCase "multipleUnitElims valid solution"

  let testCase = verifySolve (S.solve chainedPureElims) chainedPureElims
  putStrLn $ test testCase "chainedPureElims valid solution"

  let testCase = verifySolve (S.solve multiplePureElims) multiplePureElims
  putStrLn $ test testCase "multiplePureElims solution"

  contents <- readFile "data/LARGE_DIMACS_1"
  let largeCNF = S.parseCNF contents
  let testCase = verifySolve (S.solve largeCNF) largeCNF
  putStrLn $ test testCase "largeCNF valid solution"

main :: IO ()
main = do

  putStr "\n"

  putStrLn "--- TESTS FOR UNIT ELIMINATION ---"
  testCheckUnitElim
  testDoUnitElim
  testUnitElim
  putStr "\n"

  putStrLn "--- TESTS FOR PURE ELIMINATION ---"
  testCheckPureElim
  testDoPureElim
  testPureElim
  putStr "\n"

  putStrLn "--- SAT SOLVER VERIFICATION TESTS ---"
  testSolver

  putStr "\n"


