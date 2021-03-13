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

oneUnitClause = Set.fromList [Set.fromList [S.Literal 1 True]]
manyUnitClauses = Set.fromList [
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = False}], 
  Set.fromList [S.Literal {S.literalVar = 1, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 2, S.literalSign = True}],
  Set.fromList [S.Literal {S.literalVar = 3, S.literalSign = True}]]

test :: Bool -> String -> String 
test expr message =
  if expr then "TEST PASSED: " ++ message else "TEST FAILED: " ++ message 

testCheckUnitElim :: IO ()
testCheckUnitElim = do
  let testCase = S.checkUnitElim (S.Literal 1 False) oneUnitClause
  print (test (not testCase) "checkUnitElim false single element val matches")

  let testCase = S.checkUnitElim (S.Literal 1 True) oneUnitClause
  print (test testCase "checkUnitElim true single element")

  let testCase = S.checkUnitElim (S.Literal 2 True) oneUnitClause
  print (test (not testCase) "checkUnitElim false single element sign matches")

  let testCase = S.checkUnitElim  (S.Literal 3 True) manyUnitClauses
  print (test testCase "checkUnitElim true multiple unit clauses")

testDoUnitElim :: IO ()
testDoUnitElim = do
  pure ()

testCheckPureElim :: IO ()
testCheckPureElim = do
  pure ()

testDoPureElim :: IO ()
testDoPureElim = do
  pure ()

main :: IO ()
main = do

  testCheckUnitElim
  testDoUnitElim
  testCheckPureElim
  testDoPureElim


