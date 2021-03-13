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

oneClauseOneVarSI = Set.fromList [Set.fromList [S.Literal 1 True]]

test :: Bool -> String -> String 
test expr message =
  if expr then "TEST PASSED: " ++ message else "TEST FAILED: " ++ message 

testCheckUnitElim :: IO ()
testCheckUnitElim = do
  let testCase = S.checkUnitElim (S.Literal 1 False) oneClauseOneVarSI
  print (test (not testCase) "checkUnitElim false single element val matches")

  let testCase = S.checkUnitElim (S.Literal 1 True) oneClauseOneVarSI
  print (test testCase "checkUnitElim true single element")

  let testCase = S.checkUnitElim (S.Literal 2 True) oneClauseOneVarSI
  print (test (not testCase) "checkUnitElim false single element sign matches")

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


