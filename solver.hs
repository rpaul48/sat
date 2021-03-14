module Solver where

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


-- Data definitions
type Variable = Int
data Literal = Literal { literalVar :: Variable
                       , literalSign :: Bool
                       } deriving (Ord, Eq, Show)
type Clause = Set Literal
type SATInstance = Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat deriving (Eq)

data SATProgress = SATProgress {assn :: Result, satinst :: SATInstance} deriving (Show)

-- Functions for parsing input and printing output
instance Show Result where
    show Unsat             = "UNSAT"
    show (Assignment assnt) = foldl
        (\str (var, bool) ->str ++ " " ++ (if bool then "" else "-") ++ show var)
        ""
        assnt

-- | Takes an integer (positive or negative) and returns the corresponding literal.
makeLiteral :: Int -> Literal
makeLiteral litInt =
    let absLit = abs litInt
    in
        if litInt > 0
            then Literal absLit True
            else if litInt < 0
                then Literal absLit False
                else
                    error
                        "Error: Literal must be either postive or negative number"

-- | Takes the string form of a literal and returns the corresponding literal.
makeLiteralFromStr :: String -> Literal
makeLiteralFromStr litStr = makeLiteral (read litStr)

-- | Takes a list of strings representing literals and returns the clause containing
-- those literals.
makeClause :: [String] -> Clause
makeClause = Set.fromList . map makeLiteralFromStr . init

-- | Takes the contents of an input file and returns the encoded SAT problem.
parseCNF :: String -> SATInstance
parseCNF input =
    let allLines     = lines input
        -- tokenize and remove comment lines
        tokLines     = map words (filter (/= "") allLines)
        contentLines = filter (\tokLine -> head tokLine /= "c") tokLines
        -- make sure first line is problem line
        pLine        = if head (head contentLines) /= "p"
            then error "Error: DIMACS file does not have problem line"
            else head contentLines
        -- Create a set of clauses from the content lines
        cnf = Set.fromList . map makeClause . tail $ contentLines
    in  assert (pLine !! 1 == "cnf")
            . assert (all (\clause -> last clause == "0") . tail $ contentLines)
            $ cnf


-- | Prints the answer in the DIMACS standard output method.
formatOutput :: Result -> String
formatOutput Unsat = "s UNSATISFIABLE"
formatOutput res =
    "s SATISFIABLE\n" ++ "v" ++ (show res) ++ " 0"


-- | Checks if a given Literal is contained as a unit clause in a SATInstance
checkUnitElim :: Literal -> SATInstance -> Bool
checkUnitElim lit satinstance = 
    --any (\clause -> elem lit clause && 1 == Set.size clause) $ Set.toList satinstance
    elem (Set.singleton lit) $ Set.toList satinstance


-- | Does a unit clause elimination with a given Literal and SATInstance
doUnitElim :: Literal -> SATInstance -> SATInstance
--doUnitElim _ empty = empty      -- shouldn't be necessary
doUnitElim lit satinstance = 
    let flipped = Literal (literalVar lit) (not $ literalSign lit) in
    Set.map (Set.filter (/=flipped)) $ Set.filter (notElem lit) satinstance
-- vaguely tested lmao wtf is this language


-- | Does all the unit clause eliminations for a given SATInstance
unitElim :: SATProgress -> SATProgress
unitElim current = 
    -- filter to get the literals which can be unit eliminated
    let eliminable = Set.filter (flip checkUnitElim $ satinst current) (Set.unions $ satinst current) in
    if Set.size eliminable == 0
        -- return input if no literals can be eliminated
        then current 
        else 
             unitElim $ Set.foldl (\progress next -> 
                let updated = (doUnitElim next $ satinst progress); Assignment assigned = assn progress in SATProgress (Assignment $ (literalVar next, literalSign next):assigned) updated
                ) current eliminable 



-- | Checks if a given Literal is pure within a SATInstance
checkPureElim :: Literal -> SATInstance -> Bool
checkPureElim lit = 
    (==1) . Set.size . Set.filter ((== literalVar lit) . literalVar) . Set.unions


-- | Does a pure elimination with a given Literal and SATInstance
doPureElim :: Literal -> SATInstance -> SATInstance
doPureElim lit = 
    Set.filter (notElem lit)

-- | Does all the pure clause eliminations for a given SATInstance
pureElim :: SATProgress -> SATProgress
pureElim current = 
    -- filter to get the literals which can be pure eliminated
    let eliminable = Set.filter (flip checkPureElim $ satinst current) (Set.unions $ satinst current) in
    if Set.size eliminable == 0
        -- return input if no literals can be eliminated
        then current 
        else 
             pureElim $ Set.foldl (\progress next -> 
                let updated = (doPureElim next $ satinst progress); Assignment assigned = assn progress in SATProgress (Assignment $ (literalVar next, literalSign next):assigned) updated
                ) current eliminable

-- | Recursively determines a variable assignment that satisfies a CNF formula
rsolve :: SATProgress -> SATProgress
rsolve full@(SATProgress Unsat _) = full
rsolve current = 
    -- do unit elimination and pure elimination
    let eliminated = pureElim $ unitElim current in 

    -- check for UNSAT case
    if elem Set.empty $ satinst eliminated
        then SATProgress Unsat $ satinst eliminated
        else 
            if Set.null $ satinst eliminated
                then eliminated
                else let Assignment assigned = assn eliminated; choice = head $ Set.toList $ Set.unions $ satinst eliminated; choiceFlipped = Literal (literalVar choice) (not $ literalSign choice) in
                    let firstGuess = rsolve (SATProgress (Assignment $ (literalVar choice, literalSign choice):assigned) $ doUnitElim choice $ satinst eliminated) in
                        if assn firstGuess /= Unsat then firstGuess else 
                            rsolve (SATProgress (Assignment $ (literalVar choiceFlipped, literalSign choiceFlipped):assigned) $ doUnitElim choiceFlipped $ satinst eliminated)

solve :: SATInstance -> Result
solve cnf =
    -- create SATProgress
    let final = rsolve (SATProgress (Assignment []) cnf); in
        if assn final == Unsat then Unsat else
            let allVars = Set.map literalVar $ Set.unions cnf; Assignment assigned = assn final; defVars = Set.fromList $ map fst assigned; diffed = Set.toList $ Set.difference allVars defVars in Assignment $ assigned ++ map (\v -> (v, True)) diffed


main :: IO ()
main = do
    -- read in file name
    args <- getArgs
    let file = head args

    -- read and parse file contents
    contents <- readFile file
    let cnf = parseCNF contents
    --print contents
    --print '\n'
    let fst = Set.elemAt 0 $ Set.elemAt 0 cnf
    --print $ Set.elemAt 1 $ Set.elemAt 1 cnf
    let out1 = doUnitElim fst cnf
    --print fst
    --print out1
    --print cnf
    print $ solve cnf

    -- TODO: find a satisfying instance (or return unsat) and print it out
    --putStrLn "Print the solution here!"
    putStrLn "\n"
    --print $ Set.unions cnf