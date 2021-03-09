import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.List (intercalate)
import           Data.Time
import           Text.Printf
import           System.Environment
import           Control.Exception (assert)
import           Debug.Trace


-- Data definitions
type Variable = Int
data Literal = Literal { literalVar :: Variable
                       , literalSign :: Bool
                       } deriving (Ord, Eq, Show)
type Clause = Set Literal
type SATInstance = Set Clause
data Result = Assignment [(Variable, Bool)] | Unsat


-- Functions for parsing input and printing output
instance Show Result where
    show Unsat             = "UNSAT"
    show (Assignment assn) = foldl
        (\str (var, bool) ->str ++ " " ++ (if bool then "-" else "") ++ show var)
        ""
        assn

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


main :: IO ()
main = do
    -- read in file name
    args <- getArgs
    let file = head args

    -- read and parse file contents
    contents <- readFile file
    let cnf = parseCNF contents

    print contents
    print '\n'
    print cnf

    -- TODO: find a satisfying instance (or return unsat) and print it out
    putStrLn "Print the solution here!"