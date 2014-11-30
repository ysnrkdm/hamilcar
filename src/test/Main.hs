module Main where
-- friends
import qualified Usi
import qualified Board
import qualified Move
import qualified MoveGenerator
-- GHC

-- libraries
import Text.Printf (printf)

-- std
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = do
    defaultMain $ hUnitTestToTests $ TestLabel "newMoveValidation" $ TestCase moveComp

moveComp = do
    let board = Usi.bdFromSfen [
            "l6nl/5+P1gk/2nl1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5KS1/R8/LN4bKL",
            "w",
            "GR5pnsg" ]
    printf "board is now\n"
    print board
    printf "\n"
    let moves = MoveGenerator.mvGenFull board
    let movess = MoveGenerator.mvGenFullN board
    print moves
    printf "...\n"
    print movess
    printf "...\n"
    moves @=? movess
