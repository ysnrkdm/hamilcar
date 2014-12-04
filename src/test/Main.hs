module Main where
-- friends
import qualified Usi
import qualified Board
import qualified Move
import qualified MoveGenerator
import qualified Internal.Search as IS
-- GHC

-- libraries
import Text.Printf (printf)

-- std
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List

main :: IO ()
main = do
    defaultMain $ hUnitTestToTests $ TestLabel "newMoveValidation" $ TestCase mapRep
    defaultMain $ hUnitTestToTests $ TestLabel "newMoveValidation" $ TestCase moveComp

mapRep = do
    let tree = IS.Node 10 [IS.Node 20 [IS.Node 30 []], IS.Node 15 [IS.Node 20 [], IS.Node 40 []]]
    print tree
    print $ IS.maptree (\x -> x + 10) tree
--    IS.

moveComp = do
    let board = Usi.bdFromSfen [
            "l6nl/5+P1gk/2nl1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5KS1/R8/LN4bKL",
            "w",
            "GR5pnsg" ]
    printf "board is now\n"
    print board
    printf "\n"
    let moves = sort $ MoveGenerator.mvGenFull board
    let movess = sort $ MoveGenerator.mvGenFullN board
    print moves
    printf "...\n"
    print movess
    printf "...\n"
    moves @=? movess
