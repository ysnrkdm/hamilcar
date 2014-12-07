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
    print "Running test ..."
--    defaultMain $ hUnitTestToTests $ TestLabel "mapRep" $ TestCase mapRep
--    defaultMain $ hUnitTestToTests $ TestLabel "moveComp" $ TestCase moveComp
    defaultMain $ hUnitTestToTests $ TestLabel "searchTest" $ TestCase searchTest
    defaultMain $ hUnitTestToTests $ TestLabel "alphabetaTest" $ TestCase alphabetaTest

mapRep = do
    let tree = IS.Node 10 [IS.Node 20 [IS.Node 30 []], IS.Node 15 [IS.Node 20 [], IS.Node 40 []]]
    print tree
    print $ IS.maptree (\x -> x + 10) tree

searchTest = do
    let board = Usi.bdFromSfen [
            "l6nl/5+P1gk/2nl1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5KS1/R8/LN4bKL",
            "w",
            "GR5pnsg" ]
    printf "board is now\n"
    print board
    printf "\n"
    print "start searching by minmax..."
    let val = IS.evaluate board
    print "done. val is"
    print val

alphabetaTest = do
    let board = Usi.bdFromSfen [
            "l6nl/5+P1gk/2nl1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL",
            "w",
            "GR5pnsg" ]
    printf "board is now\n"
    print board
    printf "\n"
    print "start searching by alphabeta..."
    print "done. val is"
    let val = IS.alphabeta board
    print val

moveComp = do
    let board = Usi.bdFromSfen [
            "l6nl/5+P1gk/2nl1S3/p1p4Pp/3P2Sp1/1PPb2P1P/P5GS1/R8/LN4bKL",
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
