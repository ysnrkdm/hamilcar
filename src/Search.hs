module Search(
    Result(..),
    minmax,
) where
-- friends
import qualified Board
import qualified Move
import qualified MoveGenerator
import qualified Eval
-- GHC

-- libraries

-- std
import Data.Function
import Data.List

type Dep = Int
type Cnt = Int
data Result = Result {va :: Int, pv :: [Move.Mv]} deriving (Eq, Ord)

conv mv res = Result (-va res) (mv:pv res)

minmax :: Int -> Board.Bd -> Result
minmax 0 bd = Result (Eval.eval bd) []
minmax dep bd = maximumBy (on compare va) nexts
    where
        nexts = map next $ MoveGenerator.mvGenFull bd
        next mv = conv mv . minmax (dep - 1) $ Board.bdDo bd mv
