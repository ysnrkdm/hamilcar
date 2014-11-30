module Move (
    Mv(..),
    onBdPoss,
    mvToUSI,
) where
    -- friends
import qualified Util
import qualified Piece
    -- GHC

    -- libraries

    -- std
import Control.Applicative
import Data.Array

data Mv =
      Drop {dropTo :: Piece.Pos, dropPc :: Piece.Pc}
    | Mv {
        fr :: Piece.Pos,
        to :: Piece.Pos,
        mvPc :: Piece.Pc,
        cap :: Piece.Pc,
        isPro :: Bool
    } deriving (Eq, Ord)
instance Show Mv where
    -- Drop is shown as X*00, e.g., dropping black porn at 56 is P*56
    show (Drop to pc) = dropToUSI pc ++ Util.notation to
    -- Move is shown as 00XX(00)+, e.g., moving white bishop from 66 to 57 is 57WB(66). Promotion adds + at the end.
    show (Mv fr to pc _ isPro) = Util.notation to ++ show pc ++ "(" ++ Util.notation fr ++ ")" ++ Util.if' (isPro, "+", "")

isCapture = (/= Piece.Empty) . cap

mvColor (Drop {dropPc = x}) = Piece.co x
mvColor (Mv {mvPc = x}) = Piece.co x

dropToUSI = (++ "*") . show . Piece.p8

mvToUSI (Drop to pc) = dropToUSI pc ++ Util.posToUSI to
mvToUSI (Mv fr to _ _ isPro) = Util.posToUSI fr ++ Util.posToUSI to ++ Util.if' (isPro, "+", "")

onBdPoss = Util.toPos <$> range ((1, 1), (9, 9)) :: [Piece.Pos]