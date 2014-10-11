module Usi (
    usiLoop
) where
-- friends
import qualified Util
import qualified Piece
import qualified Board
import qualified Move
import qualified Search

-- GHC

-- libraries

-- std
import Control.Arrow
import Data.Array
import Data.Char
import Data.Maybe
import Data.Tuple

castEn :: Bool -> Piece.Co
castEn = Util.modiEn id

sfenStartpos :: [String]
sfenStartpos = [
        "lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL",
        "b",
        "-"
    ]

bdFromSfen :: [String] -> Board.Bd
bdFromSfen s =
    Board.Bd (Board.empSqs // sqs)
        (Board.empHs // hsFromSfen (ss !! 2))
        (Util.if' (ss !! 1 == "b", Piece.B, Piece.W))
        0
        (accumArray (flip (:)) [] Piece.pcBnd $ map swap sqs)
            where
                ss = if length s >= 3 then s else sfenStartpos
                sqs = sqsFromSfen $ head ss

pcFromSfen :: Char -> (Piece.Co, Piece.P8)
pcFromSfen = castEn . isLower &&& fromJust . Piece.p8FromUSI . toUpper

sqsFromSfen :: String -> [Board.Sq]
sqsFromSfen sfen = parser sfen (2, 4) Piece.Unp
    where
        parser :: String -> (Int, Int) -> Piece.Pro -> [Board.Sq]
        parser [] _ _ = []
        parser (x : xs) sq @ ( r, f) pro
            | isDigit x = parser xs (r, f + digitToInt x) pro
            | x == '/' = parser xs (r + 1, 4) pro
            | x == '+' = parser xs sq Piece.Pro
            | otherwise = (r * 17 + f, Piece.Pc co pro p8) : parser xs (r, f + 1) Piece.Unp
                where (co, p8) = pcFromSfen x

hsFromSfen sfen = parser sfen 1
    where
        parser [] _ = []
        parser (x : xs) cnt
            | x == '-' = []
            | isDigit x = parser xs (digitToInt x)
            | otherwise = (Piece.Pc co Piece.Unp p8, cnt) : parser xs 1
                where (co, p8) = pcFromSfen x

mvFromSfen (Board.Bd sqs _ me _ _) sfen =
    case Piece.p8FromUSI (head sfen) of
        Just p8 -> -- is drop
            let to = Util.posFromUSI [sfen !! 2, sfen !! 3]
                pc = Piece.Pc me Piece.Unp p8
            in Move.Drop to pc
        Nothing -> -- is mv
            let fr = Util.posFromUSI [sfen !! 0, sfen !! 1]
                to = Util.posFromUSI [sfen !! 2, sfen !! 3]
                mvPc = sqs ! fr
                cap = sqs ! to
                isPro = length sfen == 5
            in Move.Mv fr to mvPc cap isPro

readUSIPosition sfens =
    let ws = tail $ words sfens
        sbd = Util.if' (head ws == "startpos", ["startpos"], take 3 ws)
        bd = bdFromSfen sbd
        smvs = drop 1 $ dropWhile (/= "moves") ws
        mvs = mvsFromSfen bd smvs
    in (bd, mvs)

usiLoop bd = do
    sfens <- getLine
    let cmds = words sfens
    case head cmds of
        "isready" -> putStrLn "readyok"
        "position" -> do
            let (bd2, mvs) = readUSIPosition sfens
                bd3 = Board.bdDoMvs bd2 mvs
            usiLoop bd3
        "go" -> do
            let (Search.Result _ pv) = Search.minmax 1 bd
            putStrLn $ "bestmove " ++ Move.mvToUSI (head pv)
        otherwise -> putStrLn ("undefined command.." ++ sfens)
    usiLoop bd -- next

mvsFromSfen :: Board.Bd -> [String] -> [Move.Mv]
mvsFromSfen _ [] = []
mvsFromSfen bd (s : ss) = mv : mvsFromSfen (Board.bdDo bd mv) ss
    where mv = mvFromSfen bd s
