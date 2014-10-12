module Eval (
    eval
) where
-- friends
import Util ((|->))
import qualified Piece
import qualified Board
-- GHC

-- libraries

-- std
import Data.Array
import qualified Data.ByteString as BS
import Data.List
import Data.Int
import Data.Word
import System.IO.Unsafe

type Va = Int

-- type Al = Int

-- type Be = Int

eval :: Board.Bd -> Va
eval bd = matVa bd + bonaVa bd

matVa :: Board.Bd -> Va
matVa (Board.Bd _ hs co _ pcl) = co == Piece.B |-> negate $
    sum [pcVa pc * hs ! pc | pc <- Board.bothHsRa] +
    sum [pcVa pc * length (pcl ! pc) | pc <- Piece.pcRa]

pcVa :: Piece.Pc -> Va
pcVa = (!) $ listArray Piece.pcBnd (a ++ negate `fmap` a)
    where
        a = [
         -- FU , KY , KE , GI , KI , KA , HI  , OU
            100, 430, 450, 640, 690, 890, 1040, 15000,
         -- FUP, KYP, KEP, GIP, --, KAP,  HIP,  --
            420, 530, 540, 670, -1, 1150, 1300, -1
            ]

fv :: Int -> Va
fv i = (fromIntegral :: Int8 -> Int) . (fromIntegral :: Word8 -> Int8) $
    BS.head $
    BS.drop i fvbin

fvbin :: BS.ByteString
fvbin = unsafePerformIO $ BS.readFile "./fv.bin"

-- Constant values
k1PP :: Int
k1PP = (k1P * (k1P + 1)) `div` 2
    where
        k1P = 1476

k1All :: Int
k1All = k1PP * 81 -- 81 * (1476 * (1476 + 1)) / 2

k1HsA, k1PcA, k2HsA, k2PcA :: Array Piece.Pc Int
k1HsA = accumArray (+) 0 Piece.pcBnd $
    -- Black FU, KY, KE, GI, KI, KA, HI, OU, Promotions are zero
    zip (Board.hsRa Piece.B) [0, 38, 48, 58, 68, 78, 84] ++
    -- White FU, KY, KE, GI, KI, KA, HI, OU, Promotions are zero
    zip (Board.hsRa Piece.W) [19, 43, 53, 63, 73, 81, 87]

k2HsA = accumArray (+) 0 Piece.pcBnd $
    -- White FU, KY, KE, GI, KI, KA, HI, OU, Promotions are zero
    zip (Board.hsRa Piece.B) [0, 19, 24, 29, 34, 39, 42] ++
    -- White FU, KY, KE, GI, KI, KA, HI, OU, Promotions are zero
    zip (Board.hsRa Piece.W) [0, 19, 24, 29, 34, 39, 42]

k1PcA = listArray Piece.pcBnd $ b ++ map (+ 81) b
    where
        b = [
         -- FU, KY,  KE,  GI,  KI,  KA,  HI,   OU
            81, 225, 360, 504, 666, 828, 1152, -1,
         -- FUP, KYP, KEP, GIP, --, KAP, HIP, ---
            666, 666, 666, 666, -1, 990, 1314, -1
            ]
k2PcA = listArray Piece.pcBnd $ l ++ l
    where
        l = [
         -- FU, KY,  KE,  GI,  KI,  KA,  HI,  OU
            36, 108, 171, 252, 333, 414, 576, -1,
         -- FUP, KYP, KEP, GIP, --, KAP, HIP, ---
            333, 333, 333, 333, -1, 595, 657, -1
            ]

-- Evaluation functions
k1Va, k2Va :: Piece.Pos -> Piece.Pos -> Piece.Pos -> Va
-- KPP evalution
k1Va k p p' = fv (k * k1PP + a ! p + p')
    where
        a = listArray (0, 1475) [(i * (i + 1)) `div` 2 | i <- [0 ..]]
-- KKP evalution
k2Va k k' p = fv (k1All + (k * 81 + k') * k2P + p)
    where
        k2P = 738

-- Translate Piece.Pos to bonanza position
bonaPos :: Piece.Co -> Piece.Pos -> Piece.Pos
bonaPos co sqq = if co == Piece.B then a ! sqq else b ! sqq
    where
        a = listArray (0, 220) [sq `quot` 17 * 9 + sq `rem` 17 - 22 | sq <- [0 .. 220]]
        b = listArray (0, 220) [(80 -) $ sq `quot` 17 * 9 + sq `rem` 17 - 22 | sq <- [0 .. 220]]

posK :: Board.Pcl -> (Piece.Co, Piece.Co) -> Piece.Pos
posK pcl (kingColor, pieceColor) = bonaPos pieceColor . head $
    pcl ! Piece.Pc kingColor Piece.Unp Piece.OU

bonaVa :: Board.Bd -> Va
bonaVa (Board.Bd _ hs co _ pcl) =
    let
        {- bk0/wk0 : Black King/White King positions from Black
           bk1/wk1 : Black King/White King positions from White -}
        [bk0, wk0, bk1, wk1] =
            map (posK pcl)
                [(Piece.B, Piece.B),
                 (Piece.W, Piece.B),
                 (Piece.B, Piece.W),
                 (Piece.W, Piece.W)]
        -- KKP ------------------------------------------------------

        -- Hands evaluation
        k2h0 = sum [k2Va bk0 wk0 $ k2HsA ! p + hs ! p | p <- Board.hsRa Piece.B]
        k2h1 = sum [k2Va wk1 bk1 $ k2HsA ! p + hs ! p | p <- Board.hsRa Piece.W]
        -- Pieces evaluation
        k2p0 = sum [k2Va bk0 wk0 $ k2PcA ! p + bonaPos Piece.B po |
            p <- Board.pclRa Piece.B, po <- pcl ! p]
        k2p1 = sum [k2Va wk1 bk1 $ k2PcA ! p + bonaPos Piece.W po |
            p <- Board.pclRa Piece.W, po <- pcl ! p]
        -- Get sum
        v = k2h0 - k2h1 + k2p0 - k2p1

        -- KPP ------------------------------------------------------

        -- Hands list
        h0 = [k1HsA ! p + hs ! p | p <- Board.bothHsRa]
        h1 = [k1HsA ! p + hs ! Piece.pcOppCo p | p <- Board.bothHsRa]
        -- Pieces list
        p0 = [k1PcA ! p + bonaPos Piece.B po | p <- Piece.pcRa, po <- pcl ! p]
        p1 = [k1PcA ! Piece.pcOppCo p + bonaPos Piece.W po | p <- Piece.pcRa, po <- pcl ! p]
        -- sort Hands/Pieces position list
        lB = sortBy (flip compare) $ h0 ++ p0
        lW = sortBy (flip compare) $ h1 ++ p1
        -- Evaluation function for KPP
        f [] _ = 0
        f bs@(b : bb) ws@(w : ww) =
            sum (map (k1Va bk0 b) bs) - sum (map (k1Va wk1 w) ws) +
            f bb ww
    in
        co == Piece.B |-> negate $ (v + f lB lW) `quot` 2
