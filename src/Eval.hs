module Eval where
    import Data.Array
    import qualified Data.ByteString as BS
    import Data.List
    import Data.Int
    import Data.Word
    import System.IO.Unsafe
    import Util
    import Piece
    import Board

    type Va = Int
    type Al = Int
    type Be = Int
    eval::Bd -> Va
    eval bd = matVa bd + bonaVa bd
    matVa (Bd _ hs co _ pcl) = co == B |-> negate $
      sum[pcVa pc * hs ! pc|pc <- bothHsRa] + sum[pcVa pc * length(pcl ! pc)|pc <- pcRa]
    pcVa::Pc -> Va
    pcVa = (!) $ listArray pcBnd(a ++ negate `fmap` a)
        where a = [100, 430, 450, 640, 690, 890, 1040, 15000, 420, 530, 540, 670, -1, 1150, 1300, -1]
    fv::Int -> Va
    fv i = (fromIntegral::Int8->Int).(fromIntegral::Word8->Int8)$BS.head$BS.drop i fvbin
    fvbin = unsafePerformIO$BS.readFile "./fv.bin"
    k1HsA, k1PcA, k2HsA, k2PcA::Array Pc Int
    k1Va, k2Va::Pos -> Pos -> Pos -> Va
    k1P = 1476
    k2P = 738
    k1PP = (k1P * (k1P + 1)) `div` 2 -- (1476 * (1476 + 1)) / 2
    k1All = k1PP * 81 -- 81 * (1476 * (1476 + 1)) / 2
    k1HsA = accumArray ( + ) 0 pcBnd $ zip (hsRa B) [0, 38, 48, 58, 68, 78, 84] ++ zip (hsRa W) [19, 43, 53, 63, 73, 81, 87]
    k2HsA = accumArray ( + ) 0 pcBnd $ zip (hsRa B) [0, 19, 24, 29, 34, 39, 42] ++ zip (hsRa W) [0, 19, 24, 29, 34, 39, 42]
    k1PcA = listArray pcBnd $ b ++ map (+ 81) b
      where b = [81, 225, 360, 504, 666, 828, 1152, -1, 666, 666, 666, 666, -1, 990, 1314, -1]
    k2PcA= listArray pcBnd $ l ++ l
      where l = [36, 108, 171, 252, 333, 414, 576, -1, 333, 333, 333, 333, -1, 595, 657, -1]
    k1Va k p p' = fv $ (k * k1PP + a ! p + p') where a = listArray (0, 1475) [(i * (i + 1)) `div` 2 | i <- [0..]]
    k2Va k k' p = fv $ (k1All + (k * 81 + k') * k2P + p)
    bonaPos::Co->Pos->Pos
    bonaPos co sq = if co == B then a ! sq else b ! sq
        where
            a = listArray (0, 220) [sq `quot` 17 * 9 + sq `rem` 17 - 22 | sq <- [0..220]]
            b = listArray (0, 220) [(80 -) $ sq `quot` 17 * 9 + sq `rem` 17 - 22 | sq <- [0..220]]
    posK::Pcl -> (Co, Co) -> Pos
    posK pcl (co, coRev) = bonaPos coRev . head $ pcl ! (Pc co Unp OU)
    bonaVa::Bd -> Va
    bonaVa (Bd _ hs co _ pcl) = let
      [bk0, wk0, bk1, wk1] = map (posK pcl) [(B, B), (W, B), (B, W), (W, W)]
      k2h0 = sum [k2Va bk0 wk0 $ k2HsA ! p + hs ! p | p <- hsRa B]
      k2h1 = sum [k2Va wk1 bk1 $ k2HsA ! p + hs ! p | p <- hsRa W]
      k2p0 = sum [k2Va bk0 wk0 $ k2PcA ! p + bonaPos B po | p <- pclRa B, po <- pcl ! p]
      k2p1 = sum [k2Va wk1 bk1 $ k2PcA ! p + bonaPos W po | p <- pclRa W, po <- pcl ! p]
      v = k2h0 - k2h1 + k2p0 - k2p1
      h0 = [k1HsA ! p + hs ! p | p <- bothHsRa]
      h1 = [k1HsA ! p + hs ! pcOppCo p | p <- bothHsRa]
      p0 = [k1PcA ! p + bonaPos B po | p <- pcRa, po <- pcl ! p]
      p1 = [k1PcA ! pcOppCo p + bonaPos W po | p <- pcRa, po <- pcl ! p]
      lB = reverse.sort $ h0 ++ p0
      lW = reverse.sort $ h1 ++ p1
      f [] _ =0
      f bs @ (b:bb) ws @ (w:ww) = sum(map(k1Va bk0 b)bs) - sum(map(k1Va wk1 w)ws) + f bb ww
      in co == B |-> negate $ (v + f lB lW) `quot` 2