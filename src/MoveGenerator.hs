module MoveGenerator(
    canMv,
    minRank,
    canPro,
    mvGenFull,
) where
-- friends
import qualified Util
import qualified Piece
import qualified Board
import qualified Move
-- GHC

-- libraries

-- std
import Data.Array

canMv _ Piece.Wall = False
canMv _ Piece.Empty = True
canMv co' pc = co' /= Piece.co pc

minRank :: Piece.Pos -> Piece.Pos -> Piece.Co -> Int
minRank a b c = Util.posRank $ if c == Piece.B then min a b else 220 - max a b

canPro, canNoPro :: Piece.Pc -> Piece.Pos -> Piece.Pos -> Bool
canPro pc fr to = Piece.pcCanPro pc && 3 >= minRank fr to (Piece.co pc)

canNoPro (Piece.Pc co pr p8) fr to = pr == Piece.Pro || minRank fr to co > a ! p8
    where
        a = listArray (Piece.FU, Piece.OU) [3, 2, 2, 0, 0, 3, 3, 0]

canDrop :: Piece.Pc -> Piece.Pos -> Bool
canDrop (Piece.Pc co _ p8) to = Util.if' (co == Piece.B, id, (10 -)) (Util.posRank to) > a ! p8
    where
        a = listArray (Piece.FU, Piece.OU) [1, 1, 2, 0, 0, 0, 0, 0]

mvGenFull bd = allInNoCheck bd ++ dropMvs bd

allInNoCheck :: Board.Bd -> [Move.Mv]
allInNoCheck (Board.Bd sqs hs me _ pcl) =
    concatMap pcsMvs $ Board.sidePcl me pcl
        where
            pcsMvs :: (Piece.Pc, [Piece.Pos]) -> [Move.Mv]
            pcsMvs (pc, pcsqs) = concatMap pcMvs pcsqs
                where
                    pcMvs fr = concatMap (incMvs fr) (Piece.pcIncs pc)
                        where
                            incMvs cur inc =
                                case cap of
                                    Piece.Empty -> mvAdd ++ Util.if' (Piece.isSlider pc inc, incMvs to inc, [])
                                    Piece.Wall -> []
                                    otherwise -> if Piece.co cap == me then [] else mvAdd
                                where
                                    to = cur + inc
                                    cap = sqs ! to
                                    mvAdd =
                                        Util.if' (canPro pc fr to, (Move.Mv fr to pc cap True :), id)
                                        $ Util.if' (canNoPro pc fr to, [Move.Mv fr to pc cap False], [])

dropMvs (Board.Bd sqs hs me _ pcl) =
    let
        pcs = map fst $ Board.sideHs me hs
        space = filter ((== Piece.Empty) . (sqs !)) Move.onBdPoss
        canDrop' pc to = Util.if' (Piece.p8 pc == Piece.FU, (&& (noPawnFile ! Util.posFile to)), id) $ canDrop pc to
        noPawnFile = pawnFile // [(Util.posFile pos, False) | pos <- pcl ! Piece.Pc me Piece.Unp Piece.FU]
        pawnFile = listArray (1, 9) $ repeat True
    in
        [Move.Drop to pc | pc <- pcs, to <- space, canDrop' pc to]
