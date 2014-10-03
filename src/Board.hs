module Board (
    Sq,
    Sqs,
    Hs,
    Pcl,
    Bd(..),
    sidePcl,
    sideHs,
    bothHsRa,
    hsRa,
    pclRa,
    bdDo,
    empSqs,
    empHs,
    bdDoMvs,
) where
    -- friends
    import qualified Util
    import Util ((|+>), (//.))
    import qualified Piece
    import qualified Move
    -- GHC
    -- libraries
    -- std
    import Control.Applicative
    import Control.Arrow
    import Data.Array
    import Data.List

    type Sq = (Piece.Pos, Piece.Pc)
    type Sqs = Array Piece.Pos Piece.Pc
    type Hs = Array Piece.Pc Int
    type Pcl = Array Piece.Pc [Piece.Pos]
    data Bd = Bd {sqs::Sqs, hs::Hs, turn::Piece.Co, stage::Int, pcl::Pcl}
    instance Show Bd where
        show (Bd sqs hs co stage pcl) =
            let row r = unwords [show$sqs ! (f + r * 17) | f <- [4..12]]
                info r = case r - 1 of
                    1 -> "\tturn\t=> " ++ Util.if' (co == Piece.B, "B", "W")
                    2 -> "\tstage\t=> " ++ show stage
                    8 -> "\tBhand\t=> " ++ prettyHs Piece.B hs
                    9 -> "\tWhand\t=> " ++ prettyHs Piece.W hs
                    otherwise -> ""
            in unlines $ [row r ++ info r | r <- [2..10]] ++ map (show . second sort) (filter (([]/=) . snd) $ assocs pcl)
    empSqs = accumArray seq Piece.Wall(0, 220) $ ( , ) <$> Move.onBdPoss <*> [Piece.Empty]::Sqs
    hsBnd co = ((Piece.Pc co Piece.Unp Piece.FU), (Piece.Pc co Piece.Unp Piece.HI))
    hsRa = range . hsBnd
    bothHsRa = hsRa Piece.B ++ hsRa Piece.W
    sideHs co hs = [(pc, n) | pc <- hsRa co, let n = hs ! pc, n/= 0]
    empHs = listArray Piece.pcBnd $ repeat 0 :: Hs
    prettyHs co hs = unwords [show pc ++ show n | (pc, n) <- sideHs co hs]
    pclRa co = range ((Piece.Pc co Piece.Unp Piece.FU), (Piece.Pc co Piece.Pro Piece.HI))
    sidePcl co pcl = [(pc, pcl ! pc) | pc <- pclRa co]
    empPcl = listArray Piece.pcBnd $ repeat []
    empBd = (Board.Bd empSqs empHs Piece.B 0 empPcl)
    bdModify bd @ (Board.Bd a1 a2 a3 a4 a5) f1 f2 f3 f4 f5 = Bd (f1 a1) (f2 a2) (f3 a3) (f4 a4) (f5 a5)
    bdDoMvs, bdUndoMvs::Bd -> [Move.Mv] -> Bd
    bdDoMvs = foldl bdDo
    bdUndoMvs = foldr (flip bdUndo)
    bdDo, bdUndo::Bd -> Move.Mv -> Bd
    bdDo bd mv =
        case mv of
            (Move.Drop to pc) ->
                bdModify bd
                (// [(to, pc)])
                (//. [(pc, (subtract 1))])
                Util.oppEn (+ 1)
                (//. [(pc, (to:))])
            (Move.Mv fr to pc cap isPro) -> let tPc = (isPro |+> Piece.pcOppPro) pc in
                bdModify bd
                (// [(fr, Piece.Empty), (to, tPc)])
                (cap /= Piece.Empty |+> (//. [(Piece.pcOppCo . Piece.unpPc $ cap, (+ 1))]))
                Util.oppEn (+ 1)
                ((//. [(tPc, (to:))]) . (//. ((pc, (delete fr)):
                    Util.if' (cap /= Piece.Empty, [(cap, delete to)], []))))
    bdUndo bd mv =
        case mv of
            (Move.Drop to pc)->
                bdModify bd
                (// [(to, Piece.Empty)])
                (//. [(pc, (+ 1))])
                Util.oppEn (subtract 1)
                (//. [(pc, delete to)])
            (Move.Mv fr to pc cap isPro) ->
                let tPc = (isPro |+> Piece.pcOppPro) pc in
                bdModify bd
                (// [(fr, pc), (to, cap)])
                (cap /= Piece.Empty |+> (//.[(Piece.pcOppCo . Piece.unpPc $ cap , (subtract 1))]))
                Util.oppEn (subtract 1)
                ((//. [(tPc, (delete to))]) . (//. ((pc, (fr:)):
                    Util.if' (cap /= Piece.Empty, [(cap, (to:))], []))))