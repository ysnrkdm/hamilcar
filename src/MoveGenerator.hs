module MoveGenerator where
    import Data.Array
    import Util
    import Piece
    import Board
    import Move

    canMv _ Wall = False
    canMv _ Empty = True
    canMv co' pc = co' /= co pc
    minRank::Pos -> Pos -> Co -> Int
    minRank a b c = posRank $ if c == B then min a b else 220 - max a b
    canPro, canNoPro::Pc -> Pos -> Pos -> Bool
    canPro pc fr to = pcCanPro pc && 3 >= minRank fr to (co pc)
    canNoPro (Pc co pr p8) fr to = pr == Pro || minRank fr to co > a ! p8 where a = listArray (FU, OU) [3, 2, 2, 0, 0, 3, 3, 0]
    canDrop::Pc->Pos->Bool
    canDrop (Pc co _ p8) to = if'(co == B, id, (10 -)) (posRank to) > a ! p8 where a = listArray (FU, OU) [1, 1, 2, 0, 0, 0, 0, 0]
    mvGenFull bd = allInNoCheck bd ++ dropMvs bd
    allInNoCheck::Bd -> [Mv]
    allInNoCheck (Bd sqs hs me _ pcl) =
      concatMap pcsMvs$sidePcl me pcl where
        pcsMvs::(Pc, [Pos]) -> [Mv]
        pcsMvs (pc, pcsqs) = concatMap pcMvs pcsqs where
          pcMvs fr = concatMap (incMvs fr) (pcIncs pc) where
            incMvs cur inc =
              case cap of
                Empty -> mvAdd ++ if'(isSlider pc inc, incMvs to inc, [])
                Wall -> []
                otherwise -> if co cap == me then [] else mvAdd
              where
                to = cur + inc
                cap = sqs ! to
                mvAdd =
                  if' (canPro pc fr to, ((Mv fr to pc cap True):), id)
                  $ if' (canNoPro pc fr to, ([Mv fr to pc cap False]), [])
    dropMvs (Bd sqs hs me _ pcl) =
      let pcs = map fst $ sideHs me hs
          space = filter ((== Empty) . (sqs !)) $ onBdPoss
          canDrop' pc to = if' (p8 pc == FU, (&& (noPawnFile ! posFile to)), id)
                         $ canDrop pc to
          noPawnFile = pawnFile // [(posFile pos, False) | pos <- pcl ! (Pc me Unp FU)]
          pawnFile = listArray (1, 9) $ repeat True
      in [(Drop to pc) | pc <- pcs, to <- space, canDrop' pc to]
