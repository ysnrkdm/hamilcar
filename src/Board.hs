module Board where
    import Control.Applicative
    import Control.Arrow
    import Data.Array
    import Data.List
    import Util
    import Piece
    import Move

    -- BOARD
    type Sq = (Pos,Pc)
    type Sqs= Array Pos Pc
    type Hs= Array Pc Int
    type Pcl= Array Pc [Pos]
    data Bd=Bd{sqs::Sqs,hs::Hs,turn::Co,stage::Int,pcl::Pcl}
    instance Show Bd where
      show (Bd sqs hs co stage pcl)=
        let row r=unwords [show$sqs!(f+r*17)|f<-[4..12]]
            info r=case r-1 of
              1->"\tturn\t=> " ++if'(co==B,"B","W")
              2->"\tstage\t=> " ++show stage
              8->"\tBhand\t=> "++prettyHs B hs
              9->"\tWhand\t=> "++prettyHs W hs
              otherwise->""
        in unlines$[row r++info r|r<-[2..10]]++map (show.second sort) (filter(([]/=).snd)$assocs pcl)
    empSqs= accumArray seq Wall(0,220)$(,)<$>onBdPoss<*>[Empty] :: Sqs
    hsBnd co=((Pc co Unp FU),(Pc co Unp HI))
    hsRa=range.hsBnd
    bothHsRa=hsRa B++hsRa W
    sideHs co hs=[(pc,n)|pc<-hsRa co,let n=hs!pc,n/=0]
    empHs=listArray pcBnd$repeat 0 ::Hs
    prettyHs co hs= unwords[show pc++show n|(pc,n)<-sideHs co hs]
    pclRa co=range((Pc co Unp FU),(Pc co Pro HI))
    sidePcl co pcl=[(pc,pcl!pc)|pc<-pclRa co]
    empPcl=listArray pcBnd$repeat []
    empBd=(Bd empSqs empHs B 0 empPcl)
    bdModify bd@(Bd a1 a2 a3 a4 a5) f1 f2 f3 f4 f5= Bd(f1 a1)(f2 a2)(f3 a3)(f4 a4)(f5 a5)
    bdDoMvs,bdUndoMvs::Bd->[Mv]->Bd
    bdDoMvs  = foldl bdDo
    bdUndoMvs= foldr (flip bdUndo)
    bdDo,bdUndo::Bd->Mv->Bd
    bdDo bd mv=
      case mv of
        (Drop to pc)->
          bdModify bd
          (//[(to,pc)])
          (//.[(pc,(subtract 1))])
          oppEn (+1)
          (//.[(pc,(to:))])
        (Mv fr to pc cap isPro)-> let tPc=(isPro|+>pcOppPro)pc in
          bdModify bd
          (//[(fr,Empty),(to,tPc)])
          (cap/=Empty|+>(//.[(pcOppCo.unpPc$cap,(+1))]))
          oppEn (+1)
          ((//.[(tPc,(to:))]).
           (//.((pc,(delete fr)):
                if'(cap/=Empty,[(cap,delete to)],[]))))
    bdUndo bd mv=
      case mv of
        (Drop to pc)->
          bdModify bd
          (//[(to,Empty)])
          (//.[(pc,(+1))])
          oppEn (subtract 1)
          (//.[(pc,delete to)])
        (Mv fr to pc cap isPro)->
          let tPc=(isPro|+>pcOppPro)pc in
          bdModify bd
          (//[(fr,pc),(to,cap)])
          (cap/=Empty|+>(//.[(pcOppCo.unpPc$cap,(subtract 1))]))
          oppEn (subtract 1)
          ((//.[(tPc,(delete to))]).
           (//.((pc,(fr:)):
                if'(cap/=Empty,[(cap,(to:))],[]))))