module Main where
    import Control.Applicative
    import Control.Arrow
    import Control.Monad
    import Data.Array
    import qualified Data.ByteString as BS
    import Data.Char
    import Data.Function
    import Data.List
    import Data.Maybe
    import Data.Ord
    import Data.Tuple
    import Data.Int
    import Data.Word
    import System.Environment
    import System.IO
    import System.IO.Unsafe

    if'(e,t,f)=if e then t else f
    infixl 1 |+>
    (|+>)True func=func
    (|+>)False _=id
    infixl 1 |->
    (|->) b=(|+>)(not b)
    tuLi2(a,b)=[a,b]
    liTu2[a,b]=(a,b)
    liFirst f [a,b]=[f a,b]
    liSecond f [a,b]=[a,f b]
    convCh f t c=chr$ord c-ord f+ord t
    atod = convCh 'a' '1'
    dtoa = convCh '1' 'a'
    modiEn f=toEnum.f.fromEnum
    oppEn::Enum a=>a->a
    oppEn=modiEn (1-)
    castEn=modiEn id
    (//.) a ifs=a//(map (\(i,f)->(i,f(a!i))) ifs)
    showGrid f xs ys = unlines[unwords[show$f x y|x<-xs]|y<-ys]
    unDf=undefined
    toPos (f,r)= (13-f)+17*(r+1)
    fromPos= posFile&&&posRank
    posFile po= 13 - po`mod`17
    posRank po= po`div`17 - 1
    notation= map intToDigit.tuLi2.fromPos ::Int->String
    fromNotation= toPos.liTu2.map digitToInt ::String->Int
    posFromUSI= fromNotation.liSecond atod ::String->Int
    posToUSI= liSecond dtoa.notation ::Int->String
    onBdPoss=toPos<$>range((1,1),(9,9)) ::[Pos]
    notationWrong= show.map(10-).tuLi2.fromPos ::Pos->String
    posToWrongUSI= liFirst dtoa.notationWrong ::Pos->String
    -- PIECE
    data Co= B|W deriving (Eq, Ord, Enum, Ix)
    data Pro= Unp|Pro deriving (Eq,Ord,Enum,Ix)
    data Pc= Empty|Wall|Pc{co::Co,pro::Pro,p8::P8} deriving (Eq,Ord)
    data P8= FU|KY|KE|GI|KI|KA|HI|OU deriving (Eq, Ord, Enum, Ix)
    instance Enum Pc where
      fromEnum (Pc co pr p8)= fromEnum co*16+ fromEnum pr*8+ fromEnum p8
      toEnum x= Pc(toEnum$div x 16)(toEnum$div x 8`mod`2)(toEnum$mod x 8)
    instance Ix Pc where
      range (p1,p2)= [p1..p2]
      inRange (c,c') i= c<=i && i<=c'
      index b@(c,c') ci
        |inRange b ci= fromEnum ci-fromEnum c
        |otherwise= error$"Pc.index: out of range "++show b++show ci
    instance Show Pc where
      show Empty="   "
      show Wall="XXX"
      show (Pc co pr p8)=show co++show pr++show p8
    instance Show Co where
      show B="B"
      show W="W"
    instance Show Pro where
      show Pro="P"
      show Unp=""
    instance Show P8 where
      show=(:[]).(p8Chars!!).fromEnum
    p8Chars="PLNSGBRK"
    pcBnd=((Pc B Unp FU),(Pc W Pro OU))
    pcRa=range pcBnd
    pcOppCo pc=pc{co=oppEn$co pc}
    pcOppPro pc=pc{pro=oppEn$pro pc}
    unpPc pc=pc{pro=Unp}
    pcCanPro (Pc _ pro p8)= pro==Unp&& p8/=OU&& p8/=KI
    p8FromUSI c=toEnum<$>findIndex(toUpper c==)p8Chars
    isSlider p inc=case p8 p of {KA->even inc; HI->odd inc; KY->pro p==Unp; otherwise->False}
    blacki=f Unp FU OU is++f Pro FU GI(repeat g)++f Pro KA HI(repeat k) where
      f pro s e=zip [Pc B pro p8|p8<-[s..e]]
      is@[p,_,_,_,g,b,r,k] = map sort
       [[-17],p,[-35,-33],p++b,[-18,-17,-16,-1,1,17],[-18,-16,16,18],[-17,-1,1,17],b++r]
    pcIncs=(a!)::Pc->[Pos] where
      a=array pcBnd $ blacki ++ map(pcOppCo***map negate)blacki
    deltaToInc=(a!) where
      f n ds=[(d*l,d)|l<-[1..n],d<-ds]
      a=accumArray(+)0(-144,144)$f 1[-35,-33,33,35]++f 8[-18,-17,-16,-1,1,16,17,18]
    strDeltaToInc=showGrid(\x y->deltaToInc(x+y*17))[-8..8][-8..8]
    -- BOARD
    type Pos = Int
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
    -- MOVE
    data Mv=Drop{dropTo::Pos,dropPc::Pc}|Mv{fr::Pos,to::Pos,mvPc::Pc,cap::Pc,isPro::Bool} deriving(Eq,Ord)
    instance Show Mv where
      show (Drop to pc)= (dropToUSI pc)++(notation to)
      show (Mv fr to pc _ isPro)=notation to++show pc++"("++notation fr++")"++if'(isPro,"+","")
    isCapture=(/=Empty).cap
    mvColor (Drop{dropPc=x})=co x
    mvColor (Mv{mvPc=x})=co x
    dropToUSI=(++"*").show.p8
    mvToUSI (Drop to pc)=dropToUSI pc++posToUSI to
    mvToUSI (Mv fr to _ _ isPro)=posToUSI fr++posToUSI to++if'(isPro,"+","")
    -- MOVE GENERATOR
    canMv _ Wall=False
    canMv _ Empty=True
    canMv co' pc= co'/=co pc
    minRank::Pos->Pos->Co->Int
    minRank a b c= posRank$if c==B then min a b else 220-max a b
    canPro,canNoPro::Pc->Pos->Pos->Bool
    canPro pc fr to= pcCanPro pc && 3>=minRank fr to(co pc)
    canNoPro (Pc co pr p8) fr to = pr==Pro || minRank fr to co>a!p8 where a=listArray(FU,OU)[3,2,2,0,0,3,3,0]
    canDrop::Pc->Pos->Bool
    canDrop (Pc co _ p8) to= if'(co==B,id,(10-))(posRank to)>a!p8 where a=listArray(FU,OU)[1,1,2,0,0,0,0,0]
    mvGenFull bd=allInNoCheck bd++dropMvs bd
    allInNoCheck::Bd->[Mv]
    allInNoCheck (Bd sqs hs me _ pcl)=
      concatMap pcsMvs$sidePcl me pcl where
        pcsMvs::(Pc,[Pos])->[Mv]
        pcsMvs (pc,pcsqs)=concatMap pcMvs pcsqs where
          pcMvs fr=concatMap(incMvs fr)(pcIncs pc) where
            incMvs cur inc=
              case cap of
                Empty->mvAdd++if'(isSlider pc inc,incMvs to inc,[])
                Wall->[]
                otherwise->if co cap==me then [] else mvAdd
              where
                to=cur+inc
                cap=sqs!to
                mvAdd=
                  if'(canPro pc fr to,((Mv fr to pc cap True):),id)
                  $ if'(canNoPro pc fr to,([Mv fr to pc cap False]),[])
    dropMvs (Bd sqs hs me _ pcl)=
      let pcs=map fst$sideHs me hs
          space=filter((==Empty).(sqs!))$onBdPoss
          canDrop' pc to=if'(p8 pc==FU,(&&(noPawnFile!posFile to)),id)
                         $ canDrop pc to
          noPawnFile=pawnFile//[(posFile pos,False)|pos<-pcl!(Pc me Unp FU)]
          pawnFile=listArray(1,9)$repeat True
      in [(Drop to pc)|pc<-pcs,to<-space,canDrop' pc to]
    -- EVAL
    type Va=Int
    type Al=Int
    type Be=Int
    eval::Bd->Va
    eval bd =matVa bd+bonaVa bd
    matVa (Bd _ hs co _ pcl)= co==B|->negate$
      sum[pcVa pc*hs!pc|pc<-bothHsRa] + sum[pcVa pc*length(pcl!pc)|pc<-pcRa]
    pcVa::Pc->Va
    pcVa=(!)$listArray pcBnd(a++negate`fmap`a) where
      a=[100,430,450,640,690, 890,1040,15000, 420,530,540,670, -1,1150,1300, -1]
    fv::Int->Va
    fv i=(fromIntegral::Int8->Int).(fromIntegral::Word8->Int8)$BS.head$BS.drop i fvbin
    fvbin=unsafePerformIO$BS.readFile "./fv.bin"
    k1HsA,k1PcA,k2HsA,k2PcA::Array Pc Int
    k1Va,k2Va::Pos->Pos->Pos->Va
    k1P= 1476
    k2P= 738
    k1PP= (k1P*(k1P+1))`div`2 -- (1476*(1476+1))/2
    k1All= k1PP*81 -- 81*(1476*(1476+1))/2
    k1HsA= accumArray(+)0 pcBnd$zip(hsRa B)[0,38,48,58,68,78,84]++zip(hsRa W)[19,43,53,63,73,81,87]
    k2HsA= accumArray(+)0 pcBnd$zip(hsRa B)[0,19,24,29,34,39,42]++zip(hsRa W)[0,19,24,29,34,39,42]
    k1PcA= listArray pcBnd$b++map(+81)b
      where b= [ 81,225,360,504,666,828,1152, -1,666,666,666,666, -1,990,1314, -1]
    k2PcA= listArray pcBnd$l++l
      where l=[36,108,171,252,333,414,576, -1,333,333,333,333, -1,595,657, -1]
    k1Va k p p'=fv$(k*k1PP+a!p+p') where a=listArray(0,1475)[(i*(i+1))`div`2|i<-[0..]]
    k2Va k k' p=fv$(k1All+(k*81+k')*k2P+p)
    bonaPos::Co->Pos->Pos
    bonaPos co sq=if co==B then a!sq else b!sq
      where a=listArray(0,220)[sq`quot`17*9+sq`rem`17-22|sq<-[0..220]]
            b=listArray(0,220)[(80-)$sq`quot`17*9+sq`rem`17-22|sq<-[0..220]]
    posK::Pcl->(Co,Co)->Pos
    posK pcl (co,coRev) =bonaPos coRev.head$pcl!(Pc co Unp OU)
    bonaVa::Bd->Va
    bonaVa (Bd _ hs co _ pcl)= let
      [bk0,wk0,bk1,wk1]=map(posK pcl)[(B,B),(W,B),(B,W),(W,W)]
      k2h0=sum[k2Va bk0 wk0$k2HsA!p+hs!p|p<-hsRa B]
      k2h1=sum[k2Va wk1 bk1$k2HsA!p+hs!p|p<-hsRa W]
      k2p0=sum[k2Va bk0 wk0$k2PcA!p+bonaPos B po|p<-pclRa B,po<-pcl!p]
      k2p1=sum[k2Va wk1 bk1$k2PcA!p+bonaPos W po|p<-pclRa W,po<-pcl!p]
      v=k2h0-k2h1+k2p0-k2p1
      h0=[k1HsA!p+hs!p|p<-bothHsRa]
      h1=[k1HsA!p+hs!pcOppCo p|p<-bothHsRa]
      p0=[k1PcA!p+bonaPos B po|p<-pcRa,po<-pcl!p]
      p1=[k1PcA!pcOppCo p+bonaPos W po|p<-pcRa,po<-pcl!p]
      lB=reverse.sort$h0++p0
      lW=reverse.sort$h1++p1
      f [] _=0
      f bs@(b:bb) ws@(w:ww)=sum(map(k1Va bk0 b)bs)-sum(map(k1Va wk1 w)ws) + f bb ww
      in co==B|->negate$(v+f lB lW)`quot`2
    -- SEARCH
    type Dep=Int
    type Cnt=Int
    data Result=Result{va::Int,pv::[Mv]} deriving (Eq,Ord)
    conv mv res=Result(-va res)(mv:pv res)
    minmax::Int->Bd->Result
    minmax dep bd= f dep bd where
      f 0 bd= Result (eval bd) []
      f dep bd= maximumBy(on compare va)nexts where
        nexts= map next$mvGenFull bd
        next mv= conv mv.f(dep-1)$bdDo bd mv
    -- USI
    sfenStartpos= ["lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL","b","-"]
    bdFromSfen s=
      (Bd (empSqs//sqs) (empHs//hsFromSfen(ss!!2))
       (if'(ss!!1=="b",B,W)) 0
       (accumArray(flip(:))[]pcBnd$map swap sqs) ) where
        ss=if length s>=3 then s else sfenStartpos
        sqs=sqsFromSfen$ss!!0
    pcFromSfen::Char->(Co,P8)
    pcFromSfen= castEn.isLower &&& fromJust.p8FromUSI.toUpper
    sqsFromSfen sfen=parser sfen (2,4) Unp where
      parser::[Char]->(Int,Int)->Pro->[Sq]
      parser [] _ _=[]
      parser (x:xs) sq@(r,f) pro
        |isDigit x= parser xs (r,f+digitToInt x) pro
        |x=='/'   = parser xs (r+1,4) pro
        |x=='+'   = parser xs sq Pro
        |otherwise= ((r*17+f),Pc co pro p8):parser xs (r,f+1) Unp where (co,p8)=pcFromSfen x
    hsFromSfen sfen=parser sfen 1 where
      parser [] _=[]
      parser (x:xs) cnt
        |x=='-' = []
        |isDigit x = parser xs (digitToInt x)
        |otherwise = (Pc co Unp p8,cnt):parser xs 1 where (co,p8)=pcFromSfen x
    mvFromSfen (Bd sqs _ me _ _) sfen=
      case p8FromUSI(sfen!!0) of
        Just p8-> -- is drop
          let to=posFromUSI [sfen!!2,sfen!!3]
              pc=(Pc me Unp p8)
          in (Drop to pc)
        Nothing-> -- is mv
          let fr=posFromUSI [sfen!!0,sfen!!1]
              to=posFromUSI [sfen!!2,sfen!!3]
              mvPc=sqs!fr
              cap=sqs!to
              isPro= length sfen==5
          in (Mv fr to mvPc cap isPro)
    readUSIPosition sfens=
      let ws=tail$words sfens
          sbd=if'(head ws=="startpos",["startpos"],take 3 ws)
          bd=bdFromSfen sbd
          smvs=drop 1$dropWhile(/="moves")ws
          mvs=mvsFromSfen bd$smvs
      in (bd,mvs)
    usiLoop bd=do
      sfens<-getLine
      let cmds=words sfens
      case head cmds of
        "isready"->putStrLn$"readyok"
        "position"->do
          let(bd2,mvs)=readUSIPosition sfens
             bd3=bdDoMvs bd2 mvs
          usiLoop bd3
        "go"->do
          let(Result _ pv)=minmax 1 bd
          putStrLn$"bestmove "++mvToUSI(head pv)
        otherwise->putStrLn ("undefined command.."++sfens)
      usiLoop bd -- next
    mvsFromSfen::Bd->[String]->[Mv]
    mvsFromSfen bd []=[]
    mvsFromSfen bd (s:ss)=mv:mvsFromSfen(bdDo bd mv)ss where mv=mvFromSfen bd s
    -- -- Main
    main=do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      putStrLn"Usage: (not implement)"
      subs<-words<$>getLine
      case head subs of
        "usi"->putStrLn"id name pohanza\nid author nagato\nusiok">> usiLoop undefined
        otherwise->putStrLn"not implemented"