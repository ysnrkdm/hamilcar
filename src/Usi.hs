module Usi where
    import Control.Arrow
    import Data.Array
    import Data.Char
    import Data.Maybe
    import Data.Tuple
    import Util
    import Piece
    import Board
    import Move
    import Search

    castEn::Bool -> Co
    castEn = modiEn id
    sfenStartpos = ["lnsgkgsnl/1r5b1/ppppppppp/9/9/9/PPPPPPPPP/1B5R1/LNSGKGSNL", "b", "-"]
    bdFromSfen s =
        (Bd (empSqs//sqs) (empHs//hsFromSfen(ss!!2))
            (if'(ss!!1 == "b", B, W)) 0
            (accumArray(flip(:))[]pcBnd$map swap sqs) )
                where
                ss = if length s >= 3 then s else sfenStartpos
                sqs = sqsFromSfen$ss !! 0
    pcFromSfen::Char -> (Co, P8)
    pcFromSfen = castEn . isLower &&& fromJust . p8FromUSI . toUpper
    sqsFromSfen sfen = parser sfen (2, 4) Unp
        where
        parser::[Char] -> (Int, Int) -> Pro -> [Sq]
        parser [] _ _ =[]
        parser (x:xs) sq @( r, f) pro
            | isDigit x = parser xs (r, f + digitToInt x) pro
            | x =='/' = parser xs (r + 1, 4) pro
            | x =='+' = parser xs sq Pro
            | otherwise = ((r * 17 + f), Pc co pro p8):parser xs (r, f + 1) Unp
                where (co, p8) = pcFromSfen x
    hsFromSfen sfen = parser sfen 1 where
      parser [] _ =[]
      parser (x:xs) cnt
        | x == '-' = []
        | isDigit x = parser xs (digitToInt x)
        | otherwise = (Pc co Unp p8, cnt) : parser xs 1
            where (co, p8) = pcFromSfen x
    mvFromSfen (Bd sqs _ me _ _) sfen=
      case p8FromUSI(sfen!!0) of
        Just p8 -> -- is drop
          let to = posFromUSI [sfen !! 2, sfen !! 3]
              pc = (Pc me Unp p8)
          in (Drop to pc)
        Nothing -> -- is mv
          let fr = posFromUSI [sfen !! 0, sfen !! 1]
              to = posFromUSI [sfen !! 2, sfen !! 3]
              mvPc = sqs!fr
              cap = sqs!to
              isPro = length sfen == 5
          in (Mv fr to mvPc cap isPro)
    readUSIPosition sfens=
      let ws = tail$words sfens
          sbd = if'(head ws=="startpos", ["startpos"], take 3 ws)
          bd = bdFromSfen sbd
          smvs = drop 1$dropWhile(/="moves")ws
          mvs = mvsFromSfen bd$smvs
      in (bd, mvs)
    usiLoop bd = do
      sfens<-getLine
      let cmds = words sfens
      case head cmds of
        "isready"->putStrLn$"readyok"
        "position"->do
          let(bd2, mvs) = readUSIPosition sfens
             bd3 = bdDoMvs bd2 mvs
          usiLoop bd3
        "go"->do
          let(Result _ pv) = minmax 1 bd
          putStrLn $ "bestmove " ++ mvToUSI(head pv)
        otherwise -> putStrLn ("undefined command.." ++ sfens)
      usiLoop bd -- next
    mvsFromSfen::Bd -> [String] -> [Mv]
    mvsFromSfen bd [] = []
    mvsFromSfen bd (s : ss) = mv : mvsFromSfen(bdDo bd mv)ss
        where mv = mvFromSfen bd s
