module Main where
    import Control.Applicative
    import System.IO

    import Util
    import Piece
    import Usi

    notationWrong::Pos->String
    notationWrong = show.map(10-).tuLi2.fromPos
    posToWrongUSI::Pos->String
    posToWrongUSI = liFirst dtoa.notationWrong

    main::IO ()
    main=do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      putStrLn"Usage: (not implement)"
      subs<-words<$>getLine
      case head subs of
        "usi"->putStrLn"id name hamilcar\nid author ysnrkdm\nusiok">> usiLoop undefined
        otherwise->putStrLn"not implemented"