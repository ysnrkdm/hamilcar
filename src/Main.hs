module Main (
    main
) where
-- friends
import qualified Piece
import qualified Usi
import qualified Util

-- GHC

-- libraries

-- std
import Control.Applicative
import System.IO

notationWrong :: Piece.Pos -> String
notationWrong = show . map (10 -) . Util.tuLi2 . Util.fromPos
posToWrongUSI :: Piece.Pos -> String
posToWrongUSI = Util.liFirst Util.dtoa . notationWrong

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "Usage: (not implement)"
  subs <- words <$> getLine
  case head subs of
    "usi" -> putStrLn "id name hamilcar\nid author ysnrkdm\nusiok" >> Usi.usiLoop undefined
    otherwise -> putStrLn "not implemented"
