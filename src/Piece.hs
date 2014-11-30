module Piece (
    Pos,
    Co (..),
    Pro (..),
    Pc (..),
    P8 (..),
    pcBnd,
    pcOppPro,
    pcOppCo,
    unpPc,
    pcCanPro,
    pcIncs,
    isSlider,
    pcRa,
    p8FromUSI,
) where
-- friends
import qualified Util
-- GHC

-- libraries

-- std
import Control.Applicative
import Control.Arrow
import Data.Array
import Data.Char
import Data.List

type Pos = Int

data Co = B | W deriving (Eq, Ord, Enum, Ix)
data Pro = Unp | Pro deriving (Eq, Ord, Enum, Ix)
data Pc = Empty | Wall | Pc {co :: Co, pro :: Pro, p8 :: P8} deriving (Eq, Ord)
data P8 = FU | KY | KE | GI | KI | KA | HI | OU deriving (Eq, Ord, Enum, Ix)

instance Enum Pc where
    fromEnum (Pc co pr p8) = fromEnum co * 16 + fromEnum pr * 8 + fromEnum p8
    toEnum x = Pc (toEnum $ div x 16) (toEnum $ div x 8 `mod` 2) (toEnum $ mod x 8)
instance Ix Pc where
    range (p1, p2) = [p1 .. p2]
    inRange (c, c') i = c <= i && i <= c'
    index b @ (c, c') ci
        | inRange b ci = fromEnum ci - fromEnum c
        | otherwise= error $ "Pc.index: out of range " ++ show b ++ show ci
instance Show Pc where
    show Empty = "   "
    show Wall = "XXX"
    show (Pc co pr p8) = show co ++ show p8 ++ show pr
instance Show Co where
    show B = "B"
    show W = "W"
instance Show Pro where
    show Pro = "+"
    show Unp = " "
instance Show P8 where
    show = (: []) . (p8Chars !!) . fromEnum

p8Chars = "PLNSGBRK"

pcBnd = (Pc B Unp FU, Pc W Pro OU)

pcRa = range pcBnd

pcOppCo pc = pc {co = Util.oppEn $ co pc}
pcOppPro pc = pc {pro = Util.oppEn $ pro pc}

unpPc pc = pc {pro = Unp}

pcCanPro (Pc _ pro p8) = pro == Unp && p8 /= OU && p8 /= KI

p8FromUSI c = toEnum <$> elemIndex (toUpper c) p8Chars

isSlider p inc = case p8 p of {
    KA -> even inc;
    HI -> odd inc;
    KY -> pro p == Unp;
    otherwise -> False;
}

{-
 - Returns the move destination/direction for black player.
 - [(BP ,[-17]),
 - (BL ,[-17]),
 - (BN ,[-35,-33]),
 - (BS ,[-18,-17,-16,16,18]),
 - (BG ,[-18,-17,-16,-1,1,17]),
 - (BB ,[-18,-16,16,18]),
 - (BR ,[-17,-1,1,17]),
 - (BK ,[-18,-17,-16,-1,1,16,17,18]),
 - (BP+,[-18,-17,-16,-1,1,17]),
 - (BL+,[-18,-17,-16,-1,1,17]),
 - (BN+,[-18,-17,-16,-1,1,17]),
 - (BS+,[-18,-17,-16,-1,1,17]),
 - (BB+,[-18,-17,-16,-1,1,16,17,18]),
 - (BR+,[-18,-17,-16,-1,1,16,17,18])]
-}
blacki :: [(Pc, [Pos])]
blacki = f Unp FU OU is ++ f Pro FU GI (repeat g) ++ f Pro KA HI (repeat k)
    where
        f prom s e = zip [Pc B prom p8e | p8e <- [s .. e]]
        is @ [p, _, _, _, g, b, r, k] = map sort
            [[-17], p, [-35, -33], p ++ b, [-18, -17, -16, -1, 1, 17], [-18, -16, 16, 18], [-17, -1, 1, 17], b ++ r]

-- Returns the move destination/direction for given piece
pcIncs :: Pc -> [Pos]
pcIncs = (a !)
    where
        a = array pcBnd $ blacki ++ map (pcOppCo *** map negate) blacki

deltaToInc = (a !)
    where
        f n ds = [(d * l, d) | l <- [1 .. n], d <- ds]
        a = accumArray (+) 0 (-144, 144) $ f 1 [-35, -33, 33, 35] ++ f 8 [-18, -17, -16, -1, 1, 16, 17, 18]

strDeltaToInc = Util.showGrid (\ x y -> deltaToInc (x + y * 17)) [-8 .. 8] [-8 .. 8]
