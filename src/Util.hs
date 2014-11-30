module Util (
    modiEn,
    if',
    posFromUSI,
    oppEn,
    showGrid,
    notation,
    posToUSI,
    toPos,
    (|->),
    (//.),
    (|+>),
    posRank,
    posFile,
    tuLi2,
    fromPos,
    liFirst,
    dtoa,
) where
-- friends

-- GHC

-- libraries

-- std
import Control.Arrow
import Data.Array
import Data.Char

if' :: (Bool, t, t) -> t
if' (e, t, f) = if e then t else f

(|+>) :: Bool -> (a -> a) -> a -> a
infixl 1 |+>
(|+>) True func = func
(|+>) False _ = id

(|->) :: Bool -> (a -> a) -> a -> a
infixl 1 |->
(|->) b = (|+>) (not b)

tuLi2 :: (t, t) -> [t]
tuLi2 (a, b) = [a, b]

liTu2 :: [t] -> (t, t)
liTu2 [a, b] = (a, b)

liFirst :: (t -> t) -> [t] -> [t]
liFirst f [a, b] = [f a, b]

liSecond :: (t -> t) -> [t] -> [t]
liSecond f [a, b] = [a, f b]

convCh f t c = chr $ ord c - ord f + ord t

atod = convCh 'a' '1'
dtoa = convCh '1' 'a'

modiEn f = toEnum . f . fromEnum
oppEn :: Enum a => a -> a
oppEn = modiEn (1 -)

(//.) a ifs = a // map (\ (i, f) -> (i, f (a ! i))) ifs

showGrid f xs ys = unlines[unwords[show $ f x y | x <- xs] | y <- ys]

--unDf = undefined

toPos (f, r) = (13 - f) + 17 * (r + 1)
fromPos = posFile &&& posRank

posFile po = 13 - po `mod` 17
posRank po = po `div` 17 - 1

notation :: Int -> String
notation = map intToDigit . tuLi2 . fromPos

fromNotation :: String -> Int
fromNotation = toPos . liTu2 . map digitToInt

posFromUSI :: String -> Int
posFromUSI = fromNotation . liSecond atod
posToUSI :: Int -> String
posToUSI = liSecond dtoa . notation
