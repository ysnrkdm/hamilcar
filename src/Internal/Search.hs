module Internal.Search where
-- friends
import qualified Board
import qualified Move
import qualified MoveGenerator
import qualified Eval
-- GHC

-- libraries

-- std
import Data.Function
import Data.List

-- type Dep = Int

-- type Cnt = Int

data Result = Result {va :: Int, pv :: [Move.Mv]} deriving (Eq, Ord)

data Tree n = Node {node :: n, childNodes :: [Tree n]} deriving (Show)

conv :: Move.Mv -> Result -> Result
conv mv res = Result (-va res) (mv : pv res)

minmax :: Int -> Board.Bd -> Result
minmax 0 bd = Result (Eval.eval bd) []
minmax dep bd = maximumBy (compare `on` va) nexts
    where
        nexts = map next $ MoveGenerator.mvGenFull bd
        next mv = conv mv . minmax (dep - 1) $ Board.bdDo bd mv

{-
 -  f - function to replace Node
 -  g - function to replace Cons
 -  a - something to replace Nil
 -}
redtree :: (t -> t1 -> t2) -> (t2 -> t1 -> t1) -> t1 -> Tree t -> t2
redtree f g a Node {node = n, childNodes = c} = f n (redtree' f g a c)
--redtree' :: (t -> t1 -> t2) -> (t2 -> t1 -> t1) -> t1 -> Tree t -> t1
redtree' f g a (hd : rest) = g (redtree f g a hd) (redtree' f g a rest)
redtree' f g a [] = a

--maptree :: Tree t -> Tree b
maptree f = redtree (Node . f) (:) []

moves :: Board.Bd -> [Board.Bd]
moves bd = map (Board.bdDo bd) $ MoveGenerator.mvGenFull bd

reptree :: (t -> [t]) -> t -> Tree t
reptree f a = Node a (map (reptree f) (f a))

gametree :: Tree Board.Bd -> Tree Board.Bd
gametree p = reptree moves $ node p

maximize Node {node = n, childNodes = []} = n
maximize Node {node = n, childNodes = c} = maximum (map minimize c)
minimize Node {node = n, childNodes = []} = n
minimize Node {node = n, childNodes = c} = minimum (map maximize c)

prune 0 Node {node = n, childNodes = c} = Node n []
prune r Node {node = n, childNodes = c} = Node n $ map (prune (r - 1)) c

evaluate = maximize . maptree Eval.eval . (prune 5) . gametree