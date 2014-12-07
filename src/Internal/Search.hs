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
import Data.Ord
import Debug.Trace

-- type Dep = Int

-- type Cnt = Int

data Result = Result {va :: Int, pv :: [Move.Mv]} deriving (Eq, Ord)
instance Show Result where
    show (Result va pv) = "va: " ++ (show va) ++ ", pv : " ++ (show pv)

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
redtree' f g a (hd : rest) = g (redtree f g a hd) (redtree' f g a rest)
redtree' f g a [] = a

maptree f = redtree (Node . f) (:) []

moves :: (Board.Bd, Result) -> [(Board.Bd, Result)]
moves (bd, result) =
    map (\ x -> (Board.bdDo bd x, (conv x result))) $ MoveGenerator.mvGenFull bd

reptree :: (t -> [t]) -> t -> Tree t
reptree f a = Node a (map (reptree f) (f a))

gametree :: Board.Bd -> Tree (Board.Bd, Result)
gametree p = reptree moves $ (p, Result 0 [Move.Nil])

maximize Node {node = n, childNodes = []} = n
maximize Node {node = _, childNodes = c} = maximum (map minimize c)
minimize Node {node = n, childNodes = []} = n
minimize Node {node = _, childNodes = c} = minimum (map maximize c)

prune 0 Node {node = n, childNodes = _} = Node n []
prune r Node {node = n, childNodes = c} = Node n $ map (prune (r - 1)) c

-- minmax method
evaluate = maximize . maptree (Eval.eval . fst) . (prune 3) . gametree

-- alpha beta method
maximize' Node {node = n, childNodes = []} = n : []
maximize' Node {node = _, childNodes = c} = mapmin (map minimize' c)
minimize' Node {node = n, childNodes = []} = n : []
minimize' Node {node = _, childNodes = c} = mapmax (map maximize' c)

-- map min/max
mapmin (nums : rest) = (minimum nums) : (omitmin (minimum nums) rest)
mapmax (nums : rest) = (maximum nums) : (omitmax (maximum nums) rest)

omitmin pot [] = []
omitmin pot (nums : rest)
    | minleq nums pot = omitmin pot rest
    | otherwise = (minimum nums) : (omitmin (minimum nums) rest)

minleq [] pot = False
minleq (num : rest) pot
    | num <= pot = True
    | otherwise = minleq rest pot

omitmax pot [] = []
omitmax pot (nums : rest)
    | maxgeq nums pot = omitmax pot rest
    | otherwise = (maximum nums) : (omitmax (maximum nums) rest)

maxgeq [] pot = False
maxgeq (num : rest) pot
    | num >= pot = True
    | otherwise = maxgeq rest pot

highfirst Node {node = n, childNodes = c} = Node n (sortBy (comparing node) (map lowfirst c))
lowfirst Node {node = n, childNodes = c} = Node n (sortBy (flip $ comparing node) (map highfirst c))

-- alphabeta
alphabeta = maximum . maximize' . highfirst . maptree (Eval.eval . fst) . (prune 3) . gametree