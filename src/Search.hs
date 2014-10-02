module Search where
    import Data.Function
    import Data.List
    import Board
    import Move
    import MoveGenerator
    import Eval

    type Dep = Int
    type Cnt = Int
    data Result = Result{va::Int, pv::[Mv]} deriving (Eq, Ord)
    conv mv res = Result(-va res)(mv:pv res)
    minmax::Int -> Bd -> Result
    minmax dep bd = f dep bd where
        f 0 bd = Result (eval bd) []
        f dep bd = maximumBy(on compare va)nexts
            where
            nexts = map next$mvGenFull bd
            next mv = conv mv . f(dep - 1) $ bdDo bd mv