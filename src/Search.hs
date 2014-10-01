module Search where
--    import Control.Applicative
--    import Control.Arrow
--    import Control.Monad
--    import Data.Array
--    import qualified Data.ByteString as BS
--    import Data.Char
    import Data.Function
    import Data.List
--    import Data.Maybe
--    import Data.Ord
--    import Data.Tuple
--    import Data.Int
--    import Data.Word
--    import System.Environment
--    import System.IO
--    import System.IO.Unsafe

    import Board
    import Move
    import MoveGenerator
    import Eval
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