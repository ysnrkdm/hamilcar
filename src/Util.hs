module Util where
    import Control.Arrow
    import Data.Array
    import Data.Char

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