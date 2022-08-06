module Network.URL.Internal.Predicate

%default total

--  针对类型a的谓词
public export
Predicate : Type -> Type 
Predicate a = a -> Bool

-- 特别的，如果没有谓词，算是不通过
-- 任意一个通过，算是整体通过
public export
anyPass : List (Predicate a) -> a -> Bool
anyPass [] _ = False
anyPass [p] x = p x
anyPass (p :: ps) x = p x || anyPass ps x

-- 特别的，如果没有谓词，算是不通过
-- 全部通过，才算是整体通过
public export
allPass : List (Predicate a) -> a -> Bool
allPass [] _ = False
allPass [p] x = p x
allPass (p :: ps) x = p x && allPass ps x
