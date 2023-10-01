foldTypeExpr :: (b -> TypeExpr -> b) -> b -> TypeExpr -> b
foldTypeExpr alg acc (TupType t1 t2) = alg (alg acc t1) t2
foldTypeExpr alg acc (ListType t) = alg acc t
foldTypeExpr alg acc (Arrow tl tr) = alg (alg acc tl) tr
foldTypeExpr alg acc te = alg acc te
