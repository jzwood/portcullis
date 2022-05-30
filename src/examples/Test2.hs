{-# LANGUAGE RankNTypes #-}


module Test2 where


--hm1 -> w -> -> w w w
--hm1 z f = (f z)
--test1 -> -> z z Atom
--test1 f = (hm1 Jake f)
--
a1 :: w -> (forall x. x -> x) -> w
a1 w fx = fx w

a2 :: (forall z. z -> z) -> Bool
a2 f = a1 True f

b1 :: w -> (w -> w) -> w
b1 w fw = fw w

b2 :: (forall z. z -> z) -> Bool
b2 f = (b1 True) f

test :: (a -> b)
test = undefined
