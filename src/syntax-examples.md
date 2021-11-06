Real = Num x
Nat = Num x where x > 0
Integer = Real x where x % 0 = 0
Whole = Num x where x > 0
NonZero = Real x where x /= 0
Zero = Whole x where x = 0
Enum = Real x where x = 0 | x = 2 | x = 3

floor : Real -> Integer

divFloor : Real -> NonZero -> Integer
div num den = num / den

split : Real -> Enum
split x =
  res ?=
  x > 3 ?= 0
  x == 2 = 1
  True = 3


