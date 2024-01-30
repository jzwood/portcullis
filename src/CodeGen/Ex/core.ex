  # PORTCULLIS INTERNAL
  def plus_(a), do: fn b -> a + b end
  def minus_(a), do: fn b -> a - b end
  def mult_(a), do: fn b -> a * b end
  def div_(a), do: fn b -> a / b end
  def rem_(a), do: fn b -> rem(a, b) end
  def rem_(a), do: fn b -> rem(a, b) end
  def gt_(a), do: fn b -> a > b end
  def gte_(a), do: fn b -> a >= b end
  def lt_(a), do: fn b -> a < b end
  def lte_(a), do: fn b -> a <= b end
  def eq_(a), do: fn b -> a == b end
  def eq_(a), do: fn b -> a == b end
  def cons_(a), do: fn b -> [a | b] end

  def if_(0), do: fn _ -> fn b -> b end end
  def if_(_), do: fn a -> fn _ -> a end end

  def uncons_([]), do: fn b -> fn _ -> b end end
  def uncons_([h | t]), do: fn _ -> fn f -> f(h).(t) end end

  # USER CODE
