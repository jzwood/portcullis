function _plus_(a)
  return function (b)
    return a + b
  end
end

function _minus_(a)
  return function (b)
    return a - b
  end
end

function _mult_(a)
  return function (b)
    return a * b
  end
end

function _div_(a)
  return function (b)
    return a / b
  end
end

function _rem_(a)
  return function (b)
    return a % b
  end
end

function _gt_(a)
  return function (b)
    if a > b then
      return 1
    else
      return 0
    end
  end
end

function _gte_(a)
  return function (b)
    if a >= b then
      return 1
    else
      return 0
    end
  end
end

function _lt_(a)
  return function (b)
    if a < b then
      return 1
    else
      return 0
    end
  end
end

function _lte_(a)
  return function (b)
    if a <= b then
      return 1
    else
      return 0
    end
  end
end

function _eq_(a)
  return function (b)
    if a == b then
      return 1
    elseif type(a) == 'table' and type(b) == 'table' then
      if _eq_(a.head)(b.head) == 1 and _eq_(a.tail)(b.tail) == 1 then
        return 1
      else
        return 0
      end
    else
      return 0
    end
  end
end

function _cons_(a)
  return function (b)
    return { head = a, tail = b }
  end
end

function print_arr(tbl)
  local head = tbl.head
  if head ~= nil then
    print(head)
    print_arr(tbl.tail)
  end
end

-- signature: (id1.x -> id1.x)
function id1(x)
  return x
end

-- signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
function compose(f)
  return function (g)
    return function (x)
      return f(g(x))
    end
  end
end

-- signature: (Num -> Num)
function double(x)
  return _mult_(2.0)(x)
end

-- signature: (Num -> Num)
function quadruple(n)
  return compose(double)(double)(n)
end

-- signature: (id2.x -> id2.x)
function id2(x)
  return compose(id1)(id1)(x)
end

-- signature: Num
function __one1()
  return 1.0
end

-- signature: Num
function __one2()
  return compose(id1)(id1)(one1)
end

-- signature: (id3.z -> ([id3.z] -> [id3.z]))
function id3(x)
  return function (xs)
    return xs
  end
end

-- signature: ([tail.t] -> [tail.t])
function tail(xs)
  return (_eq_(xs)({}) > 0 and xs or id3(xs.head)(xs.tail))
end

-- signature: ((a.h -> a.h) -> (a.h -> a.h))
function a(fx)
  return function (x)
    return fx(x)
  end
end

-- signature: (b.q -> b.q)
function b(w)
  return w
end

-- signature: (c.p -> c.p)
function c(y)
  return a(b)(y)
end

-- signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
function push(ys)
  return function (x)
    return function (xs)
      return _cons_(x)(concat(xs)(ys))
    end
  end
end

-- signature: ([concat.a] -> ([concat.a] -> [concat.a]))
function concat(xs)
  return function (ys)
    return (_eq_(xs)({}) > 0 and ys or push(ys)(xs.head)(xs.tail))
  end
end

-- signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
function filter2(g)
  return function (w)
    return function (ws)
      return concat((g(w) > 0 and {w} or {}))(filter(g)(ws))
    end
  end
end

-- signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
function filter(f)
  return function (xs)
    return (_eq_(xs)({}) > 0 and xs or filter2(f)(xs.head)(xs.tail))
  end
end

-- signature: (eq.a -> (eq.a -> Atom))
function eq(x)
  return function (y)
    return _eq_(x)(y)
  end
end

-- signature: ([Num] -> [Num])
function seven(xs)
  return filter(eq(7.0))(xs)
end

-- signature: Num
function __eight()
  return _plus_(3.0)(5.0)
end

FALSE = 0;
TRUE = 1;

one1 = __one1()
one2 = __one2()
eight = __eight()

pipes = {}
