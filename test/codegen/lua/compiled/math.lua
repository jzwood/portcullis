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

function printList(tbl)
  local head = tbl.head
  if head ~= nil then
    if tbl.tail.head ~= nil then
      io.write(head, ', ')
      printList(tbl.tail)
    else
      print(head)
    end
  end
end

-- signature: ((_not.a -> (_not.a -> Atom)) -> (_not.a -> (_not.a -> Atom)))
function _not(f)
  return function (a)
    return function (b)
      return (_eq_(FALSE)(f(a)(b)) > 0 and TRUE or FALSE)
    end
  end
end

-- signature: (_length.a -> ([_length.a] -> Num))
function _length(x)
  return function (xs)
    return _plus_(1.0)(length(xs))
  end
end

-- signature: ([length.a] -> Num)
function length(xs)
  return (_eq_(xs)({}) > 0 and 0.0 or _length(xs.head)(xs.tail))
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

-- signature: (identity2.x -> ([identity2.x] -> [identity2.x]))
function identity2(x)
  return function (xs)
    return xs
  end
end

-- signature: ([tail.p] -> [tail.p])
function tail(xs)
  return (_eq_(xs)({}) > 0 and xs or identity2(xs.head)(xs.tail))
end

-- signature: ([drop.g] -> (Num -> [drop.g]))
function drop(xs)
  return function (n)
    return (_lte_(n)(0.0) > 0 and xs or drop(tail(xs))(_minus_(n)(1.0)))
  end
end

-- signature: (Num -> (_take.f -> ([_take.f] -> [_take.f])))
function _take(n)
  return function (x)
    return function (xs)
      return (_lte_(n)(0.0) > 0 and {} or _cons_(x)(take(xs)(_minus_(n)(1.0))))
    end
  end
end

-- signature: ([take.k] -> (Num -> [take.k]))
function take(xs)
  return function (n)
    return (_eq_(xs)({}) > 0 and {} or _take(n)(xs.head)(xs.tail))
  end
end

-- signature: ([slice.q] -> (Num -> (Num -> [slice.q])))
function slice(xs)
  return function (i)
    return function (j)
      return take(drop(xs)(i))(_minus_(j)(i))
    end
  end
end

-- signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
function filter2(g)
  return function (w)
    return function (ws)
      return concat((g(w) > 0 and { head = w, tail = {} } or {}))(filter(g)(ws))
    end
  end
end

-- signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
function filter(f)
  return function (xs)
    return (_eq_(xs)({}) > 0 and xs or filter2(f)(xs.head)(xs.tail))
  end
end

-- signature: (Num -> Num)
function neg(x)
  return _minus_(0.0)(x)
end

-- signature: [Num]
function __empty()
  return {}
end

-- signature: ([Num] -> [Num])
function msort(ns)
  return msort2(length(ns))(ns)
end

-- signature: (Num -> ([Num] -> [Num]))
function msort2(len)
  return function (ns)
    return (_lte_(len)(1.0) > 0 and ns or merge(msort(take(ns)(_div_(len)(2.0))))(msort(drop(ns)(_div_(len)(2.0)))))
  end
end

-- signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
function merge3(x)
  return function (xs)
    return function (y)
      return function (ys)
        return (_lte_(x)(y) > 0 and _cons_(x)(merge(xs)(_cons_(y)(ys))) or _cons_(y)(merge(_cons_(x)(xs))(ys)))
      end
    end
  end
end

-- signature: ([Num] -> (Num -> ([Num] -> [Num])))
function merge2(ys)
  return function (x)
    return function (xs)
      return (_eq_(ys)({}) > 0 and _cons_(x)(xs) or merge3(x)(xs)(ys.head)(ys.tail))
    end
  end
end

-- signature: ([Num] -> ([Num] -> [Num]))
function merge(xs)
  return function (ys)
    return (_eq_(xs)({}) > 0 and ys or merge2(ys)(xs.head)(xs.tail))
  end
end

-- signature: (Num -> (Num -> Num))
function avg(a)
  return function (b)
    return _mult_(0.5)(_plus_(a)(b))
  end
end

-- signature: ([Num] -> Num)
function mean(xs)
  return _div_(total(xs))(length(xs))
end

-- signature: (Num -> ([Num] -> Num))
function _total(x)
  return function (xs)
    return _plus_(x)(total(xs))
  end
end

-- signature: ([Num] -> Num)
function total(xs)
  return (_eq_(xs)({}) > 0 and 0.0 or _total(xs.head)(xs.tail))
end

-- signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
function compose(f)
  return function (g)
    return function (x)
      return f(g(x))
    end
  end
end

-- signature: (Atom -> (Atom -> {Atom Atom}))
function rankPet(p1)
  return function (p2)
    return (_eq_(CHIPMUNK)(p1) > 0 and {p1, p2} or {p2, p1})
  end
end

-- signature: (Num -> (Num -> Atom))
function lt(x)
  return function (y)
    return _gt_(x)(y)
  end
end

-- signature: (Num -> (Num -> Atom))
function gte(x)
  return function (y)
    return _lte_(x)(y)
  end
end

-- signature: (Num -> ([Num] -> [Num]))
function qsortp(x)
  return function (xs)
    return concat(qsort(filter(lt(x))(xs)))(_cons_(x)(qsort(filter(gte(x))(xs))))
  end
end

-- signature: ([Num] -> [Num])
function qsort(xs)
  return (_eq_(xs)({}) > 0 and xs or qsortp(xs.head)(xs.tail))
end

FALSE = 0;
TRUE = 1;
CHIPMUNK = 2;

empty = __empty()

pipes = {}
