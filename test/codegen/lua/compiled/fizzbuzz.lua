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
    print(head)
    printList(tbl.tail)
  end
end

-- signature: ((_map.a -> _map.b) -> (_map.a -> ([_map.a] -> [_map.b])))
function _map(f)
  return function (x)
    return function (xs)
      return _cons_(f(x))(map(f)(xs))
    end
  end
end

-- signature: ((map.a -> map.b) -> ([map.a] -> [map.b]))
function map(f)
  return function (xs)
    return (_eq_(xs)({}) > 0 and {} or _map(f)(xs.head)(xs.tail))
  end
end

-- signature: (Num -> (Num -> [Num]))
function range(n0)
  return function (n1)
    return (_gt_(n0)(n1) > 0 and {} or _cons_(n0)(range(_plus_(n0)(1.0))(n1)))
  end
end

-- signature: (Num -> Num)
function _fizzbuzz(n)
  return (_eq_(0.0)(_rem_(n)(15.0)) > 0 and _minus_(0.0)(35.0) or (_eq_(0.0)(_rem_(n)(5.0)) > 0 and _minus_(0.0)(5.0) or (_eq_(0.0)(_rem_(n)(3.0)) > 0 and _minus_(0.0)(3.0) or n)))
end

-- signature: (Num -> [Num])
function fizzbuzz(n)
  return map(_fizzbuzz)(range(1.0)(n))
end

-- signature: [Num]
function __fb()
  return fizzbuzz(20.0)
end

FALSE = 0;
TRUE = 1;

fb = __fb()

pipes = {}
