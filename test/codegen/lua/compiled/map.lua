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

-- signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
function _map(f)
  return function (x)
    return function (xs)
      return _cons_(f(x))(map(f)(xs))
    end
  end
end

-- signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
function map(f)
  return function (xs)
    return (_eq_(xs)({}) > 0 and {} or _map(f)(xs.head)(xs.tail))
  end
end

-- signature: (Num -> Num)
function add1(x)
  return _plus_(x)(1.0)
end

-- signature: [Num]
function __mapped()
  return map(add1)({ head = 1.0, tail = { head = 2.0, tail = { head = 3.0, tail = { head = 4.0, tail = {} } } } })
end

-- signature: ((_foldr.j -> (_foldr.k -> _foldr.k)) -> (_foldr.k -> (_foldr.j -> ([_foldr.j] -> _foldr.k))))
function _foldr(alg)
  return function (acc)
    return function (x)
      return function (xs)
        return foldr(alg)(alg(x)(acc))(xs)
      end
    end
  end
end

-- signature: ((foldr.a -> (foldr.b -> foldr.b)) -> (foldr.b -> ([foldr.a] -> foldr.b)))
function foldr(alg)
  return function (acc)
    return function (xs)
      return (_eq_(xs)({}) > 0 and acc or _foldr(alg)(acc)(xs.head)(xs.tail))
    end
  end
end

-- signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
function _foldl(alg)
  return function (acc)
    return function (x)
      return function (xs)
        return alg(x)(foldl(alg)(acc)(xs))
      end
    end
  end
end

-- signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
function foldl(alg)
  return function (acc)
    return function (xs)
      return (_eq_(xs)({}) > 0 and acc or _foldl(alg)(acc)(xs.head)(xs.tail))
    end
  end
end

-- signature: (Num -> (Num -> Num))
function add(a)
  return function (b)
    return _plus_(a)(b)
  end
end

-- signature: ([Num] -> Num)
function sum(ns)
  return foldr(add)(0.0)(ns)
end

-- signature: Num
function __total()
  return sum({ head = 1.0, tail = { head = 2.0, tail = { head = 3.0, tail = {} } } })
end

-- signature: (Num -> ([Num] -> [Num]))
function double(n)
  return function (ns)
    return _cons_(n)(_cons_(n)(ns))
  end
end

-- signature: ([Num] -> [Num])
function mapDouble(ns)
  return foldl(double)({})(ns)
end

-- signature: [Num]
function __dub()
  return mapDouble({ head = 1.0, tail = { head = 3.0, tail = { head = 5.0, tail = { head = 7.0, tail = {} } } } })
end

-- signature: (push.z -> ([push.z] -> [push.z]))
function push(x)
  return function (xs)
    return _cons_(x)(xs)
  end
end

-- signature: ([reverse.a] -> [reverse.a])
function reverse(xs)
  return foldr(push)({})(xs)
end

-- signature: (Num -> [Num])
function range(n)
  return (_lte_(n)(0.0) > 0 and {} or _cons_(n)(range(_minus_(n)(1.0))))
end

FALSE = 0;
TRUE = 1;

mapped = __mapped()
total = __total()
dub = __dub()

pipes = {}
