-- PORTCULLIS INTERNAL
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

-- USER CODE

-- signature: (Num -> Num)
function asciiDecToChar(dec)
  return (_eq_(dec)(48.0) > 0 and 0.0 or (_eq_(dec)(49.0) > 0 and 1.0 or (_eq_(dec)(50.0) > 0 and 2.0 or (_eq_(dec)(51.0) > 0 and 3.0 or (_eq_(dec)(52.0) > 0 and 4.0 or (_eq_(dec)(53.0) > 0 and 5.0 or (_eq_(dec)(54.0) > 0 and 6.0 or (_eq_(dec)(55.0) > 0 and 7.0 or (_eq_(dec)(56.0) > 0 and 8.0 or (_eq_(dec)(57.0) > 0 and 9.0 or 0.0))))))))))
end

-- signature: (Num -> ([Num] -> [Num]))
function _read(dec)
  return function (nums)
    return _cons_(_mult_(exp(10.0)(length(nums)))(asciiDecToChar(dec)))(nums)
  end
end

-- signature: ([Num] -> Num)
function read(decimals)
  return sum(foldl(_read)({})(decimals))
end

-- signature: (Num -> (Num -> Num))
function exp(x)
  return function (n)
    return (_lt_(n)(0.0) > 0 and exp(_div_(1.0)(x))(_minus_(0.0)(1.0)) or (_eq_(n)(0.0) > 0 and 1.0 or (_eq_(_rem_(n)(2.0))(0.0) > 0 and exp(_mult_(x)(x))(_div_(n)(2.0)) or _mult_(x)(exp(_mult_(x)(x))(_div_(_minus_(n)(1.0))(2.0))))))
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

-- signature: (_len.a -> (Num -> Num))
function _len(x)
  return function (len)
    return _plus_(1.0)(len)
  end
end

-- signature: ([length.a] -> Num)
function length(xs)
  return foldl(_len)(0.0)(xs)
end

-- signature: (Num -> (Num -> Num))
function add(a)
  return function (b)
    return _plus_(a)(b)
  end
end

-- signature: ([Num] -> Num)
function sum(ns)
  return foldl(add)(0.0)(ns)
end

-- signature: (_not.x -> (_not.x -> Atom))
function _not(a)
  return function (b)
    return _eq_(FALSE)(_eq_(a)(b))
  end
end

-- signature: (Num -> (Num -> Num))
function _max(num)
  return function (max)
    return (_gt_(num)(max) > 0 and num or max)
  end
end

-- signature: ([Num] -> Num)
function max(nums)
  return foldl(_max)(0.0)(nums)
end

-- signature: ({[curryCons.a] [[curryCons.a]]} -> [[curryCons.a]])
function curryCons(tup)
  return _cons_(tup[1])(tup[2])
end

-- signature: (_splitOn.z -> (_splitOn.z -> ({[_splitOn.z] [[_splitOn.z]]} -> {[_splitOn.z] [[_splitOn.z]]})))
function _splitOn(on)
  return function (x)
    return function (acc)
      return (_not(x)(on) > 0 and {_cons_(x)(acc[1]), acc[2]} or {{}, curryCons(acc)})
    end
  end
end

-- signature: (splitOn.a -> ([splitOn.a] -> [[splitOn.a]]))
function splitOn(on)
  return function (xs)
    return curryCons(foldl(_splitOn(on))({{}, {}})(xs))
  end
end

-- signature: ([Num] -> [Num])
function parse(buffer)
  return map(sum)(splitOn(0.0)(map(read)(splitOn(10.0)(buffer))))
end

-- signature: ([Num] -> Num)
function day1a(buffer)
  return max(parse(buffer))
end

FALSE = 0;
TRUE = 1;


pipes = {}
