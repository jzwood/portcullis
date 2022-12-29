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

-- signature: (Num -> (Num -> Num))
function add(a)
  return function (b)
    return _plus_(a)(b)
  end
end

-- signature: (Num -> (Num -> (Num -> Num)))
function add3(a)
  return function (b)
    return function (c)
      return _plus_(_plus_(a)(b))(c)
    end
  end
end

-- signature: (Num -> Num)
function calc0(num)
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0))
end

-- signature: (Num -> Num)
function calc1(num)
  return _plus_(_minus_(0.0)(_mult_(num)(9.0)))(_div_(3.0)(4.0))
end

-- signature: (Num -> Atom)
function choose0(num)
  return (_gt_(num)(0.0) > 0 and COOL or (_eq_(num)(0.0) > 0 and HMM or BAD))
end

-- signature: (Num -> Atom)
function choose1(num)
  return (_lte_(num)(0.0) > 0 and COOL or (_lt_(num)(100.0) > 0 and HMM or BAD))
end

-- signature: (Num -> Atom)
function isEven(num)
  return _eq_(_rem_(num)(2.0))(0.0)
end

-- signature: (append3.a -> ([append3.a] -> [append3.a]))
function append3(x)
  return function (xs)
    return _cons_(x)(_cons_(x)(_cons_(x)(xs)))
  end
end

FALSE = 0;
TRUE = 1;
COOL = 2;
HMM = 3;
BAD = 4;


pipes = {}
