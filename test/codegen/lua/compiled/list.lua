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

-- signature: (cons.a -> ([cons.a] -> [cons.a]))
function cons(x)
  return function (xs)
    return _cons_(x)(_cons_(x)(_cons_(x)(xs)))
  end
end

-- signature: (count.a -> ([count.a] -> Num))
function count(x)
  return function (xs)
    return _plus_(1.0)(length(xs))
  end
end

-- signature: ([uncons.a] -> (uncons.b -> ((uncons.a -> ([uncons.a] -> uncons.b)) -> uncons.b)))
function uncons(as)
  return function (b)
    return function (f)
      return b
    end
  end
end

-- signature: ((tail2.w -> ([tail2.w] -> [tail2.w])) -> ([tail2.f] -> [tail2.f]))
function tail2(f)
  return function (ns)
    return uncons(ns)(ns)(f)
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

-- signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
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

-- signature: (lid.a -> [lid.a])
function lid(x)
  return { head = x, tail = {} }
end

FALSE = 0;
TRUE = 1;


pipes = {}
