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

-- signature: (id.a -> id.a)
function id(x)
  return x
end

-- signature: (cmp.x -> (cmp.x -> Atom))
function cmp(a)
  return function (b)
    return _eq_(a)(b)
  end
end

-- signature: (Atom -> (Num -> Num))
function incr(won)
  return function (score)
    return _plus_(score)((won > 0 and 1.0 or _minus_(0.0)(1.0)))
  end
end

FALSE = 0;
TRUE = 1;


pipes = {
  {id, {{"&guess", 99}}, "&newCoin"},
  {cmp, {{"&guess", 99}, {"&coin", 99}}, "&win"},
  {incr, {{"&win", 99}, {"&score", 99}}, "&score"}
}
