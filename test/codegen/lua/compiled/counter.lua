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

-- signature: (Num -> (Num -> Num))
function sub(a)
  return function (b)
    return _minus_(a)(b)
  end
end

FALSE = 0;
TRUE = 1;


pipes = {
  {add, {{"&counter", 1}, {"&add", 100}}, "&counter"},
  {sub, {{"&counter", 1}, {"&sub", 100}}, "&counter"}
}
