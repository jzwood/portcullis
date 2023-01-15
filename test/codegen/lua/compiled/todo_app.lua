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

-- signature: ({Atom [Char]} -> ([[Char]] -> [[Char]]))
function update(tup)
  return function (todos)
    return (_eq_(APPEND)(tup[1]) > 0 and _cons_(tup[2])(todos) or (_eq_(DONE)(tup[1]) > 0 and remove(tup[2])(todos) or todos))
  end
end

-- signature: ([Char] -> {Atom [Char]})
function append(todo)
  return {APPEND, todo}
end

-- signature: ([Char] -> {Atom [Char]})
function done(done)
  return {DONE, done}
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

-- signature: ((_filter.x -> Atom) -> (_filter.x -> ([_filter.x] -> [_filter.x])))
function _filter(f)
  return function (x)
    return function (xs)
      return concat((f(x) > 0 and { head = x, tail = {} } or {}))(filter(f)(xs))
    end
  end
end

-- signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
function filter(f)
  return function (xs)
    return (_eq_(xs)({}) > 0 and xs or _filter(f)(xs.head)(xs.tail))
  end
end

-- signature: (neq.a -> (neq.b -> Atom))
function neq(a)
  return function (b)
    return _eq_(FALSE)(_eq_(a)(b))
  end
end

-- signature: ([Char] -> ([[Char]] -> [[Char]]))
function remove(todo)
  return function (todos)
    return filter(neq(todo))(todos)
  end
end

FALSE = 0;
TRUE = 1;
APPEND = 2;
DONE = 3;


pipes = {
  {update, {{"&update", 100}, {"&todo", 1}}, "&todo"},
  {append, {{"&append", 50}}, "&update"},
  {done, {{"&done", 50}}, "&update"}
}
