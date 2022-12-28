-- INTERNAL
function _eq_(a, b)
  if a == b then
    return true
  elseif type(a) == 'table' and type(b) == 'table' then
    return _eq_(a.head, b.head) and _eq_(a.tail, b.tail)
  else
    return false
  end
end
