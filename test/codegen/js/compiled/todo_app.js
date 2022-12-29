function _plus_(a) {
  return (b) => a + b;
}

function _minus_(a) {
  return (b) => a - b;
}

function _mult_(a) {
  return (b) => a * b;
}

function _div_(a) {
  return (b) => a / b;
}

function _rem_(a) {
  return (b) => a % b;
}

function _gt_(a) {
  return (b) => a > b ? 1 : 0;
}

function _gte_(a) {
  return (b) => a >= b ? 1 : 0;
}

function _lt_(a) {
  return (b) => a < b ? 1 : 0;
}

function _lte_(a) {
  return (b) => a <= b ? 1 : 0;
}

export function _eq_(a) {
  return (b) => {
    if (a === b) {
      return 1;
    }
    if (typeof a === "object" && typeof b === "object") {
      if (a.length === 0 && b.length === 0) return 1;
      if (a.length !== b.length) return 0;
      const length = Math.max(a.length, b.length);
      for (let i = 0; i < length; i++) {
        if (_eq_(a.at(i))(b.at(i)) === 0) return 0;
      }
      return 1;
    }
    return 0;
  };
}

function _cons_(a) {
  return (b) => [a].concat(b);
}

// signature: ([Atom [Char]] -> ([[Char]] -> [[Char]]))
export function update(tup) {
  return (todos) => (
    _eq_(Append)(tup[0]) ? _cons_(tup[1])(todos) : (
      _eq_(Done)(tup[0]) ? remove(tup[1])(todos) : todos
    )
  );
}

// signature: ([Char] -> [Atom [Char]])
export function append(todo) {
  return [Append, todo];
}

// signature: ([Char] -> [Atom [Char]])
export function done(done) {
  return [Done, done];
}

// signature: ([push.a] -> (push.a -> ([push.a] -> [push.a])))
export function push(ys) {
  return (x) => (xs) => _cons_(x)(concat(xs)(ys));
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    _eq_(xs)([]) ? ys : push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_filter.x -> Atom) -> (_filter.x -> ([_filter.x] -> [_filter.x])))
export function _filter(f) {
  return (x) => (xs) => concat((
    f(x) ? /* [x] */ [x] : /* [x] */ []
  ))(filter(f)(xs));
}

// signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
export function filter(f) {
  return (xs) => (
    _eq_(xs)([]) ? xs : _filter(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (neq.a -> (neq.b -> Atom))
export function neq(a) {
  return (b) => _eq_(False)(_eq_(a)(b));
}

// signature: ([Char] -> ([[Char]] -> [[Char]]))
export function remove(todo) {
  return (todos) => filter(neq(todo))(todos);
}

const False = 0;
const True = 1;
const Append = 2;
const Done = 3;


export const pipes = [
  [update, [["&update", 100], ["&todo", 1]], "&todo"],
  [append, [["&append", 50]], "&update"],
  [done, [["&done", 50]], "&update"]
]
