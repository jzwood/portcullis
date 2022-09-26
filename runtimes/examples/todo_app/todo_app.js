const False = 0;
const True = 1;
const Append = 2;
const Done = 3;

// signature: ([Atom [Char]] -> ([[Char]] -> [[Char]]))
export function update(tup) {
  return (todos) => (
    /* if */ equal(Append, tup[0])
      ? /* then */ [tup[1], ...todos]
      : /* else */ (
        /* if */ equal(Done, tup[0])
          ? /* then */ remove(tup[1])(todos)
          : /* else */ todos
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
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, [])
      ? /* then */ ys
      : /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_filter.x -> Atom) -> (_filter.x -> ([_filter.x] -> [_filter.x])))
export function _filter(f) {
  return (x) =>
    (xs) =>
      concat(
        (
          /* if */ f(x) ? /* then */ /* [x] */ [x] : /* else */ /* [x] */ []
        ),
      )(filter(f)(xs));
}

// signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(xs, [])
      ? /* then */ xs
      : /* else */ _filter(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (neq.a -> (neq.b -> Atom))
export function neq(a) {
  return (b) => equal(False, equal(a, b));
}

// signature: ([Char] -> ([[Char]] -> [[Char]]))
export function remove(todo) {
  return (todos) => filter(neq(todo))(todos);
}

export const pipes = [
  [update, [["&update", 100], ["&todo", 1]], "&todo"],
  [append, [["&append", 50]], "&update"],
  [done, [["&done", 50]], "&update"],
];

// INTERNAL
function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    return equal(a.at(0), b.at(0)) && equal(a.slice(1), b.slice(1));
  }
  return +false;
}

// for testing
export const _equal = equal;
