// signature: ((not.a -> (not.a -> Atom)) -> (not.a -> (not.a -> Atom)))
export function not(f) {
  return (a) => (b) => (
    equal(False, f(a)(b)) ?
    True :
    False
  );
}

// signature: (_length.a -> ([_length.a] -> Num))
export function _length(x) {
  return (xs) => (1.0 + length(xs));
}

// signature: ([length.a] -> Num)
export function length(xs) {
  return (
    equal(xs, []) ?
    0.0 :
    _length(xs.at(0))(xs.slice(1))
  );
}

// signature: ([push.h] -> (push.h -> ([push.h] -> [push.h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}

// signature: ([concat.a] -> ([concat.a] -> [concat.a]))
export function concat(xs) {
  return (ys) => (
    equal(xs, []) ?
    ys :
    push(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: (identity2.x -> ([identity2.x] -> [identity2.x]))
export function identity2(x) {
  return (xs) => xs;
}

// signature: ([tail.p] -> [tail.p])
export function tail(xs) {
  return (
    equal(xs, []) ?
    xs :
    identity2(xs.at(0))(xs.slice(1))
  );
}

// signature: ([drop.g] -> (Num -> [drop.g]))
export function drop(xs) {
  return (n) => (
    (n <= 0.0) ?
    xs :
    drop(tail(xs))((n - 1.0))
  );
}

// signature: (Num -> (_take.f -> ([_take.f] -> [_take.f])))
export function _take(n) {
  return (x) => (xs) => (
    (n <= 0.0) ?
    /* [f] */ [] :
    [x, ...take(xs)((n - 1.0))]
  );
}

// signature: ([take.k] -> (Num -> [take.k]))
export function take(xs) {
  return (n) => (
    equal(xs, []) ?
    /* [k] */ [] :
    _take(n)(xs.at(0))(xs.slice(1))
  );
}

// signature: ([slice.q] -> (Num -> (Num -> [slice.q])))
export function slice(xs) {
  return (i) => (j) => take(drop(xs)(i))((j - i));
}

// signature: ((filter2.x -> Atom) -> (filter2.x -> ([filter2.x] -> [filter2.x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    g(w) ?
    /* [x] */ [w] :
    /* [x] */ []
  ))(filter(g)(ws));
}

// signature: ((filter.j -> Atom) -> ([filter.j] -> [filter.j]))
export function filter(f) {
  return (xs) => (
    equal(xs, []) ?
    xs :
    filter2(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> Num)
export function neg(x) {
  return (0.0 - x);
}

// signature: [Num]
function $empty() {
  return /* [Num] */ [];
}

// signature: ([Num] -> [Num])
export function msort(ns) {
  return msort2(length(ns))(ns);
}

// signature: (Num -> ([Num] -> [Num]))
export function msort2(len) {
  return (ns) => (
    (len <= 1.0) ?
    ns :
    merge(msort(take(ns)((len / 2.0))))(msort(drop(ns)((len / 2.0))))
  );
}

// signature: (Num -> ([Num] -> (Num -> ([Num] -> [Num]))))
export function merge3(x) {
  return (xs) => (y) => (ys) => (
    (x <= y) ?
    [x, ...merge(xs)([y, ...ys])] :
    [y, ...merge([x, ...xs])(ys)]
  );
}

// signature: ([Num] -> (Num -> ([Num] -> [Num])))
export function merge2(ys) {
  return (x) => (xs) => (
    equal(ys, []) ?
    [x, ...xs] :
    merge3(x)(xs)(ys.at(0))(ys.slice(1))
  );
}

// signature: ([Num] -> ([Num] -> [Num]))
export function merge(xs) {
  return (ys) => (
    equal(xs, []) ?
    ys :
    merge2(ys)(xs.at(0))(xs.slice(1))
  );
}

// signature: (Num -> (Num -> Num))
export function avg(a) {
  return (b) => (0.5 * (a + b));
}

// signature: ([Num] -> Num)
export function mean(xs) {
  return (sum(xs) / length(xs));
}

// signature: (Num -> ([Num] -> Num))
export function sum2(x) {
  return (xs) => (x + sum(xs));
}

// signature: ([Num] -> Num)
export function sum(xs) {
  return (
    equal(xs, []) ?
    0.0 :
    sum2(xs.at(0))(xs.slice(1))
  );
}

// signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (Atom -> (Atom -> [Atom Atom]))
export function rankPet(p1) {
  return (p2) => (
    equal(Chipmunk, p1) ?
    [p1, p2] :
    [p2, p1]
  );
}

// signature: (Num -> (Num -> Atom))
export function lt(x) {
  return (y) => (x > y);
}

// signature: (Num -> (Num -> Atom))
export function gte(x) {
  return (y) => (x <= y);
}

// signature: (Num -> ([Num] -> [Num]))
export function qsortp(x) {
  return (xs) => concat(qsort(filter(lt(x))(xs)))([x, ...qsort(filter(gte(x))(xs))]);
}

// signature: ([Num] -> [Num])
export function qsort(xs) {
  return (
    equal(xs, []) ?
    xs :
    qsortp(xs.at(0))(xs.slice(1))
  );
}

const False = 0;
const True = 1;
const Chipmunk = 2;

export const empty = $empty()

export const pipes = [];
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
