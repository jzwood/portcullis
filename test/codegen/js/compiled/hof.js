// signature: (id1.x -> id1.x)
export function id1(x) {
  return x;
}

// signature: ((compose.b -> compose.c) -> ((compose.a -> compose.b) -> (compose.a -> compose.c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}

// signature: (Num -> Num)
export function double(x) {
  return (2.0 * x);
}

// signature: (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}

// signature: (id2.x -> id2.x)
export function id2(x) {
  return compose(id1)(id1)(x);
}

// signature: Num
function $one1() {
  return 1.0;
}

// signature: Num
function $one2() {
  return compose(id1)(id1)(one1);
}

// signature: (id3.z -> ([id3.z] -> [id3.z]))
export function id3(x) {
  return (xs) => xs;
}

// signature: ([tail.t] -> [tail.t])
export function tail(xs) {
  return (
    equal(xs, []) ?
    xs :
    id3(xs.at(0))(xs.slice(1))
  );
}

// signature: ((a.h -> a.h) -> (a.h -> a.h))
export function a(fx) {
  return (x) => fx(x);
}

// signature: (b.q -> b.q)
export function b(w) {
  return w;
}

// signature: (c.p -> c.p)
export function c(y) {
  return a(b)(y);
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

// signature: (eq.a -> (eq.a -> Atom))
export function eq(x) {
  return (y) => equal(x, y);
}

// signature: ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
}

const False = 0;
const True = 1;

export const one1 = $one1()
export const one2 = $one2()

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
