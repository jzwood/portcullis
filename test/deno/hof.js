const False = 0
const True = 1

export const one1 = $one1()
export const one2 = $one2()

// function "id" has type (x -> x)
export function id(x) {
  return x;
}
// function "compose" has type ((b -> c) -> ((a -> b) -> (a -> c)))
export function compose(f) {
  return (g) => (x) => f(g(x));
}
// function "double" has type (Num -> Num)
export function double(x) {
  return (2.0 * x);
}
// function "quadruple" has type (Num -> Num)
export function quadruple(n) {
  return compose(double)(double)(n);
}
// function "id2" has type (x -> x)
export function id2(x) {
  return compose(id)(id)(x);
}
// function "one1" has type Num
function $one1() {
  return 1.0;
}
// function "one2" has type Num
function $one2() {
  return compose(id)(id)(one1);
}
// function "id3" has type (z -> ([z] -> [z]))
export function id3(x) {
  return (xs) => xs;
}
// function "tail" has type ([t] -> [t])
export function tail(xs) {
  return (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ id3(xs.at(0))(xs.slice(1))
  );
}
// function "a" has type ((h -> h) -> (h -> h))
export function a(fx) {
  return (x) => fx(x);
}
// function "b" has type (q -> q)
export function b(w) {
  return w;
}
// function "c" has type (p -> p)
export function c(y) {
  return a(b)(y);
}
// function "push" has type ([h] -> (h -> ([h] -> [h])))
export function push(ys) {
  return (x) => (xs) => [x, ...concat(xs)(ys)];
}
// function "concat" has type ([a] -> ([a] -> [a]))
export function concat(xs) {
  return (ys) => (
    /* if */ equal(xs, []) ?
    /* then */ ys :
    /* else */ push(ys)(xs.at(0))(xs.slice(1))
  );
}
// function "filter2" has type ((x -> Atom) -> (x -> ([x] -> [x])))
export function filter2(g) {
  return (w) => (ws) => concat((
    /* if */ g(w) ?
    /* then */ /* [x] */ [w] :
    /* else */ /* [x] */ []
  ))(filter(g)(ws));
}
// function "filter" has type ((j -> Atom) -> ([j] -> [j]))
export function filter(f) {
  return (xs) => (
    /* if */ equal(xs, []) ?
    /* then */ xs :
    /* else */ filter2(f)(xs.at(0))(xs.slice(1))
  );
}
// function "eq" has type (a -> (a -> Atom))
export function eq(x) {
  return (y) => equal(x, y);
}
// function "seven" has type ([Num] -> [Num])
export function seven(xs) {
  return filter(eq(7.0))(xs);
}
// function "equal" has type (a -> (a -> Atom))
export function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (Array.isArray(a) && Array.isArray(b)) {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    const [aHead, ...aTail] = a
    const [bHead, ...bTail] = b
    return equal(aHead, bHead) && equal(aTail, bTail)
  }
  return +false;
}
