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
