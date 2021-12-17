// function "alg" has type (Num -> (Num -> Num))
function alg(a, b) {
  return ((b)+(a));
}
// function "sum" has type ([Num] -> Num)
function sum(xs) {
  return fold(alg, Num, 0.0);
}

function fold(f, init, arr) {
  return arr.reduce(f, init)
}
