// signature: (Char -> (Num -> Num))
export function charToNum(char) {
  return (fallback) => (
    equal(char, '0') ? 0.0 : (
      equal(char, '1') ? 1.0 : (
        equal(char, '2') ? 2.0 : (
          equal(char, '3') ? 3.0 : (
            equal(char, '4') ? 4.0 : (
              equal(char, '5') ? 5.0 : (
                equal(char, '6') ? 6.0 : (
                  equal(char, '7') ? 7.0 : (
                    equal(char, '8') ? 8.0 : (
                      equal(char, '9') ? 9.0 : fallback
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  );
}

// signature: (Char -> ([Num] -> [Num]))
export function _read(char) {
  return (nums) => [(exp(10.0)(length(nums)) * charToNum(char)(0.0)), ...nums];
}

// signature: ([Char] -> Num)
export function read(chars) {
  return sum(foldr(_read)(/* [Num] */ [])(reverse(chars)));
}

// signature: (Num -> (Num -> Num))
export function exp(x) {
  return (n) => (
    (n < 0.0) ? exp((1.0 / x))((0.0 - 1.0)) : (
      equal(n, 0.0) ? 1.0 : (
        equal((n % 2.0), 0.0) ? exp((x * x))((n / 2.0)) : (x * exp((x * x))(((n - 1.0) / 2.0)))
      )
    )
  );
}

// signature: ((_foldl.j -> (_foldl.k -> _foldl.k)) -> (_foldl.k -> (_foldl.j -> ([_foldl.j] -> _foldl.k))))
export function _foldl(alg) {
  return (acc) => (x) => (xs) => alg(x)(foldl(alg)(acc)(xs));
}

// signature: ((foldl.a -> (foldl.b -> foldl.b)) -> (foldl.b -> ([foldl.a] -> foldl.b)))
export function foldl(alg) {
  return (acc) => (xs) => (
    equal(xs, []) ? acc : _foldl(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_foldr.j -> (_foldr.k -> _foldr.k)) -> (_foldr.k -> (_foldr.j -> ([_foldr.j] -> _foldr.k))))
export function _foldr(alg) {
  return (acc) => (x) => (xs) => foldr(alg)(alg(x)(acc))(xs);
}

// signature: ((foldr.a -> (foldr.b -> foldr.b)) -> (foldr.b -> ([foldr.a] -> foldr.b)))
export function foldr(alg) {
  return (acc) => (xs) => (
    equal(xs, []) ? acc : _foldr(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
export function _map(f) {
  return (x) => (xs) => [f(x), ...map(f)(xs)];
}

// signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
export function map(f) {
  return (xs) => (
    equal(xs, []) ? /* [g] */ [] : _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (_len.a -> (Num -> Num))
export function _len(x) {
  return (len) => (1.0 + len);
}

// signature: ([length.a] -> Num)
export function length(xs) {
  return foldr(_len)(0.0)(xs);
}

// signature: (_rev.a -> ([_rev.a] -> [_rev.a]))
export function _rev(x) {
  return (xs) => [x, ...xs];
}

// signature: ([reverse.a] -> [reverse.a])
export function reverse(xs) {
  return foldr(_rev)(/* [a] */ [])(xs);
}

// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => (a + b);
}

// signature: ([Num] -> Num)
export function sum(ns) {
  return foldr(add)(0.0)(ns);
}

// signature: (not.x -> (not.x -> Atom))
export function not(a) {
  return (b) => equal(False, equal(a, b));
}

// signature: (Num -> (Num -> Num))
export function _max(num) {
  return (max) => (
    (num > max) ? num : max
  );
}

// signature: ([Num] -> Num)
export function max(nums) {
  return foldr(_max)(0.0)(nums);
}

// signature: ([[curryCons.a] [[curryCons.a]]] -> [[curryCons.a]])
export function curryCons(tup) {
  return [tup[0], ...tup[1]];
}

// signature: (_splitOn.z -> (_splitOn.z -> ([[_splitOn.z] [[_splitOn.z]]] -> [[_splitOn.z] [[_splitOn.z]]])))
export function _splitOn(on) {
  return (x) => (acc) => (
    not(x)(on) ? [[x, ...acc[0]], acc[1]] : [/* [z] */ [], curryCons(acc)]
  );
}

// signature: (splitOn.a -> ([splitOn.a] -> [[splitOn.a]]))
export function splitOn(on) {
  return (xs) => curryCons(foldl(_splitOn(on))([/* [a] */ [], /* [[a]] */ []])(xs));
}

// signature: ([Char] -> Num)
export function day1(rawStr) {
  return max(map(sum)(splitOn(0.0)(map(read)(splitOn('/')(rawStr)))));
}

const False = 0;
const True = 1;


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
