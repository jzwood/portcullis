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

// signature: (Num -> Num)
export function asciiDecToChar(dec) {
  return (
    _eq_(dec)(48.0) ? 0.0 : (
      _eq_(dec)(49.0) ? 1.0 : (
        _eq_(dec)(50.0) ? 2.0 : (
          _eq_(dec)(51.0) ? 3.0 : (
            _eq_(dec)(52.0) ? 4.0 : (
              _eq_(dec)(53.0) ? 5.0 : (
                _eq_(dec)(54.0) ? 6.0 : (
                  _eq_(dec)(55.0) ? 7.0 : (
                    _eq_(dec)(56.0) ? 8.0 : (
                      _eq_(dec)(57.0) ? 9.0 : 0.0
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

// signature: (Num -> ([Num] -> [Num]))
export function _read(dec) {
  return (nums) => _cons_(_mult_(exp(10.0)(length(nums)))(asciiDecToChar(dec)))(nums);
}

// signature: ([Num] -> Num)
export function read(decimals) {
  return sum(foldl(_read)(/* [Num] */ [])(decimals));
}

// signature: (Num -> (Num -> Num))
export function exp(x) {
  return (n) => (
    _lt_(n)(0.0) ? exp(_div_(1.0)(x))(_minus_(0.0)(1.0)) : (
      _eq_(n)(0.0) ? 1.0 : (
        _eq_(_rem_(n)(2.0))(0.0) ? exp(_mult_(x)(x))(_div_(n)(2.0)) : _mult_(x)(exp(_mult_(x)(x))(_div_(_minus_(n)(1.0))(2.0)))
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
    _eq_(xs)([]) ? acc : _foldl(alg)(acc)(xs.at(0))(xs.slice(1))
  );
}

// signature: ((_map.q -> _map.t) -> (_map.q -> ([_map.q] -> [_map.t])))
export function _map(f) {
  return (x) => (xs) => _cons_(f(x))(map(f)(xs));
}

// signature: ((map.t -> map.g) -> ([map.t] -> [map.g]))
export function map(f) {
  return (xs) => (
    _eq_(xs)([]) ? /* [g] */ [] : _map(f)(xs.at(0))(xs.slice(1))
  );
}

// signature: (_len.a -> (Num -> Num))
export function _len(x) {
  return (len) => _plus_(1.0)(len);
}

// signature: ([length.a] -> Num)
export function length(xs) {
  return foldl(_len)(0.0)(xs);
}

// signature: (Num -> (Num -> Num))
export function add(a) {
  return (b) => _plus_(a)(b);
}

// signature: ([Num] -> Num)
export function sum(ns) {
  return foldl(add)(0.0)(ns);
}

// signature: (_not.x -> (_not.x -> Atom))
export function _not(a) {
  return (b) => _eq_(False)(_eq_(a)(b));
}

// signature: (Num -> (Num -> Num))
export function _max(num) {
  return (max) => (
    _gt_(num)(max) ? num : max
  );
}

// signature: ([Num] -> Num)
export function max(nums) {
  return foldl(_max)(0.0)(nums);
}

// signature: ([[curryCons.a] [[curryCons.a]]] -> [[curryCons.a]])
export function curryCons(tup) {
  return _cons_(tup[0])(tup[1]);
}

// signature: (_splitOn.z -> (_splitOn.z -> ([[_splitOn.z] [[_splitOn.z]]] -> [[_splitOn.z] [[_splitOn.z]]])))
export function _splitOn(on) {
  return (x) => (acc) => (
    _not(x)(on) ? [_cons_(x)(acc[0]), acc[1]] : [/* [z] */ [], curryCons(acc)]
  );
}

// signature: (splitOn.a -> ([splitOn.a] -> [[splitOn.a]]))
export function splitOn(on) {
  return (xs) => curryCons(foldl(_splitOn(on))([/* [a] */ [], /* [[a]] */ []])(xs));
}

// signature: ([Num] -> [Num])
export function parse(buffer) {
  return map(sum)(splitOn(0.0)(map(read)(splitOn(10.0)(buffer))));
}

// signature: ([Num] -> Num)
export function day1a(buffer) {
  return max(parse(buffer));
}

const False = 0;
const True = 1;


export const pipes = [];
