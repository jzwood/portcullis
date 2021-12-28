function get(xs, i, fallback) {
  return xs[i] ?? fallback
}

function fst([a, _]) {
  return a
}

function snd([_, b]) {
  return b
}

function equal(a, b) {
  return a === b || Array.isArray(a) ? JSON.stringify(a) === JSON.stringify(b) : false
}

function notEqual(a, b) {
  return !equal(a, b)
}
