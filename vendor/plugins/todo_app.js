const False = 0
const True = 1
const Append = 2
const Done = 3


// signature: ([Atom [Char]] -> ([[Char]] -> [[Char]]))
export function update(tup) {
  return (todos) => (
    /* if */ equal(Append, tup[0]) ?
    /* then */ [tup[1], ...todos] :
    /* else */ (
      /* if */ equal(Done, tup[0]) ?
      /* then */ remove(tup[1])(todos) :
      /* else */ todos
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

// signature: ([Char] -> ([[Char]] -> [[Char]]))
export function remove(todo) {
  return (todos) => todos;
}

export const pipes = [
  [update, [["&update", 100], ["&todo", 1]], "&todo"],
  [append, [["&append", 50]], "&update"],
  [done, [["&done", 50]], "&update"]
]

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
