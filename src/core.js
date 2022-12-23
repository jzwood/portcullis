// INTERNAL
function equal(a, b) {
  if (a === b) {
    return +true;
  }
  if (typeof a === "object" && typeof b === "object") {
    if (a.length === 0 && b.length === 0) return +true;
    if (a.length !== b.length) return +false;
    for (let i = 0; i < a.length; i++) {
      if (!equal(a.at(i), b.at(i))) return +false;
    }
    return +true;
  }
  return +false;
}

// for testing
export const _equal = equal;
