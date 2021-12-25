// monkey patch things?


const emptyTree = null

// unfoldTree :: (b -> Maybe (b, a, b)) -> b -> Tree a
// typeofTop Unfold = (b -> b) -> (b -> [a, [b, b]]) -> b -> TreeType a
function unfold(stop, coalg, seed) {
  if (stop(seed)) return emptyTree
  const [a, [b1, b2]] = coalg(seed)
  return [a, unfold(stop, coalg, b1), unfold(stop, coalg, b2)]
}

// [1, [2, null, [4, null, null]], [3, null, [2, null, null]]]
// foldTree :: Tree a -> (a -> b -> b -> b) -> b -> b
function fold(tree, alg, empty) {
  const f = (tree) => {
    if (tree === emptyTree) return empty
    const [node, tree1, tree2] = tree
    return alg(node, f(tree1), f(tree2))
  }
  return f(tree)
}



var tree = [4, [3, null, null], [1, [2, null, null], null]]
var tree = [1, [2, null, null], [3, [4, null, null], null]]
