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
    const b1 = f(tree1)
    const b2 = fold(tree2, alg, empty)
    return alg(node, f(tree1), f(tree2))
  }
  return f(tree)
}


function fold([head, ...tail], alg, init) {
  if (head === undefined) return init
  //return fold(tail, alg, alg(init, head))
  return alg(fold(tail, alg, init), head)
}
