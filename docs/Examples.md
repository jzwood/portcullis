# Examples

### Merge Sort

```
tail -> [a] [a]
tail xs = !! xs 1 _ xs

msort -> [Num] [Num]
msort ns = (msort2 _ ns ns)

# msort2 is a helper for msort #
msort2 -> Num -> [Num] [Num]
msort2 len ns =
  ? <= len 1 ns
  ?? (merge (msort !! ns 0 / len 2) (msort !! ns / len 2 len))

cmpHead -> [Num] -> [Num] {[Num] [Num]}
cmpHead xs ys =
  ? < ! xs 0 0 ! ys 0 0 {xs ys}
  ?? {ys xs}

# merge base case #
merge -> [Num] -> [Num] [Num]
merge xs ys =
  ? == 0 _ xs ys
  ? == 0 _ ys xs
  ?? (merge2 (cmpHead xs ys))

# merge recusive case #
merge2 -> {[Num] [Num]} [Num]
merge2 xys = ++ !! @1 xys 0 1 (merge (tail @1 xys) @2 xys)
```

### Quicksort

```
# utils #
filter -> -> [a] Atom -> [a] [a]
filter f xs =
  ?  == 0 _ xs xs
  ?  (f xs) ++ !! xs 0 1 (filter f (tail xs))
  ?? (filter f (tail xs))

tail -> [a] [a]
tail xs = !! xs 1 _ xs

lt -> Num -> [Num] Atom
lt p xs = < ! xs 0 0 p

eq -> Num -> [Num] Atom
eq p xs = == ! xs 0 0 p

gt -> Num -> [Num] Atom
gt p xs = > ! xs 0 0 p

# quicksort base case #
qsort -> [Num] [Num]
qsort xs =
  ?  >= 1 _ xs xs
  ?? (qsort2 ! xs 0 0 xs)

# quicksort recursive case #
qsort2 -> Num -> [Num] [Num]
qsort2 pivot xs =
  ++ (qsort (filter (lt pivot) xs))
  ++ (filter (eq pivot) xs) (qsort (filter (gt pivot) xs))
```

### Higher Order Function Examples

```
compose -> -> b c -> -> a b -> a c
compose f g x = (f (g x))

double -> Num Num
double x = * 2 x

quadruple -> Num Num
quadruple n = (compose double double n)
```
