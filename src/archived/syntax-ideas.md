
{--
fizzbuzz : W -> B
fizzbuzz (W a) =
  { B "Fizz Buzz", and mod3 mod5
  { B "Fizz", mod3
  { B "Buzz", mod5
  { B (str a)
  where
    mod3 <- % a 3 = 0
    mod5 <- % a 5 = 0
    mod35 <= and mod3 mod5

fizzbuzz (I a) {
  S "fizz buzz", mod3&5 a
  S "fizz", mod3 a
  S "buzz", mod5 a
  S (quote a)
}

magic4 = ? n (N 1) -> (I 3); (N 2) -> (I 2)

SELECT (D a) FROM KITTY (N a) WHERE


a = b |> c |> d
a' = b' |> c' |> d'
a'' =
    |> b/2
  |>

a <| a |>





--}
