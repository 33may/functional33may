module Week2.Class1.Sumsqr exposing (..)

sqr: Int -> Int
sqr n =
  n * n

sum: Int -> Int
sum n =
  if n > 0
   then sqr n + sum( n - 1 )
   else 0
----------------------------------------------------------------
sum2 : List Char -> List Char
sum2 list =
    case list of
        [] ->
            []
        x :: xs ->
            Char.fromCode ((Char.toCode x) + 1) :: sum2 xs
