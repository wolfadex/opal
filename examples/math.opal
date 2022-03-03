main =
    (2 + 3)
        |> decrement()
        |> multiply(2);

decrement = \n -> n - 1;

multiply = \x, y -> x * y;