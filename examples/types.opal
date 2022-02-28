main : Int = 5.increment().multiply(2);

increment : (Int) -> Int = \n -> n + 1;

multiply : (a, a) -> a = \x, y -> x * y;