main =
    let exclaim = \s -> s ++ "!";
        hello = \name -> "Hello, " ++ name;
    in
    "Carl" . hello() . exclaim();
