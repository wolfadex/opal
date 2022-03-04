module Carl exposing (..)

import Html


main =
    Html.text (test2 () test1 test0)


test0 =
    "Hello"


test1 a =
    (\x -> x) a


test2 a b =
    b
