module Opal.InferTypes.IdSource exposing
    ( Id
    , IdSource
    , empty
    , next
    , one
    )


type IdSource
    = IdSource Int


type Id
    = Id Int


empty : IdSource
empty =
    IdSource 0


next : IdSource -> ( Id, IdSource )
next (IdSource currentId) =
    ( Id currentId, IdSource (currentId + 1) )


one : IdSource -> (Id -> b) -> ( b, IdSource )
one idSource fn =
    let
        ( currentId, nextSource ) =
            next idSource
    in
    ( fn currentId, nextSource )
