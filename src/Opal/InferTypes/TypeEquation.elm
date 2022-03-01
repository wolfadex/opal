module Opal.InferTypes.TypeEquation exposing
    ( TypeEquation
    , equals
    , unwrap
    )

import Opal.Ast.Typed


type TypeEquation
    = TypeEquation ( Opal.Ast.Typed.Type, Opal.Ast.Typed.Type )


equals : Opal.Ast.Typed.Type -> Opal.Ast.Typed.Type -> TypeEquation
equals left right =
    TypeEquation ( left, right )


unwrap : TypeEquation -> ( Opal.Ast.Typed.Type, Opal.Ast.Typed.Type )
unwrap (TypeEquation pair) =
    pair
