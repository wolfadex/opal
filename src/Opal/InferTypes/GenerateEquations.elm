module Opal.InferTypes.GenerateEquations exposing (generate)

import Opal.Ast.Typed
import Opal.InferTypes.GenerateEquations
import Opal.InferTypes.IdSource
import Opal.InferTypes.TypeEquation


generate : Opal.InferTypes.IdSource.IdSource -> Opal.Ast.Typed.Expression -> ( List Opal.InferTypes.TypeEquation.TypeEquation, Opal.InferTypes.IdSource.IdSource )
generate idSource typedExpr =
    let
        expr =
            Opal.Ast.Typed.getExpr typedExpr

        type_ =
            Opal.Ast.Typed.getType typedExpr
    in
    case expr of
        Opal.Ast.Typed.ExprLiteral (Opal.Ast.Typed.LitInt _) ->
            ( [ Opal.InferTypes.TypeEquation.equals
                    type_
                    (Opal.Ast.Typed.Concrete "Int")
              ]
            , idSource
            )

        Opal.Ast.Typed.ExprLiteral (Opal.Ast.Typed.LitString _) ->
            ( [ Opal.InferTypes.TypeEquation.equals
                    type_
                    (Opal.Ast.Typed.Concrete "String")
              ]
            , idSource
            )

        Opal.Ast.Typed.ExprBinary Opal.Ast.Typed.Sum leftExpr rightExpr ->
            let
                leftType =
                    Opal.Ast.Typed.getType leftExpr

                rightType =
                    Opal.Ast.Typed.getType rightExpr

                ( leftEquations, leftSource ) =
                    generate idSource leftExpr

                ( rightEquations, rightSource ) =
                    generate leftSource rightExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals leftType rightType
              , Opal.InferTypes.TypeEquation.equals leftType type_
              ]
                ++ leftEquations
                ++ rightEquations
            , rightSource
            )

        Opal.Ast.Typed.ExprBinary Opal.Ast.Typed.Difference leftExpr rightExpr ->
            let
                leftType =
                    Opal.Ast.Typed.getType leftExpr

                rightType =
                    Opal.Ast.Typed.getType rightExpr

                ( leftEquations, leftSource ) =
                    generate idSource leftExpr

                ( rightEquations, rightSource ) =
                    generate leftSource rightExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals leftType rightType
              , Opal.InferTypes.TypeEquation.equals leftType type_
              ]
                ++ leftEquations
                ++ rightEquations
            , rightSource
            )

        Opal.Ast.Typed.ExprBinary Opal.Ast.Typed.Product leftExpr rightExpr ->
            let
                leftType =
                    Opal.Ast.Typed.getType leftExpr

                rightType =
                    Opal.Ast.Typed.getType rightExpr

                ( leftEquations, leftSource ) =
                    generate idSource leftExpr

                ( rightEquations, rightSource ) =
                    generate leftSource rightExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals leftType rightType
              , Opal.InferTypes.TypeEquation.equals leftType type_
              ]
                ++ leftEquations
                ++ rightEquations
            , rightSource
            )

        Opal.Ast.Typed.ExprBinary Opal.Ast.Typed.Quotient leftExpr rightExpr ->
            let
                leftType =
                    Opal.Ast.Typed.getType leftExpr

                rightType =
                    Opal.Ast.Typed.getType rightExpr

                ( leftEquations, leftSource ) =
                    generate idSource leftExpr

                ( rightEquations, rightSource ) =
                    generate leftSource rightExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals leftType rightType
              , Opal.InferTypes.TypeEquation.equals leftType type_
              ]
                ++ leftEquations
                ++ rightEquations
            , rightSource
            )

        Opal.Ast.Typed.ExprBinary _ leftExpr rightExpr ->
            let
                leftType =
                    Opal.Ast.Typed.getType leftExpr

                rightType =
                    Opal.Ast.Typed.getType rightExpr

                ( leftEquations, leftSource ) =
                    generate idSource leftExpr

                ( rightEquations, rightSource ) =
                    generate leftSource rightExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals
                    leftType
                    (Debug.todo "TODO")
              , Opal.InferTypes.TypeEquation.equals
                    rightType
                    (Debug.todo "TODO")
              , Opal.InferTypes.TypeEquation.equals
                    type_
                    (Debug.todo "TODO")
              ]
                ++ leftEquations
                ++ rightEquations
            , rightSource
            )

        Opal.Ast.Typed.ExprIfElse conditionExpr thenExpr elseExpr ->
            let
                conditionType =
                    Opal.Ast.Typed.getType conditionExpr

                thenType =
                    Opal.Ast.Typed.getType thenExpr

                elseType =
                    Opal.Ast.Typed.getType elseExpr

                ( conditionEquations, conditionSource ) =
                    generate idSource conditionExpr

                ( thenEquations, thenSource ) =
                    generate conditionSource thenExpr

                ( elseEquations, idSource3 ) =
                    generate thenSource elseExpr
            in
            ( [ Opal.InferTypes.TypeEquation.equals conditionType (Opal.Ast.Typed.Concrete "Bool")
              , Opal.InferTypes.TypeEquation.equals thenType elseType
              , Opal.InferTypes.TypeEquation.equals thenType type_
              ]
                ++ conditionEquations
                ++ thenEquations
                ++ elseEquations
            , idSource3
            )

        _ ->
            Debug.todo "TODO"
