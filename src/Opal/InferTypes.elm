module Opal.InferTypes exposing (..)

import Opal.Ast.Canonical
import Opal.Ast.Typed
import Opal.InferTypes.GenerateEquations
import Opal.InferTypes.IdSource


inferExpression : Opal.Ast.Canonical.Expression -> Result String Opal.Ast.Typed.Expression
inferExpression canonicalExpr =
    let
        ( exprWithIds, idSource ) =
            assignIds canonicalExpr

        ( typeEquations, _ ) =
            Opal.InferTypes.GenerateEquations.generate idSource exprWithIds
    in
    Debug.todo "TODO"


assignIds : Opal.Ast.Canonical.Expression -> ( Opal.Ast.Typed.Expression, Opal.InferTypes.IdSource.IdSource )
assignIds expression =
    assignIdsWith Opal.InferTypes.IdSource.empty expression


assignIdsWith :
    Opal.InferTypes.IdSource.IdSource
    -> Opal.Ast.Canonical.Expression
    -> ( Opal.Ast.Typed.Expression, Opal.InferTypes.IdSource.IdSource )
assignIdsWith idSource expression =
    case expression of
        ExprLiteral (LitInt _) ->
            ( ( expression, TConcrete "Int" ), idSource )

        ExprLiteral (LitString _) ->
            ( ( expression, TConcrete "String" ), idSource )

        ExprBinary op leftExpr rightExpr ->
            let
                ( leftExprTyped, leftSource ) =
                    assignIdsWith idSource leftExpr

                ( rightExprTyped, rightSource ) =
                    assignIdsWith leftSource rightExpr
            in
            -- assignIdsWith rightSource (ExprBinary op leftExprTyped, )
            Debug.todo "TODO"

        ExprUnary Negate expr ->
            let
                ( nestedExprTyped, nestedSource ) =
                    assignIdsWith idSource expr
            in
            -- assignIdsWith nestedExprTyped expression
            Debug.todo "TODO"

        ExprFunctionApplication str exprArgs ->
            Debug.todo "TODO"

        ExprAnonymousFunction args expr ->
            Debug.todo "TODO"

        ExprWord word ->
            Debug.todo "TODO"

        ExprLetIn defs expr ->
            Debug.todo "TODO"

        ExprIfElse conditionExpr thenExpr elseExpr ->
            Debug.todo "TODO"


assignId : IdSource -> Opal.Ast.Canonical.Expression -> ( Opal.Ast.Typed.Expression, IdSource )
assignId idSource expression =
    Opal.InferTypes.IdSource.one idSource (\id -> ( expression, TVariable id ))
