module Opal.Js exposing (..)

import Dict
import Opal
    exposing
        ( BinaryOperator(..)
        , Expression(..)
        , Literal(..)
        , Module
        , UnaryOperator(..)
        )
import Result.Extra


compile : Module -> Result String String
compile module_ =
    let
        defs =
            List.map (\def -> ( def.label, def.body )) module_.definitions
                |> Dict.fromList
    in
    case ( Dict.get "main" defs, Dict.toList (Dict.remove "main" defs) ) of
        ( Just mainBody, otherDefs ) ->
            String.join "\n" (List.map compileDefinition otherDefs)
                ++ "\n"
                ++ compileDefinition ( "main", mainBody )
                ++ """

module.exports = {
  init: function (...args) {
    return typeof main === "function" ? main(...args) : main;
  },
};"""
                |> Ok

        ( Nothing, _ ) ->
            Err "Missing a 'main' function"


compileDefinition : ( String, Expression ) -> String
compileDefinition ( label, body ) =
    case body of
        ExprAnonymousFunction args expr ->
            "function " ++ label ++ "(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        _ ->
            "var " ++ label ++ " = " ++ compileExpression body


compileExpression : Opal.Expression -> String
compileExpression expression =
    case expression of
        ExprLiteral (LitInt i) ->
            String.fromInt i

        ExprBinary Sum leftExpr rightExpr ->
            compileExpression leftExpr ++ " + " ++ compileExpression rightExpr

        ExprBinary Difference leftExpr rightExpr ->
            compileExpression leftExpr ++ " - " ++ compileExpression rightExpr

        ExprBinary Product leftExpr rightExpr ->
            compileExpression leftExpr ++ " * " ++ compileExpression rightExpr

        ExprBinary Quotient leftExpr rightExpr ->
            "Math.floor(" ++ compileExpression leftExpr ++ " / " ++ compileExpression rightExpr ++ ")"

        ExprBinary PipedFunction _ _ ->
            "new Error('Piped functions should be reduced out')"

        ExprUnary Negate expr ->
            "-" ++ compileExpression expr

        ExprFunctionApplication label expressions ->
            label ++ "(" ++ String.join ", " (List.map compileExpression expressions) ++ ")"

        ExprAnonymousFunction args expr ->
            "function(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        ExprWord label ->
            label
