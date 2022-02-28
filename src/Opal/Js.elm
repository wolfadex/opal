module Opal.Js exposing (..)

import Dict
import Opal
    exposing
        ( BinaryOperator(..)
        , Definition
        , Expression(..)
        , Literal(..)
        , Module
        , UnaryOperator(..)
        )


compile : Module -> Result String String
compile module_ =
    let
        defs =
            module_.definitions
                |> List.map (simplifyDefinition >> (\def -> ( def.label, def.body )))
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


simplifyDefinition : Definition -> Definition
simplifyDefinition definition =
    { definition | body = Opal.simplify definition.body }


compileDefinition : ( String, Expression ) -> String
compileDefinition ( label, body ) =
    case body of
        ExprAnonymousFunction args expr ->
            "function " ++ label ++ "(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        _ ->
            "var " ++ label ++ " = " ++ compileExpression body ++ ";"


compileExpression : Opal.Expression -> String
compileExpression expression =
    case expression of
        ExprLiteral (LitInt i) ->
            String.fromInt i

        ExprLiteral (LitString str) ->
            "'" ++ str ++ "'"

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

        ExprBinary Concat leftExpr rightExpr ->
            compileExpression leftExpr ++ " + " ++ compileExpression rightExpr

        ExprUnary Negate expr ->
            "-" ++ compileExpression expr

        ExprFunctionApplication label expressions ->
            label ++ "(" ++ String.join ", " (List.map compileExpression expressions) ++ ")"

        ExprAnonymousFunction args expr ->
            "function(" ++ String.join ", " args ++ ") { return " ++ compileExpression expr ++ "; }"

        ExprWord label ->
            label

        ExprLetIn defs expr ->
            "(function() {"
                ++ String.join "\n" (List.map (\def -> compileDefinition ( def.label, def.body )) defs)
                ++ "\n  return "
                ++ compileExpression expr
                ++ "\n})()"
