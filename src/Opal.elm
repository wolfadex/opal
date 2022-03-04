module Opal exposing (..)

import Opal.Ast.Frontend
    exposing
        ( BinaryOperator(..)
        , Expression(..)
        , Literal(..)
        , UnaryOperator(..)
        )
import Opal.InferTypes.IdSource exposing (IdSource)
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..), Step(..), Trailing(..))
import Pratt exposing (expression)
import Transform


parse : String -> Result String Module
parse input =
    case Parser.run parseOpal input of
        Ok module_ ->
            Ok { definitions = module_.definitions }

        Err deadEnds ->
            let
                _ =
                    Debug.log "parse err" deadEnds
            in
            Err (deadEndsToString deadEnds)


type alias Module =
    { definitions : List Opal.Ast.Frontend.Definition
    }


parseOpal : Parser Module
parseOpal =
    Parser.succeed (\definitions -> { definitions = definitions })
        |= parseDefinitions
        |. Parser.spaces
        |. Parser.end


parseDefinitions : Parser (List Opal.Ast.Frontend.Definition)
parseDefinitions =
    Parser.loop [] parseDefinitionsHelper


parseDefinitionsHelper : List Opal.Ast.Frontend.Definition -> Parser (Step (List Opal.Ast.Frontend.Definition) (List Opal.Ast.Frontend.Definition))
parseDefinitionsHelper reverseDefinitions =
    Parser.oneOf
        [ Parser.succeed (\definition -> Loop (definition :: reverseDefinitions))
            |= parseDefinition
        , Parser.succeed (Done (List.reverse reverseDefinitions))
        ]


parseDefinition : Parser Opal.Ast.Frontend.Definition
parseDefinition =
    Parser.succeed (\label type_ body -> { label = label, type_ = type_, body = body })
        |. Parser.spaces
        |= parseLabel
        |= praseTypeDef
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= parseExpression
        |. Parser.spaces
        |. Parser.symbol ";"
        |> Parser.backtrackable


praseTypeDef : Parser (Maybe Opal.Ast.Frontend.Type)
praseTypeDef =
    Parser.oneOf
        [ Parser.succeed (Debug.log "type def" >> Just)
            |. Parser.spaces
            |. Parser.symbol ":"
            |. Parser.spaces
            |= parseType
            |> Parser.backtrackable
        , Parser.succeed Nothing
        ]


parseType : Parser Opal.Ast.Frontend.Type
parseType =
    Pratt.expression
        { oneOf =
            [ Pratt.literal parseConcreteType
            , Pratt.literal parseTypeVariable
            , parseFunctionType
            ]
        , andThenOneOf = []
        , spaces = Parser.spaces
        }


parseConcreteType : Parser Opal.Ast.Frontend.Type
parseConcreteType =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isUpper char)
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map Opal.Ast.Frontend.Concrete
        |> Parser.backtrackable


parseTypeVariable : Parser Opal.Ast.Frontend.Type
parseTypeVariable =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map Opal.Ast.Frontend.Variable
        |> Parser.backtrackable


parseFunctionType : Pratt.Config Opal.Ast.Frontend.Type -> Parser Opal.Ast.Frontend.Type
parseFunctionType config =
    Parser.succeed Opal.Ast.Frontend.Function
        |= Parser.sequence
            { start = "("
            , separator = ","
            , spaces = Parser.spaces
            , item = Pratt.subExpression 0 config
            , end = ")"
            , trailing = Forbidden
            }
        |. Parser.spaces
        |. Parser.symbol "->"
        |. Parser.spaces
        |= Pratt.subExpression 0 config


parseLabel : Parser String
parseLabel =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile (\char -> Char.isAlphaNum char || char == '_')
        |> Parser.getChompedString


simplify : Opal.Ast.Frontend.Expression -> Opal.Ast.Frontend.Expression
simplify =
    Transform.transformAll recurseExpression simplifyAll


recurseExpression : (Opal.Ast.Frontend.Expression -> Opal.Ast.Frontend.Expression) -> Opal.Ast.Frontend.Expression -> Opal.Ast.Frontend.Expression
recurseExpression fn expression =
    case expression of
        ExprBinary op leftExpr rightExpr ->
            ExprBinary op (fn leftExpr) (fn rightExpr)

        ExprLiteral _ ->
            expression

        ExprUnary unary expr ->
            ExprUnary unary (fn expr)

        ExprFunctionApplication func arg ->
            ExprFunctionApplication (fn func) (fn arg)

        ExprLambda args expr ->
            ExprLambda args (fn expr)

        ExprVariable _ ->
            expression

        ExprLetIn defs expr ->
            let
                recurseDef =
                    \def -> { def | body = fn def.body }
            in
            ExprLetIn (List.map recurseDef defs) (fn expr)

        ExprIfElse conditionExpr thenExpr elseExpr ->
            ExprIfElse (fn conditionExpr) (fn thenExpr) (fn elseExpr)

        ExprArgument _ ->
            expression


simplifyAll : Opal.Ast.Frontend.Expression -> Maybe Opal.Ast.Frontend.Expression
simplifyAll =
    Transform.orList_
        [ simplifyPipe
        ]


simplifyPipe : Opal.Ast.Frontend.Expression -> Opal.Ast.Frontend.Expression
simplifyPipe expression =
    case expression of
        ExprBinary PipedFunction leftExpr rightExpr ->
            ExprFunctionApplication rightExpr leftExpr

        _ ->
            expression


parseExpression : Parser Opal.Ast.Frontend.Expression
parseExpression =
    Pratt.expression
        { oneOf =
            [ parseIfElse
            , parseLetIn
            , parseLambda
            , Pratt.literal (Parser.map ExprLiteral parseLiteral)
            , Pratt.literal (Parser.map ExprVariable parseLabel)
            , Pratt.prefix 5 (Parser.symbol "-") (ExprUnary Negate)

            -- , parseFunction
            , parenthesizedExpression
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "++") (ExprBinary Concat)
            , Pratt.infixLeft 1 (Parser.symbol "+") (ExprBinary Sum)
            , Pratt.infixLeft 1 (Parser.symbol "-") (ExprBinary Difference)
            , Pratt.infixLeft 10 (Parser.symbol "*") (ExprBinary Product)
            , Pratt.infixLeft 10 (Parser.symbol "/") (ExprBinary Quotient)
            , Pratt.infixLeft 20 (Parser.symbol "|>") (ExprBinary PipedFunction)
            , Pratt.infixLeft 99 (Parser.succeed ()) Opal.Ast.Frontend.ExprFunctionApplication
            ]
        , spaces = Parser.spaces
        }


parseLetIn : Pratt.Config Opal.Ast.Frontend.Expression -> Parser Opal.Ast.Frontend.Expression
parseLetIn config =
    Parser.succeed ExprLetIn
        |. Parser.keyword "let"
        |. Parser.spaces
        |= parseDefinitions
        |. Parser.spaces
        |. Parser.keyword "in"
        |. Parser.spaces
        |= Pratt.subExpression 0 config


parseIfElse : Pratt.Config Opal.Ast.Frontend.Expression -> Parser Opal.Ast.Frontend.Expression
parseIfElse config =
    Parser.succeed ExprIfElse
        |. Parser.keyword "if"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. Parser.keyword "then"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces
        |. Parser.keyword "else"
        |. Parser.spaces
        |= Pratt.subExpression 0 config
        |. Parser.spaces



-- parseFunction : Pratt.Config Opal.Ast.Frontend.Expression -> Parser Opal.Ast.Frontend.Expression
-- parseFunction config =
--     Parser.succeed ExprFunctionApplication
--         |= parseLabel
--         |. Parser.spaces
--         |= Parser.sequence
--             { start = "("
--             , separator = ","
--             , spaces = Parser.spaces
--             , item = Pratt.subExpression 0 config
--             , end = ")"
--             , trailing = Forbidden
--             }
--         |> Parser.backtrackable


parenthesizedExpression : Pratt.Config Opal.Ast.Frontend.Expression -> Parser Opal.Ast.Frontend.Expression
parenthesizedExpression config =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Pratt.subExpression 0 config
        |. Parser.symbol ")"


parseLambda : Pratt.Config Opal.Ast.Frontend.Expression -> Parser Opal.Ast.Frontend.Expression
parseLambda config =
    Parser.succeed ExprLambda
        |= Parser.sequence
            { start = "\\"
            , separator = ","
            , spaces = Parser.spaces
            , item = parseLabel
            , end = "->"
            , trailing = Forbidden
            }
        |. Parser.spaces
        |= Pratt.subExpression 0 config


parseLiteral : Parser Opal.Ast.Frontend.Literal
parseLiteral =
    Parser.oneOf
        [ Parser.map LitInt parseInt
        , Parser.map LitString parseString
        , Parser.map LitBool parseBool
        ]


parseInt : Parser Int
parseInt =
    Parser.succeed ()
        |. Parser.chompIf Char.isDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\digitsStr ->
                case String.toInt digitsStr of
                    Just int ->
                        Parser.succeed int

                    Nothing ->
                        Parser.problem ("Expected an Int but found " ++ digitsStr)
            )


parseString : Parser String
parseString =
    Parser.succeed identity
        |. Parser.symbol "\""
        |= Parser.loop "" parseStringHelper
        |. Parser.symbol "\""


parseBool : Parser Bool
parseBool =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.keyword "True"
        , Parser.succeed False
            |. Parser.keyword "False"
        ]
        |> Parser.backtrackable


parseStringHelper : String -> Parser (Step String String)
parseStringHelper result =
    Parser.oneOf
        [ Parser.succeed (Loop (result ++ "\""))
            |. Parser.symbol "\\\""
        , Parser.succeed ()
            |. Parser.chompIf (\char -> char /= '"')
            |> Parser.getChompedString
            |> Parser.map (\charStr -> Loop (result ++ charStr))
        , Parser.succeed (Done result)
        ]


typeCheck : Module -> Result String Module
typeCheck module_ =
    Ok module_



---- HELPERS ----


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
    String.join "\n--\n" (List.map deadEndToString deadEnds)


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
    "Error ( " ++ String.fromInt deadEnd.row ++ ", " ++ String.fromInt deadEnd.col ++ " ): " ++ problemToString deadEnd.problem


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        Expecting str ->
            "Expecting " ++ str

        ExpectingInt ->
            "Expecting Int"

        ExpectingHex ->
            "Expecting Hex"

        ExpectingOctal ->
            "Expecting Octal"

        ExpectingBinary ->
            "Expecting Binary"

        ExpectingFloat ->
            "Expecting Float"

        ExpectingNumber ->
            "Expecting Number"

        ExpectingVariable ->
            "Expecting Variable"

        ExpectingSymbol symbol ->
            "Expecting Symbol " ++ symbol

        ExpectingKeyword word ->
            "Expecting Keyword " ++ word

        ExpectingEnd ->
            "Expecting End"

        UnexpectedChar ->
            "Unexpected Char"

        Problem str ->
            "Problem " ++ str

        BadRepeat ->
            "Bad Repeat"
