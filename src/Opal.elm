module Opal exposing (..)

import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..), Step(..), Trailing(..))
import Pratt
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
    { definitions : List Definition
    }


parseOpal : Parser Module
parseOpal =
    Parser.succeed (\definitions -> { definitions = definitions })
        |= parseDefinitions
        |. Parser.spaces
        |. Parser.end


parseDefinitions : Parser (List Definition)
parseDefinitions =
    Parser.loop [] parseDefinitionsHelper


parseDefinitionsHelper : List Definition -> Parser (Step (List Definition) (List Definition))
parseDefinitionsHelper reverseDefinitions =
    Parser.oneOf
        [ Parser.succeed (\definition -> Loop (definition :: reverseDefinitions))
            |= parseDefinition
        , Parser.succeed (Done (List.reverse reverseDefinitions))
        ]


type alias Definition =
    { label : String
    , type_ : Maybe Type
    , body : Expression
    }


type Type
    = TConcrete String
    | TVariable String
    | TFunction (List Type) Type


parseDefinition : Parser Definition
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


praseTypeDef : Parser (Maybe Type)
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


parseType : Parser Type
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


parseConcreteType : Parser Type
parseConcreteType =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isUpper char)
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map TConcrete
        |> Parser.backtrackable


parseTypeVariable : Parser Type
parseTypeVariable =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char && Char.isLower char)
        |. Parser.chompWhile Char.isAlphaNum
        |> Parser.getChompedString
        |> Parser.map TVariable
        |> Parser.backtrackable


parseFunctionType : Pratt.Config Type -> Parser Type
parseFunctionType config =
    Parser.succeed TFunction
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


type Expression
    = ExprLiteral Literal
    | ExprBinary BinaryOperator Expression Expression
    | ExprUnary UnaryOperator Expression
    | ExprFunctionApplication String (List Expression)
    | ExprAnonymousFunction (List String) Expression
    | ExprWord String
    | ExprLetIn (List Definition) Expression
    | ExprIfElse Expression Expression Expression


simplify : Expression -> Expression
simplify =
    Transform.transformAll recurseExpression simplifyAll


recurseExpression : (Expression -> Expression) -> Expression -> Expression
recurseExpression fn expression =
    case expression of
        ExprBinary op leftExpr rightExpr ->
            ExprBinary op (fn leftExpr) (fn rightExpr)

        ExprLiteral _ ->
            expression

        ExprUnary unary expr ->
            ExprUnary unary (fn expr)

        ExprFunctionApplication label argExprs ->
            ExprFunctionApplication label (List.map fn argExprs)

        ExprAnonymousFunction args expr ->
            ExprAnonymousFunction args (fn expr)

        ExprWord _ ->
            expression

        ExprLetIn defs expr ->
            let
                recurseDef =
                    \def -> { def | body = fn def.body }
            in
            ExprLetIn (List.map recurseDef defs) (fn expr)

        ExprIfElse conditionExpr thenExpr elseExpr ->
            ExprIfElse (fn conditionExpr) (fn thenExpr) (fn elseExpr)


simplifyAll : Expression -> Maybe Expression
simplifyAll =
    Transform.orList_
        [ simplifyPipe
        ]


simplifyPipe : Expression -> Expression
simplifyPipe expression =
    case expression of
        ExprBinary PipedFunction leftExpr (ExprFunctionApplication label argExprs) ->
            ExprFunctionApplication label (leftExpr :: argExprs)

        _ ->
            expression


type
    BinaryOperator
    -- Math
    = Sum
    | Difference
    | Product
    | Quotient
      -- Helper
    | PipedFunction
      -- Joinable
    | Concat


type UnaryOperator
    = Negate


parseExpression : Parser Expression
parseExpression =
    Pratt.expression
        { oneOf =
            [ Pratt.literal (Parser.map ExprLiteral parseLiteral)
            , Pratt.prefix 5 (Parser.symbol "-") (ExprUnary Negate)
            , parseLetIn
            , parseIfElse
            , parseFunction
            , parenthesizedExpression
            , Pratt.literal (Parser.map ExprWord parseLabel)
            , parseLambda
            ]
        , andThenOneOf =
            [ Pratt.infixLeft 1 (Parser.symbol "++") (ExprBinary Concat)
            , Pratt.infixLeft 1 (Parser.symbol "+") (ExprBinary Sum)
            , Pratt.infixLeft 1 (Parser.symbol "-") (ExprBinary Difference)
            , Pratt.infixLeft 10 (Parser.symbol "*") (ExprBinary Product)
            , Pratt.infixLeft 10 (Parser.symbol "/") (ExprBinary Quotient)
            , Pratt.infixLeft 20 (Parser.symbol ".") (ExprBinary PipedFunction)
            ]
        , spaces = Parser.spaces
        }


parseLetIn : Pratt.Config Expression -> Parser Expression
parseLetIn config =
    Parser.succeed ExprLetIn
        |. Parser.keyword "let"
        |. Parser.spaces
        |= parseDefinitions
        |. Parser.spaces
        |. Parser.keyword "in"
        |. Parser.spaces
        |= Pratt.subExpression 0 config


parseIfElse : Pratt.Config Expression -> Parser Expression
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


parseFunction : Pratt.Config Expression -> Parser Expression
parseFunction config =
    Parser.succeed ExprFunctionApplication
        |= parseLabel
        |. Parser.spaces
        |= Parser.sequence
            { start = "("
            , separator = ","
            , spaces = Parser.spaces
            , item = Pratt.subExpression 0 config
            , end = ")"
            , trailing = Forbidden
            }
        |> Parser.backtrackable


parenthesizedExpression : Pratt.Config Expression -> Parser Expression
parenthesizedExpression config =
    Parser.succeed identity
        |. Parser.symbol "("
        |= Pratt.subExpression 0 config
        |. Parser.symbol ")"


parseLambda : Pratt.Config Expression -> Parser Expression
parseLambda config =
    Parser.succeed ExprAnonymousFunction
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


type Literal
    = LitInt Int
    | LitString String


parseLiteral : Parser Literal
parseLiteral =
    Parser.oneOf
        [ Parser.map LitInt parseInt
        , Parser.map LitString parseString
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



---- TYPE CHECKER ----


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
