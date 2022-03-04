module Opal.Ast.Frontend exposing
    ( BinaryOperator(..)
    , Definition
    , Expression(..)
    , Literal(..)
    , Type(..)
    , UnaryOperator(..)
    )


type alias Definition =
    { label : String
    , type_ : Maybe Type
    , body : Expression
    }


type Type
    = Concrete String
    | Variable String
    | Function (List Type) Type


type Expression
    = ExprLiteral Literal
    | ExprVariable String
    | ExprArgument String
    | ExprBinary BinaryOperator Expression Expression
    | ExprUnary UnaryOperator Expression
    | ExprLambda (List String) Expression
    | ExprFunctionApplication Expression Expression
    | ExprIfElse Expression Expression Expression
    | ExprLetIn (List Definition) Expression


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


type Literal
    = LitInt Int
    | LitString String
    | LitBool Bool
