module Opal.Ast.Canonical exposing
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
    | ExprBinary BinaryOperator Expression Expression
    | ExprUnary UnaryOperator Expression
    | ExprFunctionApplication String (List Expression)
    | ExprAnonymousFunction (List String) Expression
    | ExprWord String
    | ExprLetIn (List Definition) Expression
    | ExprIfElse Expression Expression Expression


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
