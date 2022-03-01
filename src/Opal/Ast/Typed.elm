module Opal.Ast.Typed exposing
    ( BinaryOperator(..)
    , Definition
    , Expr(..)
    , Expression(..)
    , Literal(..)
    , Type(..)
    , UnaryOperator(..)
    , getExpr
    , getType
    )

import Opal.InferTypes.IdSource exposing (Id)


type alias Definition =
    { label : String
    , type_ : Maybe Type
    , body : Expression
    }


type Type
    = Concrete String
    | Variable Id
    | Function (List Type) Type


type Expression
    = Expression ( Expr, Type )


type Expr
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


getExpr : Expression -> Expr
getExpr (Expression ( expr, _ )) =
    expr


getType : Expression -> Type
getType (Expression ( _, type_ )) =
    type_
