{-# LANGUAGE DuplicateRecordFields #-}
module ElmFormat.AST.Expression where

import ElmFormat.AST.V0_16
import qualified ElmFormat.AST.Pattern as Pattern
import qualified ElmFormat.AST.Variable as Var
import qualified ElmFormat.Reporting.Annotation as A


---- GENERAL AST ----

data UnaryOperator =
    Negative
    deriving (Eq, Show)


data LetDeclaration
  = LetDefinition Pattern.Pattern [(Comments, Pattern.Pattern)] Comments Expr
  | LetAnnotation (Var.Ref, Comments) (Comments, Type)
  | LetComment Comment
  deriving (Eq, Show)


type Expr =
    A.Located Expr'


type IfClause =
  (Commented Expr, Commented Expr)


data Expr'
    = Unit Comments
    | Literal Literal
    | VarExpr Var.Ref

    | App Expr [(Comments, Expr)] FunctionApplicationMultiline
    | Unary UnaryOperator Expr
    | Binops Expr [(Comments, Var.Ref, Comments, Expr)] Bool
    | Parens (Commented Expr)

    | ExplicitList
        { terms :: Sequence Expr
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Range (Commented Expr) (Commented Expr) Bool

    | Tuple [Commented Expr] Bool
    | TupleFunction Int -- will be 2 or greater, indicating the number of elements in the tuple

    | Record
        { base :: Maybe (Commented LowercaseIdentifier)
        , fields :: Sequence (Pair LowercaseIdentifier Expr)
        , trailingComments :: Comments
        , forceMultiline :: ForceMultiline
        }
    | Access Expr LowercaseIdentifier
    | AccessFunction LowercaseIdentifier

    | Lambda [(Comments, Pattern.Pattern)] Comments Expr Bool
    | If IfClause [(Comments, IfClause)] (Comments, Expr)
    | Let [LetDeclaration] Comments Expr
    | Case (Commented Expr, Bool) [(Commented Pattern.Pattern, (Comments, Expr))]

    -- for type checking and code gen only
    | GLShader String
    deriving (Eq, Show)


instance A.Strippable Expr' where
  stripRegion d =
    case d of
      App e0 es b ->
        App
          (A.stripRegion $ A.map A.stripRegion e0)
          (map (fmap (A.stripRegion . A.map A.stripRegion)) es)
          b

      Tuple es b ->
        Tuple
          (map (fmap (A.stripRegion . A.map A.stripRegion)) es)
          b

      _ -> d
