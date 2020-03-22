module ElmFormat.Parse.Binop (binops) where

import Text.Parsec ((<|>), choice, try)

import qualified ElmFormat.AST.Expression as E
import qualified ElmFormat.AST.Variable as Var
import ElmFormat.Parse.Helpers (commitIf, addLocation, multilineToBool)
import ElmFormat.Parse.IParser
import ElmFormat.Parse.Whitespace
import qualified ElmFormat.Reporting.Annotation as A


binops
    :: IParser E.Expr
    -> IParser E.Expr
    -> IParser Var.Ref
    -> IParser E.Expr
binops term last anyOp =
  addLocation $
  do  ((e, ops), multiline) <- trackNewline ((,) <$> term <*> nextOps)
      return $
        case ops of
          [] ->
            A.drop e
          _ ->
            E.Binops e ops $ multilineToBool multiline
  where
    nextOps =
      choice
        [ commitIf (whitespace >> anyOp) $
            do  preOpComments <- whitespace
                op <- anyOp
                preExpressionComments <- whitespace
                expr <- Left <$> try term <|> Right <$> last
                case expr of
                  Left t -> (:) (preOpComments, op, preExpressionComments, t) <$> nextOps
                  Right e -> return [(preOpComments, op, preExpressionComments, e)]
        , return []
        ]
