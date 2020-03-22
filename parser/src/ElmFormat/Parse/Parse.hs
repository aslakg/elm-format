{-# OPTIONS_GHC -Wall #-}
module ElmFormat.Parse.Parse (parse, parseModule, parseDeclarations, parseExpressions) where

import qualified Text.Parsec.Error as Parsec

import ElmFormat.AST.V0_16 (WithEol)
import ElmFormat.AST.Declaration (TopLevelStructure, Declaration)
import qualified ElmFormat.AST.Expression
import qualified ElmFormat.AST.Module
import ElmVersion hiding (parse)
import ElmFormat.Parse.Comments (withEol)
import qualified ElmFormat.Parse.Declaration
import qualified ElmFormat.Parse.Expression
import ElmFormat.Parse.Helpers
import qualified ElmFormat.Parse.Module
import qualified ElmFormat.Reporting.Region as R
import qualified ElmFormat.Reporting.Error.Syntax as Error
import qualified ElmFormat.Reporting.Result as Result
import ElmFormat.Parse.IParser
import Text.Parsec (eof)


parseModule :: ElmVersion -> String -> Result.Result () Error.Error ElmFormat.AST.Module.Module
parseModule elmVersion src =
    parse src (ElmFormat.Parse.Module.elmModule elmVersion)


parseDeclarations :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure Declaration]
parseDeclarations elmVersion src =
    parse src (ElmFormat.Parse.Module.topLevel (ElmFormat.Parse.Declaration.declaration elmVersion) <* eof)


parseExpressions :: ElmVersion -> String -> Result.Result () Error.Error [TopLevelStructure (WithEol ElmFormat.AST.Expression.Expr)]
parseExpressions elmVersion src =
    parse src (ElmFormat.Parse.Module.topLevel (withEol $ ElmFormat.Parse.Expression.expr elmVersion) <* eof)


-- RUN PARSERS

parse :: String -> IParser a -> Result.Result wrn Error.Error a
parse source parser =
  case iParse parser source of
    Right result ->
        return result

    Left err ->
        let pos = R.fromSourcePos (Parsec.errorPos err)
            msgs = Parsec.errorMessages err
        in
            Result.throw (R.Region pos pos) (Error.Parse msgs)
