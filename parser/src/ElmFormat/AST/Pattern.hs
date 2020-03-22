{-# OPTIONS_GHC -Wall #-}
module ElmFormat.AST.Pattern where

import ElmFormat.AST.V0_16
import qualified ElmFormat.Reporting.Annotation as A


type Pattern =
    A.Located Pattern'


data Pattern'
    = Anything
    | UnitPattern Comments
    | Literal Literal
    | VarPattern LowercaseIdentifier
    | OpPattern SymbolIdentifier
    | Data [UppercaseIdentifier] [(Comments, Pattern)]
    | PatternParens (Commented Pattern)
    | Tuple [Commented Pattern]
    | EmptyListPattern Comments
    | List [Commented Pattern]
    | ConsPattern
        { first :: WithEol Pattern
        , rest :: [(Comments, Comments, Pattern, Maybe String)]
        }
    | EmptyRecordPattern Comments
    | Record [Commented LowercaseIdentifier]
    | Alias (Pattern, Comments) (Comments, LowercaseIdentifier)
    deriving (Eq, Show)
