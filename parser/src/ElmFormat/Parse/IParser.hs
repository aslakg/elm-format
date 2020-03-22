module ElmFormat.Parse.IParser where

import Control.Monad.State (State)
import qualified ElmFormat.Parse.State as State
import Text.Parsec hiding (newline, spaces, State)


type SourceM = State SourcePos
type IParser a = ParsecT String State.State SourceM a
