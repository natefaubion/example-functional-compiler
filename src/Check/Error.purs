module Check.Error where

import Check.Core as Core
import Check.Unify (UnifyError)
import Data.Array.NonEmpty (NonEmptyArray)
import Syntax.Token (Range)
import Syntax.Tree as Syntax

data CheckError
  = TypeVariableInExpr Range Syntax.Identifier
  | ExprVariableInType Range Syntax.Identifier
  | UnknownIdentifier Syntax.Identifier
  | UnknownTypesInDecl Range (NonEmptyArray (Core.Scoped Core.Uni))
  | UnificationError UnifyError
