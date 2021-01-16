module Check.Environment where

import Check.Core as Core
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Map (Map)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))
import Syntax.Token (Range, emptyRange)
import Syntax.Tree as Syntax

type BindingMap = Map Core.Identifier BindingData

data BindingNamespace
  = BindingExprVar
  | BindingTyVar

type BindingData =
  { namespace :: BindingNamespace
  , range :: Range
  , scopeLevel :: Core.ScopeLevel
  , type :: Core.Ty
  }

type UniMap = Map Core.Uni UniData

data UniSource
  = UniLetResultType Syntax.Identifier
  | UniTypeArgumentType Syntax.Identifier
  | UniArgumentType Syntax.Identifier
  | UniInstantiation Range (Core.Scoped Core.Identifier)
  | UniAppResultType Range
  | UniTypeAppResultType Range

type UniData =
  { range :: Range
  , scopeLevel :: Core.ScopeLevel
  , solution :: Maybe Core.Ty
  , sources :: NonEmptyArray UniSource
  , type :: Core.Ty
  }

primInt :: Core.Identifier
primInt = Core.Identifier "Int"

primString :: Core.Identifier
primString = Core.Identifier "String"

primTyInt :: Range -> Core.Ty
primTyInt = primTyVar primInt

primTyString :: Range -> Core.Ty
primTyString = primTyVar primString

primTyVar :: Core.Identifier -> Range -> Core.Ty
primTyVar ident range = Core.TyVar (Core.TyType range) range (Core.Scoped (Core.ScopeLevel 0) ident)

primEnvironment :: Array (Tuple Core.Identifier BindingData)
primEnvironment =
  [ Tuple primInt primBindingData
  , Tuple primString primBindingData
  ]
  where
  primBindingData :: BindingData
  primBindingData =
    { namespace: BindingTyVar
    , range: emptyRange
    , scopeLevel: Core.ScopeLevel 0
    , type: Core.TyType emptyRange
    }
