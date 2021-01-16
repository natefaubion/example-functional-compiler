module Check.Monad where

import Prelude

import Check.Core as Core
import Check.Environment (BindingMap, UniMap, UniSource, BindingData)
import Check.Error (CheckError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Free (Free, runFree)
import Control.Monad.State (StateT, gets, modify_, runStateT, state)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.CatQueue (CatQueue)
import Data.CatQueue as CatQueue
import Data.Either (Either)
import Data.Foldable (class Foldable)
import Data.Identity (Identity(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (over, un)
import Data.Tuple (Tuple(..))
import Syntax.Token (Range)

type Check = ExceptT (NonEmptyArray CheckError) (StateT CheckState (Free Identity))

runCheck :: forall a. CheckState -> Check a -> Tuple (Either (NonEmptyArray CheckError) a) CheckState
runCheck state =
  runExceptT
    >>> flip runStateT state
    >>> runFree (un Identity)

type CheckState =
  { fresh :: Int
  , constraints :: CatQueue Core.Constraint
  , environment :: BindingMap
  , univars :: UniMap
  , scopeLevel :: Core.ScopeLevel
  }

emptyState :: CheckState
emptyState =
  { fresh: 0
  , constraints: CatQueue.empty
  , environment: Map.empty
  , univars: Map.empty
  , scopeLevel: Core.ScopeLevel 0
  }

-- | Allocates a fresh unification variable with a given kind.
freshUni :: UniSource -> Core.Ty -> Range -> Check Core.Ty
freshUni source ty range = state \st -> do
  let
    uni = Core.Uni st.fresh
    uniData =
      { range
      , scopeLevel: st.scopeLevel
      , solution: Nothing
      , sources: pure source
      , type: ty
      }
  Tuple (Core.TyUni ty range (Core.Scoped st.scopeLevel uni)) $ st
    { fresh = st.fresh + 1
    , univars = Map.insert uni uniData st.univars
    }

-- | Resets the unification environment. This is done after every top-level
-- | declaration. Type-checking top-level declarations is self contained, and
-- | free unification variables should either be generalized or result in an
-- | error.
resetSubstitution :: Check Unit
resetSubstitution = modify_ \st -> st { univars = Map.empty :: _ }

-- | Yields the current scope level.
currentScope :: Check Core.ScopeLevel
currentScope = gets _.scopeLevel

-- | Swaps the scope level for another, returning the current scope.
swapScope :: Core.ScopeLevel -> Check Core.ScopeLevel
swapScope scope = state \st -> Tuple st.scopeLevel $ st { scopeLevel = scope }

-- | Emits a constraint, which should be solved by the constraint solver
-- | sometime in the future.
emit :: Core.Constraint -> Check Unit
emit con = modify_ \st -> st { constraints = CatQueue.snoc st.constraints con}

-- | Increments the scope level for a given block.
scoped :: forall a. Check a -> Check a
scoped m = join $ state \st -> do
  let scopeLevel' = over Core.ScopeLevel (_ + 1) st.scopeLevel
  let restore = _ { environment = st.environment, scopeLevel = st.scopeLevel }
  Tuple (m <* modify_ restore) $ st { scopeLevel = scopeLevel' }

-- | Adds bindings to the type-checking environment.
addToEnvironment :: forall f. Foldable f => f (Tuple Core.Identifier BindingData) -> Check Unit
addToEnvironment bindings = modify_ \st ->
  st { environment = Map.fromFoldable bindings <> st.environment }
