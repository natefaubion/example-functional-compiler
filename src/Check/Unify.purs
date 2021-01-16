module Check.Unify
  ( UnifyCheck
  , UnifyError(..)
  , unify
  ) where

import Prelude

import Check.Core as Core
import Check.Core.Traversal (rewriteTypeTopDownM)
import Check.Environment (UniData, UniMap, UniSource)
import Control.Monad.Except (lift)
import Control.Monad.Free (Free, runFree)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.State (StateT, execStateT, gets, modify_, state)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.List (List)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Syntax.Token (Range)

type UnifyState =
  { univars :: UniMap
  , errors :: Array UnifyError
  , scopeLevel :: Core.ScopeLevel
  , check :: UnifyCheck
  , range :: Range
  }

type UnifyCheck =
  { expect :: Core.Ty
  , actual :: Core.Ty
  }

data UnifyError
  = UnifyInfiniteType (Core.Scoped Core.Uni) Core.Ty UnifyCheck
  -- UnifyFailure contains an optional trace. A unification failure will have
  -- the specific types that failed, but we want to preserve the context of the
  -- original larger constraint.
  | UnifyFailure Range UnifyCheck (Maybe UnifyCheck)

type Unify = StateT UnifyState (Free Identity)

-- There can be multiple errors when unifying two types. For example:
--     (String -> Int) ~ (Foo -> Bar)
-- Has two errors, where String does not unify with Foo, and Int does not
-- unify with Bar. Instead of stopping as soon as we find an error, we collect
-- all errors among unification branches.
unify :: UniMap -> Range -> UnifyCheck -> Either (NonEmptyArray UnifyError) UniMap
unify univars range check =
  case NonEmptyArray.fromArray result.errors of
    Just errors -> Left errors
    Nothing -> Right result.univars
  where
  result =
    unify' false check.expect check.actual
      # flip execStateT { univars, errors: [], range, check, scopeLevel: Core.ScopeLevel 0 }
      # runFree (un Identity)

-- We track whether we are performing a "nested" unification (unifying children
-- of the parent constraint) so that we don't add a redundant unification trace
-- in the error where the trace is identical to the constraint that fails. An
-- alternative would be to track a full trace with a list or use a Maybe.
unify' :: Boolean -> Core.Ty -> Core.Ty -> Unify Unit
unify' nested expect actual = do
  -- We must deref both types so we don't accidentally solve a unification
  -- variable that has already been solved. We always want to make sure we are
  -- unifying against solutions if they exist.
  expect' <- uniDeref expect
  actual' <- uniDeref actual
  case expect', actual' of
    Core.TyType _, Core.TyType _ ->
      pure unit
    Core.TyVar _ _ a, Core.TyVar _ _ b | a == b ->
      pure unit
    Core.TyUni _ _ (Core.Scoped _ a), Core.TyUni _ _ (Core.Scoped _ b) |  a == b ->
      pure unit
    Core.TyApp _ _ a1 b1, Core.TyApp _ _ a2 b2 -> do
      unify' true a1 a2
      unify' true b1 b2
    Core.TyArrow _ a1 b1, Core.TyArrow _ a2 b2 -> do
      unify' true a1 a2
      unify' true b1 b2
    -- When unifying two unification variables, we compare their scope levels to
    -- not unnecessarily promote one. The one with the larger scope level (more
    -- nested) gets solved to the one with the smaller scope level.
    ty1@(Core.TyUni _ _ u1@(Core.Scoped s1 _)), ty2@(Core.TyUni _ _ u2@(Core.Scoped s2 _))
      | s1 < s2 -> uniSolve u2 ty1
      | otherwise -> uniSolve u1 ty2
    Core.TyUni _ _ u, ty ->
      uniSolve u ty
    ty, Core.TyUni _ _ u ->
      uniSolve u ty
    ty1, ty2 -> modify_ \st -> do
      let trace = if nested then Just st.check else Nothing
      let error = UnifyFailure st.range { expect: expect', actual: actual' } trace
      st { errors = Array.snoc st.errors error }

-- When solving a unification variable, we need to do some bookkeeping to make
-- sure we have a valid solution. This is primarily handled by
-- `uniCheckOccurences`, but we also take care of propagating sources for use
-- in errors.
uniSolve :: Core.Scoped Core.Uni -> Core.Ty -> Unify Unit
uniSolve scu@(Core.Scoped initScope u) initTy = do
  uniData <- state \st -> Tuple (unsafeUniLookup u st.univars) $ st { scopeLevel = initScope }
  mbTy <- uniCheckOccurrences u initTy
  case mbTy of
    -- If we get no result of uniCheckOccurences, our solution would have
    -- resulted in an infinite type.
    Nothing -> modify_ \st -> do
      let error = UnifyInfiniteType scu initTy st.check
      st { errors = Array.snoc st.errors error }
    Just ty -> do
      newScope <- gets _.scopeLevel
      let newUniData = uniData { scopeLevel = newScope, solution = Just ty }
      modify_ \st -> st { univars = Map.insert u newUniData $ uniPropagate uniData.sources ty st.univars }
      unify' true (Core.typeOfType ty) uniData.type

-- A solution is invalid if it mentions the unification variable we are solving.
-- This is the source of an "infinite type" error. If a solution mentions the
-- variable we are solving, then applying this solution would loop forever. In
-- literature, this is called the "occurs check", by which we check if the
-- variable occurs in the solution. This is done by traversing the solution
-- top-down while dereferencing unification variables.
uniCheckOccurrences :: Core.Uni -> Core.Ty -> Unify (Maybe Core.Ty)
uniCheckOccurrences u = runMaybeT <<< rewriteTypeTopDownM \ty -> do
  ty' <- lift $ uniDeref ty
  case ty' of
    Core.TyUni _ _ (Core.Scoped scope' u') -> do
      -- Make sure that the unification variable is not the variable we are
      -- currently solving. This will short-circuit and return Nothing.
      -- Otherwise we will get a rewritten type.
      guard (u /= u')
      scope <- gets _.scopeLevel
      -- TODO: This promotion is wrong. We should promote the scope of other
      -- variables we find, not the one we are solving, because it's solved!
      when (scope' < scope) do
        modify_ _ { scopeLevel = scope' }
      pure ty'
    _ ->
      pure ty'

-- Propagates sources for unification variables. When we solve a unification
-- variable with another unification variable, the solution should inherit its
-- sources. This lets us track all the places that unification variables are
-- references for error messages.
uniPropagate :: NonEmptyArray UniSource -> Core.Ty -> UniMap -> UniMap
uniPropagate sources = case _ of
  Core.TyUni _ _ (Core.Scoped _ u) ->
    Map.update (\uniData -> Just (uniData { sources = uniData.sources <> sources })) u
  _ ->
    identity

-- An invariant is that when we are unifying, any unification variables should
-- be in our UniMap. If it's not, something has gone very wrong, so we just
-- crash in the case that it is not found, or the case that it has already been
-- solved.
unsafeUniLookup :: Core.Uni -> UniMap -> UniData
unsafeUniLookup u univars = case Map.lookup u univars of
  Just uniData@{ solution: Nothing } ->
    uniData
  Just _ ->
    unsafeCrashWith $ "[solveUnivar] Unification variable ?T" <> show (unwrap u) <> " already solved."
  Nothing ->
    unsafeCrashWith $ "[solveUnivar] Unification variable ?T" <> show (unwrap u) <> " not in scope."

type UniPath = List (Tuple Core.Uni UniData)

-- Much of the time, a type may reference a unification variable that has been
-- solved. Dereferencing the unification variable involves following a path of
-- solutions (which may themselves be unification variables with solutions) until
-- we reach a type that is not a solved unification variable. During this process
-- we compress the path to avoid redundant lookups later. That is, if a solution
-- has multiple layers of indirection, we should go ahead and just store the
-- shortest paths to the solution. Path compression is only an optimization and
-- is not needed for soundness.
uniDeref :: Core.Ty -> Unify Core.Ty
uniDeref = case _ of
  ty@(Core.TyUni _ _ (Core.Scoped _ u)) ->
    state \st -> case Map.lookup u st.univars of
      Just uniData@{ solution: Just solTy } -> do
        st { univars = _ } <$> goPath st.univars (pure (Tuple u uniData)) solTy
      _ ->
        Tuple ty st
  ty ->
    pure ty
  where
  goPath :: UniMap -> UniPath -> Core.Ty -> Tuple Core.Ty UniMap
  goPath univars path = case _ of
    -- Keep following solved unification variables until we reach something
    -- that is not solved, or is not a unification variable.
    Core.TyUni _ _ (Core.Scoped _ u)
      | Just uniData@{ solution: Just ty } <- Map.lookup u univars ->
          goPath univars (List.Cons (Tuple u uniData) path) ty
    ty -> case path of
      List.Cons _ ps ->
        Tuple ty (goCompress ty univars ps)
      _ ->
        Tuple ty univars

  -- Walk back over the path, inserting the solution for all of the unification
  -- variables in the path.
  goCompress :: Core.Ty -> UniMap -> UniPath -> UniMap
  goCompress ty us = case _ of
    List.Cons (Tuple u uniData) ps ->
      goCompress ty (Map.insert u (uniData { solution = Just ty }) us) ps
    List.Nil ->
      us
