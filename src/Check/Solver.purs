module Check.Solver where

import Prelude

import Check.Core as Core
import Check.Core.Traversal (rewriteExprTopDown, rewriteTypeTopDown)
import Check.Environment (BindingData, UniData)
import Check.Error (CheckError(..))
import Check.Monad (Check)
import Check.Unify (UnifyError, unify)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (gets, modify_, state)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CatQueue as CatQueue
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type ConstraintProgress =
  { unifyErrors :: Array UnifyError
  }

type Substitution =
  { univars :: Map Core.Uni UniData
  }

solveConstraint :: Core.Constraint -> Check ConstraintProgress
solveConstraint = case _ of
  Core.ConEquals range tys -> do
    univars <- gets _.univars
    case unify univars range tys of
      Left errs ->
        pure { unifyErrors: NonEmptyArray.toArray errs }
      Right newUnivars -> do
        modify_ _ { univars = newUnivars }
        pure { unifyErrors: [] }

solve :: Check Substitution
solve = go []
  where
  go :: Array UnifyError -> Check Substitution
  go unifyErrors =
    next >>= case _ of
      Just con -> do
        progress <- solveConstraint con
        go (unifyErrors <> progress.unifyErrors)
      Nothing -> do
        for_ (NonEmptyArray.fromFoldable unifyErrors) do
          throwError <<< map UnificationError
        { univars: _ } <$> gets _.univars

  next :: Check (Maybe Core.Constraint)
  next = state \st ->
    case CatQueue.uncons st.constraints of
      Just (Tuple head tail) -> Tuple (Just head) $ st { constraints = tail }
      Nothing -> Tuple Nothing st

applySubstitutionToExpr :: Substitution -> Core.Expr -> Core.Expr
applySubstitutionToExpr sub = rewriteExprTopDown case _ of
  Core.ExprLit ty range lit ->
    Core.ExprLit (applySubstitutionToType sub ty) range lit
  Core.ExprVar ty range ident ->
    Core.ExprVar (applySubstitutionToType sub ty) range ident
  Core.ExprLet ty range letGroup expr ->
    Core.ExprLet (applySubstitutionToType sub ty) range letGroup expr
  Core.ExprApp ty range expr1 expr2 ->
    Core.ExprApp (applySubstitutionToType sub ty) range expr1 expr2
  Core.ExprAbs ty range binding expr ->
    Core.ExprAbs (applySubstitutionToType sub ty) range (applySubstitutionToBinding sub binding) expr
  Core.ExprTyApp ty range expr appTy ->
    Core.ExprTyApp (applySubstitutionToType sub ty) range expr (applySubstitutionToType sub appTy)
  Core.ExprTyAbs ty range binding expr ->
    Core.ExprTyAbs (applySubstitutionToType sub ty) range (applySubstitutionToBinding sub binding) expr

applySubstitutionToBinding :: Substitution -> Core.Binding -> Core.Binding
applySubstitutionToBinding sub (Core.Binding ty range ident) =
  Core.Binding (applySubstitutionToType sub ty) range ident

applySubstitutionToLet :: Substitution -> Core.Let -> Core.Let
applySubstitutionToLet sub = case _ of
  Core.LetOne ident expr ->
    Core.LetOne ident (applySubstitutionToExpr sub expr)
  Core.LetRec defs ->
    Core.LetRec (map (applySubstitutionToExpr sub) <$> defs)

applySubstitutionToType :: Substitution -> Core.Ty -> Core.Ty
applySubstitutionToType sub = rewriteTypeTopDown case _ of
  ty@(Core.TyType _) -> ty
  ty@(Core.TyArrow _ _ _) -> ty
  Core.TyForall range binding ty ->
    Core.TyForall range (applySubstitutionToBinding sub binding) ty
  Core.TyVar ty range ident ->
    Core.TyVar (applySubstitutionToType sub ty) range ident
  Core.TyApp ty range ty1 ty2 ->
    Core.TyApp (applySubstitutionToType sub ty) range ty1 ty2
  Core.TyUni ty range uni@(Core.Scoped scope u)
    | Just { solution: Just tySub } <- Map.lookup u sub.univars ->
        tySub
    | otherwise ->
        Core.TyUni (applySubstitutionToType sub ty) range uni

applySubstitutionToEnvironment :: Substitution -> BindingData -> BindingData
applySubstitutionToEnvironment sub binding =
  binding { type = applySubstitutionToType sub binding.type }
