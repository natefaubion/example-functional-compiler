module Eval.Normalize where

import Prelude

import Check.Core as Core
import Check.Core.Traversal (rewriteTypeWithContext)
import Data.Foldable (foldl, foldr)
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..), fst, uncurry)
import Partial.Unsafe (unsafePartial)

type EvalEnvironment =
  { exprs :: Map (Core.Scoped Core.Identifier) EvalExpr
  , types :: Map (Core.Scoped Core.Identifier) Core.Ty
  }

insertExpr :: Core.Scoped Core.Identifier -> EvalExpr -> EvalEnvironment -> EvalEnvironment
insertExpr ident expr env = env { exprs = Map.insert ident expr env.exprs }

insertType :: Core.Scoped Core.Identifier -> Core.Ty -> EvalEnvironment -> EvalEnvironment
insertType ident ty env = env { types = Map.insert ident ty env.types }

data EvalExpr
  = EvalClosure EvalEnvironment Core.Expr
  | EvalRec EvalEnvironment (NonEmptyList (Tuple (Core.Scoped Core.Identifier) Core.Expr)) Core.Expr

normalizeModule :: Core.Module -> Core.Expr
normalizeModule mod@(Core.Module decls) = do
  let
    env = evalModule mod
    ident = case NonEmptyList.last decls of
      Core.DeclLet (Core.LetOne id _) -> id
      Core.DeclLet (Core.LetRec letGroup) -> fst $ NonEmptyList.last letGroup
  unsafePartial
    $ uncurry reifyExpr
    $ uncurry evalExpr
    $ unrollExpr
    $ fromJust
    $ Map.lookup ident env.exprs

evalModule :: Core.Module -> EvalEnvironment
evalModule (Core.Module decls) = foldl evalDecl mempty decls

evalDecl :: EvalEnvironment -> Core.Decl -> EvalEnvironment
evalDecl env = case _ of
  Core.DeclLet declLet ->
    evalLet env declLet

evalLet :: EvalEnvironment -> Core.Let -> EvalEnvironment
evalLet env = case _ of
  Core.LetOne ident expr ->
    insertExpr ident (EvalClosure env expr) env
  Core.LetRec exprs ->
    foldr (\(Tuple ident expr) -> insertExpr ident (EvalRec env exprs expr)) env exprs

evalExpr :: EvalEnvironment -> Core.Expr -> Tuple EvalEnvironment Core.Expr
evalExpr = go
  where
  go env = case _ of
    expr@(Core.ExprVar _ _ ident) ->
      case Map.lookup ident env.exprs of
        Just exprSub | Tuple env' expr' <- unrollExpr exprSub ->
          go env' expr'
        _ ->
          Tuple env expr
    Core.ExprLet _ _ letGroup expr' ->
      go (evalLet env letGroup) expr'
    expr@(Core.ExprApp _ _ expr1 expr2) ->
      case evalExpr env expr1 of
        Tuple env' (Core.ExprAbs _ _ (Core.Binding _ _ ident) body) ->
          go (insertExpr ident (EvalClosure env expr2) env') body
        _ ->
          Tuple env expr
    expr@(Core.ExprTyApp _ _ expr1 ty) ->
      case evalExpr env expr1 of
        Tuple env' (Core.ExprTyAbs _ _ (Core.Binding _ _ ident) body) ->
          go (insertType ident ty env') body
        _ ->
          Tuple env expr
    expr ->
      Tuple env expr

unrollExpr :: EvalExpr -> Tuple EvalEnvironment Core.Expr
unrollExpr = case _ of
  EvalClosure env expr ->
    Tuple env expr
  EvalRec env recEnv expr ->
    Tuple env $ Core.ExprLet (Core.typeOfExpr expr) (Core.rangeOfExpr expr) (Core.LetRec recEnv) expr

type SubstEnvironment =
  { used :: Map Core.Identifier Int
  , env :: EvalEnvironment
  }

reifyExpr :: EvalEnvironment -> Core.Expr -> Core.Expr
reifyExpr = go <<< { used: mempty, env: _ }
  where
  go :: SubstEnvironment -> Core.Expr -> Core.Expr
  go { used, env } = case _ of
    Core.ExprVar ty range ident ->
      case Map.lookup ident env.exprs of
        Just expr | Tuple env' expr' <- unrollExpr expr ->
          go { used, env: env' } expr'
        Nothing ->
          Core.ExprVar (substituteType env ty) range ident
    Core.ExprApp ty range expr1 expr2 -> do
      let expr1' = go { used, env } expr1
      let Tuple env' expr2' = evalExpr env expr2
      let expr2'' = go { used, env: env' } expr2'
      Core.ExprApp (substituteType env ty) range expr1' expr2''
    expr@(Core.ExprTyApp ty range expr1 ty2) -> do
      let expr1' = go { used, env } expr1
      Core.ExprTyApp (substituteType env ty) range expr1 (substituteType env ty2)
    Core.ExprAbs ty1 range1 (Core.Binding ty2 range2 ident) body -> do
      let Tuple ident' used' = freshName used ident
      let env' = env { exprs = Map.insert ident (EvalClosure mempty (Core.ExprVar ty2 range2 ident')) env.exprs }
      let Tuple env'' body' = evalExpr env' body
      let body'' = go { used: used', env: env'' } body'
      Core.ExprAbs (substituteType env ty1) range1 (Core.Binding (substituteType env ty2) range2 ident') body
    Core.ExprTyAbs ty1 range1 (Core.Binding ty2 range2 ident) body -> do
      let Tuple ident' used' = freshName used ident
      let env' = env { types = Map.insert ident (Core.TyVar ty2 range2 ident') env.types }
      let Tuple env'' body' = evalExpr env' body
      let body'' = go { used: used', env: env'' } body'
      Core.ExprTyAbs (substituteType env ty2) range1 (Core.Binding (substituteType env ty2) range2 ident') body''
    expr ->
      expr

  freshName
    :: Map Core.Identifier Int
    -> Core.Scoped Core.Identifier
    -> Tuple (Core.Scoped Core.Identifier) (Map Core.Identifier Int)
  freshName used scident@(Core.Scoped sc ident@(Core.Identifier var)) =
    case Map.lookup ident used of
      Nothing ->
        Tuple scident (Map.insert ident 0 used)
      Just n ->
        Tuple (Core.Scoped sc (Core.Identifier (var <> "'" <> show n))) (Map.insert ident (n + 1) used)

substituteType :: EvalEnvironment -> Core.Ty -> Core.Ty
substituteType = rewriteTypeWithContext \env -> case _ of
  ty@(Core.TyVar _ _ ident) ->
    case Map.lookup ident env.types of
      Just tySub ->
        Tuple env tySub
      Nothing ->
        Tuple env ty
  ty@(Core.TyForall _ (Core.Binding _ _ ident) _) -> do
    Tuple (env { types = Map.delete ident env.types }) ty
  ty ->
    Tuple env ty
