module Check.Core.Traversal
  ( rewriteExprBottomUpM
  , rewriteExprTopDownM
  , rewriteExprWithContextM
  , rewriteExprBottomUp
  , rewriteExprTopDown
  , rewriteExprWithContext
  , foldMapExpr
  , rewriteTypeBottomUpM
  , rewriteTypeTopDownM
  , rewriteTypeWithContextM
  , rewriteTypeBottomUp
  , rewriteTypeTopDown
  , rewriteTypeWithContext
  , foldMapType
  , substituteTypeVar
  , unknownsInType
  ) where

import Prelude

import Check.Core (Binding(..), Expr(..), Identifier, Let(..), Scoped, Ty(..), Uni)
import Control.Monad.Free (runFree)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Data.Const (Const(..))
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity(..))
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.Tuple (Tuple, uncurry)

type Traversal a = forall f. Applicative f => (a -> f a) -> a -> f a
type MonadicTraversal a = forall m. Monad m => (a -> m a) -> a -> m a
type MonadicTraversalWithContext a = forall c m. Monad m => (c -> a -> m (Tuple c a)) -> c -> a -> m a
type MonoidalTraversal a = forall m. Monoid m => (a -> m) -> a -> m
type PureTraversal a = (a -> a) -> a -> a
type PureTraversalWithContext a = forall c. (c -> a -> Tuple c a) -> c -> a -> a

traverseExpr1 :: Traversal Expr
traverseExpr1 k = case _ of
  ExprLet ty range lets expr -> ExprLet ty range <$> goLet lets <*> k expr
  ExprApp ty range expr1 expr2 -> ExprApp ty range <$> k expr1 <*> k expr2
  ExprAbs ty range binding expr -> ExprAbs ty range binding <$> k expr
  ExprTyApp ty range expr arg -> ExprTyApp ty range <$> k expr <*> pure arg
  ExprTyAbs ty range binding expr -> ExprTyAbs ty range binding <$> k expr
  expr -> pure expr
  where
  goLet = case _ of
    LetOne ident expr -> LetOne ident <$> k expr
    LetRec lets -> LetRec <$> traverse (traverse k) lets

traverseType1 :: Traversal Ty
traverseType1 k = case _ of
  TyArrow range ty1 ty2 -> TyArrow range <$> k ty1 <*> k ty2
  TyForall range binding ty -> TyForall range <$> goBinding binding <*> k ty
  TyApp ty range ty1 ty2 -> TyApp ty range <$> k ty1 <*> k ty2
  ty -> pure ty
  where
  goBinding (Binding ty range var) =
    (\newTy -> Binding newTy range var) <$> k ty

bottomUpTraversal :: Traversal ~> MonadicTraversal
bottomUpTraversal traversal k = go
  where go a = k =<< traversal go a

topDownTraversal :: Traversal ~> MonadicTraversal
topDownTraversal traversal k = go
  where go a = k a >>= traversal go

topDownTraversalWithContext :: Traversal ~> MonadicTraversalWithContext
topDownTraversalWithContext traversal k = flip (runReaderT <<< go)
  where go a = ReaderT \ctx -> k ctx a >>= uncurry (flip (runReaderT <<< traversal go))

monoidalTraversal :: Traversal ~> MonoidalTraversal
monoidalTraversal traversal k = un Const <<< runFree (un Identity) <<< un Compose <<< go
  where go a = Compose (pure (Const (k a))) <*> traversal go a

purely :: MonadicTraversal ~> PureTraversal
purely traversal k = runFree (un Identity) <<< traversal (pure <<< k)

purelyWithContext :: MonadicTraversalWithContext ~> PureTraversalWithContext
purelyWithContext traversal k c = runFree (un Identity) <<< traversal (\c' a' -> pure (k c' a')) c

rewriteExprBottomUpM :: MonadicTraversal Expr
rewriteExprBottomUpM = bottomUpTraversal traverseExpr1

rewriteExprTopDownM :: MonadicTraversal Expr
rewriteExprTopDownM = topDownTraversal traverseExpr1

rewriteExprWithContextM :: MonadicTraversalWithContext Expr
rewriteExprWithContextM = topDownTraversalWithContext traverseExpr1

rewriteExprBottomUp :: PureTraversal Expr
rewriteExprBottomUp = purely rewriteExprBottomUpM

rewriteExprTopDown :: PureTraversal Expr
rewriteExprTopDown = purely rewriteExprTopDownM

rewriteExprWithContext :: PureTraversalWithContext Expr
rewriteExprWithContext = purelyWithContext rewriteExprWithContextM

foldMapExpr :: MonoidalTraversal Expr
foldMapExpr = monoidalTraversal traverseExpr1

rewriteTypeBottomUpM :: MonadicTraversal Ty
rewriteTypeBottomUpM = bottomUpTraversal traverseType1

rewriteTypeTopDownM :: MonadicTraversal Ty
rewriteTypeTopDownM = topDownTraversal traverseType1

rewriteTypeWithContextM :: MonadicTraversalWithContext Ty
rewriteTypeWithContextM = topDownTraversalWithContext traverseType1

rewriteTypeBottomUp :: PureTraversal Ty
rewriteTypeBottomUp = purely rewriteTypeBottomUpM

rewriteTypeTopDown :: PureTraversal Ty
rewriteTypeTopDown = purely rewriteTypeTopDownM

rewriteTypeWithContext :: PureTraversalWithContext Ty
rewriteTypeWithContext = purelyWithContext rewriteTypeWithContextM

foldMapType :: MonoidalTraversal Ty
foldMapType = monoidalTraversal traverseType1

substituteTypeVar :: Scoped Identifier -> Ty -> Ty -> Ty
substituteTypeVar var ty = rewriteTypeBottomUp case _ of
  TyVar _ _ var' | var == var' -> ty
  other -> other

unknownsInType :: Ty -> Array (Scoped Uni)
unknownsInType = foldMapType case _ of
  TyUni _ _ u -> [u]
  _ -> []
