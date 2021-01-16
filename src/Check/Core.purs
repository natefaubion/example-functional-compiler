module Check.Core where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Syntax.Token (Range)

newtype ScopeLevel = ScopeLevel Int

derive instance newtypeScopeLevel :: Newtype ScopeLevel _
derive newtype instance eqScopeLevel :: Eq ScopeLevel
derive newtype instance ordScopeLevel :: Ord ScopeLevel

data Scoped a = Scoped ScopeLevel a

derive instance eqScoped :: Eq a => Eq (Scoped a)
derive instance ordScoped :: Ord a => Ord (Scoped a)

newtype Identifier = Identifier String

derive instance newtypeIdentifier :: Newtype Identifier _
derive instance eqIdentifier :: Eq Identifier
derive instance ordIdentifier :: Ord Identifier

newtype Uni = Uni Int

derive instance newtypeUni :: Newtype Uni _
derive instance eqUni :: Eq Uni
derive instance ordUni :: Ord Uni

data Module
  = Module (NonEmptyList Decl)

data Decl
  = DeclLet Let

data Expr
  = ExprLit Ty Range Lit
  | ExprVar Ty Range (Scoped Identifier)
  | ExprLet Ty Range Let Expr
  | ExprApp Ty Range Expr Expr
  | ExprAbs Ty Range Binding Expr
  | ExprTyApp Ty Range Expr Ty
  | ExprTyAbs Ty Range Binding Expr

data Let
  = LetOne (Scoped Identifier) Expr
  | LetRec (NonEmptyList (Tuple (Scoped Identifier) Expr))

data Ty
  = TyType Range
  | TyArrow Range Ty Ty
  | TyForall Range Binding Ty
  | TyVar Ty Range (Scoped Identifier)
  | TyApp Ty Range Ty Ty
  | TyUni Ty Range (Scoped Uni)

data Binding
  = Binding Ty Range (Scoped Identifier)

data Lit
  = LitInt Int
  | LitString String

data Constraint =
  ConEquals Range { expect :: Ty, actual :: Ty }

typeOfExpr :: Expr -> Ty
typeOfExpr = case _ of
  ExprLit ty _ _ -> ty
  ExprVar ty _ _ -> ty
  ExprLet ty _ _ _ -> ty
  ExprApp ty _ _ _ -> ty
  ExprAbs ty _ _ _  -> ty
  ExprTyApp ty _ _ _ -> ty
  ExprTyAbs ty _ _ _ -> ty

rangeOfExpr :: Expr -> Range
rangeOfExpr = case _ of
  ExprLit _ range _ -> range
  ExprVar _ range _ -> range
  ExprLet _ range _ _ -> range
  ExprApp _ range _ _ -> range
  ExprAbs _ range _ _ -> range
  ExprTyApp _ range _ _ -> range
  ExprTyAbs _ range _ _ -> range

typeOfType :: Ty -> Ty
typeOfType = case _ of
  TyType range -> TyType range
  TyArrow range _ _ -> TyType range
  TyForall range _ _ -> TyType range
  TyVar ty _ _ -> ty
  TyApp ty _ _ _ -> ty
  TyUni ty _ _ -> ty

rangeOfType :: Ty -> Range
rangeOfType = case _ of
  TyType range -> range
  TyArrow range _ _  -> range
  TyForall range _ _ -> range
  TyVar _ range _ -> range
  TyApp _ range _ _ -> range
  TyUni _ range _ -> range
