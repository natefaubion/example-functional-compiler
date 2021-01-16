module Syntax.Tree where

import Prelude

import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Syntax.Token (Comment, PositionedToken, Range, range1, range2)

data Module =
  Module (Delimited Decl) (Array Comment)

data Wrapped a =
  Wrapped PositionedToken a PositionedToken

data Delimited a =
  Delimited a (List (Tuple PositionedToken a)) (Maybe PositionedToken)

data Identifier =
  Identifier PositionedToken String

data Decl =
  DeclLet PositionedToken Let

data Let
  = LetOne LetDefinition
  | LetRec (NonEmptyList (Tuple PositionedToken LetDefinition))

data LetDefinition =
  LetDefinition LetBinding PositionedToken Expr

data LetBinding =
  LetBinding Identifier (Maybe (Wrapped (Delimited TypeArgument))) (List Argument) (Maybe Typed)

data Argument
  = InferredArgument Identifier
  | TypedArgument (Wrapped TypedIdentifier)

data TypeArgument =
  TypeArgument Identifier (Maybe Typed)

data TypedIdentifier =
  TypedIdentifier Identifier PositionedToken Ty

data Expr
  = ExprLit Lit
  | ExprVar Identifier
  | ExprApp Expr (NonEmptyList Expr)
  | ExprFun PositionedToken (NonEmptyList Argument) PositionedToken Expr
  | ExprLet PositionedToken Let PositionedToken Expr
  | ExprTyped Expr PositionedToken Ty
  | ExprParens (Wrapped Expr)

data Lit
  = LitInt PositionedToken Int
  | LitString PositionedToken String

data Ty
  = TyType PositionedToken
  | TyVar Identifier
  | TyApp Ty (NonEmptyList Ty)
  | TyArrow Ty PositionedToken Ty
  | TyTyped Ty PositionedToken Ty
  | TyParens (Wrapped Ty)

data Typed =
  Typed PositionedToken Ty

rangeOfLet :: Let -> Range
rangeOfLet = case _ of
  LetOne def ->
    rangeOfLetDefinition def
  LetRec defs ->
    range1 (fst (NonEmptyList.head defs))
      <> rangeOfLetDefinition (snd (NonEmptyList.last defs))

rangeOfLetDefinition :: LetDefinition -> Range
rangeOfLetDefinition (LetDefinition binding _ expr) =
  rangeOfLetBinding binding <> rangeOfExpr expr

rangeOfLetBinding :: LetBinding -> Range
rangeOfLetBinding (LetBinding (Identifier tok _) tyArgs args returnTy)
  | Just (Typed _ ty) <- returnTy = range1 tok <> rangeOfType ty
  | Just arg <- List.last args = range1 tok <> rangeOfArgument arg
  | Just (Wrapped _ _ tok2) <- tyArgs = range1 tok <> range1 tok2
  | otherwise = range1 tok

rangeOfArgument :: Argument -> Range
rangeOfArgument = case _ of
  InferredArgument (Identifier tok _) -> range1 tok
  TypedArgument (Wrapped tok1 _ tok2) -> range1 tok1 <> range1 tok2

rangeOfExpr :: Expr -> Range
rangeOfExpr = case _ of
  ExprLit lit -> rangeOfLit lit
  ExprVar (Identifier tok _) -> range1 tok
  ExprApp fn args -> rangeOfExpr fn <> rangeOfExpr (NonEmptyList.last args)
  ExprFun tok _ _ expr -> range1 tok <> rangeOfExpr expr
  ExprLet tok _ _ expr -> range1 tok <> rangeOfExpr expr
  ExprTyped expr _ ty -> rangeOfExpr expr <> rangeOfType ty
  ExprParens (Wrapped tok1 _ tok2) -> range2 tok1 tok2

rangeOfLit :: Lit -> Range
rangeOfLit = case _ of
  LitInt tok _ -> range1 tok
  LitString tok _ -> range1 tok

rangeOfType :: Ty -> Range
rangeOfType = case _ of
  TyType tok -> range1 tok
  TyVar (Identifier tok _) -> range1 tok
  TyApp ctor args -> rangeOfType ctor <> rangeOfType (NonEmptyList.last args)
  TyArrow arg _ result -> rangeOfType arg <> rangeOfType result
  TyTyped ty1 _ ty2 -> rangeOfType ty1 <> rangeOfType ty2
  TyParens (Wrapped tok1 _ tok2) -> range2 tok1 tok2
