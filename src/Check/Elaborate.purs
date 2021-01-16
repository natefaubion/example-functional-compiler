module Check.Elaborate where

import Prelude

import Check.Core as Core
import Check.Core.Traversal (substituteTypeVar)
import Check.Environment (BindingData, BindingNamespace(..), UniSource(..), primEnvironment, primTyInt, primTyString)
import Check.Error (CheckError(..))
import Check.Monad (Check, addToEnvironment, currentScope, emit, freshUni, resetSubstitution, scoped)
import Check.Solver (Substitution, solve)
import Check.Solver as Solver
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldr, for_)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (List, NonEmptyList)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Syntax.Token (range1)
import Syntax.Tree as Syntax

elaborateModule :: Syntax.Module -> Check Core.Module
elaborateModule (Syntax.Module (Syntax.Delimited hd tl _) _) = do
  addToEnvironment primEnvironment
  scoped do
    let decls = NonEmptyList.cons' hd $ snd <$> tl
    Core.Module <$> for decls \decl ->
      elaborateDecl decl <* resetSubstitution

elaborateDecl :: Syntax.Decl -> Check Core.Decl
elaborateDecl = case _ of
  Syntax.DeclLet _ letDecl -> do
    Tuple letDeclElab letDeclEnv <- checkLet letDecl
    sub <- solve
    for_ (NonEmptyArray.fromArray (unsolvedInSubstitution sub)) do
      throwError <<< pure <<< UnknownTypesInDecl (Syntax.rangeOfLet letDecl)
    let letDeclSub = Solver.applySubstitutionToLet sub letDeclElab
    let letDeclEnvSub = map (Solver.applySubstitutionToEnvironment sub) <$> letDeclEnv
    addToEnvironment letDeclEnvSub
    pure $ Core.DeclLet letDeclSub
  where
  unsolvedInSubstitution :: Substitution -> Array (Core.Scoped Core.Uni)
  unsolvedInSubstitution =
    _.univars
      >>> Map.filter (_.solution >>> isNothing)
      >>> Map.toUnfoldable
      >>> map (\(Tuple u { scopeLevel }) -> Core.Scoped scopeLevel u)

inferExpr :: Syntax.Expr -> Check Core.Expr
inferExpr = case _ of
  Syntax.ExprLit lit ->
    inferLit lit
  Syntax.ExprVar ident@(Syntax.Identifier tok var) -> do
    { environment } <- get
    case Map.lookup (Core.Identifier var) environment of
      Just { namespace: BindingExprVar, scopeLevel, type: ty } ->
        pure $ Core.ExprVar ty (range1 tok) $ Core.Scoped scopeLevel $ Core.Identifier var
      Just { range } ->
        throwError $ pure $ TypeVariableInExpr range ident
      Nothing ->
        throwError $ pure $ UnknownIdentifier ident
  Syntax.ExprApp fn args -> do
    let Tuple fn' args' = flattenExprApp fn args
    fnElab <- inferExpr fn'
    checkExprApp fnElab args'
  Syntax.ExprFun tok args _ expr -> do
    argEnv <- for args inferArgument
    addToEnvironment argEnv
    exprElab <- inferExpr expr
    let
      toExprAbs exprElab' (Tuple var { range, scopeLevel, type: varTy }) = do
        let absType = Core.TyArrow range varTy (Core.typeOfExpr exprElab')
        let absRange = range <> Core.rangeOfExpr exprElab'
        let binding = Core.Binding varTy range (Core.Scoped scopeLevel var)
        -- TODO: Not sure about this range
        Core.ExprAbs absType absRange binding exprElab'
    pure $ foldr (flip toExprAbs) exprElab argEnv
  Syntax.ExprLet tok letGroup _ expr ->
    scoped do
      Tuple letGroupElab letEnvBindings <- checkLet letGroup
      addToEnvironment letEnvBindings
      exprElab <- inferExpr expr
      let range = range1 tok <> Core.rangeOfExpr exprElab
      pure $ Core.ExprLet (Core.typeOfExpr exprElab) range letGroupElab exprElab
  Syntax.ExprTyped expr _ ty -> do
    tyElab <- checkType ty $ Core.TyType $ Syntax.rangeOfType ty
    checkExpr expr tyElab
  Syntax.ExprParens (Syntax.Wrapped _ expr _) ->
    inferExpr expr

checkLet :: Syntax.Let -> Check (Tuple Core.Let (NonEmptyList (Tuple Core.Identifier BindingData)))
checkLet = case _ of
  Syntax.LetOne (Syntax.LetDefinition (Syntax.LetBinding ident@(Syntax.Identifier identTok var) tyVarBindings argBindings mbResultTy) _ expr) -> do
    scopeLevel <- currentScope
    scoped do
      tyVarEnv <- inferTypeVariables tyVarBindings
      addToEnvironment tyVarEnv
      scoped do
        letVarEnv <- for argBindings inferArgument
        addToEnvironment letVarEnv
        resultTy <- case mbResultTy of
          Just (Syntax.Typed _ resultTy) -> do
            let range = range1 identTok -- TODO: This is not a good range.
            checkType resultTy $ Core.TyType range
          Nothing -> do
            let range = range1 identTok
            let kind = Core.TyType range
            freshUni (UniLetResultType ident) kind range
        exprElab <- checkExpr expr resultTy
        let
          letExprElab = elaborateLetExpr (tyVarEnv <> letVarEnv) exprElab
          letElab = Core.LetOne (Core.Scoped scopeLevel (Core.Identifier var)) letExprElab
          bindings = NonEmptyList.singleton $ Tuple (Core.Identifier var)
            { namespace: BindingExprVar
            , range: range1 identTok
            , scopeLevel
            , type: Core.typeOfExpr letExprElab
            }
        pure $ Tuple letElab bindings
  Syntax.LetRec definitions -> do
    scopeLevel <- currentScope
    scoped do
      definitionInfos <- for definitions \(Tuple tok (Syntax.LetDefinition (Syntax.LetBinding ident@(Syntax.Identifier identTok var) tyVarBindings argBindings mbResultTy) _ expr)) -> do
        Tuple arguments resultTy <- scoped do
          tyVarEnv <- inferTypeVariables tyVarBindings
          addToEnvironment tyVarEnv
          scoped do
            letVarEnv <- for argBindings inferArgument
            addToEnvironment letVarEnv
            resultTy <- case mbResultTy of
              Just (Syntax.Typed _ resultTy) -> do
                let range = range1 identTok -- TODO: This is not a good range.
                checkType resultTy $ Core.TyType range
              Nothing -> do
                let range = range1 identTok
                let kind = Core.TyType range
                freshUni (UniLetResultType ident) kind range
            let bindingEnv = tyVarEnv <> letVarEnv
            pure $ Tuple (tyVarEnv <> letVarEnv) resultTy
        let
          definitionRange = range1 tok <> Syntax.rangeOfExpr expr
          definitionTy = elaborateLetType arguments resultTy
          binding = Tuple (Core.Identifier var)
            { namespace: BindingExprVar
            , range: definitionRange
            , scopeLevel
            , type: definitionTy
            }
        pure
          { binding
          , range: definitionRange
          , type: resultTy
          , expr
          , arguments
          }
      let definitionBindings = _.binding <$> definitionInfos
      addToEnvironment definitionBindings
      definitionElabs <- for definitionInfos \def ->
        scoped do
          addToEnvironment def.arguments
          exprElab <- checkExpr def.expr def.type
          let definitionElab = elaborateLetExpr def.arguments exprElab
          pure $ Tuple (Core.Scoped scopeLevel (fst def.binding)) definitionElab
      pure $ Tuple (Core.LetRec definitionElabs) definitionBindings
  where
  elaborateLetExpr :: List (Tuple Core.Identifier BindingData) -> Core.Expr -> Core.Expr
  elaborateLetExpr = flip $ foldr \(Tuple var { namespace, range, scopeLevel, type: ty }) expr ->
    case namespace of
      BindingExprVar -> do
        let exprTy = Core.typeOfExpr expr
        let binding = Core.Binding ty range (Core.Scoped scopeLevel var)
        Core.ExprAbs (Core.TyArrow (range <> Core.rangeOfType exprTy) ty exprTy)
          (range <> Core.rangeOfExpr expr)
          binding expr
      BindingTyVar -> do
        let exprTy = Core.typeOfExpr expr
        let binding = Core.Binding ty range (Core.Scoped scopeLevel var)
        Core.ExprTyAbs
          (Core.TyForall (range <> Core.rangeOfType exprTy)
            binding exprTy)
          (range <> Core.rangeOfExpr expr) binding expr

  elaborateLetType :: List (Tuple Core.Identifier BindingData) -> Core.Ty -> Core.Ty
  elaborateLetType = flip $ foldr \(Tuple var { namespace, range, scopeLevel, type: ty }) innerTy ->
    case namespace of
      BindingExprVar ->
        Core.TyArrow (range <> Core.rangeOfType innerTy) ty innerTy
      BindingTyVar -> do
        let binding = Core.Binding ty range (Core.Scoped scopeLevel var)
        Core.TyForall (range <> Core.rangeOfType innerTy) binding innerTy

inferTypeVariables :: Maybe (Syntax.Wrapped (Syntax.Delimited Syntax.TypeArgument)) -> Check (List (Tuple Core.Identifier BindingData))
inferTypeVariables tyVarBindings = do
  let
    tyVars = case tyVarBindings of
      Just (Syntax.Wrapped _ (Syntax.Delimited hd tl _) _) ->
        List.Cons hd $ snd <$> tl
      _ ->
        List.Nil
  scopeLevel <- currentScope
  for tyVars \(Syntax.TypeArgument ident@(Syntax.Identifier tok var) mbKind) -> do
    let range = range1 tok
    kind <- case mbKind of
      Just (Syntax.Typed _ ty) ->
        checkType ty $ Core.TyType $ Syntax.rangeOfType ty
      Nothing ->
        freshUni (UniTypeArgumentType ident) (Core.TyType range) range
    pure $ Tuple (Core.Identifier var)
      { namespace: BindingTyVar
      , range
      , scopeLevel
      , type: kind
      }

inferArgument :: Syntax.Argument -> Check (Tuple Core.Identifier BindingData)
inferArgument = case _ of
  Syntax.InferredArgument ident@(Syntax.Identifier tok var) -> do
    let range = range1 tok
    let kind = Core.TyType range
    tyElab <- freshUni (UniArgumentType ident) kind range
    scopeLevel <- currentScope
    pure $ Tuple (Core.Identifier var)
      { namespace: BindingExprVar
      , range
      , scopeLevel
      , type: tyElab
      }
  Syntax.TypedArgument (Syntax.Wrapped _ (Syntax.TypedIdentifier (Syntax.Identifier tok var) _ ty) _) -> do
    let range = range1 tok
    tyElab <- checkType ty $ Core.TyType range
    scopeLevel <- currentScope
    pure $ Tuple (Core.Identifier var)
      { namespace: BindingExprVar
      , range
      , scopeLevel
      , type: tyElab
      }

inferLit :: Syntax.Lit -> Check Core.Expr
inferLit = case _ of
  Syntax.LitInt tok value -> do
    let range = range1 tok
    pure $ Core.ExprLit (primTyInt range) range $ Core.LitInt value
  Syntax.LitString tok value -> do
    let range = range1 tok
    pure $ Core.ExprLit (primTyString range) range $ Core.LitString value

checkExpr :: Syntax.Expr -> Core.Ty -> Check Core.Expr
checkExpr expr ty = do
  exprElab <- instantiateExpr =<< inferExpr expr
  emit $ Core.ConEquals (Core.rangeOfExpr exprElab) { actual: Core.typeOfExpr exprElab, expect: ty }
  pure exprElab

checkExprApp :: Core.Expr -> NonEmptyList Syntax.Expr -> Check Core.Expr
checkExprApp expr args = do
  instantiatedExpr <- instantiateExpr expr
  NonEmptyList.foldM checkArg instantiatedExpr args
  where
  checkArg :: Core.Expr -> Syntax.Expr -> Check Core.Expr
  checkArg fn arg = case Core.typeOfExpr fn of
    Core.TyArrow range argTy resultTy -> do
      argElab <- checkExpr arg argTy
      let appRange = Core.rangeOfExpr fn <> Core.rangeOfExpr argElab
      pure $ Core.ExprApp resultTy appRange fn argElab
    ty -> do
      let tyRange = Core.rangeOfType ty
      let tyKind = Core.TyType tyRange
      argElab <- inferExpr arg
      let exprRange = Core.rangeOfExpr fn
      let appRange = exprRange <> Core.rangeOfExpr argElab
      resultTy <- freshUni (UniAppResultType appRange) tyKind tyRange
      let fnTy = Core.TyArrow appRange (Core.typeOfExpr argElab) resultTy
      emit $ Core.ConEquals exprRange { actual: ty, expect: fnTy }
      pure $ Core.ExprApp resultTy appRange fn argElab

instantiateExpr :: Core.Expr -> Check Core.Expr
instantiateExpr = applyTypeArgs
  where
  applyTypeArgs :: Core.Expr -> Check Core.Expr
  applyTypeArgs expr = case Core.typeOfExpr expr of
    Core.TyForall _ (Core.Binding kind varRange var) resultTy -> do
      let range = Core.rangeOfExpr expr
      varTy <- freshUni (UniInstantiation range var) kind varRange
      let instantiatedTy = substituteTypeVar var varTy resultTy
      applyTypeArgs $ Core.ExprTyApp instantiatedTy range expr varTy
    _ ->
      pure expr

checkType :: Syntax.Ty -> Core.Ty -> Check Core.Ty
checkType ty kind = do
  elabTy <- inferType ty
  emit $ Core.ConEquals (Core.rangeOfType elabTy)
    { actual: Core.typeOfType elabTy
    , expect: kind
    }
  pure elabTy

inferType :: Syntax.Ty -> Check Core.Ty
inferType = case _ of
  Syntax.TyType tok ->
    pure $ Core.TyType $ range1 tok
  Syntax.TyArrow arg tok result -> do
    argElab <- checkType arg $ Core.TyType $ Syntax.rangeOfType arg
    resultElab <- checkType result $ Core.TyType $ Syntax.rangeOfType result
    let range = Core.rangeOfType argElab <> Core.rangeOfType resultElab
    pure $ Core.TyArrow range argElab resultElab
  Syntax.TyVar ident@(Syntax.Identifier tok var) -> do
    { environment } <- get
    case Map.lookup (Core.Identifier var) environment of
      Just { namespace: BindingTyVar, scopeLevel, type: varTy } ->
        pure $ Core.TyVar varTy (range1 tok) $ Core.Scoped scopeLevel $ Core.Identifier var
      Just { range, scopeLevel } ->
        throwError $ pure $ ExprVariableInType range ident
      Nothing ->
        throwError $ pure $ UnknownIdentifier ident
  Syntax.TyApp ctor args -> do
    let Tuple ctor' args' = flattenTypeApp ctor args
    ctorElab <- inferType ctor'
    checkTypeApp ctorElab args'
  Syntax.TyTyped ty _ kind -> do
    kindElab <- checkType kind $ Core.TyType $ Syntax.rangeOfType kind
    checkType ty kindElab
  Syntax.TyParens (Syntax.Wrapped _ ty _) ->
    inferType ty

checkTypeApp :: Core.Ty -> NonEmptyList (Syntax.Ty) -> Check Core.Ty
checkTypeApp = NonEmptyList.foldM \ctor arg ->
  case Core.typeOfType ctor of
    Core.TyArrow range argTy resultTy -> do
      argElab <- checkType arg argTy
      let appRange = range <> Core.rangeOfType argElab
      pure $ Core.TyApp resultTy appRange ctor argElab
    ty -> do
      let tyRange = Core.rangeOfType ty
      let tyKind = Core.TyType tyRange
      argElab <- inferType arg
      let appRange = Core.rangeOfType ctor <> Core.rangeOfType argElab
      resultTy <- freshUni (UniAppResultType appRange) tyKind tyRange
      let ctorTy = Core.TyArrow tyRange (Core.typeOfType argElab) resultTy
      emit $ Core.ConEquals tyRange { actual: ty, expect: ctorTy }
      pure $ Core.TyApp resultTy appRange ctor argElab

flattenExprApp :: Syntax.Expr -> NonEmptyList Syntax.Expr -> Tuple Syntax.Expr (NonEmptyList Syntax.Expr)
flattenExprApp = case _, _ of
  Syntax.ExprParens (Syntax.Wrapped _ fn _), args ->
    flattenExprApp fn args
  Syntax.ExprApp fn args', args ->
    flattenExprApp fn (args' <> args)
  fn, args ->
    Tuple fn args

flattenTypeApp :: Syntax.Ty -> NonEmptyList Syntax.Ty -> Tuple Syntax.Ty (NonEmptyList Syntax.Ty)
flattenTypeApp = case _, _ of
  Syntax.TyParens (Syntax.Wrapped _ ctor _), args ->
    flattenTypeApp ctor args
  Syntax.TyApp ctor args', args ->
    flattenTypeApp ctor (args' <> args)
  ctor, args ->
    Tuple ctor args
