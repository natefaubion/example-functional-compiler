module Print where

import Prelude

import Ansi.Codes (Color(..), GraphicsParam)
import Check.Core as Core
import Check.Core.Traversal (unknownsInType)
import Check.Environment (UniSource(..), UniData)
import Check.Error (CheckError(..))
import Check.Monad (CheckState)
import Check.Solver (Substitution, applySubstitutionToType)
import Check.Unify (UnifyError(..))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (fold, foldMap)
import Data.List as List
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Dodo (Doc, alignCurrentColumn, enclose, flexGroup, indent, lines, spaceBreak, text, words, (<+>))
import Dodo.Ansi (bold, dim, foreground)
import Print.Precedence (Assoc(..), Prec, binaryop, parensLeft, parensRight, prec, term, unPrec)
import Syntax.Token (Position(..), Range(..), emptyRange, range1)
import Syntax.Tree as Syntax

type PrettyDoc = Doc GraphicsParam

printParseError :: String -> Position -> PrettyDoc
printParseError err pos =
  lines
    [ bold $ words
        [ text "Failed to parse input"
        , foreground Yellow (enclose (text "[") (text "]") $ printPosition pos) <> text ":"
        ]
    , indent $ text err
    ]

printError :: CheckState -> CheckError -> PrettyDoc
printError st = case _ of
  UnificationError (UnifyFailure range fail mbTrace) -> do
    let expect' = applySubstitutionToType substitution fail.expect
    let actual' = applySubstitutionToType substitution fail.actual
    let unknowns = Array.nub $ unknownsInCheck fail <> foldMap unknownsInCheck mbTrace
    lines
      [ bold $ text "Types do not match " <> printRange range <> text ":"
      , indent $ lines
          [ text "Actual type " <> printRange (Core.rangeOfType actual') <> text ":"
          , indent $ printType actual'
          , text "does not match expected type " <> printRange (Core.rangeOfType expect') <> text ":"
          , indent $ printType expect'
          , mbTrace # foldMap \trace -> do
              let expectTrace = applySubstitutionToType substitution trace.expect
              let actualTrace = applySubstitutionToType substitution trace.actual
              lines
                [ text "While checking that actual type " <> printRange (Core.rangeOfType actualTrace) <> text ":"
                , indent $ printType actualTrace
                , text "matches expected type " <> printRange (Core.rangeOfType expectTrace) <> text ":"
                , indent $ printType expectTrace
                ]
          , printUnivars substitution unknowns
          ]
      ]
  UnificationError (UnifyInfiniteType scu@(Core.Scoped _ u) ty trace) -> do
    let sub = substitution { univars = Map.delete u substitution.univars }
    let ty' = applySubstitutionToType sub ty
    let unknowns = Array.nub $ unknownsInType ty'
    let expectTrace = applySubstitutionToType sub trace.expect
    let actualTrace = applySubstitutionToType sub trace.actual
    lines
      [ bold $ text "Infinite type required " <> printRange (Core.rangeOfType ty') <> text ":"
      , indent $ lines
          [ text "The unknown type " <> printUni scu <> text " would need to equal:"
          , indent $ printType ty'
          , text "which references itself, resulting in an infinitely expanding type."
          , text "While checking that actual type " <> printRange (Core.rangeOfType actualTrace) <> text ":"
          , indent $ printType actualTrace
          , text "matches expected type " <> printRange (Core.rangeOfType expectTrace) <> text ":"
          , indent $ printType expectTrace
          , printUnivars substitution unknowns
          ]
      ]
  TypeVariableInExpr range (Syntax.Identifier tok ident) ->
    lines
      [ bold $ text "Type variable used as an expression " <> printRange (range1 tok) <> text ":"
      , indent $ lines
          [ text ident
          , text "bound at " <> printRange range
          ]
      ]
  ExprVariableInType range (Syntax.Identifier tok ident) ->
    lines
      [ bold $ text "Variable used as a type " <> printRange (range1 tok) <> text ":"
      , indent $ lines
          [ text ident
          , text "bound at " <> printRange range
          ]
      ]
  UnknownIdentifier (Syntax.Identifier tok ident) ->
    lines
      [ bold $ text "Unknown identifier " <> printRange (range1 tok) <> text ":"
      , indent $ text ident
      ]
  UnknownTypesInDecl range unknowns ->
    lines
      [ bold $ text "Unknown types in declaration " <> printRange range <> text ":"
      , indent $ lines
          [ printUnivars substitution $ NonEmptyArray.toArray unknowns
          , text "Unknowns must be annotated with a specific type or generalized with a type variable."
          ]
      ]
  where
  substitution =
    { univars: st.univars }

  unknownsInCheck check = do
    let expect' = applySubstitutionToType substitution check.expect
    let actual' = applySubstitutionToType substitution check.actual
    unknownsInType expect' <> unknownsInType actual'

printUnivars :: Substitution -> Array (Core.Scoped Core.Uni) -> PrettyDoc
printUnivars { univars } unis =
  unis
    # Array.mapMaybe (\scu@(Core.Scoped _ u) -> Tuple scu <$> Map.lookup u univars)
    # map printUniSources
    # lines
  where
  printUniSources :: Tuple (Core.Scoped Core.Uni) UniData -> PrettyDoc
  printUniSources (Tuple u { sources }) = do
    let
      better = NonEmptyArray.filter isBetterSource sources
      purpose'
        | Array.null better = NonEmptyArray.toArray sources
        | otherwise = better
    purpose'
      # map (printUniSourceItem u)
      # lines

  printUniSourceItem :: Core.Scoped Core.Uni -> UniSource -> PrettyDoc
  printUniSourceItem u p =
    words
      [ text "*"
      , printUni u
      , text "is an unknown"
      , printUniSource p
      ]

  isBetterSource :: UniSource -> Boolean
  isBetterSource = case _ of
    UniLetResultType _ -> true
    UniTypeArgumentType _ -> true
    UniArgumentType _ -> true
    _ -> false

printUniSource :: UniSource -> PrettyDoc
printUniSource = case _ of
  UniLetResultType (Syntax.Identifier tok var) ->
    words
      [ text "result type for"
      , text var
      , printRange (range1 tok)
      ]
  UniTypeArgumentType (Syntax.Identifier tok var) ->
    words
      [ text "type for type argument"
      , text var
      , printRange (range1 tok)
      ]
  UniArgumentType (Syntax.Identifier tok var) ->
    words
      [ text "type for argument"
      , text var
      , printRange (range1 tok)
      ]
  UniInstantiation range (Core.Scoped _ (Core.Identifier var)) ->
    words
      [ text "instantiation for type variable"
      , text var
      , printRange range
      ]
  UniAppResultType range ->
    words
      [ text "result type for function application"
      , printRange range
      ]
  UniTypeAppResultType range ->
    words
      [ text "result type for type application"
      , printRange range
      ]

printModule :: Core.Module -> PrettyDoc
printModule (Core.Module decls) = lines $ printDecl <$> decls

printDecl :: Core.Decl -> PrettyDoc
printDecl = case _ of
  Core.DeclLet (Core.LetOne (Core.Scoped _ (Core.Identifier var)) letExpr) ->
    lines
      [ words
          [ text "let"
          , text var
          , text ":"
          , alignCurrentColumn $ flexGroup $ printType (Core.typeOfExpr letExpr)
          , text "="
          ]
      , indent (flexGroup (printExpr letExpr)) <> text ";"
      ]
  Core.DeclLet (Core.LetRec defs) -> do
    let
      recs =
        defs # map \(Tuple (Core.Scoped _ (Core.Identifier var)) letExpr) ->
          lines
            [ words
                [ text "rec"
                , text var
                , text ":"
                , printType (Core.typeOfExpr letExpr)
                , text "="
                ]
            , indent $ flexGroup $ printExpr letExpr
            ]
    lines
      [ text "let"
      , indent (lines recs) <> text ";"
      ]

printExpr :: Core.Expr -> PrettyDoc
printExpr expr = unPrec (printExprPrec expr)

printExprPrec :: Core.Expr -> Prec Int PrettyDoc
printExprPrec = go
  where
  go = case _ of
    Core.ExprLit _ _ (Core.LitInt value) ->
      term $ text $ show value
    Core.ExprLit _ _ (Core.LitString value) ->
      term $ text $ show value
    Core.ExprVar _ _ (Core.Scoped _ (Core.Identifier var)) ->
      term $ text var
    Core.ExprLet _ _ (Core.LetOne (Core.Scoped _ (Core.Identifier var)) letExpr) expr -> do
      prec bottom $ lines
        [ words
            [ text "let"
            , text var
            , text ":"
            , printType (Core.typeOfExpr letExpr)
            , text "="
            ]
        , indent (flexGroup (printExpr letExpr)) <> text ";"
        , printExpr expr
        ]
    Core.ExprLet _ _ (Core.LetRec defs) expr -> do
      let
        recs =
          defs # map \(Tuple (Core.Scoped _ (Core.Identifier var)) letExpr) ->
            lines
              [ words
                  [ text "rec"
                  , text var
                  , text ":"
                  , printType $ Core.typeOfExpr letExpr
                  , text "="
                  ]
              , indent $ flexGroup $ printExpr letExpr
              ]
      prec bottom $ lines
        [ text "let"
        , indent (lines recs) <> text ";"
        , printExpr expr
        ]
    (Core.ExprApp _ _ expr1 expr2) -> do
      let
        -- We don't need to print parens around abtractions if it is the last
        -- argument, so we special case them and wrap them with `term`.
        lastArg = case expr2 of
          Core.ExprAbs _ _ _ _ -> term $ printExpr expr2
          Core.ExprTyAbs _ _ _ _ -> term $ printExpr expr2
          _ -> printExprPrec expr2
      prec top $ goApp (pure lastArg) expr1
    expr@(Core.ExprTyApp _ _ _ _) -> do
      prec top $ goApp mempty expr
    Core.ExprAbs _ _ (Core.Binding ty _ (Core.Scoped _ (Core.Identifier var))) expr -> do
      let arg = words [ text var, text ":", printType ty ]
      prec bottom $ fold
        [ words
            [ text "\\" <> enclose (text "(") (text ")") arg
            , text "->"
            ]
        , spaceBreak
        , printExpr expr
        ]
    Core.ExprTyAbs _ _ (Core.Binding ty _ (Core.Scoped _ (Core.Identifier var))) expr -> do
      let arg = words [ text var, text ":", printType ty ]
      prec bottom $ fold
        [ words
            [ text "\\" <> enclose (text "[") (text "]") arg
            , text "->"
            ]
        , spaceBreak
        , printExpr expr
        ]

  goApp args = case _ of
    Core.ExprApp _ _ e1 e2 ->
      goApp (List.Cons (printExprPrec e2) args) e1
    Core.ExprTyApp _ _ e1 ty1 -> do
      let tyApp = term $ enclose (text "[") (text "]") $ printType ty1
      goApp (List.Cons tyApp args) e1
    e1 -> do
      let app expr = spaceBreak <> indent (parensRight AssocLeft top expr)
      parensLeft AssocLeft top (printExprPrec e1) <> flexGroup (foldMap app args)

printType :: Core.Ty -> PrettyDoc
printType ty = unPrec (printTypePrec ty)

printTypePrec :: Core.Ty -> Prec Int PrettyDoc
printTypePrec = go
  where
  go = case _ of
    Core.TyType _ ->
      term $ text "type"
    Core.TyVar _ _ (Core.Scoped _ (Core.Identifier var)) ->
      term $ text var
    Core.TyUni _ _ u ->
      term $ printUni u
    Core.TyApp _ _ ty1 ty2 ->
      binaryop AssocLeft top (<+>) (go ty1) (go ty2)
    Core.TyArrow _ ty1 ty2 -> do
      let op a b = a <+> text "->" <+> b
      binaryop AssocRight bottom op (go ty1) (go ty2)
    Core.TyForall _ (Core.Binding kind _ (Core.Scoped _ (Core.Identifier var))) ty ->
      prec bottom $ fold
        [ enclose (text "[") (text "]") $ words
            [ text var
            , text ":"
            , printType kind
            ]
        , spaceBreak
        , printType ty
        ]

printUni :: Core.Scoped Core.Uni -> PrettyDoc
printUni (Core.Scoped _ (Core.Uni u)) = foreground Blue $ text $ "?T" <> show u

printPosition :: Position -> PrettyDoc
printPosition (Position line column) =
  fold
    [ text $ show (line + 1)
    , text ":"
    , text $ show (column + 1)
    ]

printRange :: Range -> PrettyDoc
printRange range@(Range pos1 pos2)
  | range == emptyRange =
      dim $ text "[internal]"
  | otherwise =
      foreground Yellow $ enclose (text "[") (text "]") $ words
        [ printPosition pos1
        , text "-"
        , printPosition pos2
        ]
