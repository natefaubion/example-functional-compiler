module Syntax.Parser
  ( Parser
  , runParser
  , parseModule
  , parseDecl
  , parseExpr
  , parseType
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Lazy (defer)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Free (Free, runFree)
import Control.Monad.State (gets, put)
import Data.Either (Either)
import Data.Identity (Identity(..))
import Data.Lazy as Lazy
import Data.List (List)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.String as String
import Data.Tuple (Tuple(..))
import Syntax.Lexer (TokenStep(..), TokenStream(..), lex)
import Syntax.Token (Comment, Position(..), PositionedToken(..), Range(..), Token(..), printTokenName)
import Syntax.Token as Token
import Syntax.Tree (Argument(..), Decl(..), Delimited(..), Expr(..), LetBinding(..), Lit(..), Module(..), Ty(..), TypeArgument(..), Typed(..), TypedIdentifier(..), Wrapped(..))
import Syntax.Tree as Tree
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, failWithPosition)
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators (lookAhead, optionMaybe)
import Text.Parsing.Parser.Pos as Pos

-- Free Identity is just to act as a trampoline so we don't stack overflow in
-- a JS runtime while parsing large documents.
type Parser = ParserT TokenStream (Free Identity)

runParser :: forall a. String -> Parser a -> Either ParseError a
runParser s = runFree (un Identity) <<< Parser.runParserT (lex s)

eof :: (Token -> String) -> Parser (Array Comment)
eof mkError = do
  TokenStream stream <- gets \(ParseState stream _ _) -> stream
  case Lazy.force stream of
    TokenError _ error -> Parser.fail error
    TokenEOF leading _ -> pure leading
    TokenCons (PositionedToken _ _ tok _) _ _ -> do
      let expected = mkError tok
      if String.null expected then
        Parser.fail $ "Unexpected " <> printTokenName tok <> "; expected EOF"
      else
        Parser.fail $ "Unexpected " <> printTokenName tok <> "; expected " <> expected

expectMap :: forall a. (PositionedToken -> Maybe a) -> Parser a
expectMap pred = do
  TokenStream stream <- gets \(ParseState stream _ _) -> stream
  case Lazy.force stream of
    TokenError _ error -> Parser.fail error
    TokenEOF _ _  -> Parser.fail "Unexpected EOF"
    TokenCons ptok@(PositionedToken _ (Range pos _) tok _) nextPos next ->
      case pred ptok of
        Nothing ->
          failWithPosition
            ("Unexpected " <> printTokenName tok)
            (toParserPosition pos)
        Just a -> do
          put $ ParseState next (toParserPosition nextPos) true
          pure a

-- `Text.Parsing.Parser` does not track expected alternatives, so we are doing
-- it manually by munging the error string. This is not pretty, and it doesn't
-- track optional branches, but it works well enough. This does not account for
-- `empty` however, so we should not use `oneOf`. A different workaround might
-- be to put in another state layer to track it ourselves, or to implement the
-- feature upstream.
withExpected :: forall a. (Maybe Token -> String) -> Parser a -> Parser a
withExpected mkError = flip catchError appendError
  where
  appendError perr@(Parser.ParseError err pos) = do
    mbTok <- lookAhead $ optionMaybe $ expect (const true)
    let expected = mkError (Token.token <$> mbTok)
    if String.null expected then
      throwError perr
    else
      case String.lastIndexOf (String.Pattern "; expected ") err of
        Just ix1 ->
          case String.lastIndexOf (String.Pattern expected) err of
            Just ix2 | ix2 > ix1 ->
              throwError perr
            _ -> do
              let err' = err <> ", " <> expected
              throwError $ Parser.ParseError err' pos
        _ -> do
          let err' = err <> "; expected " <> expected
          throwError $ Parser.ParseError err' pos

toParserPosition :: Position -> Pos.Position
toParserPosition (Position line column) = Pos.Position { line, column }

expect :: (Token -> Boolean) -> Parser PositionedToken
expect pred = expectMap \ptok@(PositionedToken _ _ tok _) ->
  if pred tok then Just ptok else Nothing

token :: Token -> Parser PositionedToken
token tok = withExpected (\_ -> printTokenName tok) $ expect (eq tok)

delimited :: forall a. Token -> Parser a -> Parser (Delimited a)
delimited delim p = go List.Nil =<< p
  where
  -- Why not use List.many? The naive Applicative implementation of this
  -- parser would be:
  -- ```purescript
  --   Delimited
  --     <$> p
  --     <*> List.many (try (Tuple <$> token delim <*> p))
  --     <*> optionMaybe (token delim)
  -- ```
  -- But the necessary `try` (necessary due to supporting a trailing
  -- delimiter) is very problematic. Introducing `try` means our parser will
  -- backtrack to this point if there is _any_ failure in `p`. This is much
  -- too far, and will lead to extremely poor errors. Any failure in this term
  -- parser will result in trying to close off this result and continuing. We
  -- want parse failures in `p` to be reported instead.
  go :: List (Tuple PositionedToken a) -> a -> Parser (Delimited a)
  go stk head =
    optionMaybe (token delim) >>= case _ of
      Nothing -> pure $ done stk head Nothing
      Just tok ->
        optionMaybe p >>= case _ of
          Nothing -> pure $ done stk head (Just tok)
          Just next -> go (List.Cons (Tuple tok next) stk) head

  done :: List (Tuple PositionedToken a) -> a -> Maybe PositionedToken -> Delimited a
  done stk head last = Delimited head (List.reverse stk) last

wrapped :: forall a. Token -> Parser a -> Token -> Parser (Wrapped a)
wrapped l p r = Wrapped <$> token l <*> p <*> token r

many1 :: forall a. Parser a -> Parser (NonEmptyList a)
many1 p = NonEmptyList.cons' <$> p <*> List.many p

parseModule :: Parser Module
parseModule = Module <$> delimited Semicolon parseDecl <*> (eof eofTokenError)
  where
  -- It is easy to miss a semicolon separator between lets. We special case the
  -- next token on `Let` to suggest inserting a semicolon.
  eofTokenError = case _ of
    Let -> printTokenName Semicolon
    _ -> printTokenName Let

parseDecl :: Parser Decl
parseDecl = DeclLet <$> token Let <*> parseLet

-- All parsers for `Let` must be deferred, because `Let` contains an expression,
-- which can contain other `Let`s. Parser cycles must be evaluated on demand.
parseLet :: Parser Tree.Let
parseLet = defer \_ ->
  Tree.LetOne <$> parseLetDefinition
    <|> Tree.LetRec <$> many1 (Tuple <$> token Rec <*> parseLetDefinition)

parseLetDefinition :: Parser Tree.LetDefinition
parseLetDefinition = defer \_ ->
  Tree.LetDefinition
    <$> parseLetBinding
    <*> token Equals
    <*> parseExpr

parseLetBinding :: Parser LetBinding
parseLetBinding = defer \_ ->
  LetBinding
    <$> parseIdentifier
    <*> optionMaybe parseQuantifier
    <*> List.many parseArgument
    <*> optionMaybe parseTyped
  where
  parseQuantifier =
    wrapped SquareLeft (delimited Comma parseTypeArgument) SquareRight

parseArgument :: Parser Argument
parseArgument = parseInferredArgument <|> parseTypedArgument
  where
  parseInferredArgument =
    InferredArgument
      <$> parseIdentifier

  parseTypedArgument =
    TypedArgument
      <$> wrapped ParenLeft parseTypedIdentifier ParenRight

  parseTypedIdentifier =
    TypedIdentifier
      <$> parseIdentifier
      <*> token Colon
      <*> parseType

parseTypeArgument :: Parser TypeArgument
parseTypeArgument = TypeArgument <$> parseIdentifier <*> optionMaybe parseTyped

-- All parsers for `Expr` must be deferred since it is recursive.
parseExpr :: Parser Expr
parseExpr = defer \_ -> do
  expr <- parseExpr1
  ExprTyped expr <$> token Colon <*> parseType
    <|> pure expr

parseExpr1 :: Parser Expr
parseExpr1 = defer \_ ->
  parseExprLet <|> parseExpr2
  where
  -- Staging `ExprLet` before `ExprApp` means that you cannot use `let` as
  -- an argument without wrapping it in parens. This is done to yield better
  -- errors. Since this syntax is not whitespace sensitive, and semicolons are
  -- used as a separator, we can get odd errors in a top-level declaration if
  -- the semicolon is omitted. For example, if we allowed let as an argument:
  -- ```
  --   let foo = 42
  --   let bar = 12;
  -- ```
  -- This would parse as:
  -- ```
  --   let foo = 42 (let bar = 12;)
  -- ```
  -- Yielding an "Expected expression" error _after_ the `bar` declaration. By
  -- not allowing let as an argument, the error is presented after `foo`, which
  -- is what we want.
  parseExprLet = defer \_ -> do
    ExprLet
      <$> token Let
      <*> parseLet
      <*> token Semicolon
      <*> parseExpr

parseExpr2 :: Parser Expr
parseExpr2 = defer \_ -> do
  expr <- parseExpr3
  ExprApp expr <$> many1 parseExpr3
    <|> pure expr

parseExpr3 :: Parser Expr
parseExpr3 = defer \_ ->
  parseExprFun <|> parseExprAtom
  where
  parseExprFun = defer \_ -> do
    ExprFun
      <$> token Lambda
      <*> many1 parseArgument
      <*> token Arrow
      <*> parseExpr

parseExprAtom :: Parser Expr
parseExprAtom = withExpected (const "expression") $ defer \_ ->
  ExprLit <$> parseLit
    <|> ExprVar <$> parseIdentifier
    <|> ExprParens <$> wrapped ParenLeft parseExpr ParenRight

-- All parsers for `Ty` must be deferred since it is recursive.
parseType :: Parser Ty
parseType = defer \_ -> do
  ty <- parseType1
  TyTyped ty <$> token Colon <*> parseType1
    <|> pure ty

parseType1 :: Parser Ty
parseType1 = defer \_ -> do
  ty <- parseType2
  TyArrow ty <$> token Arrow <*> parseType
    <|> pure ty

parseType2 :: Parser Ty
parseType2 = defer \_ -> do
  atom <- parseTypeAtom
  TyApp atom <$> many1 parseTypeAtom
    <|> pure atom

parseTypeAtom :: Parser Ty
parseTypeAtom = withExpected (const "type") $ defer \_ ->
  TyType <$> token Ty
    <|> TyVar <$> parseIdentifier
    <|> TyParens <$> wrapped ParenLeft parseType ParenRight

parseTyped :: Parser Typed
parseTyped = Typed <$> token Colon <*> parseType

parseLit :: Parser Lit
parseLit = parseInt <|> parseString
  where
  parseInt = expectMap \ptok@(PositionedToken _ _ tok _) ->
    case tok of
      Int _ value -> Just $ LitInt ptok value
      _ -> Nothing

  parseString = expectMap \ptok@(PositionedToken _ _ tok _) ->
    case tok of
      String _ value -> Just $ LitString ptok value
      _ -> Nothing

parseIdentifier :: Parser Tree.Identifier
parseIdentifier = expectMap \ptok@(PositionedToken _ _ tok _) ->
  case tok of
    Identifier ident -> Just $ Tree.Identifier ptok ident
    _ -> Nothing
