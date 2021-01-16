module Syntax.Lexer
  ( TokenStream(..)
  , TokenStep(..)
  , lex
  ) where

import Prelude

import Control.Plus ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, oneOf)
import Data.Int as Int
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Data.String.CodeUnits as SCU
import Partial.Unsafe (unsafeCrashWith)
import Syntax.Token (Comment(..), Position(..), PositionedToken(..), Range(..), Token(..))
import Text.Parsing.StringParser (ParseError(..), Parser(..), PosString, fail)
import Text.Parsing.StringParser.CodePoints (regex)

-- A TokenStream is a lazy cons-list of tokens, terminating with either an
-- end-of-file node, or with an error while parsing a token. This lets us parse
-- on-demand, and avoid lexing an entire document just to potentially fail in
-- the language parser. We can also backtrack in the language parser without
-- having to re-lex any tokens.
newtype TokenStream = TokenStream (Lazy TokenStep)

data TokenStep
  = TokenError Position String
  -- There may be trailing comments in a module, so we emit those as well as the
  -- final position in the document.
  | TokenEOF (Array Comment) Position
  | TokenCons PositionedToken Position TokenStream

-- Lexing proceeds by:
-- * Parsing a single token
-- * Parsing trailing comments (comments up until the next token or newline)
-- * Parsing leading comments for the _next_ token.
-- * Emitting a PositiongedToken and a Position for the start of the next token.
lex :: String -> TokenStream
lex = init <<< { str: _, pos: 0 }
  where
  -- We need to prime the token stream by parsing the initial leading comments
  -- for the first token, at which point we can proceed with the main parsing
  -- loop.
  init :: PosString -> TokenStream
  init str = TokenStream $ Lazy.defer \_ -> do
    let Parser k = leadingComments
    case k str of
      Left _ ->
        -- Since leading comments are completely optional, it's not possible
        -- for the parser to fail. An alternative is to always parse non-empty
        -- comments, treating failure as empty comments.
        unsafeCrashWith "Leading comments can't fail."
      Right { result: leading, suffix } -> do
        let TokenStream next = go (foldl bumpComment (Position 0 0) leading) leading suffix
        Lazy.force next

  go :: Position -> Array Comment -> PosString -> TokenStream
  go startPos leading str = TokenStream $ Lazy.defer \_ ->
    if str.pos == SCU.length str.str then
      TokenEOF leading startPos
    else do
      let Parser k = token'
      case k str of
        Left { pos, error: ParseError error } ->
          -- The parser position may potentially be in the middle of a token,
          -- depending on how the token parser is implemented. This takes the
          -- difference and adjusts the position accordingly.
          TokenError (bumpMultiline startPos (String.take pos str.str)) error
        Right { result, suffix } -> do
          let
            endPos = bumpToken startPos result.token
            nextStart = foldl bumpComment (foldl bumpComment endPos result.trailing) result.nextLeading
            posToken = PositionedToken leading (Range startPos endPos) result.token result.trailing
          TokenCons posToken nextStart (go nextStart result.nextLeading suffix)

  token' :: Parser { token :: Token, trailing :: Array Comment, nextLeading :: Array Comment }
  token' =
    { token: _, trailing: _, nextLeading: _ }
      <$> token
      <*> trailingComments
      <*> leadingComments

bumpToken :: Position -> Token -> Position
bumpToken pos@(Position line col) = case _ of
  Let -> Position line (col + 3)
  Rec -> Position line (col + 3)
  Ty -> Position line (col + 4)
  Equals -> Position line (col + 1)
  Comma -> Position line (col + 1)
  Colon -> Position line (col + 1)
  Semicolon -> Position line (col + 1)
  Lambda -> Position line (col + 1)
  Arrow -> Position line (col + 2)
  ParenLeft -> Position line (col + 1)
  ParenRight -> Position line (col + 1)
  SquareLeft -> Position line (col + 1)
  SquareRight -> Position line (col + 1)
  Identifier ident -> Position line (col + String.length ident)
  Int raw _ -> Position line (col + String.length raw)
  String raw _ -> bumpMultiline pos raw

bumpComment :: Position -> Comment -> Position
bumpComment pos@(Position line col) = case _ of
  Comment comm -> Position line (col + String.length comm)
  Spaces str -> bumpMultiline pos str

bumpMultiline :: Position -> String -> Position
bumpMultiline (Position line col) str = Position newLine newCol
  where
  lines = split (Pattern "\n") str
  lastLine = maybe 0 String.length $ Array.last lines
  newLine = line + Array.length lines - 1
  newCol
    | line == newLine = col + lastLine
    | otherwise = lastLine

leadingComments :: Parser (Array Comment)
leadingComments = Array.many $ leadingSpace <|> lineComment

trailingComments :: Parser (Array Comment)
trailingComments = Array.many $ trailingSpace <|> lineComment

leadingSpace :: Parser Comment
leadingSpace = Spaces <$> regex "(\\s|\\n)+"

trailingSpace :: Parser Comment
trailingSpace = Spaces <$> regex "\\s+"

lineComment :: Parser Comment
lineComment = Comment <$> regex "--[^\\n]*"

-- This is not necessarily an efficient token parser, but it is straightforward.
-- Tokens can effectively be parsed with basic regular expressions since they are
-- not recursive. This is a place to aggressively optimize with a hand-written
-- parser if we need more overall parser performance.
token :: Parser Token
token = oneOf
  [ Equals <$ regex "="
  , Comma <$ regex ","
  , Colon <$ regex ":"
  , Semicolon <$ regex ";"
  , Lambda <$ regex "\\\\"
  , Arrow <$ regex "->"
  , ParenLeft <$ regex "\\("
  , ParenRight <$ regex "\\)"
  , SquareLeft <$ regex "\\["
  , SquareRight <$ regex "\\]"
  , identifierOrKeyword <$> regex "[_a-zA-Z][_a-zA-Z0-9]*"
  , int
  , string
  ] <|> fail "Unexpected input"
  where
  int = do
    raw <- regex "[0-9]+"
    case Int.fromString raw of
      Just n -> pure $ Int raw n
      Nothing -> fail "Invalid integer"

  string = do
    raw <- regex "\"(\\\\\"|[^\"])*\""
    pure $ String raw $ SCU.drop 1 $ SCU.dropRight 1 raw

  identifierOrKeyword = case _ of
    "let" -> Let
    "rec" -> Rec
    "type" -> Ty
    ident -> Identifier ident
