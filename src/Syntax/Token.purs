module Syntax.Token where

import Prelude

data Token
  = Let
  | Rec
  | Ty
  | Equals
  | Comma
  | Colon
  | Semicolon
  | Lambda
  | Arrow
  | ParenLeft
  | ParenRight
  | SquareLeft
  | SquareRight
  | Identifier String
  | Int String Int
  | String String String

derive instance eqToken :: Eq Token

data Position = Position Int Int

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position

data Range = Range Position Position

derive instance eqRange :: Eq Range
derive instance ordRange :: Ord Range

instance semigroupRange :: Semigroup Range where
  append (Range start1 end1) (Range start2 end2)
    | start1 < start2 = Range start1 end2
    | otherwise = Range start2 end2

data Comment
  = Comment String
  | Spaces String

derive instance eqComment :: Eq Comment

data PositionedToken
  = PositionedToken (Array Comment) Range Token (Array Comment)

token :: PositionedToken -> Token
token (PositionedToken _ _ tok _) = tok

range1 :: PositionedToken -> Range
range1 (PositionedToken _ range _ _) = range

range2 :: PositionedToken -> PositionedToken -> Range
range2 (PositionedToken _ start _ _) (PositionedToken _ end _ _) = start <> end

emptyRange :: Range
emptyRange = Range (Position 0 0) (Position 0 0)

printTokenName :: Token -> String
printTokenName = case _ of
  Let -> "'let'"
  Rec -> "'rec'"
  Ty -> "'type'"
  Equals -> "'='"
  Comma -> "','"
  Colon -> "':'"
  Semicolon -> "';'"
  Lambda -> "'\\'"
  Arrow -> "'->'"
  ParenLeft -> "'('"
  ParenRight -> "')'"
  SquareLeft -> "'['"
  SquareRight -> "']'"
  Identifier _ -> "identifier"
  Int _ _ -> "int literal"
  String _ _ -> "string literal"
