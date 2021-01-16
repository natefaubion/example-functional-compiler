module Main where

import Prelude

import Check.Elaborate (elaborateModule)
import Check.Monad (emptyState, runCheck)
import Data.Array (any)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Dodo (print, twoSpaces)
import Dodo.Ansi (ansiGraphics)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Eval.Normalize (normalizeModule)
import Node.Encoding as Encoding
import Node.FS.Aff as FS
import Node.Process (argv)
import Print (printError, printExpr, printModule, printParseError)
import Syntax.Parser (parseModule, runParser)
import Syntax.Token (Position(..))
import Text.Parsing.Parser (ParseError(..))
import Text.Parsing.Parser.Pos as Pos

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect argv
  let filePath = Array.take 1 (Array.drop 2 args)
  case filePath of
    [fp] -> do
      source <- FS.readTextFile Encoding.UTF8 fp
      case runParser source parseModule of
        Left (ParseError err (Pos.Position { column, line })) ->
          Console.log $ printDoc $ printParseError err (Position line column)
        Right mod -> do
          let Tuple res st = runCheck emptyState (elaborateModule mod)
          case res of
            Left errs ->
              for_ errs \err ->
                Console.log $ printDoc $ printError st err
            Right coreMod
              | any (eq "-n") args ->
                  Console.log $ printDoc $ printExpr $ normalizeModule coreMod
              | otherwise ->
                  Console.log $ printDoc $ printModule coreMod
    _ ->
      Console.error "File path needed"
  where
  printDoc =
    print ansiGraphics twoSpaces
