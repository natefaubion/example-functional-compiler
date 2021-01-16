module Print.Precedence where

import Prelude

import Dodo (Doc, alignCurrentColumn, enclose, text)

data Assoc = AssocLeft | AssocRight | NoAssoc

data Fixity p = Fixity Assoc p | NoFixity

data Prec p a = Prec (Fixity p) a

unPrec :: forall p a. Prec p a -> a
unPrec (Prec _ a) = a

binaryop
  :: forall p a
   . Ord p
  => Assoc
  -> p
  -> (Doc a -> Doc a -> Doc a)
  -> Prec p (Doc a)
  -> Prec p (Doc a)
  -> Prec p (Doc a)
binaryop assoc1 p1 op a b =
  Prec (Fixity assoc1 p1) $ op lhs rhs
  where
  lhs = parensLeft assoc1 p1 a
  rhs = parensRight assoc1 p1 b

parens :: forall a. Doc a -> Doc a
parens = enclose (text "(") (text ")") <<< alignCurrentColumn

parensLeft :: forall p a. Ord p => Assoc -> p -> Prec p (Doc a) -> Doc a
parensLeft assoc1 p1 (Prec f doc) = case assoc1, f of
  AssocLeft, Fixity AssocLeft p2
    | p2 < p1 -> parens doc
    | otherwise -> doc
  _, Fixity _ p2
    | p2 <= p1 -> parens doc
  _, _ -> doc

parensRight :: forall p a. Ord p => Assoc -> p -> Prec p (Doc a) -> Doc a
parensRight assoc1 p1 (Prec f doc) = case assoc1, f of
  AssocRight, Fixity AssocRight p2
    | p2 < p1 -> parens doc
    | otherwise -> doc
  _, Fixity _ p2
    | p2 <= p1 -> parens doc
  _, _ -> doc

term :: forall p a. a -> Prec p a
term = Prec NoFixity

prec :: forall p a. p -> a -> Prec p a
prec = Prec <<< Fixity NoAssoc
