module Data.Euterpea.DSL.ParserExtensins
        ( many1Nel
        , sepBy1Nel
        ) where

import Prelude ((<$>))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(Just))
import Data.List (List(..))
import Data.List.NonEmpty as Nel
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (sepBy1, many1)

-- | a version of many1 which returns a Parser for a NonEmptyList
-- | rather than for a List
-- | see issue https://github.com/purescript-contrib/purescript-string-parsers/issues/36
many1Nel :: forall a. Parser a -> Parser (Nel.NonEmptyList a)
many1Nel p = unsafePartial (go p)
  where
    go :: forall a1 . Partial => Parser a1 -> Parser (Nel.NonEmptyList a1)
    go p' =
      lToNel <$> many1 p'

-- | ditto for sepBy1
sepBy1Nel :: forall a sep. Parser a -> Parser sep -> Parser (Nel.NonEmptyList a)
sepBy1Nel p sep = unsafePartial (go p sep)
  where
    go :: forall a1 sep1. Partial => Parser a1 -> Parser sep1 -> Parser (Nel.NonEmptyList a1)
    go p' sep' =
      lToNel <$> sepBy1 p' sep'

lToNel :: ∀ a. Partial => List a -> Nel.NonEmptyList a
lToNel l =
  case (Nel.fromList l) of
    Just nel -> nel
