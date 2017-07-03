module Data.Euterpea.DSL.ParserExtensins
        ( many1Nel
        , many1TillNel
        , sepBy1Nel
        ) where

import Prelude ((<$>))
import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(Just))
import Data.List (List(..))
import Data.List.NonEmpty as Nel
import Text.Parsing.StringParser (Parser)
import Text.Parsing.StringParser.Combinators (sepBy1, many1, many1Till)

-- | a version of many1 which returns a Parser for a NonEmptyList
-- | rather than for a List
-- | see issue https://github.com/purescript-contrib/purescript-string-parsers/issues/36
many1Nel :: ∀ a. Parser a -> Parser (Nel.NonEmptyList a)
many1Nel p = unsafePartial (go p)
  where
    go :: forall a1 . Partial => Parser a1 -> Parser (Nel.NonEmptyList a1)
    go p' =
      lToNel <$> many1 p'

-- | ditto for many1Till
many1TillNel :: ∀ a end. Parser a -> Parser end -> Parser (Nel.NonEmptyList a)
many1TillNel p end = unsafePartial (go p end)
  where
    go :: forall a1 end1. Partial => Parser a1 -> Parser end1 -> Parser (Nel.NonEmptyList a1)
    go p' end' =
      lToNel <$> many1Till p' end'

-- | ditto for sepBy1
sepBy1Nel :: ∀ a sep. Parser a -> Parser sep -> Parser (Nel.NonEmptyList a)
sepBy1Nel p sep = unsafePartial (go p sep)
  where
    go :: forall a1 sep1. Partial => Parser a1 -> Parser sep1 -> Parser (Nel.NonEmptyList a1)
    go p' sep' =
      lToNel <$> sepBy1 p' sep'

lToNel :: ∀ a. Partial => List a -> Nel.NonEmptyList a
lToNel l =
  case (Nel.fromList l) of
    Just nel -> nel
