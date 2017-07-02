module Data.Euterpea.DSL.Parser where

import Prelude (($), (<$>), (<$), (<*>), (<*), (*>), (<<<), (<>))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.String as S
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.List (List(..), singleton, (:))
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable, foldr)
import Data.Rational (fromInt)
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, try)
import Text.Parsing.StringParser.String (anyChar, anyDigit, char, string, regex, skipSpaces)
import Text.Parsing.StringParser.Combinators (choice, sepBy1, many1, (<?>))
import Data.Euterpea.Music (Dur, Octave, Pitch(..), PitchClass(..), Primitive(..), Music (..), NoteAttribute(..)) as Eut
import Data.Euterpea.Music1 as Eut1
import Data.Euterpea.Notes as Eutn
import Data.Euterpea.Transform as Eutt

polyphony :: Parser Eut1.Music1
polyphony =
  music <|> voices

voices :: Parser Eut1.Music1
voices =
  lineToMusic  <$> ((keyWord "Par") *> sepBy1 music (char ','))

{-}
control :: Parser (Eut1.Music1)
control =
  fix $
    (keyWord "Control") *> (keyWord "Instrument") *> (many1 anyChar)
-}


music :: Parser Eut1.Music1
music =
  choice
    [
      line
    , lines
    , chord
    ]

lines :: Parser Eut1.Music1
lines =
  lineToMusic <$> ((keyWord "Seq") *> sepBy1 line (char ','))

line :: Parser Eut1.Music1
line =
  lineToMusic <$> ((keyWord "Line") *> sepBy1 chordOrPrim (char ','))

chordOrPrim :: Parser (Eut1.Music1)
chordOrPrim = chord <|> prim

chord :: Parser (Eut1.Music1)
chord =
  Eutt.chord <$> ((keyWord "Chord") *> sepBy1 primNote1 (char ','))


prim :: Parser (Eut1.Music1)
prim =  Eut.Prim <$> (note1 <|> rest)

primNote1 :: Parser Eut1.Music1
primNote1 = Eut.Prim <$> note1

note1 :: Parser (Eut.Primitive Eut1.Note1)
note1 =
  buildNote1 <$> keyWord "Note" <*> duration <*> pitch <*> volume

rest :: ∀ a. Parser (Eut.Primitive a)
rest =
  Eut.Rest <$> (keyWord "Rest" *> duration)

pitch :: Parser Eut.Pitch
pitch =
  Eut.Pitch <$> pitchClass <*> octave

duration :: Parser Eut.Dur
duration =
  (choice
    [
      bn   -- brevis note
    , wn   -- whole note
    , hn   -- half note
    , qn   -- quarter note
    , sn   -- sixteenth note
    , tn   -- thirtysecond note etc.
    ]
   ) <* skipSpaces

bn :: Parser Eut.Dur
bn = Eutn.bn <$ string "bn"

wn :: Parser Eut.Dur
wn = Eutn.wn <$ string "wn"

hn :: Parser Eut.Dur
hn = Eutn.hn <$ string "hn"

qn :: Parser Eut.Dur
qn = Eutn.qn <$ string "qn"

sn :: Parser Eut.Dur
sn = Eutn.sn <$ string "sn"

tn :: Parser Eut.Dur
tn = Eutn.tn <$ string "tn"

pitchClass :: Parser Eut.PitchClass
pitchClass =
  (choice
    [
      css
    , cs
    , c
    , cf
    , cff  -- etc.
    ]
   ) <* skipSpaces


css :: Parser Eut.PitchClass
css = Eut.Css <$ string "Css"

cs :: Parser Eut.PitchClass
cs = Eut.Cs <$ string "Cs"

c :: Parser Eut.PitchClass
c = Eut.C <$ string "C"

cf :: Parser Eut.PitchClass
cf = Eut.Cf <$ string "Cf"

cff :: Parser Eut.PitchClass
cff = Eut.Cff <$ string "Cff"

octave :: Parser Eut.Octave
octave =
  (digit <|> ten) <* skipSpaces

volume :: Parser Int
volume = anyInt <* skipSpaces

anyInt :: Parser Int
anyInt =
  (fromMaybe 0 <<< fromString) <$> regex "(0|[1-9][0-9]*)"

anyString :: Parser String
anyString = fromCharList <$> many1 anyChar

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = S.fromCharArray <<< fromFoldable

keyWord :: String -> Parser String
keyWord target =
  (string target) <* skipSpaces

digit :: Parser Int
digit = (fromMaybe 0 <<< fromString <<< S.singleton) <$> anyDigit

ten :: Parser Int
ten = 10 <$ string "10"

buildNote1 :: String -> Eut.Dur -> Eut.Pitch -> Int -> Eut.Primitive Eut1.Note1
buildNote1 _ dur pitch vol =
  let
    note1 = Eut1.Note1 pitch $ singleton (Eut.Volume vol)
  in
    Eut.Note dur note1

-- | Euterpea requires that all lines end in a zero rest as a marker
-- | which is not how the parser will build naturally lines
-- | so make sure it's terminated properly
lineToMusic :: List Eut1.Music1 -> Eut1.Music1
lineToMusic ms =
  Eutt.line $ ms <> (singleton <<< Eut.Prim <<< Eut.Rest <<< fromInt) 0
