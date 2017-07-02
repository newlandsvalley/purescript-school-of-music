module Data.Euterpea.DSL.Parser where

import Prelude (($), (<$>), (<$), (<*>), (<*), (*>), (<<<))
import Control.Alt ((<|>))
import Data.String as S
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (fromString)
import Data.List (List(..), singleton)
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, try)
import Text.Parsing.StringParser.String (anyDigit, string, regex)
import Text.Parsing.StringParser.Combinators (between, choice, many, many1, manyTill, option, optionMaybe, sepBy, (<?>))
import Data.Euterpea.Music (Dur, Octave, Pitch(..), PitchClass(..), Primitive(..), NoteAttribute(..)) as Eut
import Data.Euterpea.Music1 as Eut1
import Data.Euterpea.Notes as Eutn

primitive :: Parser (Eut.Primitive Eut1.Note1)
primitive =  note1 <|> rest

note1 :: Parser (Eut.Primitive Eut1.Note1)
note1 =
  buildNote1 <$> string "Note" <*> duration <*> pitch <*> volume

rest :: ∀ a. Parser (Eut.Primitive a)
rest =
  Eut.Rest <$> (string "Rest" *> duration)

pitch :: Parser Eut.Pitch
pitch =
  Eut.Pitch <$> pitchClass <*> octave

duration :: Parser Eut.Dur
duration =
  choice
    [
      bn   -- brevis note
    , wn   -- whole note
    , hn   -- half note
    , qn   -- quarter note
    , sn   -- sixteenth note
    , tn   -- thirtysecond note etc.
    ]

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
  choice
    [
      css
    , cs
    , c
    , cf
    , cff  -- etc.
    ]

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
  digit <|> ten

volume :: Parser Int
volume = anyInt

digit :: Parser Int
digit = (fromMaybe 0 <<< fromString <<< S.singleton) <$> anyDigit

anyInt :: Parser Int
anyInt =
  (fromMaybe 0 <<< fromString) <$> regex "(0|[1-9][0-9]*)"

ten :: Parser Int
ten = 10 <$ string "10"


buildNote1 :: String -> Eut.Dur -> Eut.Pitch -> Int -> Eut.Primitive Eut1.Note1
buildNote1 _ dur pitch vol =
  let
    note1 = Eut1.Note1 pitch $ singleton (Eut.Volume vol)
  in
    Eut.Note dur note1
