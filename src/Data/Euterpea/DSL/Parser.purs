module Data.Euterpea.DSL.Parser
        ( PositionedParseError(..)
        , parse
        ) where

import Prelude (class Show, show, ($), (<$>), (<$), (<*>), (<*), (*>), (<<<), (<>))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.String as S
import Data.Maybe (fromMaybe)
import Data.Bifunctor (bimap)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.List (singleton)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos)
import Text.Parsing.StringParser.String (anyChar, anyDigit, char, string, regex, skipSpaces, eof)
import Text.Parsing.StringParser.Combinators (choice, many1, (<?>))
import Data.Euterpea.DSL.ParserExtensins (many1Nel, sepBy1Nel)
import Data.Euterpea.Music (Dur, Octave, Pitch(..), PitchClass(..), Primitive(..), Music (..), NoteAttribute(..), Control(..), InstrumentName(..)) as Eut
import Data.Euterpea.Music1 (Music1, Note1(..)) as Eut1
import Data.Euterpea.Notes as Eutn
import Data.Euterpea.Transform as Eutt

polyphony :: Parser Eut1.Music1
polyphony =
  ( music <|> voices ) <* eof

voices :: Parser Eut1.Music1
voices =
  Eutt.chord1  <$> ((keyWord "Par") *> many1Nel music)

music :: Parser Eut1.Music1
music =
  fix \unit ->
    (choice
      [
        prim
      , lines
      , line
      , repeat
      , chord
      , control
      ]
    ) <?> "music"

-- | for the initial version of the DSL parser, we'll restrict control to just
-- | setting the instrument name.
-- | we can then extend control once we have basic polyphony working
control :: Parser (Eut1.Music1)
control =
  fix \unit -> instrumentName

instrumentName :: Parser (Eut1.Music1)
instrumentName =
  fix \unit -> buildInstrument <$> keyWord "Instrument" <*> instrument <*> music

-- | for the time being we'll restrict ourselves to the common MIDI instruments (plus cello and violin)
-- | that are defined as defaults here: https://github.com/Euterpea/Euterpea2/blob/master/Euterpea/IO/MIDI/ToMidi.lhs
instrument :: Parser Eut.InstrumentName
instrument =
  (choice
    [
      piano
    , marimba
    , vibraphone
    , bass
    , flute
    , tenorSax
    , steelGuitar
    , violin
    , viola
    , cello
    , stringEnsemble
    ]
  ) <* skipSpaces
    <?> "instrument"

piano :: Parser Eut.InstrumentName
piano = Eut.AcousticGrandPiano <$ keyWord "piano"

marimba :: Parser Eut.InstrumentName
marimba = Eut.Marimba <$ keyWord "marimba"

vibraphone :: Parser Eut.InstrumentName
vibraphone = Eut.Vibraphone <$ keyWord "vibraphone"

bass :: Parser Eut.InstrumentName
bass = Eut.AcousticBass <$ keyWord "bass"

flute :: Parser Eut.InstrumentName
flute = Eut.Flute <$ keyWord "flute"

tenorSax :: Parser Eut.InstrumentName
tenorSax = Eut.TenorSax <$ keyWord "tenor sax"

steelGuitar :: Parser Eut.InstrumentName
steelGuitar = Eut.AcousticGuitarSteel <$ keyWord "steel guitar"

violin :: Parser Eut.InstrumentName
violin = Eut.Violin <$ keyWord "violin"

viola :: Parser Eut.InstrumentName
viola = Eut.Viola <$ keyWord "viola"

cello :: Parser Eut.InstrumentName
cello = Eut.Cello <$ keyWord "cello"

stringEnsemble :: Parser Eut.InstrumentName
stringEnsemble = Eut.StringEnsemble1 <$ keyWord "string ensemble"

repeat :: Parser Eut1.Music1
repeat =
  buildRepeat <$> ((keyWord "Repeat") *> (keyWord "(") *>  lines  <* (keyWord ")"))

lines :: Parser Eut1.Music1
lines =
  -- Eutt.line1 <$> ((keyWord "Seq") *> many1TillNel line endSeq)
  Eutt.line1 <$> ((keyWord "Seq") *> many1Nel line)

line :: Parser Eut1.Music1
line =
  Eutt.line1 <$> ((keyWord "Line") *> sepBy1Nel chordOrPrim separator)

chordOrPrim :: Parser (Eut1.Music1)
chordOrPrim = chord <|> prim

chord :: Parser (Eut1.Music1)
chord =
  Eutt.chord1 <$> ((keyWord "Chord") *> (keyWord "[") *> sepBy1Nel primNote1 separator <* (keyWord "]"))

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
    , en   -- eighth note
    , sn   -- sixteenth note
    , tn   -- thirtysecond note etc.
    , sfn  -- sixtyfourth note etc.
    , dwn  -- dotted whole note
    , dhn  -- dotted half note
    , dqn  -- dotted quarter note
    , den  -- dotted eighth note
    , dsn  -- dotted sixteenth note
    , ddhn -- double-dotted half note
    , ddqn -- double-dotted quarter note
    , dden -- double-dotted eighth note
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

en :: Parser Eut.Dur
en = Eutn.en <$ string "en"

sn :: Parser Eut.Dur
sn = Eutn.sn <$ string "sn"

tn :: Parser Eut.Dur
tn = Eutn.tn <$ string "tn"

sfn :: Parser Eut.Dur
sfn = Eutn.sfn <$ string "sfn"

dwn :: Parser Eut.Dur
dwn = Eutn.dwn <$ string "dwn"

dhn :: Parser Eut.Dur
dhn = Eutn.dhn <$ string "dhn"

dqn :: Parser Eut.Dur
dqn = Eutn.dqn <$ string "dqn"

den :: Parser Eut.Dur
den = Eutn.den <$ string "den"

dsn :: Parser Eut.Dur
dsn = Eutn.dsn <$ string "dsn"

ddhn :: Parser Eut.Dur
ddhn = Eutn.ddhn <$ string "ddhn"

ddqn :: Parser Eut.Dur
ddqn = Eutn.ddqn <$ string "ddqn"

dden :: Parser Eut.Dur
dden = Eutn.dden <$ string "dden"


pitchClass :: Parser Eut.PitchClass
pitchClass =
  (choice
    [
      css
    , cs
    , cff
    , cf
    , c
    , dss
    , ds
    , dff
    , df
    , d
    , ess
    , es
    , eff
    , ef
    , e
    , fss
    , fs
    , fff
    , ff
    , f
    , gss
    , gs
    , gff
    , gf
    , g
    , ass
    , as
    , aff
    , af
    , a
    , bss
    , bs
    , bff
    , bf
    , b
    ]
   ) <* skipSpaces
     <?> "pitch class"

-- |  This boilerplate is tedious, but in the absence of any generic Read
-- |  behaviour, is probably as straightforward as it gets at the moment

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

dss :: Parser Eut.PitchClass
dss = Eut.Dss <$ string "Dss"

ds :: Parser Eut.PitchClass
ds = Eut.Ds <$ string "Ds"

d :: Parser Eut.PitchClass
d = Eut.D <$ string "D"

df :: Parser Eut.PitchClass
df = Eut.Df <$ string "Df"

dff :: Parser Eut.PitchClass
dff = Eut.Dff <$ string "Dff"

ess :: Parser Eut.PitchClass
ess = Eut.Ess <$ string "Ess"

es :: Parser Eut.PitchClass
es = Eut.Es <$ string "Es"

e :: Parser Eut.PitchClass
e = Eut.E <$ string "E"

ef :: Parser Eut.PitchClass
ef = Eut.Ef <$ string "Ef"

eff :: Parser Eut.PitchClass
eff = Eut.Eff <$ string "Eff"

fss :: Parser Eut.PitchClass
fss = Eut.Fss <$ string "Fss"

fs :: Parser Eut.PitchClass
fs = Eut.Fs <$ string "Fs"

f :: Parser Eut.PitchClass
f = Eut.F <$ string "F"

ff :: Parser Eut.PitchClass
ff = Eut.Ff <$ string "Ff"

fff :: Parser Eut.PitchClass
fff = Eut.Fff <$ string "Fff"

gss :: Parser Eut.PitchClass
gss = Eut.Gss <$ string "Gss"

gs :: Parser Eut.PitchClass
gs = Eut.Gs <$ string "Gs"

g :: Parser Eut.PitchClass
g = Eut.G <$ string "G"

gf :: Parser Eut.PitchClass
gf = Eut.Gf <$ string "Gf"

gff :: Parser Eut.PitchClass
gff = Eut.Gff <$ string "Gff"

ass :: Parser Eut.PitchClass
ass = Eut.Ass <$ string "Ass"

as :: Parser Eut.PitchClass
as = Eut.As <$ string "As"

a :: Parser Eut.PitchClass
a = Eut.A <$ string "A"

af :: Parser Eut.PitchClass
af = Eut.Af <$ string "Af"

aff :: Parser Eut.PitchClass
aff = Eut.Aff <$ string "Aff"

bss :: Parser Eut.PitchClass
bss = Eut.Bss <$ string "Bss"

bs :: Parser Eut.PitchClass
bs = Eut.Bs <$ string "Bs"

b :: Parser Eut.PitchClass
b = Eut.B <$ string "B"

bf :: Parser Eut.PitchClass
bf = Eut.Bf <$ string "Bf"

bff :: Parser Eut.PitchClass
bff = Eut.Bff <$ string "Bff"



octave :: Parser Eut.Octave
octave =
  (digit <|> ten) <* skipSpaces

volume :: Parser Int
volume = int <* skipSpaces

separator :: Parser Char
separator =
  (char ',') <* skipSpaces

-- | Parse a positive integer (with no sign).
int :: Parser Int
int =
  (fromMaybe 0 <<< fromString) <$> anyInt
    <?> "expected a positive integer"

anyInt :: Parser String
anyInt =
  regex "0|[1-9][0-9]*"

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

buildRepeat :: Eut1.Music1 -> Eut1.Music1
buildRepeat l =
  Eut.Seq l l

buildNote1 :: String -> Eut.Dur -> Eut.Pitch -> Int -> Eut.Primitive Eut1.Note1
buildNote1 _ dur p vol =
  Eut.Note dur $ Eut1.Note1 p $ singleton (Eut.Volume vol)

buildInstrument :: String -> Eut.InstrumentName -> Eut1.Music1 -> Eut1.Music1
buildInstrument _ inst mus =
  Eut.Modify (Eut.Instrument inst) mus

-- | a parse error and its accompanying position in the text
newtype PositionedParseError = PositionedParseError
  { pos :: Int
  , error :: String
  }

instance showKeyPositionedParseError :: Show PositionedParseError where
  show (PositionedParseError err) = err.error <> " at position " <> show err.pos

-- | Run a parser for an input string, returning either a positioned error or a result.
runParser1 :: forall a. Parser a -> String -> Either PositionedParseError a
runParser1 (Parser p) s =
  let
    formatErr :: { pos :: Pos, error :: ParseError } -> PositionedParseError
    formatErr { pos : pos, error : ParseError err } =
      PositionedParseError { pos : pos, error : err}
  in
    bimap formatErr _.result (p { str: s, pos: 0 })

-- | Entry point - Parse a Euterpea DSL score.
parse :: String -> Either PositionedParseError Eut1.Music1
parse s =
  case runParser1 polyphony s of
    Right n ->
      Right n

    Left err ->
      Left err
