module Data.Euterpea.DSL.Parser
        ( PositionedParseError(..)
        , Binding
        , BindingMap
        , parse
        ) where

import Prelude (class Show, pure, show, ($), (<$>), (<$), (<*>), (<*), (*>), (<<<), (<>), (>>=), (-))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.String as S
import Data.Bifunctor (bimap)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.List (List, singleton)
import Data.List.NonEmpty as Nel
import Data.Map (Map, empty, fromFoldable, lookup, union) as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Array (fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable)
import Data.Rational (Rational, fromInt, rational)
import Text.Parsing.StringParser (Parser(..), ParseError(..), Pos, try, fail)
import Text.Parsing.StringParser.String (anyChar, anyDigit, char, string, regex, skipSpaces)
import Text.Parsing.StringParser.Combinators (choice, many1, optionMaybe, (<?>))
import Data.Euterpea.DSL.ParserExtensions (many1Nel, sepBy1Nel)
import Data.Euterpea.Music (Dur, Octave, Pitch(..), PitchClass(..), Primitive(..), Music (..),
       NoteAttribute(..), PhraseAttribute(..), Dynamic(..), Control(..)) as Eut
import Data.Euterpea.Music1 (Music1, Note1(..)) as Eut1
import Data.Euterpea.Instrument (InstrumentName, read)
import Data.Euterpea.Notes as Eutn
import Data.Euterpea.Transform as Eutt

-- | investigation of a Parser DSL that includes let bindings
-- | that define 'variables' holding music sequences which can be placed
-- | int the body of the score by using a 'call'
type Binding = Tuple String Eut1.Music1

type BindingMap =
  Map.Map String Eut1.Music1

-- | top level production
musicProcedure :: BindingMap -> Parser Eut1.Music1
musicProcedure bnds =
  complexMusic bnds <|> simpleMusic bnds
    <?> "music procedure"

-- | a complex score with let bindings to music 'functions' preceding the score
-- | which are added to (and take precedence over) bindings from the outer scope
complexMusic :: BindingMap -> Parser Eut1.Music1
complexMusic outerBnds =
  bindings >>= ( \bnds -> music $ Map.union bnds outerBnds)

-- | a simple score, with no functions
simpleMusic :: BindingMap -> Parser Eut1.Music1
simpleMusic bnds =
  music bnds

bindings :: Parser BindingMap
bindings =
  (fix \unit ->
    buildBindings <$>
      keyWord("Let") <*> (many1Nel bind) <*> keyWord("In")
  ) <?> "bindings"

bind :: Parser Binding
bind =
  (fix \unit ->
    buildBinding <$>
      identifier <*> (keyWord "=") <*> (music Map.empty)
  )  <?> "bind"

music :: BindingMap -> Parser Eut1.Music1
music bnds =
  fix \unit ->
    (choice
      [
        prim
      , lines bnds
      , line
      , chord
      , voices bnds
      , control bnds
      ]
    ) <?> "music"

voices :: BindingMap -> Parser Eut1.Music1
voices bnds =
  fix \unit ->
     -- buildVoices <$> (keyWord "Par") <*> many1Nel (musicProcedure bnds)
     buildVoices <$> (keyWord "Par") <*> many1Nel (procedureOrVariable bnds)

procedureOrVariable :: BindingMap -> Parser Eut1.Music1
procedureOrVariable bnds =
  (musicProcedure bnds) <|> (variable bnds)

-- | expand a variable as a Music tree
variable :: BindingMap -> Parser Eut1.Music1
variable bnds =
  identifier >>= (\name ->
    macroExpand name bnds)

-- | for the initial version of the DSL parser, we'll restrict control to just
-- | setting the instrument name
control :: BindingMap -> Parser Eut1.Music1
control bnds =
  fix \unit ->
    choice
      [
        instrumentName bnds
      , transpose bnds
      , tempo bnds
      , phraseAttributes bnds
      ]

instrumentName :: BindingMap -> Parser Eut1.Music1
instrumentName bnds =
  fix \unit -> buildInstrument <$> keyWord "Instrument" <*> instrument <*> (music bnds)

transpose :: BindingMap -> Parser Eut1.Music1
transpose bnds =
  fix \unit -> buildTranspose <$> keyWord "Transpose" <*> signedInt <*> (music bnds)

tempo :: BindingMap -> Parser Eut1.Music1
tempo bnds =
  fix \unit -> buildTempo <$> keyWord "Tempo" <*> (try fraction <|> vulgar) <*> (music bnds)

phraseAttributes :: BindingMap -> Parser Eut1.Music1
phraseAttributes bnds =
  fix \unit -> buildPhrase <$> keyWord "PhraseAtts" <*> (many1 phraseAttribute) <*> (music bnds)

phraseAttribute :: Parser Eut.PhraseAttribute
phraseAttribute =
  loudness
  -- more to follow

loudness :: Parser Eut.PhraseAttribute
loudness =
  fix \unit -> buildLoudness <$> keyWord "Loudness" <*> (try fraction <|> vulgar)

instrument :: Parser InstrumentName
instrument =
  (regex "[a-z][a-z0-9_]*" <* skipSpaces >>= (\name ->
    checkInstrument name)
  ) <?> "instrument"

lines :: BindingMap ->  Parser Eut1.Music1
lines bnds =
  Eutt.line1 <$> ((keyWord "Seq") *> many1Nel (lineOrVariable bnds))

lineOrVariable :: BindingMap -> Parser Eut1.Music1
lineOrVariable bnds =
  line <|> (variable bnds)

line :: Parser Eut1.Music1
line =
  Eutt.line1 <$> ((keyWord "Line") *> sepBy1Nel chordOrPrim separator)

chordOrPrim :: Parser Eut1.Music1
chordOrPrim = chord <|> prim

chord :: Parser (Eut1.Music1)
chord =
  Eutt.chord1 <$> ((keyWord "Chord") *> (keyWord "[") *> sepBy1Nel primNote1 separator <* (keyWord "]"))

prim :: Parser Eut1.Music1
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

signedInt :: Parser Int
signedInt =
  buildSignedInt <$> optionMaybe (string "-" <|> string "+") <*> int <* skipSpaces

anyInt :: Parser String
anyInt =
  regex "0|[1-9][0-9]*"

fraction :: Parser Rational
fraction =
  rational <$> int <* char '/' <*> int <* skipSpaces

-- | an integer presented as a vulgar fraction
vulgar :: Parser Rational
vulgar =
  fromInt <$> int <* skipSpaces

anyString :: Parser String
anyString = fromCharList <$> many1 anyChar

fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = S.fromCharArray <<< fromFoldable

keyWord :: String -> Parser String
keyWord target =
  (string target) <* skipSpaces

identifier :: Parser String
identifier = regex "[a-z][a-zA-Z0-9]*" <* skipSpaces

digit :: Parser Int
digit = (fromMaybe 0 <<< fromString <<< S.singleton) <$> anyDigit

ten :: Parser Int
ten = 10 <$ string "10"

buildBinding :: String -> String -> Eut1.Music1 -> Binding
buildBinding name _ mus =
  Tuple name mus

buildBindings :: String -> Nel.NonEmptyList Binding -> String -> BindingMap
buildBindings  _ bnds _ =
  Map.fromFoldable bnds

buildVoices :: String -> Nel.NonEmptyList Eut1.Music1 -> Eut1.Music1
buildVoices _ vs =
  Eutt.chord1 vs

buildNote1 :: String -> Eut.Dur -> Eut.Pitch -> Int -> Eut.Primitive Eut1.Note1
buildNote1 _ dur p vol =
  Eut.Note dur $ Eut1.Note1 p $ singleton (Eut.Volume vol)

buildInstrument :: String -> InstrumentName -> Eut1.Music1 -> Eut1.Music1
buildInstrument _ inst mus =
  Eut.Modify (Eut.Instrument inst) mus

buildTranspose :: String -> Int -> Eut1.Music1 -> Eut1.Music1
buildTranspose _ pitchShift mus =
  Eut.Modify (Eut.Transpose pitchShift) mus

buildTempo :: String -> Rational -> Eut1.Music1 -> Eut1.Music1
buildTempo _ tmp mus =
  Eut.Modify (Eut.Tempo tmp) mus

buildPhrase :: String -> List Eut.PhraseAttribute -> Eut1.Music1 -> Eut1.Music1
buildPhrase _ phraseAtts mus =
  Eut.Modify (Eut.Phrase phraseAtts) mus

buildLoudness :: String -> Rational -> Eut.PhraseAttribute
buildLoudness _ ld =
  Eut.Dyn (Eut.Loudness ld)

buildSignedInt :: Maybe String -> Int -> Int
buildSignedInt sign val =
  case sign of
    Just "-" -> (0 - val)
    _ -> val

-- | macro expand a 'function' name to give the function
-- | contents (which is Music).  Fail if the name is unknown
macroExpand :: String -> BindingMap -> Parser Eut1.Music1
macroExpand name bmap =
  case Map.lookup name bmap of
    Just m -> pure m
    _ -> fail $ "variable " <> name <> ": not found"

-- | check an instrument name against the Gleitzman names, fail if unknown
checkInstrument :: String -> Parser InstrumentName
checkInstrument name  =
  case read name of
    Just i -> pure i
    _ -> fail $ "instrument: " <> name <> " not known"

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
  case runParser1 (musicProcedure Map.empty) s of
    Right n ->
      Right n

    Left err ->
      Left err
