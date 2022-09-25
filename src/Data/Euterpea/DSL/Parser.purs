module Data.Euterpea.DSL.Parser
        ( PSoM
        , Binding
        , BindingMap
        , parse
        ) where

import Prelude (pure, ($), (<$>), (<$), (<*>), (<*), (*>), (<<<), (<>), (>>=), (-))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.String.CodePoints (codePointFromChar, singleton)
import Data.Int (fromString)
import Data.Either (Either(..))
import Data.List (singleton) as L
import Data.List.NonEmpty as Nel
import Data.Map (Map, empty, fromFoldable, lookup, union) as Map
import Data.Maybe (Maybe(Just), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Rational (Rational, fromInt, (%))
import StringParser (Parser, ParseError, runParser, try, fail)
import StringParser.CodePoints (anyDigit, char, string, regex, skipSpaces)
import StringParser.Combinators (between, choice, many1, sepBy1, optional, optionMaybe, (<?>))
import Data.Euterpea.Music (Dur, Octave, Pitch(..), PitchClass(..), Primitive(..), Music (..),
       NoteAttribute(..), PhraseAttribute(..), Control(..), Tempo(..), Articulation(..)) as Eut
import Data.Euterpea.Dynamics (Dynamic(..), StdLoudness, read) as Dyn
import Data.Euterpea.Music1 (Music1, Note1(..)) as Eut1
import Data.Euterpea.Notes as Eutn
import Data.Euterpea.Transform as Eutt
import Data.Midi.Instrument (InstrumentName, readGleitzman)

type PSoM =
  {  title :: String
  ,  music :: Eut1.Music1
  }

-- | investigation of a Parser DSL that includes let bindings
-- | that define 'variables' holding music sequences which can be placed
-- | int the body of the score by using a 'call'
type Binding = Tuple String Eut1.Music1

type BindingMap =
  Map.Map String Eut1.Music1

defaultVolume :: Int
defaultVolume = 100

psom ::Parser PSoM
psom =
  buildPSoM <$>
    quotedString <*> (try $ optional comment *> musicProcedure Map.empty)

-- | a music procedure (the top level music production)
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
  (fix \_ ->
    buildBindings <$>
      keyWord("Let") <*> (many1 bind) <*> keyWord("In")
  ) <?> "bindings"

bind :: Parser Binding
bind =
  (fix \_ ->
    buildBinding <$>
      identifier <*> (keyWord "=") <*> (music Map.empty)
  )  <?> "bind"

music :: BindingMap -> Parser Eut1.Music1
music bnds =
  fix \_ ->
    (choice
      [
        prim
      , lines bnds
      , line bnds
      , chord
      , voices bnds
      , control bnds
      ]
    ) <?> "music"

bracketedMusic :: BindingMap -> Parser Eut1.Music1
bracketedMusic bnds =
  between (string "(" <* skipSpaces) (string ")" <* skipSpaces) (music bnds)

voices :: BindingMap -> Parser Eut1.Music1
voices bnds =
  fix \_ ->
    Eutt.chord1 <$> ((keyWord "Par") *> (try $ optional comment) *> many1 (procedureOrVariable bnds))

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
  fix \_ ->
    Eut.Modify <$>
      (choice
        [
          instrumentName
        , transpose
        , tempo
        , phraseAttributes
        ]
      )  <*> (bracketedMusic bnds)

instrumentName :: Parser Eut.Control
instrumentName =
  Eut.Instrument <$> (keyWord "Instrument" *> instrument)

transpose :: Parser Eut.Control
transpose =
  Eut.Transpose <$> (keyWord "Transpose" *> signedInt)

tempo :: Parser Eut.Control
tempo =
  Eut.Tempo <$> (keyWord "Tempo" *> (try fraction <|> vulgar))

phraseAttributes :: Parser Eut.Control
phraseAttributes =
  (Eut.Phrase  <<< Nel.toList)  <$> (keyWord "PhraseAtts" *> (many1 phraseAttribute))
    <?> "phrase attributes"

phraseAttribute :: Parser Eut.PhraseAttribute
phraseAttribute =
  (choice
     [ loudness
     , stdLoudness
     , crescendo
     , diminuendo
     , accent
     , ritardando
     , accelerando
     , staccato
     , legato
     , slurred
    ]
  ) <?> "phrase attribute"

loudness :: Parser Eut.PhraseAttribute
loudness =
  Eut.Dyn <$> Dyn.Loudness <$> (keyWord "Loudness" *> vulgar)

stdLoudness :: Parser Eut.PhraseAttribute
stdLoudness =
  Eut.Dyn <$> Dyn.StdLoudness <$> (keyWord "StdLoudness" *> dynamicMarking)

crescendo :: Parser Eut.PhraseAttribute
crescendo =
  Eut.Dyn <$> Dyn.Crescendo <$> (keyWord "Crescendo" *> (try fraction <|> vulgar))

diminuendo :: Parser Eut.PhraseAttribute
diminuendo =
  Eut.Dyn <$> Dyn.Diminuendo <$> (keyWord "Diminuendo" *> (try fraction <|> vulgar))

accent :: Parser Eut.PhraseAttribute
accent =
  Eut.Dyn <$> Dyn.Accent <$> (keyWord "Accent" *> (try fraction <|> vulgar))

ritardando :: Parser Eut.PhraseAttribute
ritardando =
  Eut.Tmp <$> Eut.Ritardando <$> (keyWord "Ritardando" *> (try fraction <|> vulgar))

accelerando :: Parser Eut.PhraseAttribute
accelerando =
  Eut.Tmp <$> Eut.Accelerando <$> (keyWord "Accelerando" *> (try fraction <|> vulgar))

staccato :: Parser Eut.PhraseAttribute
staccato =
  Eut.Art <$> Eut.Staccato <$> (keyWord "Staccato" *> (try fraction <|> vulgar))

legato :: Parser Eut.PhraseAttribute
legato =
  Eut.Art <$> Eut.Legato <$> (keyWord "Legato" *> (try fraction <|> vulgar))

slurred :: Parser Eut.PhraseAttribute
slurred =
  Eut.Art <$> Eut.Slurred <$> (keyWord "Slurred" *> (try fraction <|> vulgar))

instrument :: Parser InstrumentName
instrument =
  (regex "[a-z][a-z0-9_]*" <* skipSpaces >>= (\name ->
    checkInstrument name)
  ) <?> "instrument"

lines :: BindingMap ->  Parser Eut1.Music1
lines bnds =
  Eutt.line1 <$> ((keyWord "Seq") *> (try $ optional comment) *> many1 (linesOptions bnds))

linesOptions :: BindingMap -> Parser Eut1.Music1
linesOptions bnds =
  (line bnds) <|> (variable bnds) <|> (control bnds)

line ::  BindingMap -> Parser Eut1.Music1
line bnds =
  Eutt.line1 <$> ((keyWord "Line") *> sepBy1 (lineOptions bnds) separator)

lineOptions :: BindingMap -> Parser Eut1.Music1
lineOptions bnds =
  chord <|> prim <|> (control bnds)

chord :: Parser (Eut1.Music1)
chord =
  Eutt.chord1 <$> ((keyWord "Chord") *> (keyWord "[") *> sepBy1 primNote1 separator <* (keyWord "]"))

prim :: Parser Eut1.Music1
prim =  Eut.Prim <$> (note1 <|> rest)

primNote1 :: Parser Eut1.Music1
primNote1 = Eut.Prim <$> note1

note1 :: Parser (Eut.Primitive Eut1.Note1)
note1 =
  buildNote1 <$> keyWord "Note" <*> duration <*> pitch

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

{-
volume :: Parser Int
volume = int <* skipSpaces
-}

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
  (%) <$> int <* char '/' <*> int <* skipSpaces

-- | an integer presented as a vulgar fraction
vulgar :: Parser Rational
vulgar =
  fromInt <$> int <* skipSpaces

dynamicMarking :: Parser Dyn.StdLoudness
dynamicMarking =
  (regex "[A-Z]+" <* skipSpaces >>= (\name ->
    checkDynamicMarking name)
  ) <?> "dynamic marking"

keyWord :: String -> Parser String
keyWord target =
  (string target) <* skipSpaces

identifier :: Parser String
identifier = regex "[a-z][a-zA-Z0-9]*" <* skipSpaces

commentChars :: Parser String
commentChars = regex "[ -,:-@a-zA-Z0-9/./-]+"

quotedString :: Parser String
quotedString =
  string "\""
     *> regex "(\\\\\"|[^\"\n])*"
     <* (string "\"" <* skipSpaces)
     <?> "quoted string"

-- | single line comment
comment :: Parser String
comment =
  string "--" *> commentChars <* endOfLine <* skipSpaces
    <?> "comment"

-- | support both from file and in-browser text area
endOfLine :: Parser String
endOfLine =
  (string "\r" <|> string "\n")
    <?> "endOfLine"

digit :: Parser Int
digit = (fromMaybe 0 <<< fromString <<< singleton <<< codePointFromChar) <$> anyDigit

ten :: Parser Int
ten = 10 <$ string "10"

buildBinding :: String -> String -> Eut1.Music1 -> Binding
buildBinding name _ mus =
  Tuple name mus

buildBindings :: String -> Nel.NonEmptyList Binding -> String -> BindingMap
buildBindings  _ bnds _ =
  Map.fromFoldable bnds

buildNote1 :: String -> Eut.Dur -> Eut.Pitch -> Eut.Primitive Eut1.Note1
buildNote1 _ dur p  =
  Eut.Note dur $ Eut1.Note1 p $ L.singleton (Eut.Volume defaultVolume)

buildSignedInt :: Maybe String -> Int -> Int
buildSignedInt sign val =
  case sign of
    Just "-" -> (0 - val)
    _ -> val

buildPSoM :: String -> Eut1.Music1 -> PSoM
buildPSoM title mus =
  { title : title
  , music : mus
  }

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
  case readGleitzman name of
    Just i -> pure i
    _ -> fail $ "instrument: " <> name <> " not known"

-- | check a dynamic marking against those allowed in the ADT
checkDynamicMarking :: String -> Parser Dyn.StdLoudness
checkDynamicMarking name  =
  case Dyn.read name of
    Just i -> pure i
    _ -> fail $ "dynamic marking: " <> name <> " not known"

-- | Entry point - Parse a Euterpea DSL score.
parse :: String -> Either ParseError PSoM
parse s =
  case runParser psom s of
    Right n ->
      Right n

    Left err ->
      Left err
