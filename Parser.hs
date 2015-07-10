{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module Parser (Instruction(..), 
                Token(..), 
                EntryPoint(..),
                parseEverything) where 

import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
-- import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell, lift)
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, toLower, ord)
import Data.List (genericLength)
import Data.String (IsString, fromString)
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
import Data.Map.Strict as Map (Map, singleton)

import Debug.Trace (trace)

import Instructions


traceM :: (Monad m) => String -> m ()
traceM str = trace str $ return ()

testfile :: FilePath
testfile = "programs/testfile6"

main :: IO ()
main = do
    putStrLn $ "# object module for file: " ++ testfile
    c <- readFile testfile
    let result :: Either ParseError (Integer, EntryPoint, [Token], Labels)
        result = parseEverything testfile c
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r
    putStrLn "okay"

parseEverything :: SourceName 
        -> String 
        -> Either ParseError (Integer, EntryPoint, [Token], Labels)
parseEverything name str = do
    let eith :: Either ParseError (Integer, EntryPoint, [Token])
        (eith, labels) = parseEverything' name str
    case eith of
        Left err -> Left err
        Right (i, m, xs) -> return (i, m, xs, labels)

parseEverything' :: SourceName -> String 
        -> (Either ParseError (Integer, EntryPoint, [Token]), Labels)
parseEverything' = (runWriter .) . runParserT instructions (0, "", "")


{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
MyState is the state. 
Writer Labels is the transformed monad.
-}
type MyParser a = ParsecT String MyState (Writer Labels) a

{-
i use the Integer to count words, both so i can output the text length and 
    so i can track where the labels are.
the EntryPoint is the name of the entry point after it has been found. 
    it is the empty string if no entry point is found.
the last String is the name of the current non-local label.
    it does not need to be known outside of the parsing stage.
-}
type MyState = (Integer, EntryPoint, String)

{-
Writer Labels is the transformed monad. when a label is parsed,
    a String and Integer pair is added to the Map using its Monoid instance.  
    the String in this case is the name of the label; 
        the Integer is its location. 
-}
type Labels = Map.Map String Integer

{-
Token is an algebraic data type
a Lit can be a number like 65 or a letter like 'a
an Op is any of the opcodes
a Label is a label
-}
data Token = Lit Integer
            | Op Instruction
            | Label String Integer -- the int is the current location counter
            | NewLabel String
            | DS Integer
            | DW [Integer]
            | EQU String Integer 
            | Entry EntryPoint
            | Extern [String]
            | Public String -- FIXME: [String], just like Extern i think
            deriving (Show)

newtype EntryPoint = EntryPoint String deriving (Eq)

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

--------------------------------- parser begins here

instructions :: MyParser (Integer, EntryPoint, [Token])
instructions = do 
    res <- join <$> many (try instruction) `sepBy` skipJunk
    skipMany skipJunk *> eof
    (counter, entry, _) <- getState
    return (counter, entry, res)

instruction :: MyParser Token
instruction = skipMany skipJunk *>
    fmap Lit intOrChar <* loc (+1)
    <|> try asmEQU 
    <|> try asmDS 
    <|> try asmDW 
    <|> try asmEntry 
    <|> try asmExtern 
    <|> try asmPublic
    <|> try newLabel 
    <|> try opcode <* loc (+1)
    <|> label <* loc (+1)


newLabel :: MyParser Token
newLabel = NewLabel <$> (newGlobalLabel <|> newLocalLabel) <* skipMany skipJunk
        <?> "label"

label :: MyParser Token
-- label = Label <$> (localLabel <|> globalLabel) <* skipMany skipJunk
label = do 
    str <- labelName
    int <- getLoc
    return $ Label str int

labelName :: MyParser String
labelName = (localLabel <|> globalLabel) <* skipMany skipJunk <?> "label"

newLocalLabel :: MyParser String
newLocalLabel = do
    name <- localLabel <* char ':'
--     traceM $ ">>> name is: " ++ name
    pos <- getLoc -- get the current position in the count
--     lift $ tell (Map.singleton name pos) -- add it to the map of labels
    addToLabels name pos -- add it to the map of labels
    return name

newGlobalLabel :: MyParser String
newGlobalLabel = do
    name <- globalLabel <* char ':' 
    setLabelPrefix name -- new current scope
    pos <- getLoc -- get the current position in the count
--     lift $ tell (Map.singleton name pos) -- add it to the map of labels
    addToLabels name pos -- add it to the map of labels
    return name

localLabel :: MyParser String
localLabel = do
    char '@'
    tailer <- many labelChar
    header <- getLabelPrefix
--     traceM $ header is: " ++ header
    return (header ++ '-' : tailer) -- join them with '-' to prevent clashes

globalLabel :: MyParser String
globalLabel = do
    header <- letter
    tailer <- many labelChar
    return (header:tailer)


opcode :: MyParser Token
opcode = do 
    -- all the opcodes are in capitals
    uppers <- map toUpper <$> many1 letter <?> "opcode" 
     -- try to read it as an Instruction
    case readMaybe uppers `mplus` readOpSynonym uppers of --alternatively, <|>
        Just x -> return (Op x)
        Nothing -> fail "can't parse as opcode"
    where
        readOpSynonym :: String -> (Maybe Instruction)
        readOpSynonym "INDIR" = Just PUSHS
        readOpSynonym "BT"    = Just BNE
        readOpSynonym "BF"    = Just BEQ
        readOpSynonym "POPPC" = Just RETURN
        readOpSynonym _       = Nothing

intOrChar :: MyParser Integer
intOrChar = int <|> asmChar where 
    int :: MyParser Integer
    int = read <$> (many1 digit)
    asmChar :: MyParser Integer
    asmChar = toInteger . ord <$> (char '\'' *> anyChar)

skipJunk :: MyParser ()
skipJunk = skipSpaces <|> skipComment
-- skipJunk = spaces <|> skipComment

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

skipComment :: MyParser ()
skipComment = spaces *> char ';' *> skipToEOL
-- skipComment = char ';' *> skipToEOL

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space
-- skipToEOL = many (noneOf "\n") *> return ()
-- skipToEOL = skipMany (noneOf "\n") *> skipMany1 space -- skip past the '\n'

-- caseInsensitiveChar :: Char -> GenParser Char state Char
caseInsensitiveChar :: Char -> MyParser Char
caseInsensitiveChar = (\c -> (char (toLower c) <|> char (toUpper c)) *> pure c)

-- caseInsensitiveString :: String -> GenParser Char state String
caseInsensitiveString :: String -> MyParser String
caseInsensitiveString = sequence . map caseInsensitiveChar

{- from the assembler documentation:
A label is any sequence of letters, digits, dots, and underscores, beginning 
with a letter or at-sign '@' character, but excluding reserved words. A label 
is also called an identifier. Labels longer than 30 characters in length are 
silently truncated to 30 characters. 
-}
labelChar :: MyParser Char
labelChar = letter <|> digit <|> oneOf "._"

-- loc f applies f to the location counter
loc :: (Integer -> Integer) -> MyParser ()
loc = modifyState . (\f (i, s, labl) -> (f i, s, labl))

-- returns the current location counter
getLoc :: MyParser Integer
getLoc = getState >>= \(i, _, _) -> return i

-- setEntry "main" makes the entry point "main", provided it isn't already set
setEntry :: EntryPoint -> MyParser ()
setEntry = modifyState . setEntry' where 
    -- pattern match the empty string
    setEntry' name (i, "", labl) = (i, name, labl) 
    setEntry' _ _ = error "multiple entries?" 

addToLabels :: String -> Integer -> MyParser ()
addToLabels name val = lift $ tell (Map.singleton name val)

setLabelPrefix :: String -> MyParser ()
setLabelPrefix = modifyState . setLabelPrefix' where
    setLabelPrefix' str (i, ent, _) = (i, ent, str)

getLabelPrefix :: MyParser String
getLabelPrefix = getState >>= \(_, _, labl) -> return labl


-- FIXME: expressions don't support labels
expr   :: MyParser Integer
expr   = (term <* spaces) `chainl1` (addop) <?> "expression" -- FIXME

term   :: MyParser Integer
term   = (factor <* spaces) `chainl1` (mulop) <?> "term"

factor :: MyParser Integer
factor = intOrChar <|> parens expr <?> "factor"
    where parens = between (char '(' *> spaces) (char ')')

mulop :: MyParser (Integer -> Integer -> Integer)
mulop = spaces *> char '*' *> spaces *> return (*)
    <|> spaces *> char '/' *> spaces *> return div 
    <|> spaces *> char '%' *> spaces *> return rem

addop :: MyParser (Integer -> Integer -> Integer)
addop = spaces *> char '+' *> spaces *> return (+)
    <|> spaces *> char '-' *> spaces *> return (-)

asmEQU :: MyParser Token
asmEQU = do
    name <- globalLabel <* skipSpaces
    def <- caseInsensitiveString "equ" *> skipSpaces *> expr
    addToLabels name def -- add it to the map of labels
    return $ EQU name def

asmDS :: MyParser Token
asmDS = do
    caseInsensitiveString "ds" *> skipSpaces <?> "DS"
--     size <- intOrChar
    size <- expr
-- a DS should not increase the text length...
    loc (+size) -- but its operand should, by its value
    return (DS size)

-- FIXME: still doesn't handle strings or other fancy things like expressions
asmDW :: MyParser Token
asmDW = do
    caseInsensitiveString "dw" *> skipSpaces <?> "DW"
--     args <- intOrChar `sepBy1` (char ',' *> spaces)
    args <- fmap join dwArgs
    -- a DW should increase the location counter by the number of arguments
    loc (\x -> x + (genericLength args))
    return (DW args)

dwArgs :: MyParser [[Integer]]
dwArgs = (fmap (:[]) intOrChar <|> litString) `sepBy1` (char ',' *> spaces)

litString :: MyParser [Integer]
litString = map (toInteger . ord) 
        <$> (char '\"' *> anyChar `manyTill` char '\"')

asmEntry :: MyParser Token
asmEntry = do 
    caseInsensitiveString "entry" <* skipSpaces <?> "ENTRY"
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    setEntry (EntryPoint name)
    return (Entry (EntryPoint name))

asmExtern :: MyParser Token 
asmExtern = do
    caseInsensitiveString "extern" <* skipSpaces <?> "EXTERN"
    args <- (localLabel <|> globalLabel) `sepBy1` (char ',' *> spaces)
    forM_ args (flip addToLabels (-9)) -- FIXME: what value should it have?
--     traverse_ (flip addToLabels (-9)) args -- FIXME: what value should it have?
--     mapM_ (flip addToLabels (-9)) args  -- FIXME: what value should it have?
--     traceM $ "asmExtern: args: " ++ show args
    return $ Extern (args)

asmPublic :: MyParser Token 
asmPublic = do
    caseInsensitiveString "public" <* skipSpaces <?> "PUBLIC"
    args <- (localLabel <|> globalLabel) `sepBy1` (char ',' *> spaces)
    forM_ args (flip addToLabels (-9)) -- FIXME: what value should it have?
--     traverse_ (flip addToLabels (-9)) args -- FIXME: what value should it have?
--     mapM_ (flip addToLabels (-9)) args  -- FIXME: what value should it have?
    return $ Public (show args)
