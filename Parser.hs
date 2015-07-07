{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module Parser18 (Instruction(..), 
                Word(..), 
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
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map (Map, singleton)

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
    let result :: Either ParseError (Integer, EntryPoint, [Word], Labels)
        result = parseEverything testfile c
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r
    putStrLn "okay"

parseEverything :: SourceName 
        -> String 
        -> Either ParseError (Integer, EntryPoint, [Word], Labels)
parseEverything name str = do
    let eith :: Either ParseError (Integer, EntryPoint, [Word])
        (eith, labels) = parseEverything' name str
    case eith of
        Left err -> Left err
        Right (i, m, xs) -> return (i, m, xs, labels)

parseEverything' :: SourceName -> String 
        -> (Either ParseError (Integer, EntryPoint, [Word]), Labels)
parseEverything' = (runWriter .) . runParserT parseInstructions (0, "", "")


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
Word is an algebraic data type
a Lit can be a number like 65 or a letter like 'a
an Op is any of the opcodes
a Label is a label
-}
data Word = Lit Integer
          | Op Instruction
          | Label String
          | NewLabel String
          | DS Integer
          | DW [Integer]
          | Entry EntryPoint
          deriving (Show)

newtype EntryPoint = EntryPoint String deriving (Eq)

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

--------------------------------- parser begins here

parseInstructions :: MyParser (Integer, EntryPoint, [Word])
parseInstructions = do 
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    skipMany skipJunk
    eof
    (counter, entry, _) <- getState
    return (counter, entry, res)

parseInstruction :: MyParser Word
parseInstruction = do
    skipMany skipJunk
    code <- Lit <$> parseIntOrChar <* loc (+1)
        <|> try parseDS 
        <|> try parseDW 
        <|> try parseEntry 
        <|> try parseNewLabel 
        <|> try parseOpcode <* loc (+1)
        <|> parseLabel <* loc (+1)
    return code




parseNewLabel :: MyParser Word
parseNewLabel = NewLabel <$> (parseNewGlobalLabel <|> parseNewLocalLabel) 
                <* skipMany skipJunk

parseLabel :: MyParser Word
parseLabel = Label <$> (parseLocalLabel <|> parseGlobalLabel) 
            <* skipMany skipJunk

parseNewLocalLabel :: MyParser String
parseNewLocalLabel = do
    name <- parseLocalLabel
    char ':'
    pos <- getLoc -- get the current position in the count
    lift $ tell (Map.singleton name pos) -- add it to the map of labels
    return (name)

parseNewGlobalLabel :: MyParser String
parseNewGlobalLabel = do
    name <- parseGlobalLabel
    char ':' 
    setLabelPrefix name -- new current scope
    pos <- getLoc -- get the current position in the count
    lift $ tell (Map.singleton name pos) -- add it to the map of labels
    return (name)
    
parseLocalLabel :: MyParser String
parseLocalLabel = do
    char '@'
    tailer <- many labelChar
    header <- getLabelPrefix
    let name = header ++ '-' : tailer -- join them with '-' to prevent clashes
    return name

parseGlobalLabel :: MyParser String
parseGlobalLabel = do
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    return name





parseOpcode :: MyParser Word
parseOpcode = do 
    uppers <- map toUpper <$> many1 letter -- all the opcodes are in capitals
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


parseIntOrChar :: MyParser Integer
parseIntOrChar = parseInt <|> parseChar where 
    parseInt :: MyParser Integer
    parseInt = read <$> (many1 digit)
    parseChar :: MyParser Integer
    parseChar = toInteger . ord <$> (char '\'' >> anyChar)

skipJunk :: MyParser ()
skipJunk = skipSpaces <|> skipComment
-- skipJunk = spaces <|> skipComment

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

skipComment :: MyParser ()
skipComment = spaces >> char ';' >> skipToEOL
-- skipComment = char ';' >> skipToEOL

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline >> skipMany space
-- skipToEOL = many (noneOf "\n") >> return ()
-- skipToEOL = skipMany (noneOf "\n") >> skipMany1 space -- skip past the '\n'

-- caseInsensitiveChar :: Char -> GenParser Char state Char
caseInsensitiveChar :: Char -> MyParser Char
caseInsensitiveChar = (\c -> (char (toLower c) <|> char (toUpper c)) >> pure c)

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
loc = modifyState . (\f (i, s, label) -> (f i, s, label))

getLoc :: MyParser Integer
getLoc = getState >>= \(i, _, _) -> return i

-- setEntry "main" makes the entry point "main", provided it isn't already set
setEntry :: EntryPoint -> MyParser ()
setEntry = modifyState . setEntry' where 
    -- pattern match the empty string
    setEntry' name (i, "", label) = (i, name, label) 
    setEntry' _ _ = error "multiple entries?" 

setLabelPrefix :: String -> MyParser ()
setLabelPrefix = modifyState . setLabelPrefix' where
    setLabelPrefix' str (i, ent, _) = (i, ent, str)

getLabelPrefix :: MyParser String
getLabelPrefix = getState >>= \(_, _, label) -> return label








-- FIXME: expressions don't work at all
expr   :: MyParser Integer
expr   = term `chainl1` addop

term   :: MyParser Integer
term   = factor `chainl1` mulop

factor :: MyParser Integer
factor = between (char '(') (char ')') expr <|> parseIntOrChar

mulop   :: MyParser (Integer -> Integer -> Integer)
mulop   =   do{ char '*'; return (*)   }
        <|> do{ char '/'; return (div) }
        <|> do{ char '%'; return (rem) }

addop   :: MyParser (Integer -> Integer -> Integer)
addop   =   do{ char '+'; return (+) }
        <|> do{ char '-'; return (-) }

parseDS :: MyParser Word
parseDS = do
    caseInsensitiveString "ds" >> skipSpaces
    size <- parseIntOrChar
--     a DS should not increase the text length...
    loc (+size) -- but its operand should, by its value
    return (DS size)

-- FIXME: still doesn't handle strings or other fancy things like expressions
parseDW :: MyParser Word
parseDW = do
    caseInsensitiveString "dw" >> skipSpaces
    args <- parseIntOrChar `sepBy1` (char ',' >>spaces)
        -- a DW should increase the location counter by the number of arguments
    loc (\x -> x + (genericLength args))
    return (DW args)

parseEntry :: MyParser Word
parseEntry = do 
    caseInsensitiveString "entry"
    skipSpaces
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    setEntry (EntryPoint name)
    return (Entry (EntryPoint name))
