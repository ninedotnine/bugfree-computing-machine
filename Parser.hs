{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module Parser17 (Instruction(..), 
                Word(..), 
                EntryPoint(..),
                parseEverything) where 

import Text.ParserCombinators.Parsec hiding (try, label, labels)
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels)
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
-- import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell, lift)
-- import Control.Monad.State (runState, evalState, execState)
import Control.Applicative hiding (many, (<|>))
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
traceM string = trace string $ return ()

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
-- parseEverything' x y = runWriter $ runParserT parseInstructions (0, "", "") x y


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
--           | Entry String
          | Entry EntryPoint
          deriving (Show)

-- newtype EntryPoint = EntryPoint { name :: String } deriving (Eq)
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
    code <- fmap Lit parseIntOrChar
        <|> try parseDS 
        <|> try parseDW 
        <|> try parseEntry 
        <|> try parseNewLabel 
--         <|> parseWord
        <|> try parseOpcode
        <|> fmap Label parseLabel
    loc (+1) -- add one to the location counter
    traceM $ "parseInstruction: " ++ show code
    return code

-- FIXME: local labels are not implemented
{-
parseNewLabel :: MyParser Word
parseNewLabel = do
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    char ':' 
    skipMany skipJunk
    pos <- getLoc -- get the current position in the count
    lift $ tell (Map.singleton name pos) -- add it to the map of labels
    loc (\x -> x-1) -- a label should not increase the text length
    return (NewLabel name)
-}

parseNewLabel :: MyParser Word
parseNewLabel = parseNewGlobalLabel <|> parseNewLocalLabel

parseNewLocalLabel :: MyParser Word
parseNewLocalLabel = do
    char '@' 
    tailer <- many labelChar
    char ':'
    header <- getLabelPrefix
    let name = header ++ '-' : tailer -- join them with '-' to prevent clashes
    skipMany skipJunk
    pos <- getLoc -- get the current position in the count
    lift $ tell (Map.singleton name pos) -- add it to the map of labels
    loc (\x -> x-1) -- a label should not increase the text length
    return (NewLabel name)

parseNewGlobalLabel :: MyParser Word
parseNewGlobalLabel = do
    header <- letter
    tailer <- many labelChar
    char ':' 
    let name = (header:tailer)
    setLabelPrefix name
    pos <- getLoc -- get the current position in the count
    lift $ tell (Map.singleton name pos) -- add it to the map of labels
    loc (\x -> x-1) -- a label should not increase the text length
    skipMany skipJunk
    return (NewLabel name)

-- parseLabel :: MyParser Word
parseLabel :: MyParser String
parseLabel = do
    traceM "parseLabel"
    result <- parseLocalLabel <|> parseGlobalLabel
    traceM $ "parseLabel: " ++ result
    return result
    
-- parseLocalLabel :: MyParser Word
parseLocalLabel :: MyParser String
parseLocalLabel = do
    traceM "parseLocalLabel"
    char '@'
    tailer <- many labelChar
    header <- getLabelPrefix
    let name = header ++ '-' : tailer -- join them with '-' to prevent clashes
    traceM $ "parseLocalLabel: " ++ name
    skipMany skipJunk
--     return (Label name)
    return name

parseGlobalLabel :: MyParser String
-- parseGlobalLabel :: MyParser Word
parseGlobalLabel = do
    traceM "parseGlobalLabel"
    header <- letter
    tailer <- many labelChar
    skipMany skipJunk
    let name = (header:tailer)
--     return (Label name)
    traceM ("parseGlobalLabel: " ++ name)
    return name

{-
-- OLD GOOD VERSION
parseWord :: MyParser Word
parseWord = readInstr <$> many1 labelChar where 
    readInstr :: String -> Word
    readInstr str = case readMaybe (map toUpper str) of
        Just x -> Op x
        Nothing -> Label str
--         Nothing -> parseLabel
-- -}

{-
-- THIS WORKS BUT IS GROSS
parseWord :: MyParser Word
-- parseWord = readInstr <$> many1 labelChar where 
parseWord = do 
    str <- many1 labelChar
    traceM $ "parseWord: " ++ str
    let uppers = map toUpper str
    case readMaybe uppers of
        Just x -> return $ Op x
--         Nothing -> return $ Label str
--         Nothing -> Label <$> parseLabel
        Nothing -> do 
--             label <- parseOpSynonym <|> parseLabel 
--             label <- parseLabel 
--             return (Label label)
            case parseOpSynonym uppers of
                Just x -> return x
                Nothing -> return (Label str)
-- -}

parseOpcode :: MyParser Word
parseOpcode = do 
    str <- many1 letter
    let uppers = map toUpper str
    case readMaybe uppers of 
        Just x -> return (Op x)
        Nothing -> do
            case parseOpSynonym uppers of
                Just x -> return x
                Nothing -> fail "can't parse as opcode"
-- -}
    
{-
-- parseOpSynonym :: MyParser Word
-- parseOpSynonym :: MyParser String
parseOpSynonym = choice synonyms where
    synonyms :: [MyParser String]
    synonyms = [(caseInsensitiveString "indir" >> return ("PUSHS")),
                (caseInsensitiveString "bt"    >> return ("BNE")),
                (caseInsensitiveString "bf"    >> return ("BEQ")),
                (caseInsensitiveString "poppc" >> return ("RETURN"))]
--     synonyms = [(caseInsensitiveString "indir" >> return (Op PUSHS)),
--                 (caseInsensitiveString "bt"    >> return (Op BNE)),
--                 (caseInsensitiveString "bf"    >> return (Op BEQ)),
--                 (caseInsensitiveString "poppc" >> return (Op RETURN))]
-}
parseOpSynonym :: String -> (Maybe Word)
parseOpSynonym "INDIR" = Just (Op PUSHS)
parseOpSynonym "BT"    = Just (Op BNE)
parseOpSynonym "BF"    = Just (Op BEQ)
parseOpSynonym "POPPC" = Just (Op RETURN)
parseOpSynonym _       = Nothing


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
labelChar = do
    traceM "labelChar"
    letter <|> digit <|> oneOf "._"

-- loc f applies f to the location counter
loc :: (Integer -> Integer) -> MyParser ()
loc = modifyState . updateLocationCounter where 
    updateLocationCounter :: (Integer -> Integer) -> MyState -> MyState
    updateLocationCounter f (i, s, label) = (f i, s, label)

getLoc :: MyParser Integer
getLoc = do
    (i, _, _) <- getState
    return i

-- setEntry "main" makes the entry point "main", provided it isn't already set
setEntry :: EntryPoint -> MyParser ()
setEntry = modifyState . setEntry' where 
    setEntry' name (i, "", label) = (i, name, label) -- pattern match the empty string
    setEntry' _ _ = error "multiple entries?" 

setLabelPrefix :: String -> MyParser ()
setLabelPrefix = modifyState . setLabelPrefix' where
    setLabelPrefix' str (i, ent, _) = (i, ent, str)

getLabelPrefix :: MyParser String
getLabelPrefix = do 
    (_, _, label) <- getState
    return label








expr   :: MyParser Integer
expr   = term   `chainl1` addop

term   :: MyParser Integer
term   = factor `chainl1` mulop

factor :: MyParser Integer
factor = between (char '(') (char ')') expr <|> parseIntOrChar

mulop   :: MyParser (Integer -> Integer -> Integer)
mulop   =   do{ char '*'; return (*)   }
        <|> do{ char '/'; return (div) }

addop   :: MyParser (Integer -> Integer -> Integer)
addop   =   do{ char '+'; return (+) }
        <|> do{ char '-'; return (-) }



parseDS :: MyParser Word
parseDS = do
    caseInsensitiveString "ds" >> skipSpaces
    size <- parseIntOrChar
--     a DS should not increase the text length...
--     modifyState (+size) -- but its operand should, by its value
--     modifyState (updateLocationCounter (+size)) -- but its operand should, by its value
    loc (+size) -- but its operand should, by its value
    return (DS size)

-- FIXME: still doesn't handle strings or other fancy things like expressions
parseDW :: MyParser Word
parseDW = do
    caseInsensitiveString "dw" >> skipSpaces
    args <- parseIntOrChar `sepBy1` (char ',' >>spaces)
        -- a DW should increase the text length by the number of arguments
    loc (\x -> x + (genericLength args) - 1)
    return (DW args)

parseEntry :: MyParser Word
parseEntry = do 
    caseInsensitiveString "entry"
    skipSpaces
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    loc (\x -> x-1) -- the entry should not increase the text length
    setEntry (EntryPoint name)
    return (Entry (EntryPoint name))
