module Parser12 (Instruction(..), Word(..), parseInstructions) where 

import Text.ParserCombinators.Parsec hiding (try)
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser)
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
import Control.Monad (join)
-- import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell, lift)
-- import Control.Monad.State (runState, evalState, execState)
import Control.Applicative hiding (many, (<|>))
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, toLower, ord)
import Data.List (genericLength)
import Text.Read (readMaybe)

import Instructions

testfile = "programs/testfile5"

main :: IO ()
main = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: TEST LOL"
    c <- readFile testfile
--     let result = runWriter $ runParserT parseInstructions 0 testfile c
    let tempResult :: Writer 
                        [(String, Integer)] 
                        (Either ParseError (Integer, String, [Word]))
        tempResult = runParserT parseInstructions (0, "") testfile c
        result :: (Either ParseError (Integer, String, [Word]), [(String, Integer)])
--         result = runWriter $ runParserT parseInstructions (0, "") testfile c
        result = runWriter $ tempResult
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case fst result of
        Left error -> putStrLn $ "error: " ++ (show error)
        Right r -> print r
    putStrLn "okay"

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
(Integer, String) is the state. 
    i use the Integer to count words, both so i can output the text length and 
        so i can track where the labels are.
    the String is the name of the entry point after it has been found. 
        it is the empty string if no entry point is found.
Writer [(String, Integer)] is the transformed monad. when a label is parsed,
    a (String, Integer) pair is appended using its monoid instance. the String
    in this case is the name of the label; the Integer is its location. 
-}
type MyParser a = ParsecT 
                    String 
                    (Integer, String) 
                    (Writer [(String, Integer)]) 
                    a

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
          | Entry String
          deriving (Show)

--------------------------------- parser begins here

-- parseInstructions :: MyParser (Integer, [Word])
parseInstructions :: MyParser (Integer, String, [Word])
parseInstructions = do 
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    skipMany skipJunk
    eof
    (count, entry) <- getState
--     count <- getState
    return (count, entry, res)

parseInstruction :: MyParser Word
parseInstruction = do
    skipMany skipJunk
    code <- parseInt 
        <|> parseChar 
        <|> try parseDS 
        <|> try parseDW 
        <|> try parseEntry 
        <|> try parseNewLabel 
        <|> parseWord
--     modifyState (+1) -- add one to the count of instructions
--     modifyState (updateLocationCounter (+1)) -- add one to the count of instructions
    loc (+1) -- add one to the count of instructions
    return code

parseDS :: MyParser Word
parseDS = do
    caseInsensitiveString "ds" >> skipSpaces
    (Lit size) <- parseInt
--     a DS should not increase the text length...
--     modifyState (+size) -- but its operand should, by its value
--     modifyState (updateLocationCounter (+size)) -- but its operand should, by its value
    loc (+size) -- but its operand should, by its value
    return (DS size)

-- FIXME: still doesn't handle strings or other fancy things like expressions
parseDW :: MyParser Word
parseDW = do
    caseInsensitiveString "dw" >> skipSpaces
    args <- map unLit <$> (parseInt <|> parseChar) `sepBy1` (char ',' >>spaces)
        -- a DW should increase the text length by the number of arguments
--     modifyState (\x -> x + (genericLength args) - 1) 
--     modifyState (updateLocationCounter (\x -> x + (genericLength args) - 1))
    loc (\x -> x + (genericLength args) - 1)
    return (DW args)
        where unLit (Lit x) = x

parseEntry :: MyParser Word
parseEntry = do 
    caseInsensitiveString "entry"
    skipSpaces
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
--     modifyState (\x -> x-1) -- the entry should not increase the text length
--     modifyState (updateLocationCounter (\x -> x-1)) -- the entry should not increase the text length
    loc (\x -> x-1) -- the entry should not increase the text length
--     setEntry name
    modifyState (\(i, s) -> (i, name))
    return (Entry name)

-- FIXME: local labels are not implemented
parseNewLabel :: MyParser Word
parseNewLabel = do
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    char ':' 
    skipMany skipJunk
--     pos <- getState -- get the current position in the count
    pos <- getLocationCounter -- get the current position in the count
    lift $ tell [(name, pos)] -- append it to the list of labels
--     modifyState (\x -> x-1) -- a label should not increase the text length
--     modifyState (updateLocationCounter (\x -> x-1)) -- a label should not increase the text length
    loc (\x -> x-1) -- a label should not increase the text length
    return (NewLabel name)

parseWord :: MyParser Word
parseWord = readInstr <$> many1 letter where 
    readInstr :: String -> Word
    readInstr str = case readMaybe (map toUpper str) of
        Just x -> Op x
        Nothing -> Label str

parseInt :: MyParser Word
parseInt = Lit . read <$> (many1 digit)

parseChar :: MyParser Word
parseChar = Lit . toInteger . ord <$> (char '\'' >> anyChar)

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

updateLocationCounter :: (Integer -> Integer) -> (Integer, String) -> (Integer, String)
updateLocationCounter f (i, s) = (f i, s)

loc :: (Integer -> Integer) -> MyParser ()
loc = modifyState . updateLocationCounter

setEntry :: String -> MyParser ()
setEntry = modifyState . setEntry'
    where 
        setEntry' str (i, "") = (i, str)
        setEntry' _ _ = error "multiple entries?" 

getLocationCounter = do
    (i, s) <- getState
    return i
