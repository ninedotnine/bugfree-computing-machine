module Parser9 (Instruction(..), parseInstructions) where 

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
import Data.Char (toUpper, ord)
import Text.Read (readMaybe)

import Instructions

main :: IO ()
main = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: TEST LOL"
--     c <- readFile "testfile"
--     c <- readFile "testfile2"
    c <- readFile "testfile4"
    let result = runWriter $ runParserT parseInstructions 0 "testfile" c
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
Integer is the state. i use it to count words, both so i can output the 
    text length and so i can track where the labels are.
Writer [(String, Integer)] is the transformed monad. when a label is parsed,
    a (String, Integer) pair is appended using its monoid instance. the String
    in this case is the name of the label; the Integer is its location. 
-}
type MyParser a = ParsecT String Integer (Writer [(String, Integer)]) a

{-
Word is an algebraic data type
a Lit can be a number like 65 or a letter like 'a
an Op is any of the opcodes
a Label is a label
-}
data Word = Lit Integer
          | Op Instruction
          | Label String
          deriving (Show)

--------------------------------- parser begins here

parseInstructions :: MyParser (Integer, [Word])
parseInstructions = do 
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    skipMany skipJunk
    eof
    count <- getState
    return (count, res)

parseInstruction :: MyParser Word
parseInstruction = do
    skipMany skipJunk
    code <- parseInt <|> parseChar <|> try parseLabel <|> parseOpcode
    modifyState (+1) -- add one to the count of instructions
    return code

parseLabel :: MyParser Word
parseLabel = do
    name <- many1 letter
    char ':' 
    skipMany skipJunk
    pos <- getState -- get the current position in the count
    lift $ tell [(name, pos)] -- append it to the list of labels
    modifyState (\x -> x-1) -- a label should not increase the text length
    return (Label name)

skipJunk :: MyParser ()
skipJunk = skipSpaces <|> skipComment

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

parseOpcode :: MyParser Word
parseOpcode = (Op . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?
--             Nothing -> fail <?> "borked" --  what should i do here?

parseInt :: MyParser Word
parseInt = Lit . read <$> (many1 digit)

parseChar :: MyParser Word
parseChar = Lit . toInteger . ord <$> (char '\'' >> anyChar)

skipComment :: MyParser ()
skipComment = spaces >> char ';' >> skipToEOL
-- skipComment = char ';' >> skipToEOL

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline >> skipMany space
-- skipToEOL = many (noneOf "\n") >> return ()
-- skipToEOL = skipMany (noneOf "\n") >> skipMany1 space -- skip past the '\n'
