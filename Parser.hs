module Parser3 (Instruction(..), parseInstructions) where 

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
import Control.Monad (join)
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, ord)
import Text.Read (readMaybe)

import Instructions

main :: IO ()
main = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: TEST LOL"
    c <- readFile "testfile"
--     case runState (runParserT parseWords 0 "parser2" c) of
--     case iParse parseWords 0 "parser2" c of
--     case iParse parseWords "parser2" c of
--     case c of
    case runParser parseInstructions 0 "dum" c of
--     case runParsecT parseInstructions 0 "dum" c of
        Left error -> putStrLn $ "error: " ++ (show error)
        Right r -> print r
--         Right r -> putStrLn "success"

-- type ParserCounter s u a = ParsecT s u (State Int) a
type ParserCounter a = ParsecT String () (State Int) a

--------------------------------- parser begins here

-- parseWords :: ParserCounter String () a
parseWords :: ParserCounter a
parseWords = undefined

iParse :: ParserCounter a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input = 
--     execState source_name $ runParserT aParser () source_name input
    undefined

----------------------------------

play :: String -> Either ParseError (Integer, Int)
play s = runParser pmain 0 "parameter" s

pmain :: ParsecT [Char] Int Identity (Integer, Int)
pmain = do
    x <- pnum `chainl1` pplus
    eof
    n <- getState
    return (x,n)

pnum :: ParsecT [Char] Int Identity Integer
pnum = do
    x <- read <$> many1 digit
    modifyState (+1)
    return x

pplus :: ParsecT [Char] u Identity (Integer -> Integer -> Integer)
pplus = char '+' >> return (+)

------------------------------------------------------------

parseInstructions :: ParsecT String Integer Identity (Integer, [Int])
parseInstructions = do 
--     res <- join <$> (many (try parseInstruction)) `sepBy` skipSpaces
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    pos <- fmap sourceLine getPosition
--     res <- many (try parseInstruction)
--     skipMany (skipComment <|> skipSpaces)
    skipMany skipJunk
    eof
    count <- getState
--     return (res ++ [pos]) -- interesting use of monad!
    return (count, res)
--     return (length (show st)):res

parseInstruction = do
    skipMany skipJunk
    code <- parseOpcode <|> parseInt <|> parseChar
    modifyState (+1) -- add one to the count of instructions
    return code

-- addSym :: String -> Parsec String [String] ()
-- addSym = void . updateState . (:) -- append a string to the symbol list


-- skipJunk :: Parser ()
skipJunk = skipSpaces <|> skipComment

-- skipSpaces :: Parser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

{-
parseInstruction :: Parser Int
parseInstruction = do 
    parseOpcode -- like PUSHV
    <|> parseInt   -- like 10
    <|> parseChar  -- like 'y 
--     <?> "instruction, int, or 'y"
-}

parseOpcode :: ParsecT String Integer Identity Int
parseOpcode = (fromEnum . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?
--             Nothing -> parserFail "unrecognize opcode" -- what should i do here?

{-
parseOpcode :: ParsecT String Integer Identity Int
-- parseOpcode = undefined -- FIXME -- or uncomment the above
parseOpcode = do 
    res <- parseMaybeOpcode 
    case res of 
        Just x -> x
        Nothing -> do
            parserZero
--             parserFail "borked"
--             parserReturn 4
parseMaybeOpcode :: ParsecT String Integer Identity (Maybe Int)
parseMaybeOpcode = readMaybe . map toUpper <$> many1 letter 
            -}


-- parseInt :: Parser Int
parseInt = read <$> (many1 digit)

-- parseChar :: Parser Int
parseChar = ord <$> (char '\'' >> letter)

-- skipComment :: Parser ()
skipComment = spaces >> char ';' >> skipToEOL
-- skipComment = char ';' >> skipToEOL

-- skipToEOL :: Parser ()
-- skipToEOL = many (noneOf "\n") >> return ()
-- skipToEOL = skipMany (noneOf "\n") >> skipMany1 space -- skip past the '\n'
skipToEOL = anyChar `manyTill` newline >> skipMany space
