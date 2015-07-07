module Parser4 (Instruction(..), parseInstructions) where 

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim hiding (runParser)
-- import Text.Parsec.Prim hiding (try, runParser)
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
    let result = runWriter $ runParserT parseInstructions 0 "testfile" c
--     case runState (runParserT parseWords 0 "parser2" c) of
--     case iParse parseWords 0 "parser2" c of
--     case iParse parseWords "parser2" c of
--     case c of
--     case runParser parseInstructions 0 "dum" c of
--     case runParsecT parseInstructions 0 "dum" c of
    putStr "result is: " >> print result
    case fst result of
        Left error -> putStrLn $ "error: " ++ (show error)
        Right r -> print r
--         Right r -> putStrLn "success"
    putStrLn "okay"

-- type ParserCounter s u a = ParsecT s u (State Int) a
type ParserCounter a = ParsecT String Integer (State Int) a

--------------------------------- parser begins here

-- parseInstructions :: ParsecT String Integer Identity (Integer, [Int])
parseInstructions :: ParsecT String Integer (Writer String) (Integer, [Int])
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

parseInstruction :: ParsecT String Integer (Writer String) Int
parseInstruction = do
    skipMany skipJunk
--     code <- parseOpcode <|> parseInt <|> parseChar
--     code <- try parseOpcode <|> parseInt <|> parseChar <|> parseLabel
    code <- try parseWord <|> parseInt <|> parseChar <|> parseLabel
    modifyState (+1) -- add one to the count of instructions
    -- FIXME: i don't think i should add one if i just parsed a label
    return code

parseWord = do
    word <- many1 letter `notFollowedBy` char ':'
    let num = readOpCode word




parseLabel = do
    name <- many1 letter
    char ':' 
    skipMany skipJunk
    lift $ tell name
    return 100



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

-- parseOpcode :: Parser Int
parseOpcode = (fromEnum . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?
--             Nothing -> fail <?> "borked" --  what should i do here?

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

