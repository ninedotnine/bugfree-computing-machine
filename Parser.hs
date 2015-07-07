module Parser7 (Instruction(..), parseInstructions) where 

import Text.ParserCombinators.Parsec hiding (try)
import Text.Parsec.Prim hiding (runParser)
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
import Control.Monad (join)
import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State (runState, evalState, execState)
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
    c <- readFile "testfile2"
    let result = runWriter $ runParserT parseInstructions 0 "testfile" c
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case fst result of
        Left error -> putStrLn $ "error: " ++ (show error)
        Right r -> print r
    putStrLn "okay"

type ParserCounter a = ParsecT String Integer (State Int) a

data Word = Lit Integer
          | Op Instruction
          | Label String
          deriving (Show)

--------------------------------- parser begins here

-- parseInstructions :: ParsecT String Integer (Writer String) (Integer, [Word])
parseInstructions :: ParsecT String Integer (Writer [String]) (Integer, [Word])
parseInstructions = do 
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    pos <- fmap sourceLine getPosition
    skipMany skipJunk
    eof
    count <- getState
    return (count, res)

parseInstruction :: ParsecT String Integer (Writer [String]) Word
parseInstruction = do
    skipMany skipJunk
    code <- parseInt <|> parseChar <|> try parseLabel <|> parseOpcode
    modifyState (+1) -- add one to the count of instructions
    return code

-- parseLabel :: ParsecT String Integer (Writer String) Word
parseLabel :: ParsecT String Integer (Writer [String]) Word
parseLabel = do
    name <- many1 letter
    char ':' 
    skipMany skipJunk
    lift $ tell [name] -- works
--     tell <$> [name]
--     tell <$> [name]
--     lift $ tell $ pure name
--     liftM $ liftM $ tell name
    modifyState (\x -> x-1) -- a label should not increase the text length
    return (Label name)

-- addSym :: String -> Parsec String [String] ()
-- addSym :: String -> ParsecT String [String] Identity ()
-- addSym = updateState . (:) -- append a string to the symbol list
-- addSym = void . updateState . (:) -- append a string to the symbol list

-- skipJunk :: Parser ()
skipJunk = skipSpaces <|> skipComment

-- skipSpaces :: Parser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

-- parseOpcode :: Parser Int
-- parseOpcode :: ParsecT String Integer (Writer String) Word
-- parseOpcode = (fromEnum . readInstr . map toUpper) <$> many1 letter 
parseOpcode = (Op . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?
--             Nothing -> fail <?> "borked" --  what should i do here?

-- parseInt :: ParsecT String Integer (Writer String) Word
parseInt = Lit . read <$> (many1 digit)

-- parseChar :: ParsecT String Integer (Writer String) Word
{-
parseChar = do -- this one works!!!
    char '\''
    x <- letter
    return $ Lit $ toInteger $ ord x
    -} 

-- equivalent!!
parseChar = Lit . toInteger . ord <$> (char '\'' >> letter)

-- skipComment :: Parser ()
skipComment = spaces >> char ';' >> skipToEOL
-- skipComment = char ';' >> skipToEOL

-- skipToEOL :: Parser ()
-- skipToEOL = many (noneOf "\n") >> return ()
-- skipToEOL = skipMany (noneOf "\n") >> skipMany1 space -- skip past the '\n'
skipToEOL = anyChar `manyTill` newline >> skipMany space



