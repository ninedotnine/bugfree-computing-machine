module Parser2 (Instruction(..), parseInstructions) where 

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
import Control.Monad (join)
import Control.Monad.Identity
import Control.Monad.Writer
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
--     c <- getContents
    c <- readFile "testfile"
--     c <- parseFromFile parseInstructions "testfile"
--     case runParser "" c of
--     case runParser parseInstructions "dum" c of
--     case parse parseInstructions "dum" c of
--     case runParserT parseInstructions 0 "parser2" c of
    case runIdentity (runParserT parseInstructions 0 "parser2" c) of
--     case c of
        Left error -> putStrLn $ "error: " ++ (show error)
--         Right r -> mapM_ print r
        Right r -> print r

-- test = parseTest parseInstruction

--------------------------------- parser begins here

parseInstructions = do 
--     res <- join <$> (many (try parseInstruction)) `sepBy` skipSpaces
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    pos <- fmap sourceLine getPosition
--     res <- many (try parseInstruction)
--     skipMany (skipComment <|> skipSpaces)
    skipMany skipJunk
    eof
--     return (res ++ [pos]) -- interesting use of monad!
    return res
--     return (length (show st)):res

parseInstruction = do
    skipMany skipJunk
    code <- parseOpcode <|> parseInt <|> parseChar
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

-- parseOpcode :: Parser Int
parseOpcode = (fromEnum . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?

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
