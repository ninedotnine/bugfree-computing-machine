module Parser5 (Instruction(..), parseInstructions) where 

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
    putStr "result is: " >> print result
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

-- parseInstructions :: ParsecT String Integer (Writer String) (Integer, [Int])
parseInstructions :: ParsecT String Integer (Writer String) (Integer, [Word])
parseInstructions = do 
    res <- join <$> (many (try parseInstruction)) `sepBy` skipJunk
    pos <- fmap sourceLine getPosition
    skipMany skipJunk
    eof
    count <- getState
    return (count, res)

-- parseInstruction :: ParsecT String Integer (Writer String) Int
parseInstruction :: ParsecT String Integer (Writer String) Word
parseInstruction = do
    skipMany skipJunk
    code <- parseOpcode <|> parseInt <|> parseChar
    modifyState (+1) -- add one to the count of instructions
    -- FIXME: i don't think i should add one if i just parsed a label
    return code

{-
parseWord = do
    word <- many1 letter `notFollowedBy` char ':'
    let num = parseOpCode word
    -}




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
parseOpcode :: ParsecT String Integer (Writer String) Word
-- parseOpcode = (fromEnum . readInstr . map toUpper) <$> many1 letter 
parseOpcode = (Op . readInstr . map toUpper) <$> many1 letter 
    where 
        readInstr :: String -> Instruction
        -- readInstr = read
        readInstr str = case readMaybe str of
            Just x -> x
            Nothing -> ERROR -- what should i do here?
--             Nothing -> fail <?> "borked" --  what should i do here?

-- parseInt :: Parser Int
parseInt :: ParsecT String Integer (Writer String) Word
parseInt = Lit . read <$> (many1 digit)

-- parseChar :: Parser Int
parseChar :: ParsecT String Integer (Writer String) Word
-- parseChar = (Lit . toInteger) $ (ord <$> (char '\'' >> letter))
-- parseChar = (return . Lit . toInteger) (ord <$> (char '\'' >> letter))
{-
parseChar = do -- this one works!!!
    char '\''
    x <- letter
    return $ Lit $ toInteger $ ord x
    -} 

{-
parseChar = do
    x <- char '\'' >> letter
    return $ Lit $ toInteger $ ord x
    -}

parseChar = (Lit . toInteger . ord) <$> (char '\'' >> letter)
--     return $ Lit $ toInteger $ ord x

-- dumbparseChar2 :: ParsecT String Integer (Writer String) Word
-- dumbparseChar2 = do
--     char '\''
--     x <- letter
--     return $ Lit $ toInteger $ ord x

-- dumbparseChar :: ParsecT String Integer (Writer String) Int
-- dumbparseChar = (ord <$> (char '\'' >> letter))


-- skipComment :: Parser ()
skipComment = spaces >> char ';' >> skipToEOL
-- skipComment = char ';' >> skipToEOL

-- skipToEOL :: Parser ()
-- skipToEOL = many (noneOf "\n") >> return ()
-- skipToEOL = skipMany (noneOf "\n") >> skipMany1 space -- skip past the '\n'
skipToEOL = anyChar `manyTill` newline >> skipMany space

