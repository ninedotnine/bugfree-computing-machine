import Text.ParserCombinators.Parsec hiding (runParser)
import Text.Parsec.Prim hiding (try)
import Control.Monad (join)
import Control.Monad.Writer
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, ord)
import Text.Read (readMaybe)

import Instructions

main :: IO ()
main = do
    c <- readFile "testfile"
{-
--     case runParser parser 0 "testfile" c of
    case runParserT parser 0 "testfile" c of
        Left error -> putStrLn $ "error: " ++ (show error)
--         Right r -> mapM_ print r
        Right r -> print r
        -}
--     result <- runParserT parser 0 "testfile" c
--     result <- runWriter =<< (play c)
    let result = runWriter $ runParserT parser 0 "testfile" c
    putStr "result is: " >> print result
    case fst result of
        Left error -> putStrLn $ "error: " ++ (show error)
--         Right r -> mapM_ print r
        Right r -> print r
    putStrLn "okay"

-- test = parseTest parser " tnsoe"

play :: String -> Writer String (Either ParseError Integer)
play input = do
    result <- runParserT parser 0 "testfile" input
    return result

play2 :: String -> Writer String (Either ParseError Integer)
play2 input = runParserT parser 0 "testfile" input

type WParser a = ParsecT String Integer (Writer String) a

--------------------------------- parser begins here

-- parser :: WParser Integer
parser :: ParsecT String Integer (Writer String) Integer
parser = do 
    n <- getState
    return n

-- charCount = do
--     x <- char
--     modifyState (+1)
--     return x
