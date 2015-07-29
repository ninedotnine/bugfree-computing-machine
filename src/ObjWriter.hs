{-# LANGUAGE OverloadedStrings #-} 
-- {-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module ObjWriter where 

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
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
import Data.Map.Strict as Map (Map, singleton)

import Debug.Trace (trace)

import Instructions
import ObjParser

-- should really import these from ObjParser
type Relocs = [Integer]
type Publics = Map.Map String Integer
type Externs = Map.Map String [Integer]
type Offset  = Integer

traceM :: (Monad m) => String -> m ()
traceM str = trace str $ return ()

testfile :: FilePath
-- testfile = "programs/testfile6"
testfile = "../lib/writes.out"

main :: IO ()
main = do
    putStrLn $ "# file: " ++ testfile
    c <- readFile testfile
    let result :: Either ParseError String
        result = parseEverything testfile c
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r
    putStrLn "FIN"

parseEverything :: SourceName -> String -> Either ParseError String
parseEverything name str = do
    let eith :: Either ParseError Bool
        output :: String
        (eith, output) = runWriter $ runParserT instructions [] name str 
    case eith of
        Left err -> Left err
        Right False -> error "when would this happen?"
        Right True -> return output

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Relocs is the state. 
Writer String is the transformed monad.
-}
type MyParser a = ParsecT String Relocs (Writer String) a

--------------------------------- parser begins here

-- returns true if it succeeds
instructions :: MyParser Bool
instructions = return True 
-- instructions = do
    
--     res <- join <$> many (try instruction) `sepBy` skipJunk
--     skipMany skipJunk *> eof
--     (counter, entry, _) <- getState
--     return (counter, entry, res)
