{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE FlexibleContexts #-} 
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
import Control.Monad.Identity
-- import Control.Monad.Writer (Writer, runWriter, tell, lift)
import Control.Monad.Writer 
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
        Right r -> putStrLn r
    putStrLn "FIN"

parseEverything :: SourceName -> String -> Either ParseError String
parseEverything name str = do
    let eith :: Either ParseError Bool
        output :: String
        (eith, output) = runWriter $ runParserT instructions emptyInfo name str
    case eith of
        Left err -> Left err
        Right False -> error "when would this happen?"
        Right True -> return output

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Info is the state. 
Writer String is the transformed monad.
-}
type MyParser a = ParsecT String Info (Writer String) a

--------------------------------- parser begins here

-- returns true if it succeeds
instructions :: MyParser Bool
instructions = do
    gen "%SSX+Executable\n"
    info <- getState
    gen $ show (getLineCount info) ++ " text length\n"
    case getEntry info of
        Just x -> gen $ (show x) ++ " ENTRY\n"
        Nothing -> gen "0 ENTRY (default)\n"
    return True 

gen :: String -> ParsecT String Info (Writer String) () 
gen = lift . tell
