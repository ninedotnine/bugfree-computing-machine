-- {-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
-- {-# OPTIONS_GHC -Wall #-} 
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

-- module Parser (Instruction(..), 
--                 Token(..), 
--                 EntryPoint(..),
--                 parseEverything) where 

-- module SXXParser (MyVector, populateVector, printVector) where
module ObjParser  where


import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
import Data.Functor.Identity
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
import Control.Monad.Trans (liftIO)
import Control.Monad.Primitive (PrimState)
-- import Control.Monad.Identity
import Control.Monad.Writer (Writer, runWriter, tell, lift)
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, toLower, ord)
import Data.List (genericLength, intersperse)
import Data.String (IsString, fromString)
import Data.Int
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
import Data.Map.Strict as Map (Map, singleton)

import Debug.Trace (trace)

import Instructions

header :: MyParser ()
header = string "%SXX+O" >> skipToEOL >> spaces

percentSeparator :: MyParser ()
percentSeparator = char '%' >> skipToEOL

-- traceM :: (Monad m) => String -> m ()
-- traceM str = trace str $ return ()

testfile :: FilePath
testfile = "../object_files/test.out"

main :: IO ()
main = do
    contents <- readFile testfile
    let result :: Either ParseError Integer
--     let
        result = runParser pass1 0 "namey" contents
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) 
                    ++ "\ncontnts:\n" ++ contents
        Right r -> print r
    putStrLn "okay"

pass1 :: MyParser Integer
pass1 = do 
    header 
    text_length <- readNum <* skipToEOL
    percentSeparator
    readText
    lenth <- getState
    when (lenth /= text_length) $ fail "bad length"
    percentSeparator
--     relocDict
    return text_length

readText :: MyParser ()
readText = skipMany (dw <|> instruction)

dw :: MyParser ()
dw = do
    char ':'
    val <- readNum <* skipToEOL
    modifyState (+val)

instruction :: MyParser ()
instruction = do
    _ <- readNum <* skipToEOL
    modifyState (+1)

relocDict :: MyParser ()
relocDict = do
    undefined

    

    
-- skipSpaces :: MyParser ()
-- skipSpaces = many1 space >> return ()

-- sxxDW :: MyVector -> MyParser ()
{-
sxxDW mem = do 
    char ':'
    val <- readNum
    index <- getState
    liftIO $ putStrLn $ "dw: " ++ show val
    liftIO $ write mem index val
    modifyState (+ (fromIntegral val))

instruction :: MyVector -> MyParser ()
instruction mem = do 
    val <- readNum
    index <- getState
    liftIO $ putStrLn $ "read: " ++ show val
    liftIO $ write mem index val
    modifyState (+1)
    return ()
-- fmap Lit intOrChar <* loc (+1)
--     <|> try asmEQU 
--     <|> try asmDS 
--     <|> try asmDW 
--     <|> try opcode <* loc (+1)
--     <|> label <* loc (+1)

-}
readNum :: MyParser Integer
-- readNum = read <$> many1 digit 
readNum = do
    str <- readMaybe <$> many1 digit 
--     liftIO $ print str
    case str of
        Just x -> return x
        Nothing -> fail "oopsie"

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Int is the state, it is the line counter
Identity is the transformed monad.
-}

-- type MyParser a = ParsecT String (Map.Map uuuInteger Identity a
type MyParser a = ParsecT String Integer Identity a

newtype EntryPoint = EntryPoint String deriving (Eq)

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space
