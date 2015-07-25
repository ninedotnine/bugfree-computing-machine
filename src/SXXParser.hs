-- {-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ImplicitParams #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module SXXParser (MyVector, populateVector, printVector) where


import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
import Data.Vector.Generic hiding ((++), Vector, mapM_)
import qualified Data.Vector.Unboxed as IM
-- import Data.Vector.Unboxed.Mutable (write, MVector)
import qualified Data.Vector.Unboxed.Mutable as V
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
import Control.Monad.Trans (liftIO)
-- import Control.Monad.Primitive (PrimState)
-- import Control.Monad.Identity
-- import Control.Monad.Writer (Writer, runWriter, tell, lift)
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
-- import Data.Char (toUpper, toLower, ord)
-- import Data.List (genericLength, intersperse)
import Data.List (intersperse)
-- import Data.String (IsString, fromString)
import Data.Int (Int32)
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
-- import Data.Map.Strict as Map (Map, singleton)

-- import Debug.Trace (traceM)

import SXXVector

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Int is the state, it is the current index of the vector
IO is the transformed monad.
-}

type MyParser a = ParsecT String Int IO a

-- type MyVector = MVector (PrimState IO) Int32


baseAddr :: Integral a => a
baseAddr = 16 -- the base address must be bigger than 15

populateVector :: MyVector -> String -> IO ()
populateVector mem input = do
--     let result :: Either ParseError ()
    result <- runParserT (fillVector mem) baseAddr "input" input
    case result of
        Left err -> putStrLn ("error: " ++ (show err)) >> exitFailure
        Right () -> putStrLn "successful parse"

fillVector :: MyVector -> MyParser ()
fillVector mem = do 
    header
    textLength <- readNum <* skipToEOL
    liftIO $ putStrLn $ "textlength: " ++ show textLength 
    entry <- readNum <* skipToEOL 
    percentSeparator
    liftIO $ putStrLn $ "entry: " ++ show entry 
    instruction mem `endBy` skipSpaces
    lastInstruction <- getState 
    when (toInt textLength /= lastInstruction - baseAddr) $ 
        fail ("textLength is not " ++ show textLength)
    percentSeparator
    liftIO $ putStr "instruction space before reloc: " 
    liftIO $ printVector baseAddr (lastInstruction - baseAddr) mem
    relocatable mem `endBy` skipSpaces *> percentSeparator
    eof
    liftIO $ putStr "instruction space after reloc: " 
    liftIO $ printVector baseAddr (lastInstruction - baseAddr) mem

header :: MyParser ()
header = string "%SXX+E" >> skipToEOL >> spaces >> skipComments

percentSeparator :: MyParser ()
percentSeparator = char '%' >> skipToEOL >> skipComments

instruction :: MyVector -> MyParser ()
instruction mem = notDS mem <|> sxxDS

sxxDS :: MyParser ()
sxxDS = do 
    val <- toInt <$> (char ':' *> readNum)
    liftIO $ putStrLn $ "ds: " ++ show val
    modifyState (+val)

notDS :: MyVector -> MyParser ()
notDS mem = let ?mem = mem in do
    val <- readNum
    index <- getState
    liftIO $ putStrLn $ "read: " ++ show val
--     liftIO $ V.write mem index val
    liftIO $ write index val
    modifyState (+1)

readNum :: MyParser Int32
readNum = do
    str <- many1 digit  
    case readMaybe str of
        Just x -> return x
        Nothing -> fail "oopsie"
    
-- FIXME : is this what relocatable should do with the numbers it reads?
-- is DS-allocated space supposed to be in the relocation dict?
relocatable :: MyVector -> MyParser ()
relocatable mem = let ?mem = mem in do 
    index <- toInt <$> readNum 
    val <- liftIO $ V.read mem (index + baseAddr)
    liftIO $ write (index + baseAddr) (val + baseAddr)

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> ""

skipComments :: MyParser ()
skipComments = skipMany (spaces *> char '#' *> skipToEOL)
-- skipComment :: MyParser ()
-- skipComment = spaces *> char '#' *> skipToEOL
-- skipComment = char '#' *> skipToEOL

printVector :: Int -> Int -> MyVector -> IO ()
printVector i n vec = do
    let frozen :: IO (IM.Vector Int32)
        frozen = freeze $ V.slice i n vec
    lst <- toList <$> frozen
    putChar '['
    mapM_ putStr (intersperse ", " (show <$> lst))
    putStrLn "]"

{-
    --------------------------------- old parser begins here

    -- instructions :: MyParser (Integer, EntryPoint, [Token])
instructions :: MyParser (Integer, EntryPoint, [Token])
    instructions = do 
    res <- join <$> many (try instruction) `sepBy` skipJunk
    skipMany skipJunk *> eof
    (counter, entry, _) <- getState
return (counter, entry, res)

    instruction :: MyParser Token
    instruction = skipMany skipJunk *>
fmap Lit intOrChar <* loc (+1)
    <|> try asmEQU 
    <|> try asmDS 
    <|> try asmDW 
    <|> try asmEntry 
    <|> try asmExtern 
    <|> try asmPublic
    <|> try newLabel 
    <|> try opcode <* loc (+1)
    <|> label <* loc (+1)

skipJunk :: MyParser ()
skipJunk = skipSpaces <|> skipComment
-- skipJunk = spaces <|> skipComment

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

-}

