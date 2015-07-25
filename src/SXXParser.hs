-- {-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
-- {-# OPTIONS_GHC -Wall #-} 
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

-- module Parser (Instruction(..), 
--                 Token(..), 
--                 EntryPoint(..),
--                 parseEverything) where 

module SXXParser (MyVector, populateVector, printVector) where


import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
import Data.Vector.Generic hiding ((++), Vector, mapM_)
import qualified Data.Vector.Unboxed as IM
import Data.Vector.Unboxed.Mutable (new, write, MVector)
import qualified Data.Vector.Unboxed.Mutable as V
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

type MyVector = MVector (PrimState IO) Int32

baseAddr :: Int
baseAddr = 16 -- the base address must be bigger than 15

populateVector :: MyVector -> String -> IO ()
-- populateVector  = undefined
populateVector mem input = do
--     print "dumb"
--     let result :: Either ParseError (Int16)
    result <- runParserT (fillVector mem) baseAddr "input" input
    case result of
        Left err -> putStrLn ("error: " ++ (show err)) >> exitFailure
        Right r -> putStrLn "successful parse" 
--     print result

-- fillVector :: MyParser ()
fillVector :: MyVector -> MyParser ()
fillVector mem = do 
    header
    textLength <- readNum <* skipToEOL
    liftIO $ putStrLn $ "textlength: " ++ show textLength 
    entry <- readNum <* skipToEOL
    liftIO $ putStrLn $ "entry: " ++ show entry 
    percentSeparator
--     instruction mem `sepBy` skipSpaces
    instruction mem `endBy` skipSpaces
    percentSeparator
    relocatable `endBy` skipSpaces
    percentSeparator
    eof
    lastInstruction <- getState 
    liftIO $ putStr "instruction space: " 
    liftIO $ printVector baseAddr (lastInstruction - baseAddr) mem
    liftIO $ unless (fromIntegral textLength == lastInstruction - baseAddr) $ 
        putStrLn ("textLength: " ++ show textLength ++ " but actually " ++
            show (lastInstruction - baseAddr)) >> exitFailure

header :: MyParser ()
header = string "%SXX+E" >> skipToEOL >> spaces

percentSeparator :: MyParser ()
percentSeparator = char '%' >> skipToEOL

traceM :: (Monad m) => String -> m ()
traceM str = trace str $ return ()

testfile :: FilePath
testfile = "programs/testfile6"

main :: IO ()
main = undefined
{-
    c <- readFile testfile
    let result :: Either ParseError (Integer, EntryPoint, [Token], Labels)
        result = parseEverything testfile c
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r
    putStrLn "okay"
    -}

skipSpaces :: MyParser ()
skipSpaces = many1 space >> return ()

instruction :: MyVector -> MyParser ()
instruction mem = notDS mem <|> sxxDS mem

sxxDS :: MyVector -> MyParser ()
sxxDS mem = do 
    val <- char ':' *> readNum
    liftIO $ putStrLn $ "ds: " ++ show val
    modifyState (+ (fromIntegral val))

notDS :: MyVector -> MyParser ()
notDS mem = do
    val <- readNum
    index <- getState
    liftIO $ putStrLn $ "read: " ++ show val
    liftIO $ write mem index val
    modifyState (+1)
    return ()

readNum :: MyParser Int32
-- readNum = read <$> many1 digit 
readNum = do
    str <- readMaybe <$> many1 digit  
--     liftIO $ print str
    case str of
        Just x -> return x
        Nothing -> fail "oopsie"
    
-- FIXME : what should relocatable do with the numbers it reads?
relocatable :: MyParser ()
relocatable = readNum >> return ()


{-
parseEverything :: SourceName 
        -> String 
        -> Either ParseError (Integer, EntryPoint, [Token], Labels)
parseEverything name str = do
    let eith :: Either ParseError (Integer, EntryPoint, [Token])
        (eith, labels) = parseEverything' name str
    case eith of
        Left err -> Left err
        Right (i, m, xs) -> return (i, m, xs, labels)

parseEverything' :: SourceName -> String 
        -> (Either ParseError (Integer, EntryPoint, [Token]), Labels)
parseEverything' = (runWriter .) . runParserT instructions (0, "", "")
-}


{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Int is the state, it is the current index of the vector
IO is the transformed monad.
-}

type MyParser a = ParsecT String Int IO a

newtype EntryPoint = EntryPoint String deriving (Eq)

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

    --------------------------------- parser begins here

    -- instructions :: MyParser (Integer, EntryPoint, [Token])
    {-
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

skipComment :: MyParser ()
skipComment = spaces *> char '#' *> skipToEOL
-- skipComment = char '#' *> skipToEOL
-}

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space

-----------------------------------------

printVector :: Int -> Int -> MyVector -> IO ()
printVector i n vec = do
    let frozen :: IO (IM.Vector Int32)
        frozen = freeze $ V.slice i n vec
    lst <- toList <$> frozen
    putChar '['
    mapM_ putStr (intersperse ", " (show <$> lst))
    putStrLn "]"
