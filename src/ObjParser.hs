-- {-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE TypeSynonymInstances #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# OPTIONS_GHC -Wall #-} 
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module ObjParser  where


-- import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
import Data.Functor.Identity
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
-- import Control.Monad.Trans (liftIO)
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
import Data.String (IsString, fromString)
-- import Data.Int
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
-- import Data.Map.Strict as Map (Map, singleton)
import qualified Data.Map.Strict as Map 

-- import Debug.Trace (trace)

-- import Instructions

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
--     let result :: Either ParseError Integer
    let result :: Either ParseError Info
        result = runParser pass1 (makeInfo "#MAIN0" 0) "namey" contents
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) 
                    ++ "\ncontnts:\n" ++ contents
        Right r -> print r
    putStrLn "okay"

-- pass1 :: MyParser Integer
pass1 :: MyParser Info
pass1 = do 
    header 
    text_length <- readNum <* skipToEOL
    percentSeparator
    readText
    lenth <- getLineCount <$> getState
    when (lenth /= text_length) $ fail "bad length"
    percentSeparator
    relocDict
--     return text_length
    percentSeparator
    eep -- Entry, Externs, Publics
    percentSeparator 
    eof
    state <- getState
    return state

readText :: MyParser ()
readText = skipMany (dw <|> instruction)

dw :: MyParser ()
dw = do
    val <- char ':' *> readNum <* skipToEOL
    increaseLineCount val

instruction :: MyParser ()
instruction = readNum *> skipToEOL >> increaseLineCount 1

relocDict :: MyParser ()
-- relocDict = many reloc >> return ()
relocDict = skipMany reloc 

reloc :: MyParser ()
reloc = do
    val <- readNum <* skipToEOL
    info <- getState
    putState $ addReloc val info

eep :: MyParser ()
eep = skipMany (entry <|> extern <|> public)

-- FIXME : this breaks if the entry label is not "main"
entry :: MyParser ()
entry = do 
    info <- getState
    when (getEntry info /= Nothing) $ fail "multiple ENTRY"
    ent <- string "ENTRY main " *> readNum <* skipToEOL
    putState $ setEntry (Just ent) info


extern :: MyParser ()
-- extern = undefined
extern = do
    string "EXTERN " *> skipToEOL

public :: MyParser ()
public = string "PUBLIC " *> skipToEOL
    
-- increaseLineCount :: (Integer -> Integer) -> MyParser ()
increaseLineCount :: Integer -> MyParser ()
-- modifyLineCount f = modifyState f
increaseLineCount x = do 
    infos <- getState
    putState $ modifyLineCount (+x) infos

    
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
Info is the state
Identity is the transformed monad.
-}

-- type MyParser a = ParsecT String (Map.Map uuuInteger Identity a
-- type MyParser a = ParsecT String ((Map.Map String Integer, Integer) Identity a
-- type MyParser a = ParsecT String Integer Identity a

type MyParser a = ParsecT String Info Identity a

{-
Info is a type 
String is the file name
Integer is the offset
Integer is the line counter
Maybe Integer is the entry
Integer is the is the line counter
Identity is the transformed monad.
-}

-- FIXME : does Info really need the name and offset? 
data Info = Info { getName      :: String,
                   getOffset    :: Offset,
                   getLineCount :: Integer,
                   getRelocs    :: Relocs,
                   getEntry     :: Maybe Integer,
                   getPublics   :: Publics,
                   getExterns   :: Externs }
                   deriving (Show)

emptyInfo :: Info
emptyInfo = Info "" 0 0 [] Nothing Map.empty Map.empty

makeInfo :: String -> Offset -> Info
makeInfo name off = Info name off 0 [] Nothing Map.empty Map.empty

modifyLineCount :: (Integer -> Integer) -> Info -> Info
modifyLineCount f (Info s o lc r e pubs exts) = Info s o (f lc) r e pubs exts

addReloc :: Integer -> Info -> Info
addReloc x (Info s o lc r e pubs exts) = Info s o lc (r++[x]) e pubs exts

setEntry :: Maybe Integer -> Info -> Info
setEntry e (Info s o lc r Nothing pubs exts) = Info s o lc r e pubs exts
setEntry _ _ = error "setEntry called when entry was not Nothing" 

type Relocs = [Integer]

type Publics = Map.Map String Integer
type Externs = Map.Map String [Integer]

type Offset  = Integer

newtype EntryPoint = EntryPoint String deriving (Eq)

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space
