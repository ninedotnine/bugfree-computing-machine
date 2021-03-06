{-# LANGUAGE CPP #-} 
-- {-# LANGUAGE OverloadedStrings #-} 
-- {-# LANGUAGE TypeSynonymInstances #-} 
-- {-# LANGUAGE FlexibleInstances #-} 
-- {-# OPTIONS_GHC -Wall #-} 
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

-- module ObjParser (pass1, Info(..), makeInfo, emptyInfo) where
module ObjParser (
                pass1, 
                Info(..), 
                Offset,
                Val(..),
                Relocs,
                Publics,
                Externs,
                makeInfo, 
                emptyInfo,
                modifyLineCount
                ) where


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
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many, optional)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
-- import Data.Char (toUpper, toLower, ord)
-- import Data.List (genericLength, intersperse)
-- import Data.String (IsString, fromString)
-- import Data.Int
-- import Data.Foldable (traverse_)
-- import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
-- import Data.Map.Strict as Map (Map, singleton)
#if __GLASGOW_HASKELL__ < 706
import qualified Data.Map as Map 
#else
import qualified Data.Map.Strict as Map 
#endif

-- import Debug.Trace (trace)

-- import Instructions

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Info is the state
Identity is the transformed monad.
-}

type MyParser a = ParsecT String Info Identity a

{-
Info is a data type 
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
                   getText      :: [Val],
                   getRelocs    :: Relocs,
                   getEntry     :: Maybe Integer,
                   getPublics   :: Publics,
                   getExterns   :: Externs }
                   deriving (Show)

data Val = Val Integer
         | DS Integer
         deriving (Eq)
instance Show Val where
    show (Val x) = show x
    show (DS x) = ':' : show x

type Relocs = [Integer]

type Publics = Map.Map String Integer
type Externs = Map.Map String [Integer]

type Offset  = Integer

-- traceM :: (Monad m) => String -> m ()
-- traceM str = trace str $ return ()

testfile :: FilePath
-- testfile = "../object_files/test.out"
-- testfile = "../lib/writes.out"
testfile = "../lib/reads.out"

main :: IO ()
main = do
    contents <- readFile testfile
    let result :: Either ParseError Info
        result = pass1 "dumfile.out" 0 contents
    putStr "result is: " >> print result
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) 
                    ++ "\ncontnts:\n" ++ contents
        Right r -> print r
    putStrLn "okay"

pass1 :: String -> Offset -> String -> Either ParseError Info
pass1 name off input = 
    runParser readObjectFile (makeInfo ('#': name) off) name input

readObjectFile :: MyParser Info
readObjectFile = do 
    header >> skipComments 
    text_length <- readNum <* skipToEOL <* skipComments 
    percentSeparator >> skipComments 
    readText
    lenth <- getLineCount <$> getState
    when (lenth /= text_length) $ fail "bad length"
    percentSeparator
    relocDict
    percentSeparator
    eep -- Entry, Externs, Publics
    percentSeparator 
    eof
    state <- getState
    return state

header :: MyParser ()
header = string "%SXX+O" >> skipToEOL >> spaces

percentSeparator :: MyParser ()
percentSeparator = char '%' >> skipToEOL

readText :: MyParser ()
readText = skipMany (ds <|> instruction)

ds :: MyParser ()
ds = do
    val <- char ':' *> readNum <* skipToEOL
    info <- getState
    putState $ addInstruction (DS val) info
    increaseLineCount val

instruction :: MyParser ()
instruction = do 
    num <- readNum 
    info <- getState
    putState $ addInstruction (Val num) info
    skipToEOL >> increaseLineCount 1

increaseLineCount :: Integer -> MyParser ()
increaseLineCount x = do 
    infos <- getState
    putState $ modifyLineCount (+x) infos

relocDict :: MyParser ()
relocDict = skipMany $ do 
    val <- readNum <* skipToEOL
    info <- getState
    putState $ addReloc val info
--     readNum <* skipToEOL >>= (getState >>= ) . (putState .) . addReloc

eep :: MyParser ()
eep = skipMany (try entry <|> extern <|> public)

-- FIXME : this discards the name of the entry. is that okay? 
entry :: MyParser ()
entry = do 
    ent <- string "ENTRY " *> label *> skipSpaces *> readNum <* skipToEOL
    info <- getState
    when (getEntry info /= Nothing) $ fail "multiple ENTRY"
    putState $ setEntry (Just ent) info


extern :: MyParser ()
extern = do
    name <- string "EXTERN " *> label 
    skipSpaces 
    addrs <- readNum `endBy1` skipSpaces
    infos <- getState
    let pubs = getPublics infos
        exts = getExterns infos
    when (Map.member name pubs || Map.member name exts) $
        fail $ "label " ++ name ++ " is already PUBLIC or EXTERN"
    putState (addToExterns infos name addrs)

public :: MyParser ()
public = do 
    name <- string "PUBLIC " *> label
    addr <- skipSpaces *> readNum <* skipToEOL
    infos <- getState
    let pubs = getPublics infos
        exts = getExterns infos
    when (Map.member name pubs || Map.member name exts) $
        fail $ "label " ++ name ++ " is already PUBLIC or EXTERN"
    putState (addToPublics infos name addr)

label :: MyParser String
label = do
    first <- letter
    rest <- many labelChar
    return (first:rest)

labelChar :: MyParser Char
labelChar = letter <|> digit <|> oneOf "._"

skipSpaces :: MyParser ()
skipSpaces = many1 space >> return ()

skipComments :: MyParser ()
skipComments = skipMany (spaces *> char '#' *> skipToEOL)

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space

readNum :: MyParser Integer -- read is safe here: (many1 digit) is readable
readNum = sign >>= (read <$> many1 digit >>=) . (return .)

sign :: MyParser (Integer -> Integer)
sign = char '-' *> return negate <|> optional (char '+') *> return id

----------------------------------------

-- operations on the Info type
emptyInfo :: Info
emptyInfo = Info "" 0 0 [] [] Nothing Map.empty Map.empty

makeInfo :: String -> Offset -> Info
makeInfo name off = Info name off 0 [] [] Nothing Map.empty Map.empty

modifyLineCount :: (Integer -> Integer) -> Info -> Info
modifyLineCount f (Info s o lc txt r e pubs exts) = Info s o (f lc) txt r e pubs exts

addInstruction :: Val -> Info -> Info
addInstruction x (Info s o lc txt r e pubs exts) = 
    Info s o lc (txt ++ [x]) r e pubs exts

addReloc :: Integer -> Info -> Info
addReloc x (Info s o lc txt r e pubs exts) = Info s o lc txt (r++[x]) e pubs exts

setEntry :: Maybe Integer -> Info -> Info
setEntry e (Info s o lc txt r Nothing pubs exts) = Info s o lc txt r e pubs exts
setEntry _ _ = error "setEntry called when entry was not Nothing" 

addToPublics :: Info -> String -> Integer -> Info
addToPublics (Info s o lc txt r e pubs exts) name addr = 
    Info s o lc txt r e (Map.insert name addr pubs) exts

addToExterns :: Info -> String -> [Integer] -> Info
addToExterns (Info s o lc txt r e pubs exts) name addr = 
    Info s o lc txt r e pubs (Map.insert name addr exts)
