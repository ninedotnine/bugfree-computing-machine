{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module ObjWriter (pass2) where 

import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
-- import Control.Monad.Identity
-- import Control.Monad.Writer (Writer, runWriter, tell, lift)
import Control.Monad.Writer 
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many, optional)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
-- import Data.Char (toUpper, toLower, ord)
-- import Data.List (genericLength)
-- import Data.String (IsString, fromString)
-- import Data.Foldable (traverse_)
-- import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
import Data.Map.Strict as Map (Map, singleton, fromList)

import Debug.Trace (trace)
-- import Control.Exception

-- import Instructions
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

-- this is how i think the Info that is passed in will look
expected :: Info
expected = Info "writes.out" 0 15 [5, 11] Nothing 
    (Map.fromList [("write_string",0)]) (Map.fromList [])

main :: IO ()
main = do
    putStrLn $ "# file: " ++ testfile
    c <- readFile testfile
    let result :: Either ParseError String
        result = pass2 testfile expected c
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) ++ "\n########\n" ++ c
        Right r -> putStrLn r 
    putStrLn "FIN"

pass2 :: SourceName -> Info -> String -> Either ParseError String
pass2 name info str = do
    let eith :: Either ParseError Bool
        output :: String
        (eith, output) = runWriter $ runParserT instructions info name str
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
    info <- getState
    header >> skipComments
    gen "%SSX+Executable\n"

    text_length <- readNum <* skipToEOL <* skipComments 
    when (text_length /= getLineCount info) $ fail $ "length wrong"
    gen $ show (getLineCount info) ++ " text length\n"

    case getEntry info of
        Just x -> gen $ (show x) ++ " ENTRY\n"
        Nothing -> gen "0 ENTRY (default)\n"
    percentSeparator "text" >> skipComments

    let relocs = getRelocs info
        off = getOffset info

    putState $ modifyLineCount (*0) info -- set linecount to zero
    readText off relocs
    final_linecount <- getLineCount <$> getState
    when (text_length /= final_linecount) $ fail $ "problem with length"

    gen "\n" >> percentSeparator "relocation dictionary" 
    relocDict relocs
    gen "\n" >> percentSeparator "eof!"

    return True 

header :: MyParser ()
header = string "%SXX+O" >> skipToEOL >> spaces

readText :: Offset -> Relocs -> MyParser ()
readText _ [] = skipMany (dw <|> instruction)
readText off (r:relocs) = do 
    lineCount <- getLineCount <$> getState
    when (lineCount > r) $ fail $ "external fixup address " ++ show r ++ 
        "does not match object module location"
    if (lineCount == r) 
        then (dw <|> instruction) >> readText off relocs
        else (dw <|> instruction) >> readText off (r:relocs)

dw :: MyParser ()
dw = dwOffset 0

dwOffset :: Offset -> MyParser ()
dwOffset off = do
    val <- char ':' *> readNum <* skipToEOL
    gen ('\n' : show (val+off))
    increaseLineCount val

instruction :: MyParser ()
instruction = readNum >>= (gen . (('\n':) . show)) 
    >> skipToEOL >> increaseLineCount 1

increaseLineCount :: Integer -> MyParser ()
increaseLineCount x = do 
    infos <- getState
    putState $ modifyLineCount (+x) infos

-- here, i could assert that the numbers i'm reading are the same as in my list
-- but they're the same ones i read in earlier, so what's the point?
relocDict :: Relocs -> MyParser ()
relocDict relocs = do 
    forM relocs $ gen . (('\n':) . show)
    skipMany (readNum >> skipToEOL) 

readNum :: MyParser Integer -- read is safe here: (many1 digit) is readable
readNum = sign >>= (read <$> many1 digit >>=) . (return .)

sign :: MyParser (Integer -> Integer)
sign = char '-' *> return negate <|> optional (char '+') *> return id

percentSeparator :: String -> MyParser ()
percentSeparator str = char '%' >> skipToEOL >> gen ("% " ++ str)

skipSpaces :: MyParser ()
skipSpaces = many1 space >> return ()

skipComments :: MyParser ()
skipComments = skipMany (spaces *> char '#' *> skipToEOL)

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space

gen :: String -> ParsecT String Info (Writer String) () 
gen = lift . tell
