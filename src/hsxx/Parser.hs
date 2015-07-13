-- {-# LANGUAGE OverloadedStrings #-} 
-- {-# OPTIONS_GHC -Wall #-} 
-- {-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

-- module Parser (Instruction(..), 
--                 Token(..), 
--                 EntryPoint(..),
--                 parseEverything) where 

module Parser (MyVector, populateVector) where


import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
import Data.Vector.Unboxed.Mutable (new, write, MVector)
import qualified Data.Vector.Unboxed.Mutable as V
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
import Control.Monad.Primitive (PrimState)
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
import Data.Int
-- import Data.Foldable (traverse_)
import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
import Data.Map.Strict as Map (Map, singleton)

import Debug.Trace (trace)

import Instructions

type MyVector = MVector (PrimState IO) Int32

populateVector :: MyVector -> String -> IO ()
-- populateVector  = undefined
populateVector mem input = do
--     print "dumb"
--     let result :: Either ParseError (Int16)
    result <- runParserT fillVector 16 "input" input -- start filling instructions at 16
    case result of
        Left err -> putStrLn ("error: " ++ (show err)) >> exitFailure
        Right r -> putStrLn "successful parse"
--     print result
    
-- fillVector :: MyParser ()
fillVector :: ParsecT String Int16 IO ()
fillVector = do 
    string "%SXX-E" >> skipToEOL
--     res <- join <$> many (try instruction) `sepBy` skipJunk
--     skipMany skipJunk *> eof
--     (counter, entry, _) <- getState
--     return (counter, entry, res)

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
Int16 is the state, it is the current index of the vector
IO is the transformed monad.
-}
type MyParser a = ParsecT String Int16 IO a

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
