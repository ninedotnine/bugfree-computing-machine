{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module SXXParser (MyVector, populateVector, printVector) where

import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
import Data.Vector.Generic hiding ((++), Vector, mapM_)
import qualified Data.Vector.Unboxed as IM (Vector)
import qualified Data.Vector.Unboxed.Mutable as V (read, slice)
import Control.Monad (when)
import Control.Monad.Trans (liftIO)
import Control.Applicative hiding (many, optional)
import Data.List (intersperse)
import Data.Int (Int32, Int16)

import SXXVector

#if __GLASGOW_HASKELL__ > 705
import Text.Read (readMaybe)
#else
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
#endif

-- import Debug.Trace (traceM)

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
Int is the state, it is the current index of the vector
IO is the transformed monad.
-}

type MyParser a = ParsecT String Int IO a

baseAddr :: Integral a => a
baseAddr = 16 -- the base address must be bigger than 15

populateVector :: MyVector -> String -> IO Int16
populateVector mem input = do
--     let result :: Either ParseError ()
    result <- runParserT (fillVector mem) baseAddr "input" input
    case result of
        Left err -> putStrLn ("error: " ++ (show err)) >> exitFailure
        Right entry -> return entry

-- if successful, returns the entry point
fillVector :: MyVector -> MyParser Int16
fillVector mem = do 
    header
    textLength <- natural <* skipToEOL
    entry <- natural <* skipToEOL 
    percentSeparator
    instruction mem `endBy` (try skipComments <|> skipSpaces)
    lastInstruction <- getState 
    let counted = lastInstruction - baseAddr
    when (toInt textLength /= counted) $
        fail ("textLength " ++ show textLength ++ " counted " ++ show counted)
    percentSeparator
    relocatable mem `endBy` skipSpaces *> percentSeparator
    eof
    return (fromIntegral entry)

header :: MyParser ()
header = string "%SXX+E" >> skipToEOL >> spaces >> skipComments

percentSeparator :: MyParser ()
percentSeparator = char '%' >> skipToEOL >> skipComments

instruction :: MyVector -> MyParser ()
instruction mem = notDS mem <|> sxxDS

sxxDS :: MyParser ()
sxxDS = do 
    val <- toInt <$> (char ':' *> natural)
    liftIO $ putStrLn $ "ds: " ++ show val
    modifyState (+val)

notDS :: MyVector -> MyParser ()
notDS mem = let ?mem = mem in do
    val <- integer
    index <- getState
    liftIO $ write index val
    modifyState (+1)

integer :: MyParser Int32
integer = sign <*> natural
    where
        sign :: MyParser (Int32 -> Int32)
        sign = char '-' *> return negate <|> optional (char '+') *> return id

-- parses a non-negative number
natural :: MyParser Int32
natural = do
    str <- many1 digit  
    case readMaybe str of
        Just x -> return x
        Nothing -> fail "oopsie"
    
-- FIXME : is this what relocatable should do with the numbers it reads?
-- is DS-allocated space supposed to be in the relocation dict?
relocatable :: MyVector -> MyParser ()
relocatable mem = let ?mem = mem in do 
    index <- toInt <$> natural 
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
