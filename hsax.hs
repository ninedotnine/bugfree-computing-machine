{-# OPTIONS_GHC -Wall #-} 
-- module Hsax where
-- import Prelude hiding (mapM_, words, error, fst, snd)
import Prelude hiding (mapM_, words, error)
-- import System.IO (hGetContents)
import System.Environment (getArgs)
-- import System.Exit  (exitFailure)
import System.Time (getClockTime)
-- import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec (ParseError)
-- import Text.Parsec.Prim (runParserT)
-- import Control.Monad.Writer (runWriter)
import Control.Monad
-- import Control.Monad
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
-- import Text.Read 

-- import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map (showTree)
-- import Data.Maybe (fromJust)
import Data.List (intersperse)

import Parser

main :: IO ()
main = do
    (filename, input) <- getFileData
--     let result :: (Either ParseError (Integer, String, [Word]), [(String, Integer)])
--     let result :: Either ParseError (Integer, String, [Word], [(String, Integer)])
    let result :: Either ParseError (Integer, EntryPoint, [Word], Map String Integer)
        result = parseEverything filename input 
--     putStrLn "GENERATED BY HSAX LOL"
--     print result
    case result of
        Left error -> putStrLn $ "HEY! " ++ (show error)
        Right r -> outputResult filename r
--         Right r -> print r

outputResult :: FilePath -> (Integer, EntryPoint, [Word], Map String Integer) -> IO ()
outputResult filename (textLength, entry, words, labels) = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: " ++ filename
    putStr "# " >> getClockTime >>= print
    putStrLn $ show textLength ++ " text length \n% text"
    mapM_ (putStrLn . prettyPrint) words

    putStrLn "% relocation dictionary"
    putStrLn "LOL FIXME RELOCDICT"

    putStrLn "% ENTRY, EXTERN, and PUBLIC references"
    writeEntry
    putStrLn "LOL FIXME EXTERN AND PUBLIC"
    putStrLn "% end of object module"

    putStrLn "-------------------- LABELS --------------------"
    putStr (Map.showTree labels)
    where
        prettyPrint :: Word -> String
        prettyPrint (DW xs) = concat $ intersperse "\n" (map show xs)
        prettyPrint (DS x) = ':' : show x
        prettyPrint (Entry x) = "# entry found: " ++ show x
        prettyPrint (Op x) = show (fromEnum x) ++ " # " ++ show x
        prettyPrint (Lit x) = show x
        prettyPrint (NewLabel str) = "# " ++ str
        prettyPrint (Label str) = show (labels ! str)
            -- (!) is unsafe, but should be fine here 
            -- because i used these very labels to populate the map 
        writeEntry :: IO ()
        writeEntry = unless (entry == EntryPoint "") $ 
            putStrLn $ "ENTRY " ++ show entry ++ " " ++ (show (labels ! show entry))

--------------------------------------------

getFileData :: IO (FilePath, String)
getFileData = getArgs >>= \args -> if length args < 1
    then do 
        contents <- getContents
--         putStrLn "no file name provided"
        return ("stdin", contents)
    else let name = head args in do 
        contents <- readFile name
        return (name, contents)

-- getFilename :: IO String
{-
getFilename = do 
    args <- getArgs
    if length args < 1
        then putStrLn "no file name provided" >> exitFailure
        else return (head args)
            -}
{-
getFilename = getArgs >>= \args -> if length args < 1 
    then putStrLn "no file name provided" >> return "stdin"
    else return (head args)
-}