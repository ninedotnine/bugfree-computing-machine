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
import Control.Monad
import Control.Monad.Writer
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
--     let result :: (Either ParseError (Integer, String, [Token]), [(String, Integer)])
--     let result :: Either ParseError (Integer, String, [Token], [(String, Integer)])
    let result :: Either ParseError (Integer, EntryPoint, [Token], Map String Integer)
        result = parseEverything filename input 
--     putStrLn "GENERATED BY HSAX LOL"
--     print result
    case result of
        Left error -> putStrLn $ "HEY! " ++ (show error)
        Right r -> outputResult filename r
--         Right r -> print r

outputResult :: FilePath -> (Integer, EntryPoint, [Token], Map String Integer) -> IO ()
outputResult filename (textLength, entry, toks, labels) = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: " ++ filename
    putStr "# " >> getClockTime >>= print
    putStrLn $ show textLength ++ " text length \n% text"

    putStrLn "# --------------------TOKENS--------------------"
    putStrLn $ "# " ++ show toks
    putStrLn "# -------------------- LABELS --------------------"
--     putStr (Map.showTree labels)
    putStr (unlines (map ("# "++) (lines (Map.showTree labels))))
    putStrLn "# -------------------- TEXT --------------------"

    let (text, metadata) = runWriter (gen' toks)
        (relocs, exts, pubs) = metadata :: ([Integer], [String], [String])
--     mapM_ (putStrLn . gen) toks
    putStrLn text
    
    putStr "# METADATA >>>>>>>>>>>>>>>>>> "
    print metadata

    putStrLn "% relocation dictionary"
    mapM_ print relocs

    putStrLn "% ENTRY, EXTERN, and PUBLIC references"
    writeEntry
    mapM_ (putStrLn . ("EXTERN "++ )) exts
    mapM_ (putStrLn . ("PUBLIC"++)) pubs
    putStrLn "% end of object module"

    where
{-
the writer: ([Integer], [String], [String])
            [Integer] is the relocatable stuff
            [String] is the externs
            [String] is the publics
-}
        gen' :: [Token] -> Writer ([Integer], [String], [String]) String
        gen' toksies = do
            lins <- mapM gen toksies
            return (unlines lins)
--         gen :: Token -> Writer ([Integer] String 
        gen :: Token -> Writer ([Integer], [String], [String]) String
        gen (DW xs) = return $ concat $ intersperse "\n" (map show xs)
        gen (DS x) = return $ ':' : show x
        gen (Entry x) = return $ "# entry found: " ++ show x
        gen (Op x) = return $ show (fromEnum x) ++ " # " ++ show x
        gen (Lit x) = return $ show x
        gen (NewLabel str) = return $ "# " ++ str
        gen (Label str loc) = do
--             tell ([loc], [], [])
            addReloc loc
            return $ show (labels ! str) ++ "     # label: " ++ str
        gen (EQU name val) = return $ "# equ here: " ++ name ++ 
                                                    " = " ++ show val
        gen (Extern name) = do
            mapM_ addExtern name
            return $ "# extern here: " ++ show name
        gen (Public name) = do
            addPublic name
            return $ "# public here: " ++ name
            -- (!) is unsafe, but should be fine here 
            -- because i used these very labels to populate the map 
        writeEntry :: IO ()
        writeEntry = unless (entry == EntryPoint "") $ 
            putStrLn $ "ENTRY " ++ show entry ++ " " ++ (show (labels ! show entry))

--------------------------------------------

addReloc :: Integer -> Writer ([Integer], [String], [String]) ()
addReloc x = tell ([x], [], [])

addExtern :: String -> Writer ([Integer], [String], [String]) ()
addExtern str = tell ([], [str], [])

addPublic :: String -> Writer ([Integer], [String], [String]) ()
addPublic str = tell ([], [], [str])

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
