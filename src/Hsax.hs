{-# LANGUAGE CPP #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-} 
-- module Hsax where
-- import Prelude hiding (mapM_, words, error, fst, snd)
-- import Prelude hiding (mapM_, words, error)
import Prelude hiding (mapM_, words)
-- import System.IO (hGetContents)
import System.Environment (getArgs)
-- import System.Exit  (exitFailure)
-- import System.Time (getClockTime)
#if __GLASGOW_HASKELL__ > 709
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
#endif
-- import Text.ParserCombinators.Parsec (parse)
-- import Text.ParserCombinators.Parsec (ParseError)
import Text.Parsec hiding (labels)
-- import Text.Read (readMaybe)
import Control.Monad
import Control.Monad.Writer
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*), (<*>), (*>))
#endif
-- import Control.Applicative hiding ((<|>))

#if __GLASGOW_HASKELL__ < 706
import Data.Map ((!))
import qualified Data.Map as Map (showTree)
#else
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map (showTree)
#endif
import Data.List (intersperse)

import AsmParser
import AsmExprEval

main :: IO ()
main = do
    (filename, input) <- getFileData
--     let result :: (Either ParseError (Integer, String, [Token]), [(String, Integer)])
--     let result :: Either ParseError (Integer, String, [Token], [(String, Integer)])
    let result :: Either ParseError (Integer, EntryPoint, [Token], Labels)
        result = parseEverything filename input 
    case result of
        Left err -> putStrLn $ "HEY! NOPE!" ++ (show err)
        Right r -> outputResult filename r

outputResult :: FilePath -> (Integer, EntryPoint, [Token], Labels) -> IO ()
outputResult filename (textLength, entry, toks, labels) = do
    putStrLn "%SXX+Object Module"
    putStrLn $ "# object module for file: " ++ filename
#if __GLASGOW_HASKELL__ > 709
    -- defaultTimeLocale is not available in ghc 7.4, but is in 7.10
    getCurrentTime >>= putStrLn . ("# " ++) . formatTime defaultTimeLocale "%c"
#else
    putStrLn "# your ghc is quite old."
#endif
--     putStr "# " >> getClockTime >>= print -- requires System.Time
    putStrLn "# generated by hsax"
    putStrLn $ show textLength ++ " text length \n% text"

    putStrLn "# --------------------TOKENS--------------------"
    putStrLn $ "# " ++ show toks
    putStrLn "# -------------------- LABELS --------------------"
    putStr (unlines (map ("# "++) (lines (Map.showTree labels))))
    putStrLn "# -------------------- TEXT --------------------"

    let (text, metadata) = runWriter (unlines <$> mapM gen toks)
        (relocs, exts, pubs) = metadata :: ([Integer], [String], [String])
    putStrLn text

    putStrLn "# -------------------- METADATA --------------------"
    putStrLn $ "# relocs: " ++ show relocs
    putStrLn $ "# exts: " ++ show exts
    putStrLn $ "# pubs: " ++ show pubs

    putStrLn "% relocation dictionary"
    mapM_ print relocs

    putStrLn "% ENTRY, EXTERN, and PUBLIC references"
    unless (entry == EntryPoint "") $ putStrLn $ 
        "ENTRY " ++ show entry ++ " " ++ (show (labels ! show entry))
    mapM_ (putStrLn . ("EXTERN "++ )) exts -- FIXME addrs follow label
    forM_ pubs $ putStrLn . (\x -> "PUBLIC " ++ x ++ ' ':show (labels ! x))
    putStrLn "% end of object module"

    where
{-
the writer: ([Integer], [String], [String])
            [Integer] is the relocatable stuff
            [String] is the externs
            [String] is the publics
-}
        gen :: Token -> Writer ([Integer], [String], [String]) String
        gen (DW xs) = return $ concat $ intersperse "\n" (map show xs)
        gen (DS x) = return $ ':' : show x
        gen (Entry x) = return $ "# entry found: " ++ show x
        gen (Op x) = return $ show (fromEnum x) ++ " # " ++ show x
        gen (Lit x) = return $ show x
        gen (LitExpr str loc) = case runParser expr (labels) "eval" str of
            Right x -> do
                when (isRelocatable x) (addReloc loc)
                return $ show x
            Left err -> error $ "EXPLODE" ++ (show err) -- FIXME: i hope this never happens
        gen (NewLabel str) = return $ "# " ++ str
        gen (Label str loc) = let val = labels ! str in do
            -- (!) is unsafe, but should be fine here 
            -- because i used these very labels to populate the map 
            when (isRelocatable val) (addReloc loc)
            return $ show val ++ "     # label: " ++ str
        gen (EQU name val) = -- FIXME make this check relocatableness
            return ("# equ here: " ++ name ++ " = " ++ show val)
        gen (Extern names) = addExtern names >>
            return ("# extern here: " ++ (concat $ intersperse ", " $ names))
        gen (Public names) = addPublic names >>
            return ("# public here: " ++ (concat $ intersperse ", " $ names))



--------------------------------------------

isRelocatable :: Val -> Bool
isRelocatable (Rel _) = True
isRelocatable (Abs _) = False

addReloc :: Integer -> Writer ([Integer], [String], [String]) ()
addReloc x = tell ([x], [], [])

addExtern :: [String] -> Writer ([Integer], [String], [String]) ()
addExtern = mapM_ (\str -> tell ([], [str], []))

addPublic :: [String] -> Writer ([Integer], [String], [String]) ()
addPublic = mapM_ (\str -> tell ([], [], [str]))

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
