{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE FlexibleContexts #-} 
-- {-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

-- module ObjPrinter (pass2) where 
module ObjPrinter  where 


import System.Environment (getArgs)
import System.Exit 
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad
import Data.Traversable (mapAccumL)
import Data.Either (partitionEithers)
-- import qualified Data.Map as Map

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
import Data.List (intersperse)
import Data.Maybe (fromJust)
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

printInfos :: [Info] -> Integer -> IO ()
printInfos infos textlength = do
    let entries = filter hasEntry infos
--     putStr "## ENTRIES: ## " >> print entries
    when (length entries > 1) $ putStrLn "multiple entries" >> exitFailure

    putStrLn "%SXX+Executable"
    putStrLn $ show textlength ++ " text length"
    if null entries 
        then putStrLn "0 ENTRY (default)"
        else let e = show (fromJust (getEntry (head entries))) 
            in putStrLn $ e ++ " ENTRY" -- fromJust is safe here

    let relocs :: [Integer]
        -- do these need to be adjusted for offsets? 
        -- do they need to be sorted? 
        relocs = concatMap getRelocs infos

    putStrLn "% text"
--     mapM_ printText infos
    forM_ infos $ \info -> do
        putStrLn $ getName info
        putStrLn $ ("# offset: " ++) $ show $ getOffset info
        printText (getText info) (getOffset info) 
            (dropWhile (< getOffset info) relocs) (getOffset info)
        

    putStrLn "% relocation dictionary"
    mapM_ print relocs
    putStrLn "% eof!!"

-- printAllText :: [Info] -> Relocs -> IO ()
-- printAllText infos relocs = forM_ infos

printText :: [Val] -> Integer -> Relocs -> Offset -> IO ()
printText text _ [] _ = putStrLn $ concat $ intersperse "\n" $ map show text
printText text count (r:relocs) offset = do
--     putStrLn $ concat $ intersperse "\n" $ map show text
    when (count > r) (putStrLn ("external fixup address " ++ show r ++ 
        " does not match object module location") >> exitFailure)
    let val = head text
    if count == r
        then print (getVal val + offset)
            >> printText (tail text) ((addCount val) count) relocs offset
        else print val
            >> printText (tail text) ((addCount val) count) (r:relocs) offset

getVal :: Val -> Integer
getVal (Val x) = x
getVal (DS x) = x

addCount :: Val -> Integer -> Integer
addCount (Val _) = (+1)
addCount (DS x) = (+x)

hasEntry :: Info -> Bool
hasEntry (Info _ _ _ _ _ (Just _) _ _) = True
hasEntry _ = False
