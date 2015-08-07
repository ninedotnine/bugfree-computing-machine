{-# LANGUAGE CPP #-} 
{-# OPTIONS_GHC -Wall #-} 

import System.Environment (getArgs)
import System.Exit 
import Data.List (sortBy)
import Data.Function (on)
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad
import Data.Traversable (mapAccumL)
import Data.Either (partitionEithers)
#if __GLASGOW_HASKELL__ < 706
import qualified Data.Map as Map
#else
import qualified Data.Map.Strict as Map
#endif
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Decommenter
import ObjParser
-- import ObjWriter
import ObjPrinter

main :: IO ()
main = do
    args <- getArgs
    inputs <- map decomment <$> mapM readFile args
    let zippy = zip args inputs
    let eithers :: [Either ParseError Info]
        eithers = map (\(fn, txt) -> pass1 fn 0 txt) zippy
    let (errors, rights) = partitionEithers eithers
    when (not (null errors)) $ mapM_ print errors >> exitFailure
    let infos :: [Info]
        (totalTextLength, infos) = mapAccumL adder' 0 rights
--         offsets' = mapAccumL (\x y -> (x+y, x+y)) 0 offsets
--         offsets' = mapAccumL adder 0 offsets
    let entries = filter hasEntry infos
    when (length entries > 1) $ putStrLn "multiple entries" >> exitFailure

    let infos' = map (adjustText . adjustExterns . adjustPublics) infos
    pubs <- combinePublics' infos'
    let extList = sortBy (compare `on` fst) $ concat $ externsToList <$> infos'
    when (doubles extList) $ putStrLn "multiple externs" >> exitFailure
    let infos'' = map (fixExterns pubs extList) infos'
    printInfos infos'' totalTextLength

-- this takes an info and returns a new info with its externs offset
adjustExterns :: Info -> Info
adjustExterns info = let 
    newExterns :: Externs
    newExterns = Map.map (map (+ (getOffset info))) (getExterns info)
    in info {getExterns = newExterns}

externsToList :: Info -> [(Integer, String)]
externsToList info = let
    exts :: [(String, [Integer])]
    exts = Map.toList $ getExterns info
    mapped :: [[(Integer, String)]]
    mapped = map f exts
    in concat mapped
    where 
        f :: (String, [Integer]) -> [(Integer, String)]
        f (str, xs) = (map ((flip (,)) str) xs)

-- doubles returns true if two tuples have the same first element
-- the input list must be sorted for this to work!
doubles :: [(Integer, String)] -> Bool
doubles [] = False
doubles (_:[]) = False
doubles (x:y:xs) = (fst x == fst y) || doubles (y:xs)
    

-- first Integer is the addr
-- second Integer is the ref
-- String is the symbol
-- type Pref = (Integer, Integer, String)


-- Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- (Offset -> b -> (Offset, Either ParseError Info)) -> Offset 
--                 -> [b] -> (Offset, [Either ParseError Info])
-- adder'' :: Offset -> b -> (Offset, Either ParseError Info)
-- adder'' x (fn, txt) = let info = pass1 fn x txt in (x+getOffset info, info)

-- adder :: Offset -> Offset -> (Offset, Offset)
-- adder = (\x y -> (x+y, x+y))

adder' :: Offset -> Info -> (Offset, Info)
adder' x (Info name off lc txt relocs entry pubs exts) = 
    (x + lc, Info name (x + off) lc txt relocs entry pubs exts)

combinePublics' :: [Info] -> IO Publics
combinePublics' infos = do
    let pubs = map getPublics infos
        maybePubs = foldM combinePublics Map.empty pubs
    case maybePubs of 
        Just xs -> return xs
        Nothing -> putStrLn "multiply defined publics" >> exitFailure

combinePublics :: Publics  -> Publics -> Maybe Publics
combinePublics xs ys = if Map.intersection xs ys == Map.empty 
    then Just $ Map.union xs ys
    else Nothing

-- this takes an info and returns a new info with its publics offset
adjustPublics :: Info -> Info
adjustPublics info = let 
    newPublics :: Publics
    newPublics = Map.map (+ (getOffset info)) (getPublics info)
    in info {getPublics = newPublics}

-- this takes an info and returns a new info with its text externified
fixExterns :: Publics -> [(Integer, String)] -> Info -> Info
fixExterns dict exts info = let
    newText :: [Val]
--     newText = f 0 (getText info) (map fst exts)
    newText = f (getOffset info) (getText info) exts
    in info {getText = newText}
    where
        f :: Integer -> [Val] -> [(Integer, String)] -> [Val]
        f _ text [] = text
        f count text ((loc,name):is) 
            | text == [] = text
            | loc == count = (Val (dict Map.! name)) 
                            : (f (count+1) (tail text) is)
            | loc > count = head text : f (count+1) (tail text) ((loc,name):is)
            | otherwise = text

-- this takes an info and returns a new info with its text values relocated
adjustText :: Info -> Info
adjustText info = let 
    newText :: [Val]
    newText = f 0 (getOffset info) (getText info) (getRelocs info)
    in info {getText = newText}
    where
        f :: Integer -> Integer -> [Val] -> Relocs -> [Val]
        f _ _ text [] = text
        f count off text (i:relocs) 
            | text == [] = error "text ran out before relocs" 
            | i == count = (addVal (head text) off) 
                            : (f (count+1) off (tail text) relocs)
            | i > count = head text : f (count+1) off (tail text) (i:relocs)
            | otherwise = error "count too high" 

addVal :: Val -> Integer -> Val
addVal (Val x) y = Val (x + y)
addVal (DS _) _ = error "DS in addval, tried to relocate a DS"

-- badformat :: IO ()
-- badformat = putStrLn "hsxxl: bad format" >> exitFailure
