-- {-# OPTIONS_GHC -Wall #-} 

import System.Environment (getArgs)
import System.Exit 
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad
import Data.Traversable (mapAccumL)
import Data.List
import Data.Either (partitionEithers)
#if __GLASGOW_HASKELL__ < 706
import qualified Data.Map as Map
#else
import qualified Data.Map.Strict as Map
#endif

import Decommenter
import ObjParser
-- import ObjWriter
import ObjPrinter

main :: IO ()
main = do
    args <- getArgs
    inputs <- map decomment <$> mapM readFile args
--     (filename, input) <- getFileData
--     mapM_ (print . pass1 
    let zippy = zip args inputs
--     forM_ zippy $ print . (\(fn, txt) -> pass1 fn 0 txt)
    let eithers :: [Either ParseError Info]
        eithers = map (\(fn, txt) -> pass1 fn 0 txt) zippy
--     mapM_ print eithers
    let (errors, rights) = partitionEithers eithers
    when (not (null errors)) $ mapM_ print errors >> exitFailure
    let infos :: [Info]
        (totalTextLength, infos) = mapAccumL adder' 0 rights
--         offsets' = mapAccumL (\x y -> (x+y, x+y)) 0 offsets
--         offsets' = mapAccumL adder 0 offsets
    let entries = filter hasEntry infos
--     putStr "## ENTRIES: ## " >> print entries
    when (length entries > 1) $ putStrLn "multiple entries" >> exitFailure

--     putStrLn  "++++++++++++++++++++++++++++++++++++++++++"
--     mapM_ print infos

--     putStrLn  "**********************************************"
--     pubs <- combinePublics' infos
--     print pubs

--     putStrLn  "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
    printInfos (map (adjustText . adjustPublics) infos) totalTextLength
--     printInfos infos totalTextLength


{-
    let input = head inputs
        filename = head args
    let result :: Either ParseError Info
        result = pass1 filename 0 input
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) 
                    ++ "\ninput:\n" ++ input
        Right info -> do 
        let result2 :: Either ParseError String
            result2 = pass2 info input
        case result2 of
            Left err -> putStrLn $ "error: " ++ (show err) 
                        ++ "\ninput:\n" ++ input
            Right output -> putStrLn output
            -}

-- first Integer is the addr
-- second Integer is the ref
-- String is the symbol
type Pref = (Integer, Integer, String)


-- Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
-- (Offset -> b -> (Offset, Either ParseError Info)) -> Offset 
--                 -> [b] -> (Offset, [Either ParseError Info])
-- adder'' :: Offset -> b -> (Offset, Either ParseError Info)
-- adder'' x (fn, txt) = let info = pass1 fn x txt in (x+getOffset info, info)

adder :: Offset -> Offset -> (Offset, Offset)
adder = (\x y -> (x+y, x+y))

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

badformat :: IO ()
badformat = putStrLn "hsxxl: bad format" >> exitFailure
