-- {-# OPTIONS_GHC -Wall #-} 

import System.Environment (getArgs)
import System.Exit 
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad
import Data.Traversable (mapAccumL)
import Data.Either (partitionEithers)
-- import qualified Data.Map as Map

-- import Decommenter
import ObjParser
-- import ObjWriter
import ObjPrinter

main :: IO ()
main = do
    args <- getArgs
    inputs <- mapM readFile args
--     (filename, input) <- getFileData
--     mapM_ (print . pass1 
    let zippy = zip args inputs
--     forM_ zippy $ print . (\(fn, txt) -> pass1 fn 0 txt)
    let eithers :: [Either ParseError Info]
        eithers = map (\(fn, txt) -> pass1 fn 0 txt) zippy
--     mapM_ print eithers
    let (errors, rights) = partitionEithers eithers
    when (not (null errors)) $ mapM_ print errors >> exitFailure
--     let infos =  map 

--     let offsets = map getOffset infos
--     let offsets = mapAccumL getOffset infos
    let infos :: [Info]
        (totalTextLength, infos) = mapAccumL adder' 0 rights
--         offsets' = mapAccumL (\x y -> (x+y, x+y)) 0 offsets
--         offsets' = mapAccumL adder 0 offsets
    let entries = filter hasEntry infos
    putStr "## ENTRIES: ## " >> print entries
    when (length entries > 1) $ putStrLn "multiple entries" >> exitFailure

    putStrLn  "++++++++++++++++++++++++++++++++++++++++++"
    mapM_ print infos

    putStrLn  "**********************************************"
    printInfos infos totalTextLength


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

badformat :: IO ()
badformat = putStrLn "hsxxl: bad format" >> exitFailure
