{-# OPTIONS_GHC -Wall #-} 

import System.Environment (getArgs)
import System.Exit 
import Text.ParserCombinators.Parsec (ParseError)
-- import qualified Data.Map as Map

-- import Decommenter
import ObjParser
import ObjWriter

main :: IO ()
main = do
    (filename, input) <- getFileData
    let result :: Either ParseError Info
        result = pass1 filename 0 input
    putStrLn  "------------------------------------------"
    case result of
        Left err -> putStrLn $ "error: " ++ (show err) 
                    ++ "\ninput:\n" ++ input
        Right info -> do 
        let result2 :: Either ParseError String
            result2 = pass2 filename info input
        case result2 of
            Left err -> putStrLn $ "error: " ++ (show err) 
                        ++ "\ninput:\n" ++ input
            Right output -> putStrLn output
        putStrLn "DONE"

-- first Integer is the addr
-- second Integer is the ref
-- String is the symbol
type Pref = (Integer, Integer, String)

-- this is identical to the one in hsax; combine them!
getFileData :: IO (FilePath, String)
getFileData = getArgs >>= \args -> if length args < 1
    then do 
        contents <- getContents
--         putStrLn "no file name provided"
        return ("stdin", contents)
    else let name = head args in do 
        contents <- readFile name
        return (name, contents)

badformat :: IO ()
badformat = putStrLn "hsxxl: bad format" >> exitFailure
