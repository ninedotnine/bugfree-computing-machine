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
    args <- getArgs
    inputs <- mapM readFile args
--     (filename, input) <- getFileData
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
            result2 = pass2 filename info input
        case result2 of
            Left err -> putStrLn $ "error: " ++ (show err) 
                        ++ "\ninput:\n" ++ input
            Right output -> putStrLn output

-- first Integer is the addr
-- second Integer is the ref
-- String is the symbol
type Pref = (Integer, Integer, String)

badformat :: IO ()
badformat = putStrLn "hsxxl: bad format" >> exitFailure
