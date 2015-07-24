import System.Environment (getArgs)
import System.Exit 
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Char
import Data.List
import Text.Read (readMaybe)
import qualified Data.Map as Map

import Decommenter

main :: IO ()
main = do
    (filename, input) <- getFileData
--     let (text, entry) = runState (pass (decomment input)) 0
--     let result  = runMaybeT $ runState (pass (decomment input)) 0
--     let result  = runExceptT $ runState (pass (decomment input)) 0
    let res :: State Integer (Either String (String, Integer)) 
        res = runExceptT (pass (decomment input))
        (result,  entry) = runState res 0
--     putStrLn text
    case result of
--         Right (text, entry) -> putStrLn text
        Right val -> print result >> putStrLn (fst val) >> putStrLn (
                    "and textlength is: " ++ show (snd val))
        
        Left msg -> putStrLn msg
--     print result
    putStrLn "DONE"




-- first Integer is the addr
-- second Integer is the ref
-- String is the symbol
type Pref = (Integer, Integer, String)

----------------------------------------

-- pass :: String -> State Integer String
-- pass :: String -> StateT Integer (Either String) String
-- pass :: String -> Either String (State Integer String)
pass :: String -> ExceptT String (State Integer) (String, Integer)
-- pass input = return input
pass input = do
    let lins = lines input
    when (not ("%SXX+O" `isPrefixOf` (head lins))) $ 
        throwE "first line wrong"
    let lins2 = tail lins
    when (null lins2) $ throwE "error, line 2"
    let justTEXT_LENGTH = readMaybeInt (head lins2) 
--     case justTEXT_LENGTH of
--         Just x -> TEXTLENGTH <- x
--         Nothing -> 
    
    let tEXT_LENGTH = 4
    let (text, nontext) = break readable lins
    return (input, tEXT_LENGTH)

readable :: String -> Bool
readable str = case readMaybeInt str of
    Just _ -> True
    Nothing -> if null str || head str /= ':'
        then False
        else readable (tail str) -- FIXME: allows stuff like "::::43"

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe . takeWhile (not . isSpace)

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
