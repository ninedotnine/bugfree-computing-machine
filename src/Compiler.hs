
import Text.Parsec.String (parseFromFile)

import CParser hiding (main)

testfile :: FilePath
testfile = "../test/c/else.c"

main :: IO ()
main = do
    eith <- parseFromFile (parseLex parseDefs) testfile
    case eith of 
        Left err -> print err
        Right r -> print r
    putStrLn "done"
