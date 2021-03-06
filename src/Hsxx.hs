{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE BangPatterns, CPP, DataKinds, ImplicitParams #-}
import Prelude hiding (or, and)

import Control.Monad (forever, join, when, ap, liftM2, mplus)

import Data.Char (ord, chr, isDigit, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32, Int16)
import qualified Data.Vector.Unboxed.Mutable as V (new, write)

import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)

import Instructions
import SXXParser
import SXXVector

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>), (<*))
#endif

#if __GLASGOW_HASKELL__ > 705
import Text.Read (readMaybe)
#else
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
#endif


main :: IO ()
main = do
    stdin <- getContents
    mem <- V.new 16384
    V.write mem 0 (16384 :: Int32) -- initialize SP
    args <- getArgs
    input <- readFile (head args)
    entryPoint <- populateVector mem input 
    pc <- newIORef (15 + entryPoint)
    inptr <- newIORef stdin
    putStrLn "beginning execution --------------------"
    forever $ execute mem pc inptr

{-
printStack :: MyVector -> IO ()
printStack vec = do 
    let ?mem = vec
    sp <- toInt <$> getSP
    putStrLn $ "sp is : " ++ show sp
    putStr "stack: "
    printVector sp (16384 - (sp)) vec
-}

execute :: MyVector -> IORef Int16 -> IORef String -> IO ()
execute !mem !pc stdin = do 
    let ?mem = mem
        ?pc  = pc 
    incPC
    instr <- toEnum . toInt <$> (deref =<< toCell <$> getPC)
    case instr of
        BKPT  -> putStrLn "what is the sxx debugger?"
        PUSH  -> getArg >>= deref >>= push
        PUSHV -> getArg >>= push
        PUSHS -> pop >>= deref >>= push
        PUSHX -> liftM2 (+) pop getArg >>= deref >>= push

        -- these allow arbitrary writes to memory
        POP  -> join $ liftM2 write getArg pop
        POPS -> join $ liftM2 (flip write) pop pop
        POPX -> join $ liftM2 (flip write) pop ((+) <$> getArg <*> pop)

        DUPL  -> getSP >>= deref >>= push
        SWAP  -> pop >>= (pop >>=) . (. push) . (>>) . push

        OVER  -> getSP >>= deref . (+1) >>= push
        DROP  -> addSP 1
        ROT   -> do
            sp <- getSP
            val <- deref sp
            write sp =<< deref (sp+2)
            write (sp+2) =<< deref (sp+1)
            write (sp+1) val

        TSTLT -> pop >>= \x -> push $ if x  < 0 then 1 else 0
        TSTLE -> pop >>= \x -> push $ if x <= 0 then 1 else 0
        TSTGT -> pop >>= \x -> push $ if x  > 0 then 1 else 0
        TSTGE -> pop >>= \x -> push $ if x >= 0 then 1 else 0
        TSTEQ -> pop >>= \x -> push $ if x == 0 then 1 else 0
        TSTNE -> pop >>= \x -> push $ if x /= 0 then 1 else 0

        BNE -> pop >>= (getArg >>=) . (. setPC) . when . toBool
        BEQ -> pop >>= (getArg >>=) . (. setPC) . when . (==0)
        BR  -> getArg >>= setPC
        CALL  -> do
            push . (+2) . toCell =<< getPC
            setPC =<< getArg
        CALLS -> do
            val <- pop
            push . toCell . (+1) =<< getPC
            setPC val
        RETURN -> pop >>= setPC 
        RETN   -> pop >>= (getArg >>=) . (. addSP) . (<*) . setPC
        HALT -> putStrLn "execution halted" >> exitSuccess 

        -- binary operations on the stack
        ADD -> push =<< liftM2 (+) pop pop 
        SUB -> push =<< liftM2 subtract pop pop
        MUL -> push =<< liftM2 (*) pop pop -- who's magnitude?
        DIV -> push =<< liftM2 (flip div) pop pop
        MOD -> push =<< liftM2 (flip rem) pop pop
        OR  -> push =<< liftM2 or pop pop
        AND -> push =<< liftM2 and pop pop
        XOR -> push =<< liftM2 ((ap . ((and . not') .) . and) <*> or) pop pop

        NOT -> push . not' =<< pop
        NEG -> push . negate =<< pop
        ADDX -> push =<< liftM2 (+) getArg pop
        ADDSP -> addSP =<< getArg

        READ  -> push =<< readn stdin
        PRINT -> pop >>= (putStr . show)
        READC  -> push =<< readc stdin
        PRINTC -> pop >>= (putChar . chr . toInt) >> hFlush stdout 
        TRON  -> putStrLn "TRON does nothing"
        TROFF -> putStrLn "TROFF does nothing"
        DUMP  -> putStrLn "DUMP does nothing"
        ERROR -> putStrLn "ERROR isn't even a real opcode"

readn :: IORef String -> IO Int32
readn stdin = do 
    input <- dropWhile isSpace <$> readIORef stdin
    -- FIXME: this allows '+' and '-' even in the middle or after a num
    let (num, etc) = span (liftM2 (||) (`elem` "+-") isDigit) input
    writeIORef stdin etc
    case readMaybe num `mplus` readPlusMaybe num of
        Just v -> return v
        Nothing -> error $ "not an integer: \"" ++ num ++ "\""
    where readPlusMaybe str = if not (null str) && head str == '+'
            then readMaybe (tail str)
            else Nothing

readc :: IORef String -> IO Int32
readc stdin = do
    input <- readIORef stdin
    case input of
        [] -> return (-1)
        (char:etc) -> writeIORef stdin etc >> return (toCell (ord char))
