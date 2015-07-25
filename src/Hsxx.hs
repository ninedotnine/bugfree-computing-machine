{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
import Prelude hiding (or, and)
import Data.IORef (IORef, newIORef) 
import Data.Char

import Control.Monad
-- import Control.Monad.Primitive (PrimState)

-- import Data.Vector.Unboxed.Mutable (new, write, MVector)
-- import Data.Vector.Unboxed.Mutable (new)
import qualified Data.Vector.Unboxed.Mutable as V
 
-- import qualified Data.ByteString.Lazy.Char8 as L
-- import qualified Data.Vector                as U
-- import Data.Vector.Unboxed hiding ((++))
-- import Data.Vector.Unboxed.Mutable hiding ((++))
-- import Data.Vector.Generic hiding ((++))
-- import Data.Vector.Generic.Mutable hiding (MVector(..), (++))
import Data.Int
import System.Environment
import System.Exit

import Instructions
import SXXParser
import SXXVector

main :: IO ()
main = do
    mem <- V.new 16384
    V.write mem 0 (16384 :: Int32) -- initialize SP
    printStack mem
    args <- getArgs
    input <- readFile (head args)
    populateVector mem input 
    let entry = 16 :: Int16
    pc <- newIORef (entry-1)
    putStrLn "beginning execution --------------------"
    forever $ do 
        execute mem pc 
--         printStack mem

printStack :: MyVector -> IO ()
printStack vec = do 
    let ?mem = vec
    sp <- toInt <$> getSP
    putStrLn $ "sp is : " ++ show sp
    putStr "stack: "
    printVector sp (16384 - (sp)) vec

execute :: MyVector -> IORef Int16 -> IO ()
execute mem pc = do 
--     putStrLn "now beginning execute"    
    let ?mem = mem
        ?pc  = pc 
    incPC
--     pointer <- toCell <$> getPC
--     putStrLn $ "pointer is: " ++ show pointer
--     instr <- toEnum . toInt <$> deref pointer
    instr <- toEnum . toInt <$> (deref =<< toCell <$> getPC)
--     putStrLn $ "HANDLING INSTRUCTION: " ++ show instr

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
        DROP  -> modSP (+1)
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

        -- we subtract 1 from the addr since PC will be incremented afterward
        BNE -> pop >>= (getArg >>=) . (. (setPC . subtract 1)) . when . toBool
        BEQ -> pop >>= (getArg >>=) . (. (setPC . subtract 1)) . when . (==0)
        BR  -> getArg >>= setPC . subtract 1
        CALL  -> do
            push . toCell =<< getPC
            setPC . (subtract 1) =<< getArg
        CALLS -> do
            val <- pop
            push . toCell . (subtract 1) =<< getPC
            setPC . (subtract 1) $ val
        RETURN -> pop >>= setPC . (+1)
        RETN   -> pop >>= (getArg >>=) . (. (modSP . (+))) . (<*) . setPC
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

        NOT -> push =<< liftM (not' ) pop
        NEG -> push =<< liftM negate pop
        ADDX -> push =<< liftM2 (+) getArg pop
        ADDSP -> modSP . (+) =<< getArg

        PRINT -> pop >>= (putStr . show)
        PRINTC -> pop >>= (putChar . chr . toInt)
        TRON  -> putStrLn "TRON does nothing"
        TROFF -> putStrLn "TROFF does nothing"
        DUMP  -> putStrLn "DUMP does nothing"
        ERROR -> putStrLn "ERROR isn't even a real opcode"
        _     -> error ("ERROR: " ++ show instr ++ "NOT done yet")

