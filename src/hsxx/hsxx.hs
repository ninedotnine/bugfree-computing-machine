{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
-- import Prelude hiding (read)
import Data.IORef
import Data.Char

import Control.Monad
-- import Control.Monad.Primitive (PrimState)

-- import Data.Vector.Unboxed.Mutable (new, write, MVector)
import Data.Vector.Unboxed.Mutable (new, write)
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
import Parser

main :: IO ()
main = do
    mem <- new 16384
    write mem 0 (16383 :: Int32) -- initialize SP
    args <- getArgs
    input <- readFile (head args)
--     fill mem input 
    populateVector mem input 
    let entry = 16 :: Int16
    pc <- newIORef entry
--     mainLoop pc mem 
    putStrLn "beginning execution --------------------"
    forever $ execute mem pc >> inc pc

inc :: IORef Int16 -> IO ()
inc pc = modifyIORef' pc (+1)

{-
dec :: IORef Int16 -> IO ()
dec pc = modifyIORef' pc (\x -> x-1)
-}

incSP :: MyVector -> IO ()
incSP mem = do 
    sp <- V.read mem 0
    write mem 0 (sp+1)

decSP :: MyVector -> IO ()
decSP mem = do 
    sp <- V.read mem 0
    write mem 0 (sp-1)

getSP :: MyVector -> IO Int32
getSP mem = V.read mem 0
    
    {-
fill :: MyVector -> String -> IO ()
fill mem input = do 
    let instructions = zip [16..] $ map read $ words input
--     print newVals -- instructions start at index 16
    forM_ instructions (uncurry (write mem))
    -}

execute :: MyVector -> IORef Int16 -> IO ()
execute mem pc = do 
--     putStrLn "now beginning execute"    
    pointer <- fromIntegral <$> readIORef pc
    let getArg :: IO Int32
        getArg = do
            inc pc
            deref (pointer+1)

--     putStrLn $ "pointer is: " ++ show pointer
    instr <- toEnum . fromIntegral <$> deref pointer
--     putStrLn $ "HANDLING INSTRUCTION: " ++ show instr
    case instr of
        BKPT  -> do
            putStrLn "what is the sxx debugger?"
        PUSH  -> do
            val <- getArg
            push =<< (deref val)
        PUSHV -> do
            val <- getArg
            push val
        PUSHS -> do
            popped <- pop
            val <- deref popped
            push val
        PUSHX -> do
            val <- pop
            arg <- getArg
            push =<< deref (val + arg)
        POP   -> do
            val <- pop
            addr <- getArg
            write mem (fromIntegral addr) val
        POPS  -> do
            val <- pop
            addr <- pop
            write mem (fromIntegral addr) val
        POPX  -> do
            val <- pop
            addr <- pop
            arg <- getArg
            write mem (fromIntegral (addr + arg)) val
        DUPL  -> do
            sp <- getSP mem
            val <- deref sp
            push val
        SWAP  -> do
            val1 <- pop
            val2 <- pop
            push val1
            push val2
        OVER  -> do
            sp <- getSP mem
            val <- deref (sp+1)
            push val
        DROP  -> do
            incSP mem 
--         ADDX  -> push =<< liftM2 (+) getArg (pop)
        ROT   -> do
            sp <- getSP mem
            val <- deref sp
            write mem (fromIntegral sp) =<< deref (sp+2)
            write mem (fromIntegral (sp+2)) =<< deref (sp+1)
            write mem (fromIntegral (sp+1)) val
        TSTLT -> do
            val <- pop
            if val < 0
                then push 1
                else push 0
        TSTLE -> pop >>= \x -> if x <= 0 then push 1 else push 0
        TSTGT -> pop >>= \x -> if x  > 0 then push 1 else push 0
        TSTGE -> pop >>= \x -> if x >= 0 then push 1 else push 0
        TSTEQ -> pop >>= \x -> if x == 0 then push 1 else push 0
        TSTNE -> pop >>= \x -> if x /= 0 then push 1 else push 0
        NOT   -> do 
            val <- pop
            if val == 0 
                then push 1
                else push 0
        NEG   -> do 
            val <- pop
            push (negate val)
        ADDX  -> do 
            val <- getArg
            result <- pop 
            push (result + val)
        PRINT -> do
            val <- pop
            putStr (show val)
        PRINTC -> do
            val <- chr . fromIntegral <$> pop 
            putChar val
        HALT  -> do
            putStrLn "execution halted"
            exitSuccess 
        _     -> error ("ERROR: " ++ show instr)
    where
        pop :: IO Int32
        pop = do
        --     putStrLn "pop!"
            sp <- getSP mem
            result <- deref sp
            incSP mem
            return result
        deref :: Int32 -> IO Int32
        deref val = do
        --     putStrLn $ "deref: val is: " ++ show val
            V.read mem (fromIntegral val)
        push :: Int32 -> IO ()
        push val = do
        --     putStrLn $ "push: val is: " ++ show val
            decSP mem
            sp <- fromIntegral <$> getSP mem
        --     putStrLn $ "push: sp is: " ++ show sp
            write mem sp val


