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

deref :: MyVector -> Int32 -> IO Int32
deref mem val = do
--     putStrLn $ "deref: val is: " ++ show val
    V.read mem (fromIntegral val)

push :: MyVector -> Int32 -> IO ()
push mem val = do
--     putStrLn $ "push: val is: " ++ show val
    decSP mem
    sp <- fromIntegral <$> getSP mem
--     putStrLn $ "push: sp is: " ++ show sp
    write mem sp val

pop :: MyVector -> IO Int32
pop mem = do
--     putStrLn "pop!"
    sp <- getSP mem
    result <- deref mem sp
    incSP mem
    return result

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
            deref mem (pointer+1)

--     putStrLn $ "pointer is: " ++ show pointer
    instr <- toEnum . fromIntegral <$> deref mem pointer
--     putStrLn $ "HANDLING INSTRUCTION: " ++ show instr
    case instr of
        BKPT  -> do
            putStrLn "what is the sxx debugger?"
        PUSH  -> do
            val <- getArg
            push mem =<< (deref mem val)
        PUSHV -> do
            val <- getArg
            push mem val
        PUSHS -> do
            popped <- pop mem
            val <- deref mem popped
            push mem val
        PUSHX -> do
            val <- pop mem
            arg <- getArg
            push mem =<< deref mem (val + arg)
        POP   -> do
            val <- pop mem
            addr <- getArg
            write mem (fromIntegral addr) val
        POPS  -> do
            val <- pop mem
            addr <- pop mem
            write mem (fromIntegral addr) val
        DUPL  -> do
            sp <- getSP mem
            val <- deref mem sp
            push mem val
        SWAP  -> do
            val1 <- pop mem
            val2 <- pop mem
            push mem val1
            push mem val2
--         ADDX  -> push mem =<< liftM2 (+) getArg (pop mem)
        NOT   -> do 
            val <- pop mem
            if val == 0 
                then push mem 1
                else push mem 0
        NEG   -> do 
            val <- pop mem
            push mem (negate val)
        ADDX  -> do 
            val <- getArg
            result <- pop mem 
            push mem (result + val)
        PRINT -> do
            val <- pop mem
            putStr (show val)
        PRINTC -> do
            val <- chr . fromIntegral <$> pop mem 
            putChar val
        HALT  -> do
            putStrLn "execution halted"
            exitSuccess 
        _     -> error ("ERROR: " ++ show instr)
