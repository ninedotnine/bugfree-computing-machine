{-# OPTIONS_GHC -Wall #-} 
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ImplicitParams #-}
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

decSP :: MyVector -> IO ()
decSP mem = do 
    sp <- V.read mem 0
    write mem 0 (sp-1)

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
    let ?mem = mem
        ?pc  = pc 
    pointer <- toAddr <$> readIORef pc
--     putStrLn $ "pointer is: " ++ show pointer
    instr <- toEnum . toInt <$> deref pointer
--     instr <- toEnum . toEnum
--         <$> (deref =<< toAddr <$> readIORef pc)
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
            writeV (toInt addr) val
        POPS  -> do
            val <- pop
            addr <- pop
            writeV (toInt addr) val
        POPX  -> do
            val <- pop
            addr <- pop
            arg <- getArg
            writeV (toInt (addr + arg)) val
        DUPL  -> do
            sp <- getSP
            val <- deref sp
            push val
        SWAP  -> do
            val1 <- pop
            val2 <- pop
            push val1
            push val2
        OVER  -> do
            sp <- getSP
            val <- deref (sp+1)
            push val
        DROP  -> do
            incSP
--         ADDX  -> push =<< liftM2 (+) getArg (pop)
        ROT   -> do
            sp <- getSP
            val <- deref sp
            writeV (toInt sp) =<< deref (sp+2)
            writeV (toInt (sp+2)) =<< deref (sp+1)
            writeV (toInt (sp+1)) val
        TSTLT -> do
            val <- pop
            if val < 0
                then push 1
                else push 0
        TSTLE -> pop >>= \x -> push $ if x <= 0 then 1 else 0
        TSTGT -> pop >>= \x -> push $ if x  > 0 then 1 else 0
        TSTGE -> pop >>= \x -> push $ if x >= 0 then 1 else 0
        TSTEQ -> pop >>= \x -> push $ if x == 0 then 1 else 0
        TSTNE -> pop >>= \x -> push $ if x /= 0 then 1 else 0
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
            val <- chr . toInt <$> pop 
            putChar val
        HALT  -> do
            putStrLn "execution halted"
            exitSuccess 
        _     -> error ("ERROR: " ++ show instr)

-- this code requires the ghc ImplicitParams extension
pop :: (?mem :: MyVector) => IO Int32
pop = deref =<< getSP <* incSP

deref :: (?mem :: MyVector) => Int32 -> IO Int32
deref val = V.read ?mem (toInt val)

push :: (?mem :: MyVector) => Int32 -> IO ()
push val = decSP ?mem >> getSP >>= (\sp -> writeV (toInt sp) val)

getArg :: (?pc :: IORef Int16, ?mem :: MyVector) => IO Int32
getArg = inc ?pc >> readIORef ?pc >>= deref . toAddr

writeV :: (?mem :: MyVector) => Int -> Int32 -> IO ()
writeV = write ?mem

getSP :: (?mem :: MyVector) => IO Int32
getSP = V.read ?mem 0

incSP :: (?mem :: MyVector) => IO ()
--         incSP = getSP >>= (\sp -> write mem 0 (sp+1))
incSP = modVal 0 (+1)

modVal :: (?mem :: MyVector) => Int -> (Int32 -> Int32) -> IO ()
-- modVal = ((.) . (>>=) . V.read ?mem) <*> ((.) . writeV)
modVal x f = V.read ?mem x >>= (\val -> writeV x (f val))


-- for converting to the type of the instruction pointer
-- toPC :: Integral a => a -> Int16
-- toPC = fromIntegral

-- for converting to the type of the memory vector
toAddr :: Integral a => a -> Int32
toAddr = fromIntegral

-- for converting to the type used by many haskell library functions
toInt :: Integral a => a -> Int
toInt = fromIntegral
