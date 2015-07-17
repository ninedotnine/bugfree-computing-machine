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
import Parser

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
--     pointer <- toCell <$> readIORef pc
--     putStrLn $ "pointer is: " ++ show pointer
--     instr <- toEnum . toInt <$> deref pointer
    instr <- toEnum . toInt <$> (deref =<< toCell <$> readIORef pc)
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
        BNE   -> do
            val <- pop
            addr <- getArg
            when (val /= 0) (setPC (toPC (addr-1)))
        BEQ   -> do
            val <- pop
            addr <- getArg
            when (val == 0) (setPC (toPC (addr-1)))
        BR    -> do
            addr <- getArg
            setPC (toPC (addr-1)) -- it will be incremented soon anyway
        CALL  -> do
            push . toCell =<< readIORef pc
            setPC . toPC . (subtract 1) =<< getArg
        CALLS -> do
            val <- pop
            push . toCell . (subtract 1) =<< readIORef pc
            setPC . toPC . (subtract 1) $ val
        RETURN -> do
            pop >>= setPC . toPC . (+1)
        RETN   -> do
            val <- pop
            arg <- getArg
            modVal 0 (+arg) -- increase the stack pointer
            setPC (toPC val)
        HALT  -> do
            putStrLn "execution halted"
            exitSuccess 
        ADD -> do 
            val1 <- pop
            val2 <- pop
            push (val2 + val1)
        SUB -> push =<< liftM2 subtract pop pop -- who's magnitude? 
        MUL -> push =<< liftM2 (*) pop pop
        DIV -> push =<< liftM2 (flip div) pop pop
        MOD -> push =<< liftM2 (flip rem) pop pop
        OR  -> push =<< liftM2 (?|) pop pop
        AND -> push =<< liftM2 (&) pop pop
        XOR -> push =<< liftM2 ((ap . (((&) . not') .) . (&)) <*> (?|)) pop pop
--             val1 <- pop
--             val2 <- pop
--             push $ (not' (val1 & val2)) & (val1 ?| val2)
--         push =<< liftM2 (\val1 val2 -> (not' (val1 & val2)) & (val1 ?|
-- val2)) pop pop -- these work too
        NOT   -> do 
            val <- pop
            if val == 0 
                then push 1
                else push 0
        NEG   -> do 
            val <- pop
            push (negate val)
--         ADDX  -> push =<< liftM2 (+) getArg (pop)
        ADDX  -> do 
            val <- getArg
            result <- pop 
            push (result + val)
        ADDSP -> do
            arg <- getArg
            modVal 0 (+arg)
        PRINT -> do
            val <- pop
            putStr (show val)
        PRINTC -> do
            val <- chr . toInt <$> pop 
            putChar val
        TRON  -> putStrLn "TRON does nothing"
        TROFF -> putStrLn "TROFF does nothing"
        DUMP  -> putStrLn "DUMP does nothing"
        ERROR -> putStrLn "ERROR isn't even a real opcode"
        _     -> error ("ERROR: " ++ show instr ++ "NOT done yet")


-- this is for boolean operations
class SXXBool a where
    toInt32 :: a -> Int32
    fromInt32 :: Int32 -> a
    toBool :: a -> Bool
    (&) :: (SXXBool b) => a -> b -> Int32
    (&) a b = toInt32 (toBool a && toBool b)
    (?|) :: (SXXBool b) => a -> b -> Int32
    (?|) a b = toInt32 (toBool a || toBool b)
    not' :: (SXXBool a) => a -> Int32
--     not' x = if toBool x then 0 else 1
    not' = toInt32 . not . toBool 

instance SXXBool Bool where
    toBool = id
    toInt32 True = 1
    toInt32 False = 0
    fromInt32 0 = False
    fromInt32 _ = True

instance SXXBool Int32 where
--     toBool 0 = False
--     toBool _ = True
    toBool = (/= 0)
    toInt32 = id
    fromInt32 0 = 0
    fromInt32 _ = 1

-- this code requires the ghc ImplicitParams extension
pop :: (?mem :: MyVector) => IO Int32
pop = deref =<< getSP <* incSP

deref :: (?mem :: MyVector) => Int32 -> IO Int32
deref val = V.read ?mem (toInt val)

push :: (?mem :: MyVector) => Int32 -> IO ()
push val = decSP >> getSP >>= (\sp -> writeV (toInt sp) val)

getArg :: (?pc :: IORef Int16, ?mem :: MyVector) => IO Int32
getArg = incPC >> readIORef ?pc >>= deref . toCell

writeV :: (?mem :: MyVector) => Int -> Int32 -> IO ()
writeV = V.write ?mem

getSP :: (?mem :: MyVector) => IO Int32
getSP = V.read ?mem 0

incSP :: (?mem :: MyVector) => IO ()
--         incSP = getSP >>= (\sp -> write mem 0 (sp+1))
incSP = modVal 0 (+1)

decSP :: (?mem :: MyVector) => IO ()
decSP = modVal 0 (subtract 1)

modVal :: (?mem :: MyVector) => Int -> (Int32 -> Int32) -> IO ()
-- modVal = ((.) . (>>=) . V.read ?mem) <*> ((.) . writeV)
modVal x f = V.read ?mem x >>= (\val -> writeV x (f val))

incPC :: (?pc :: IORef Int16) => IO ()
incPC = modifyIORef' ?pc (+1)

setPC :: (?pc :: IORef Int16) => Int16 -> IO ()
setPC x = writeIORef ?pc x

{-
dec :: IORef Int16 -> IO ()
dec pc = modifyIORef' pc (\x -> x-1)
-}


-- for converting to the type of the instruction pointer
toPC :: Integral a => a -> Int16
toPC = fromIntegral

-- for converting to the type of the memory vector
toCell :: Integral a => a -> Int32
toCell = fromIntegral

-- for converting to the type used by many haskell library functions
toInt :: Integral a => a -> Int
toInt = fromIntegral
