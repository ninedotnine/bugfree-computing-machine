{-# LANGUAGE ImplicitParams #-}

-- module SXXVector (MyVector, populateVector, printVector) where
module SXXVector  where

import Data.Vector.Generic hiding ((++), Vector, mapM_)
import qualified Data.Vector.Unboxed as IM
-- import Data.Vector.Unboxed.Mutable (write, MVector)
import qualified Data.Vector.Unboxed.Mutable as V

import Control.Monad.Primitive (PrimState)

import Data.Int
import Data.IORef

type MyVector = V.MVector (PrimState IO) Int32

-- this code requires the ghc ImplicitParams extension
pop :: (?mem :: MyVector) => IO Int32
pop = deref =<< getSP <* modSP (+1)

deref :: (?mem :: MyVector) => Int32 -> IO Int32
deref val = V.read ?mem (toInt val)

push :: (?mem :: MyVector) => Int32 -> IO ()
push val = modSP (subtract 1) >> getSP >>= (\sp -> write sp val)

getArg :: (?pc :: IORef Int16, ?mem :: MyVector) => IO Int32
getArg = incPC >> readIORef ?pc >>= deref . toCell

-- write :: (?mem :: MyVector) => Int -> Int32 -> IO ()
write :: (?mem :: MyVector, Integral a) => a -> Int32 -> IO ()
write = V.write ?mem . toInt

getSP :: (?mem :: MyVector) => IO Int32
getSP = V.read ?mem 0

modSP :: (?mem :: MyVector) => (Int32 -> Int32) -> IO ()
modSP = modVal 0 

modVal :: (?mem :: MyVector) => Int -> (Int32 -> Int32) -> IO ()
-- modVal = ((.) . (>>=) . V.read ?mem) <*> ((.) . write)
modVal x f = V.read ?mem x >>= (\val -> write x (f val))

incPC :: (?pc :: IORef Int16) => IO ()
incPC = modifyIORef' ?pc (+1)

setPC :: (?pc :: IORef Int16, Integral a) => a -> IO ()
-- setPC :: (?pc :: IORef Int16) => Int16 -> IO ()
setPC x = writeIORef ?pc (toPC x)
-- setPC = (writeIORef ?pc) . toPC

-- for converting to the type of the instruction pointer
toPC :: Integral a => a -> Int16
toPC = fromIntegral

-- for converting to the type of the memory vector
toCell :: Integral a => a -> Int32
toCell = fromIntegral

-- for converting to the type used by many haskell library functions
toInt :: Integral a => a -> Int
toInt = fromIntegral
