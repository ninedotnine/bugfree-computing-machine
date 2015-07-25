{-# LANGUAGE ImplicitParams #-}

-- module SXXVector (MyVector, populateVector, printVector) where
module SXXVector  where

import qualified Data.Vector.Unboxed as IM
import qualified Data.Vector.Unboxed.Mutable as V

import Control.Monad.Primitive (PrimState)

import Data.Int (Int32, Int16)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')

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

getPC :: (?pc :: IORef Int16) => IO Int16
getPC = readIORef ?pc

-- for converting to the type of the instruction pointer
toPC :: Integral a => a -> Int16
toPC = fromIntegral

-- for converting to the type of the memory vector
toCell :: Integral a => a -> Int32
toCell = fromIntegral

-- for converting to the type used by many haskell library functions
toInt :: Integral a => a -> Int
toInt = fromIntegral

-- this is for boolean operations
class SXXBool a where
    toInt32 :: a -> Int32
    toBool :: a -> Bool
    and :: (SXXBool b) => a -> b -> Int32
    and a b = toInt32 (toBool a && toBool b)
    -- or cannot be named (|) because '|' is a reserved word
    or :: (SXXBool b) => a -> b -> Int32
    or a b = toInt32 (toBool a || toBool b)
    not' :: (SXXBool a) => a -> Int32
    not' = toInt32 . not . toBool 

instance SXXBool Bool where
    toBool = id
    toInt32 True = 1
    toInt32 False = 0

instance SXXBool Int32 where
    toBool = (/= 0)
    toInt32 = id
--     toInt32 = toCell
