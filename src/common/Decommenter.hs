{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Decommenter (decomment) where

-- FIXME : a bug when '#' appears in quotes

import qualified Data.Text as T (Text, null, lines, takeWhile, dropWhile, head)
import qualified Data.Text.IO as TextIO (interact)

import Data.Monoid
import Data.String (IsString)
import Data.Char (isSpace)
{- 
this module exports decomment, which removes:
    empty lines
    lines beginning with '#' 
    anything in a line following a '#' 
 -}

main :: IO ()
-- main = interact decomment
main = TextIO.interact decomment

decomment :: (Stringy a) => a -> a
decomment = unlines' . map (dropWhile' isSpace) . removeInlineComments
            . removeLines . lines' 


removeLines :: (Stringy a) => [a] -> [a]
removeLines = filter $ (&&) <$> (not . null') <*> ((/='#') . head')

removeInlineComments :: (Stringy a) => [a] -> [a]
removeInlineComments = fmap $ takeWhile' (/= ('#')) -- FIXME: quoted strings

class (IsString a, Monoid a) => Stringy a where
    head' :: a -> Char
    dropWhile' :: (Char -> Bool) -> a -> a
    takeWhile' :: (Char -> Bool) -> a -> a
    null' :: a -> Bool
    lines' :: a -> [a]

-- this is just like the unlines of the Prelude, except no newline at the end
    unlines' :: [a] -> a
    unlines' [] = ""
    unlines' (x:xs) = mconcat $ x : fmap ("\n" <>) xs

instance Stringy T.Text where
    head' = T.head
    dropWhile' = T.dropWhile
    takeWhile' = T.takeWhile
    null' = T.null
    lines' = T.lines

instance Stringy [Char] where
    head' = head
    dropWhile' = dropWhile
    takeWhile' = takeWhile
    null' = null
    lines' = lines
