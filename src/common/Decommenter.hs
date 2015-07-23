{-# LANGUAGE OverloadedStrings #-}

module Decommenter (decomment) where

-- FIXME : a bug when '#' appears in quotes


-- import Control.Monad (liftM2)
import Control.Monad 
import Control.Applicative

import qualified Data.Text as T (Text, null, lines, takeWhile, dropWhile, head)
import qualified Data.Text.IO as TextIO (interact)

import Data.Monoid
import Data.String (IsString)
import Data.Char (isSpace)
import Prelude hiding (unlines)
{- 
this module exports decomment, which removes:
    empty lines
    lines beginning with '#' 
    anything in a line following a '#' 
 -}

-- main = interact decomment
main = TextIO.interact decomment'

decomment' :: T.Text -> T.Text
decomment' = unlines . map (T.dropWhile isSpace) . removeInlineComments 
             . removeLines . T.lines
    where 
        removeLines = filter $ not . ((||) . T.null <*> (=='#') . T.head)
        removeInlineComments = map (T.takeWhile (/= ('#'))) 
    {-
        removeLines :: [T.Text] -> [T.Text]
        removeLines = filter (not . badLine) where
            badLine :: T.Text -> Bool
            badLine line = T.null line || (T.head line == ('#' ))
--             -}

decomment :: String -> String
-- decomment = unlines . map (takeWhile (/= ('#'))) . removeLines . lines where
decomment = unlines . map (dropWhile isSpace) . removeInlineComments 
            . removeLines . lines where
--     removeLines = filter $ liftM2 (||) (not . null) ((/='#') . head)
--     removeLines = filter (ap ((||) . ('#' /=) . head) (not . null))
--     removeLines = filter (ap ((||) . not . null) (('#' /=) . head))
    removeLines = filter $ not . ((||) . null <*> (== '#' ) . head)
    removeInlineComments = map $ takeWhile (/= ('#')) -- FIXME: quoted strings
--         removeLines = filter (not . badLine) where 
--             badLine :: String -> Bool

-- this is just like the unlines of the Prelude, except no newline at the end
unlines :: (IsString a, Monoid a) => [a] -> a
unlines [] = ""
unlines (x:xs) = mconcat $ x : fmap ("\n" <>) xs
