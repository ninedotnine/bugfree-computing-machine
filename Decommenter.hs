module Decommenter (decomment) where

-- FIXME : prints empty lines when they start with #

-- {-# LANGUAGE OverloadedStrings #-}

-- import Control.Monad (liftM2)
import Control.Monad 
import Control.Applicative
-- import Data.Text (Text, null, lines, unlines, takeWhile, head)
import qualified Data.Text as T (Text, null, lines, unlines, takeWhile, head)
-- import Data.Text.IO (interact)
-- import Prelude (not, (.), (||), (==), (/=), map, filter)
-- import Prelude hiding (interact)

{- 
this module exports decomment, which removes:
    empty lines
    lines beginning with '#' 
    anything in a line following a '#' 
 -}

-- main = interact decomment'
main = interact decomment

decomment' :: T.Text -> T.Text
decomment' = T.unlines . map (T.takeWhile (/= ('#'))) . removeLines . T.lines
    where
        removeLines = filter $ (||) . not . T.null <*> (/='#') . T.head
    {-
        removeLines :: [T.Text] -> [T.Text]
        removeLines = filter (not . badLine) where
            badLine :: T.Text -> Bool
            badLine line = T.null line || (T.head line == ('#' ))
--             -}

decomment :: String -> String
decomment = unlines . map (takeWhile (/= ('#'))) . removeLines . lines where
--     removeLines = filter $ liftM2 (||) (not . null) ((/='#') . head)
--     removeLines = filter (ap ((||) . ('#' /=) . head) (not . null))
--     removeLines = filter (ap ((||) . not . null) (('#' /=) . head))
    removeLines = filter $ (||) . not . null <*> (/='#') . head
