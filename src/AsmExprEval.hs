{-# LANGUAGE CPP #-} 
{-# OPTIONS_GHC -Wall #-} 

module AsmExprEval (expr) where

import Data.Char (ord)
import Text.Parsec hiding (labels)
import qualified Data.Map as Map (lookup)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>), (<*), (*>))
#endif

import AsmParser

type EvalParser a = Parsec String Labels a

expr :: EvalParser Val
expr = (term <* spaces) `chainl1` (addop) <?> "expression"

term :: EvalParser Val
term = (factor <* spaces) `chainl1` (mulop) <?> "term"

factor :: EvalParser Val
factor = intOrChar <|> parens expr <|> var <?> "factor"
    where parens = between (char '(' *> spaces) (char ')')

var :: EvalParser Val
var = do
    name <- labelName <?> ""
    labels <- getState
    case Map.lookup name labels of
        Nothing -> fail $ "undefined in pass 2: " ++ name
        Just x -> return x

labelName :: EvalParser String
labelName = (try localLabel <|> globalLabel) <?> "label"

localLabel :: EvalParser String
localLabel = do
    global <- globalLabel
    tailer <- char '@' *> many1 labelChar
    return (global ++ '@' : tailer) -- join them with '-' to prevent clashes

globalLabel :: EvalParser String
globalLabel = (:) <$> letter <*> (many labelChar)

labelChar :: EvalParser Char
labelChar = letter <|> digit <|> oneOf "._"


addop :: EvalParser (Val -> Val -> Val)
addop = spaces *> char '+' *> spaces *> return (addVal (+))
    <|> spaces *> char '-' *> spaces *> return (addVal subtract)

mulop :: EvalParser (Val -> Val -> Val)
mulop = spaces *> char '*' *> spaces *> return (mulVal (*))
    <|> spaces *> char '/' *> spaces *> return (mulVal div)
    <|> spaces *> char '%' *> spaces *> return (mulVal rem)

addVal :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
addVal op (Abs x) (Abs y) = Abs (x `op` y)
addVal op (Rel x) (Abs y) = Rel (x `op` y)
addVal op (Abs x) (Rel y) = Rel (x `op` y)
addVal _ (Rel _) (Rel _) = error "addVal: both operands can't be relocatable"

mulVal :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
mulVal op (Abs x) (Abs y) = Abs (x `op` y)
mulVal _ _ _ = error "mulVal: operands must be absolute"

intOrChar :: EvalParser Val
intOrChar = Abs <$> (sign <*> (try octInt <|> try hexInt <|> int <|> asmChar) <?> "lit")
    where
    int, octInt, hexInt, asmChar :: EvalParser Integer
    octInt = char '0' *> (read . ("0o"++) <$> many1 octDigit)
    hexInt = string "0x" *> (read . ("0x"++) <$> many1 hexDigit)
    int = read <$> (many1 digit)
    asmChar = toInteger . ord <$> (char '\'' *> anyChar)
    sign :: EvalParser (Integer -> Integer)
    sign = char '-' *> return negate <|> optional (char '+') *> return id
