{-# OPTIONS_GHC -Wall #-} 

module AsmExprEval (expr) where

import Data.Char (ord)
import Text.Parsec hiding (labels)
import qualified Data.Map as Map (lookup)

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


mulop :: EvalParser (Val -> Val -> Val)
mulop = spaces *> char '*' *> spaces *> return mulVal
    <|> spaces *> char '/' *> spaces *> return divVal
    <|> spaces *> char '%' *> spaces *> return modVal

addop :: EvalParser (Val -> Val -> Val)
addop = spaces *> char '+' *> spaces *> return addVal
    <|> spaces *> char '-' *> spaces *> return subVal

addVal :: Val -> Val -> Val
addVal (Abs x) (Abs y) = Abs (x + y)
addVal (Rel x) (Abs y) = Rel (x + y)
addVal (Abs x) (Rel y) = Rel (x + y)
addVal (Rel _) (Rel _) = error "addVal: both operands can't be relocatable"

subVal :: Val -> Val -> Val
subVal (Abs x) (Abs y) = Abs (x - y)
subVal (Rel x) (Abs y) = Rel (x - y)
subVal (Abs x) (Rel y) = Rel (x - y)
subVal (Rel _) (Rel _) = error "subVal: both operands can't be relocatable"

mulVal :: Val -> Val -> Val
mulVal (Abs x) (Abs y) = Abs (x * y)
mulVal _ _ = error "mulVal: operands must be absolute"

divVal :: Val -> Val -> Val
divVal (Abs x) (Abs y) = Abs (x `div` y)
divVal _ _ = error "divVal: operands must be absolute"

modVal :: Val -> Val -> Val
modVal (Abs x) (Abs y) = Abs (x `rem` y)
modVal _ _ = error "modVal: operands must be absolute"

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
