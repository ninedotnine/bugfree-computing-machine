{-# LANGUAGE CPP #-} 
{-# OPTIONS_GHC -Wall #-} 

module AsmExprEval (expr) where

import Text.Parsec hiding (labels)
import qualified Data.Map as Map (lookup)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>), (<*), (*>))
#endif

import AsmParser

type EvalParser a = Parsec String Labels a

expr :: EvalParser Val
expr = term `chainl1` addop <?> "expression"

term :: EvalParser Val
term = factor `chainl1` mulop <?> "term"

factor :: EvalParser Val
factor = intOrChar <|> parens expr <|> var <?> "factor"
    where parens = between (char '(') (char ')')

var :: EvalParser Val
var = do
    name <- labelName <?> "label"
    labels <- getState
    case Map.lookup name labels of
        Nothing -> fail $ "undefined in pass 2: " ++ name
        Just x -> return x

labelName :: EvalParser String
labelName = do
    global <- (:) <$> letter <*> (many labelChar)
    tailer <- optionMaybe (char '@' *> many1 labelChar)
    case tailer of
        Just local -> return (global ++ '@' : local) -- join them with '@'
        Nothing -> return global

labelChar :: EvalParser Char
labelChar = letter <|> digit <|> oneOf "._"

addop :: EvalParser (Val -> Val -> Val)
addop = char '+' *> return (addVal (+))
    <|> char '-' *> return (addVal subtract)

mulop :: EvalParser (Val -> Val -> Val)
mulop = char '*' *> return (mulVal (*))
    <|> char '/' *> return (mulVal div)
    <|> char '%' *> return (mulVal rem)

addVal :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
addVal op (Abs x) (Abs y) = Abs (x `op` y)
addVal op (Rel x) (Abs y) = Rel (x `op` y)
addVal op (Abs x) (Rel y) = Rel (x `op` y)
addVal _ (Rel _) (Rel _) = error "addVal: both operands can't be relocatable"

mulVal :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
mulVal op (Abs x) (Abs y) = Abs (x `op` y)
mulVal _ _ _ = error "mulVal: operands must be absolute"

intOrChar :: EvalParser Val
intOrChar = Abs <$> (sign <*> (read <$> (many1 digit)) <?> "lit")
    where
        sign :: EvalParser (Integer -> Integer)
        sign = char '-' *> return negate <|> optional (char '+') *> return id
