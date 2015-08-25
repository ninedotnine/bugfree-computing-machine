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
factor = litInt <|> parens expr <|> var <?> "factor"
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
addop = char '+' *> return addVal
    <|> char '-' *> return subVal

mulop :: EvalParser (Val -> Val -> Val)
mulop = char '*' *> return (mulValWith (*))
    <|> char '/' *> return (mulValWith div)
    <|> char '%' *> return (mulValWith rem)

addVal :: Val -> Val -> Val
addVal (Abs x) (Abs y) = Abs (x + y)
addVal (Rel x) (Abs y) = Rel (x + y)
addVal (Abs x) (Rel y) = Rel (x + y)
addVal (Rel _) (Rel _) = error "addVal: both operands can't be relocatable"

subVal :: Val -> Val -> Val
subVal (Abs x) (Abs y) = Abs (x + y)
subVal (Rel x) (Abs y) = Rel (x + y)
subVal (Abs x) (Rel y) = Rel (x + y)
subVal (Rel x) (Rel y) = Abs (x - y)

mulValWith :: (Integer -> Integer -> Integer) -> Val -> Val -> Val
mulValWith op (Abs x) (Abs y) = Abs (x `op` y)
mulValWith _ _ _ = error "mulValWith: both operands must be absolute"

litInt :: EvalParser Val
litInt = benjamins <|> intOrChar

benjamins :: EvalParser Val
benjamins = do
    num <- char '$' *> (sign <*> (read <$> (many1 digit)))
    return (Rel num)

intOrChar :: EvalParser Val
intOrChar = Abs <$> (sign <*> (read <$> (many1 digit)) <?> "lit")

sign :: EvalParser (Integer -> Integer)
sign = char '-' *> return negate <|> optional (char '+') *> return id
