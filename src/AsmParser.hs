{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# OPTIONS_GHC -Wall #-} 
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-} 

module AsmParser (Instruction(..), 
                Token(..), 
                EntryPoint(..),
                Val(..),
                Labels,
                runAsmParser) where

import Text.ParserCombinators.Parsec hiding (try, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (runParser)
import Text.Parsec.Prim hiding (runParser, label, labels, (<|>))
-- import Text.Parsec.Prim hiding (try, runParser)
-- import Text.Parsec.Prim (Parsec)
-- import Text.Parsec.Prim
-- import Control.Monad (join)
import Control.Monad
-- import Control.Monad.Identity
-- import Control.Monad.State (runState, evalState, execState)
-- import Control.Applicative hiding (many, (<|>))
import Control.Applicative hiding (many, optional)
-- import Control.Applicative ((<$>))
-- import Control.Applicative hiding ((<|>))
import Data.Char (toUpper, toLower, ord)
import Data.List (genericLength)
import Data.String (IsString, fromString)
-- import Data.Foldable (traverse_)
-- import Text.Read (readMaybe)

-- import Data.Map.Strict (Map, (!))
-- import qualified Data.Map.Strict as Map (Map, singleton)
#if __GLASGOW_HASKELL__ < 706
import qualified Data.Map as Map (Map, singleton, member, insert)
#else
import qualified Data.Map.Strict as Map (Map, singleton, member, insert)
#endif

import Debug.Trace (trace)

import Instructions

#if __GLASGOW_HASKELL__ > 705
import Text.Read (readMaybe)
#else
readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
#endif

runAsmParser :: SourceName -> String ->
                Either ParseError (Integer, EntryPoint, [Token], Labels)
runAsmParser name input = runParser instructions initState name input
    where initState = (0, "", "", Map.singleton "SP" (Abs 0))

{-
MyParser is a type 
ParsecT is a monad transformer
String is the stream type
MyState is the state. 
-}
type MyParser a = Parsec String MyState a

{-
i use the Integer to count words, both so i can output the text length and 
    so i can track where the labels are.
the EntryPoint is the name of the entry point after it has been found. 
    it is the empty string if no entry point is found.
the last String is the name of the current non-local label.
    it does not need to be known outside of the parsing stage.
when a label is parsed, a String and Integer pair is added to the Map 
    the String in this case is the name of the label; 
        the Integer is its location. 
-}
type MyState = (Integer, EntryPoint, String, Labels)

type Labels = Map.Map String Val

data Val = Abs Integer
        | Rel Integer -- this could probably be done with phantom types?

instance Show Val where
    show (Abs x) = show x
    show (Rel x) = show x

{-
Token is an algebraic data type
a Lit can be a number like 65 or a letter like 'a
an Op is any of the opcodes
a Label is a label
-}
data Token = Lit Integer
            | LitExpr Expr
            | Op Instruction (Maybe Expr)
            | Label String Integer -- the int is the current location counter
            | NewLabel String
            | DS Integer
            | DW [Integer]
            | EQU String Integer 
            | Entry EntryPoint
            | Extern [String]
            | Public [String]
            deriving (Show)

newtype EntryPoint = EntryPoint String deriving (Eq)

type Expr = (String, Integer) -- Integer is the location to add to reloc dict

instance Show EntryPoint where 
    show (EntryPoint name) = name
instance IsString EntryPoint where
    fromString = EntryPoint

--------------------------------- parser begins here

instructions :: MyParser (Integer, EntryPoint, [Token], Labels)
instructions = do 
    res <- join <$> many (try instruction) `sepBy` skipJunk
    skipMany skipJunk *> eof
    (counter, entry, _, labels) <- getState
    return (counter, entry, res, labels)

instruction :: MyParser Token
instruction = skipMany skipJunk *>
--     fmap Lit intOrChar <* loc (+1)
    try asmEQU
    <|> try asmDS 
    <|> try asmDW 
    <|> try asmEntry 
    <|> try asmExtern 
    <|> try asmPublic
    <|> try newLabel 
    <|> try opcode <* loc (+1)
    <|> try label <* loc (+1)
--     <|> LitExpr <$> (,) <$> asmExprStr <*> getLoc <* loc (+1)
    <|> do 
        str <- asmExprStr
        place <- getLoc
        loc (+1)
        return $ LitExpr (str, place)


newLabel :: MyParser Token
newLabel = NewLabel <$> (newGlobalLabel <|> newLocalLabel) <* skipMany skipJunk
        <?> "label"

label :: MyParser Token
label = Label <$> labelName <*> getLoc

labelName :: MyParser String
labelName = (localLabel <|> globalLabel) <* skipMany skipJunk <?> "label"

newLocalLabel :: MyParser String
newLocalLabel = do
    name <- localLabel <* spaces <* char ':'
    pos <- getLoc -- get the current position in the count
    addToLabels name (Rel pos) -- add it to the map of labels
    return name

newGlobalLabel :: MyParser String
newGlobalLabel = do
    name <- globalLabel <* spaces <* char ':'
    setLabelPrefix name -- new current scope
    pos <- getLoc -- get the current position in the count
    addToLabels name (Rel pos) -- add it to the map of labels
    return name

localLabel :: MyParser String
localLabel = do
    char '@'
    tailer <- many labelChar
    header <- getLabelPrefix
    return (header ++ '@' : tailer) -- join them with '@' to prevent clashes

globalLabel :: MyParser String
globalLabel = (:) <$> letter <*> (many labelChar)

opcode :: MyParser Token
opcode = do 
    -- all the opcodes are in capitals
    uppers <- map toUpper <$> many1 letter <?> "opcode" 
     -- try to read it as an Instruction
    case readMaybe uppers `mplus` readOpSynonym uppers of --alternatively, <|>
        Just x -> if x `elem` argOps
            then do
                arg <- asmExprStr
                loc (+1)
                currentLoc <- getLoc
                return (Op x (Just (arg, currentLoc)))
            else return (Op x Nothing)
        Nothing -> fail "can't parse as opcode"
    where
        readOpSynonym :: String -> (Maybe Instruction)
        readOpSynonym "INDIR" = Just PUSHS
        readOpSynonym "BT"    = Just BNE
        readOpSynonym "BF"    = Just BEQ
        readOpSynonym "POPPC" = Just RETURN
        readOpSynonym _       = Nothing
        argOps :: [Instruction]
        argOps = [PUSH, PUSHV, PUSHX, POP, POPX, BNE, BEQ, BR, 
                CALL, RETN, ADDX, ADDSP] -- these instructions take arguments

asmExprStr :: MyParser String
asmExprStr = (do
    first <- spaces *> term <* spaces
    more <- many ((:) <$> oneOf "+-" <*> term)
    return (concat (first:more))) <?> "expression"

term :: MyParser String
term = (do
    first <- spaces *> factor <* spaces
    more <- many ((:) <$> oneOf "*/%" <*> factor)
    return (concat (first:more))) <?> "term"

factor :: MyParser String
factor = spaces *> (benjamins <|> (show <$> intOrChar) <|> subExpr <|> labelName) <* spaces <?> "factor"
--     where subExpr = concat <$> sequence [string "(", tempexpr, string ")"] 
    where subExpr = char '(' <:> asmExprStr <++> string ")"

benjamins :: MyParser String
benjamins = do
    char '$' 
    pos <- getLoc
    return ('$' : show pos) -- FIXME

intOrChar :: MyParser Integer
intOrChar = sign <*> (try octInt <|> try hexInt <|> int <|> asmChar) <?> "lit"
    where
    int, octInt, hexInt, asmChar :: MyParser Integer
    octInt = char '0' *> (read . ("0o"++) <$> many1 octDigit)
    hexInt = string "0x" *> (read . ("0x"++) <$> many1 hexDigit)
    int = read <$> (many1 digit)
    asmChar = toInteger . ord <$> (char '\'' *> anyChar)
    sign :: MyParser (Integer -> Integer)
    sign = char '-' *> return negate <|> optional (char '+') *> return id

skipJunk :: MyParser ()
skipJunk = skipSpaces <|> skipComment <?> ""
-- skipJunk = spaces <|> skipComment

skipSpaces :: MyParser ()
skipSpaces = skipMany1 space <?> "" -- silence this; it's handled elsewhere

skipComment :: MyParser ()
skipComment = spaces *> char ';' *> skipToEOL
-- skipComment = char ';' *> skipToEOL

skipToEOL :: MyParser ()
skipToEOL = anyChar `manyTill` newline *> skipMany space
-- skipToEOL = many (noneOf "\n") *> return ()
-- skipToEOL = skipMany (noneOf "\n") *> skipMany1 space -- skip past the '\n'

caseInsensitiveChar :: Char -> MyParser Char
caseInsensitiveChar c = (char (toLower c) <|> char (toUpper c)) *> pure c

caseInsensitiveString :: String -> MyParser String
caseInsensitiveString = sequence . map caseInsensitiveChar

{- from the assembler documentation:
A label is any sequence of letters, digits, dots, and underscores, beginning 
with a letter or at-sign '@' character, but excluding reserved words. A label 
is also called an identifier. Labels longer than 30 characters in length are 
silently truncated to 30 characters. 
-}
labelChar :: MyParser Char
labelChar = letter <|> digit <|> oneOf "._"

-- loc f applies f to the location counter
loc :: (Integer -> Integer) -> MyParser ()
loc = modifyState . (\f (i, s, labl, labels) -> (f i, s, labl, labels))

-- returns the current location counter
getLoc :: MyParser Integer
getLoc = getState >>= \(i, _, _, _) -> return i

-- setEntry "main" makes the entry point "main", provided it isn't already set
setEntry :: EntryPoint -> MyParser ()
setEntry = modifyState . setEntry' where 
    -- pattern match the empty string
    setEntry' :: EntryPoint -> MyState -> MyState
    setEntry' name (i, "", labl, labels) = (i, name, labl, labels) 
    setEntry' _ _ = error "multiple entries?" -- FIXME: use parser monad fail

addToLabels :: String -> Val -> MyParser ()
addToLabels name val = do 
    (i, ent, labl, labels) <- getState
    let truncName = take 30 name -- silently truncate after 30 characters
    if truncName `Map.member` labels
        then error $ "multiply defined label: " ++ name
        else putState (i, ent, labl, Map.insert truncName val labels)

setLabelPrefix :: String -> MyParser ()
setLabelPrefix = modifyState . setLabelPrefix' where
    setLabelPrefix' str (i, ent, _, labels) = (i, ent, str, labels)

getLabelPrefix :: MyParser String
getLabelPrefix = getState >>= \(_, _, labl, _) -> return labl


-- FIXME: figure out how to delete all of this. labels are cool now.
pass1expr   :: MyParser Integer
pass1expr   = (pass1term <* spaces) `chainl1` (addop) <?> "expression" -- FIXME

pass1term   :: MyParser Integer
pass1term   = (pass1factor <* spaces) `chainl1` (mulop) <?> "pass1term"

pass1factor :: MyParser Integer
pass1factor = intOrChar <|> parens pass1expr <?> "pass1factor"
    where parens = between (char '(' *> spaces) (char ')')

mulop :: MyParser (Integer -> Integer -> Integer)
mulop = spaces *> char '*' *> spaces *> return (*)
    <|> spaces *> char '/' *> spaces *> return div 
    <|> spaces *> char '%' *> spaces *> return rem

addop :: MyParser (Integer -> Integer -> Integer)
addop = spaces *> char '+' *> spaces *> return (+)
    <|> spaces *> char '-' *> spaces *> return (-)

asmEQU :: MyParser Token
asmEQU = do
    name <- globalLabel <* skipSpaces <?> ""
    def <- caseInsensitiveString "equ" *> skipSpaces *> pass1expr
    addToLabels name (Abs def) -- add it to the map of labels
    return $ EQU name def

asmDS :: MyParser Token
asmDS = do
    caseInsensitiveString "ds" *> skipSpaces <?> "DS"
    size <- pass1expr
-- a DS should not increase the text length...
    loc (+size) -- but its operand should, by its value
    return (DS size)

-- FIXME: still doesn't handle strings or other fancy things like expressions
asmDW :: MyParser Token
asmDW = do
    caseInsensitiveString "dw" *> skipSpaces <?> "DW"
--     args <- intOrChar `sepBy1` (char ',' *> spaces)
    args <- fmap join dwArgs
    -- a DW should increase the location counter by the number of arguments
    loc (\x -> x + (genericLength args))
    return (DW args)

dwArgs :: MyParser [[Integer]]
dwArgs = (fmap (:[]) intOrChar <|> litString) `sepBy1` (char ',' *> spaces)

litString :: MyParser [Integer]
litString = map (toInteger . ord) 
        <$> (char '\"' *> anyChar `manyTill` char '\"')

asmEntry :: MyParser Token
asmEntry = do 
    caseInsensitiveString "entry" <* skipSpaces <?> "ENTRY"
    header <- letter
    tailer <- many labelChar
    let name = (header:tailer)
    setEntry (EntryPoint name)
    return (Entry (EntryPoint name))

asmExtern :: MyParser Token 
asmExtern = do
    caseInsensitiveString "extern" <* skipSpaces <?> "EXTERN"
    args <- globalLabel `sepBy1` (char ',' *> spaces)
    forM_ args (flip addToLabels (Abs (-9))) -- FIXME: what value should it have?
--     traverse_ (flip addToLabels (-9)) args -- FIXME: what value should it have?
--     mapM_ (flip addToLabels (-9)) args  -- FIXME: what value should it have?
--     traceM $ "asmExtern: args: " ++ show args
    return $ Extern (args)

asmPublic :: MyParser Token 
asmPublic = do
    caseInsensitiveString "public" <* skipSpaces <?> "PUBLIC"
    args <- globalLabel `sepBy1` (char ',' *> spaces)
    forM_ args (flip addToLabels (Rel (-9))) -- FIXME: what value should it have?
--     traverse_ (flip addToLabels (-9)) args -- FIXME: what value should it have?
--     mapM_ (flip addToLabels (-9)) args  -- FIXME: what value should it have?
--     return $ Public (show args)
    return $ Public (args)


(<:>) :: MyParser Char -> MyParser String -> MyParser String
a <:> b = (:) <$> a <*> b
(<++>) :: MyParser String -> MyParser String -> MyParser String
a <++> b = (++) <$> a <*> b
