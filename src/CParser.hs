-- module CParser where

-- import System
import System.FilePath
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import qualified Data.Map as Map 
import Foreign.C.String
import Foreign.Ptr
import Control.Monad

lexerStyle :: Token.LanguageDef ()
lexerStyle = Token.LanguageDef {
    Token.commentStart   = "/*",
    Token.commentEnd     = "*/",
    Token.commentLine    = "//",
    Token.nestedComments = False,
    Token.identStart     = letter <|> char '_',
    Token.identLetter    = alphaNum <|> oneOf "_" ,
    Token.opStart        = Token.opLetter lexerStyle,
    Token.opLetter       = oneOf "+-*/%=~&|^!?:.<>&*,;",
    Token.reservedOpNames= ["+=", "-=", "*=", "/=", "%=", 
                            "&=", "|=", "^=", "<<=", ">>=",
                            "<<", ">>", "&&", "||", "==", "!=", "<=", ">="],
    Token.reservedNames  = ["auto", "bool", "break", "case", "char", "const",
        "continue", "default", "do", "double", "else", "enum", "extern", 
        "float", "for", "goto", "if", "int", "long", "register", "return", 
        "short", "signed", "sizeof", "static", "struct", "switch", "typedef", 
        "union", "unsigned", "void", "volatile", "while" ],
    Token.caseSensitive  = True }

data Expr = Number Int
          | Identifier String
          | FuncCall String [Expr]
          deriving (Show)

type Stmts = [Stmt]
data Stmt = StmtWhile Expr Stmts
          | StmtIf Expr Stmts (Maybe Stmts)
          | StmtCall String [Expr]
          deriving (Show)

data CFuncDef = CFunc String [String] Stmts
    deriving (Show) 

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lexerStyle

parens, braces :: Parser a -> Parser a
parens = Token.parens lexer
braces = Token.braces lexer

natural :: Parser Integer
natural = Token.natural lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved, reservedOp :: String -> Parser ()
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

comma, semi :: Parser String
comma = Token.comma lexer
semi = Token.semi lexer

parseExpr :: Parser Expr
parseExpr = Number . fromIntegral <$> natural

parseStmt :: Parser Stmt
parseStmt = undefined

parseCall :: Parser Stmt
parseCall = do
    funcName <- identifier
    params <- parens $ parseExpr `endBy` comma
    return $ StmtCall funcName params


parseIf :: Parser Stmt
parseIf = do
    reserved "if"
    condition <- parens parseExpr
    thenDo <- braces $ statement `endBy` semi
    elseDo <- optionMaybe (reserved "else" *> braces (statement `endBy` semi))
    return $ StmtIf condition thenDo elseDo

statement :: Parser Stmt
statement = parseCall
        <|> parseIf

parseCFuncDef :: Parser CFuncDef
parseCFuncDef = do
    reserved "int"
    funcName <- identifier
    params <- parens $ reserved "void" >> return []
    body <- braces $ statement `endBy` semi
    return $ CFunc funcName params body

parseDefs :: Parser [CFuncDef]
parseDefs = many parseCFuncDef <?> "function definition"

parseLex :: Parser a -> Parser a
parseLex p = do 
    whiteSpace
    x <- p
    eof
    return x

main :: IO ()
main = do
    putStrLn "HELLO"
    eith <- parseFromFile (parseLex parseDefs) "../test/c/else.c"
    case eith of 
        Left err -> print err
        Right r -> print r
    putStrLn "done"
