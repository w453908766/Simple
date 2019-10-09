{-# LANGUAGE OverloadedStrings #-}

module Parser (
  parseExpr,
  parseModule
) where

import Control.Monad.Identity (Identity)
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import qualified Data.Text.Lazy as L

import Lexer
import Syntax


variable :: Parser Expr
variable = do
  x <- identifier
  return (Var x)

number :: Parser Expr
number = do
  n <- Tok.natural lexer
  return (Lit (LInt (fromIntegral n)))

bool :: Parser Expr
bool = (reserved "True" >> return (Lit (LBool True)))
    <|> (reserved "False" >> return (Lit (LBool False)))

lambda :: Parser Expr
lambda = do
  reservedOp "\\"
  args <- many identifier
  reservedOp "->"
  body <- expr
  return $ foldr Lam body args

ifthen :: Parser Expr
ifthen = do
  reserved "if"
  cond <- expr
  reservedOp "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return (If cond tr fl)

aexp :: Parser Expr
aexp =
      parens expr
  <|> bool
  <|> number
  <|> ifthen
  <|> lambda
  <|> variable

term :: Parser Expr
term = do
  xs <- many1 aexp
  return $ case xs of
    [x] -> x
    xs  -> foldl1 App xs

infixOp :: String -> (a -> a -> a) -> Ex.Assoc -> Ex.Operator L.Text () Identity a
infixOp x f = Ex.Infix (reservedOp x >> return f)

--table :: Operators Expr
table :: [[Ex.Operator L.Text () Identity Expr]] 
table = [
    [
      infixOp "*" (Op Mul) Ex.AssocLeft
    ],
    [
      infixOp "+" (Op Add) Ex.AssocLeft
    , infixOp "-" (Op Sub) Ex.AssocLeft
    ],
    [
      infixOp "==" (Op Eql) Ex.AssocLeft
    ]
  ]

expr :: Parser Expr
expr = Ex.buildExpressionParser table term

define :: Parser Decl
define = do
  reserved "define"
  name <- identifier
  args <- many identifier
  reservedOp "="
  body <- expr
  return $ Define name (foldr Lam body args)

val :: Parser Decl
val = do
  ex <- expr
  return $ Define "it" ex

decl :: Parser Decl
decl = define <|> val

top :: Parser Decl
top = do
  x <- decl
  optional semi
  return x

modl ::  Parser [Decl]
modl = many top

parseExpr :: L.Text -> Either ParseError Expr
parseExpr input = parse (contents expr) "<stdin>" input

parseModule ::  FilePath -> L.Text -> Either ParseError [Decl]
parseModule fname input = parse (contents modl) fname input
