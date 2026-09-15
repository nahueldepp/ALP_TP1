module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST

-----------------------
-- Función para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "skip", "if", "else", "repeat", "until"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "++"
                        , "--"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser de expresiones enteras
-----------------------------------
--rehago la grmatica para dar jerarquía
{-
intexp :: = interm  
intterm :: = factor + factor | factor - factor 
factor :: = (intexp) | nat  | var++ | var--| var | -var
            
-}
addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)

-- No se que hacer con el minus U, preguntar
mulop :: Parser (Exp Int -> Exp Int -> Exp Int)
mulop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)

--esto no sé si esta bien, preguntar
varop :: Parser (Exp Int)
varop = do 
          v <- identifier lis 
          ((try(reservedOp lis "++") >> return (VarInc v))
            <|> (try(reservedOp lis "--") >> return (VarDec v)))
          (return (Var v))

{-chainl1 p op parses one or more occurrences of p, 
separated by op Returns a value obtained by a left associative application of all functions returned by op to the values returned by p. 
This parser can for example be used to eliminate left recursion which typically occurs in expression grammars.
https://hackage.haskell.org/package/parsec-3.1.18.0/docs/Text-Parsec.html#g:1-}
intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop


intterm :: Parser (Exp Int)
intterm = chainl1 factor  mulop

factor  :: Parser (Exp Int)
factor  = (parens lis intexp)
  <|> (do 
              reservedOp lis "-"
              e <- factor
              return (UMinus e))
    <|> try (do
                n <- natural lis
                return (Const (fromInteger n))) 
      <|> varop

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = chainl1 


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
