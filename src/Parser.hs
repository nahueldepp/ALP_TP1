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
intexp :: = interm  | intterm + intterm | intterm - intterm
intterm :: =  factor * factor | factor / factor|
factor :: = (intexp) | nat  | var++ | var--| var | -var
            
-}
addop :: Parser (Exp Int -> Exp Int -> Exp Int)
addop = (reservedOp lis "+" >> return Plus)
  <|> (reservedOp lis "-" >> return Minus)


mulop :: Parser (Exp Int -> Exp Int -> Exp Int)
mulop = (reservedOp lis "*" >> return Times)
  <|> (reservedOp lis "/" >> return Div)


varop :: Parser (Exp Int)
varop = do 
          v <- identifier lis 
          (try(reservedOp lis "++") >> return (VarInc v))
            <|> (try(reservedOp lis "--") >> return (VarDec v))
              <|> return (Var v)
            

intexp :: Parser (Exp Int)
intexp = chainl1 intterm addop


intterm :: Parser (Exp Int)
intterm = chainl1 intfactor  mulop

intfactor  :: Parser (Exp Int)
intfactor  = (parens lis intexp)
  <|> (do 
              reservedOp lis "-"
              e <- intfactor
              return (UMinus e))
    <|> try (do
                n <- natural lis
                return (Const (fromInteger n))) 
      <|> varop

------------------------------------
--- Parser de expresiones booleanas
------------------------------------

--modifico la gramatica
{-
boolexp   :: = !boolterm | boolterm && boolterm | boolterm || boolterm
boolterm  :: = boolfactor | boolfactor == boolfactor | boolfactor != boolfactor 
              |boolfactor < boolfactor | boolfactor > boolfacor 
boolfactor :: = true | false  | (boolexpr)
-}  



relop :: Parser (Exp Int -> Exp Int -> Exp Bool)
relop =
        (reservedOp lis "==" >> return Eq)
        <|> (reservedOp lis "!=" >> return NEq)
        <|> (reservedOp lis ">"  >> return Gt)
        <|> (reservedOp lis "<"  >> return Lt)

orop :: Parser (Exp Bool -> Exp Bool -> Exp Bool)
orop = (reservedOp  lis "||" >> (return Or))

andop :: Parser (Exp Bool -> Exp Bool -> Exp Bool) 
andop = (reservedOp lis "&&"  >> (return And))

comparison :: Parser (Exp Bool)
comparison = do
                e1 <- intexp
                op <- relop
                e2 <- intexp
                return (op e1 e2)


boolexp :: Parser (Exp Bool)
boolexp =   chainl1 boolterm orop


boolterm :: Parser (Exp Bool)
boolterm =  chainl1 boolfactor andop

boolfactor :: Parser (Exp Bool)
boolfactor =  (parens lis boolexp)
              <|> try(do
                  reservedOp lis "!"
                  b <- boolexp
                  return (Not b))
              <|> comparison
              <|> (reserved lis "true" >> (return BTrue))
              <|> (reserved lis "false" >> (return BFalse))
              

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm =  (reserved lis "skip" >> (return Skip))
        <|> try(do 
                  v <- identifier lis 
                  reserved lis "=" 
                  iexp <- intexp 
                  return (Let v iexp))
          <|> try(do
                    reserved  lis "if"
                    b <- boolexp
                    com1 <- braces lis comm
                    reserved lis "else"
                    com2 <- braces lis comm
                    return (IfThenElse b com1 com2) 
                    )
            <|> try(do
                      reserved lis "repeat" 
                      com <- braces lis comm
                      reserved lis "until"
                      b <- boolexp
                      (return (RepeatUntil com b)))
                        


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
