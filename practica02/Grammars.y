{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }


%token            
  -- Tokens ya presentes en MINILISP01
  '('                    { TokenPA }
  ')'                    { TokenPC }
  '+'                    { TokenSuma }
  '-'                    { TokenResta }
  not                   { TokenNot }
  bool                  { TokenBool $$ }
  nat                 { TokenNum $$ }


  -- RETO 1:
  -- Agrega aqui las reglas lexicas para:
  --   and, or, *, /, expt, <, >, <=, >=, eq, add1, sub1, zero?
  -- Recuerda reconocer <= y >= como tokens completos.
  and                   { TokenAnd }
  or                    { TokenOr }
  '*'                    { TokenMul }
  '/'                    { TokenDiv }
  expt                  { TokenExpt }
  '<'                    { TokenLT }
  '>'                    { TokenGT }
  "<="                  { TokenLE }
  ">="                  { TokenGE }
  eq                    { TokenEq }
  add1                  { TokenAdd1 }
  sub1                  { TokenSub1 }
  "zero?"                { TokenZeroP }

  nil                   { TokenNil }
  list                  { TokenList }
%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }

-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;
--   * operadores estrictamente binarios: expt y eq;
--   * operadores unarios: not, add1, sub1, zero?.

ASA : '(' '+' list1 ')'        { Add $3 }    
    | '(' '-' list1 ')'        { Sub $3 }
    | '(' '*' list1 ')'        { Mul $3 }
    | '(' '/' list1 ')'        { Div $3 }
    | '(' and list1 ')'        { And $3 }
    | '(' or list1 ')'         { Or $3 }
    | '(' not ASA ')'          { Not $3 }
    | '(' add1 ASA ')'         { Add1 $3 }
    | '(' sub1 ASA ')'         { Sub1 $3 }
    | '(' "zero?" ASA ')'       { ZeroP $3 }
    | '(' expt ASA ASA ')'     { Expt $3 $4 }
    | '(' '<' list1 ')'        { Lt $3 }
    | '(' '>' list1 ')'        { Gt $3 }
    | '(' "<=" list1 ')'       { Le $3 }
    | '(' ">=" list1 ')'       { Ge $3 }
    | '(' eq ASA ASA ')'       { EqP $3 $4 }


-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

ASA : nil                      { Nil }
    | '(' list list1 ')'     { List $3 }

list1 : ASA ASA             {[$1, $2]}
     | ASA list1             {$1 : $2}


{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  | Nil
  | List [ASA]
  deriving (Eq, Show)
}
