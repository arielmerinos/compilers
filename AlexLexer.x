-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos

-- ¿Cómo correr el código?
-- Instalar la herramienta alex
-- alex AlexLexer.x -o Lexer.hs 
-- ghci Lexer.hs   Así es cómo lo corrió el profe (un poco diferente a cómo viene en el ejemplo de la documentación)
-- Ejemplo: > lexer " L2 :=1;  L3 :=0;  while  -( L2 = L2 )  do  L2 := L2 +1;  L3 := L3 +1 "      
-- > [Loc 2,Assign,Number 1,Seq,Loc 3,Assign,Number 0,Seq,While,Not,LP,Loc 2,Equal,Loc 2,RP,Do,Loc 2,Assign,Loc 2,Sum,Number 1,Seq,Loc 3,Assign,Loc 3,Sum,Number 1]

{
module Lexer where
}

%wrapper "basic"

$digit = 0-9               -- digits

tokens :-

  $white+                        ;
  ":="                           { \s -> Assign }
  "if"                           { \s -> If }
  "then"                         { \s -> Then }
  "else"                         { \s -> Else }
  ";"                            { \s -> Seq}
  "while"                        { \s -> While }
  "do"                           { \s -> Do }
  "skip"                         { \s -> Skip }
  "if"                           { \s -> If }
  "true"                         { \s -> Boolean True }
  "false"                        { \s -> Boolean False }
  "="                            { \s -> Equal }
  "&"                            { \s -> And }
  "-"                            { \s -> Not }
  "L"$digit+                     {\s -> Loc (read (drop 1 s)) } -- Eliminamos el primer elemento de s que sabemos que es "L"
  $digit+                        { \s -> Number (read s) }
  "("                            { \s -> LP }
  ")"                            { \s -> RP }
  "+"                            { \s -> Sum }

{
  
-- Each action has type :: String -> Token

-- The token type:
data Token 
  = Assign 
  | If 
  | Then 
  | Else 
  | Seq 
  | While 
  | Do 
  | Skip 
  | Boolean Bool 
  | Equal 
  | And 
  | Not 
  | Loc Int 
  | Number Int 
  | LP 
  | RP 
  | Sum 
  deriving Show

lexer = alexScanTokens
}