-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::
-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos
----------------------------------------------------------------------------

-- ¿Cómo correr el código?
-- Instalar la herramienta Happy
-- happy ParserHappy.y -o Parser.hs 
-- ghci Parser.hs 
-- Ejemplos:
-- Escribir "main" (sin comillas) y después presionar ENTER
-- Posterior a ello proceder con el ejemplo:
-- L2 :=1; L3 :=0; while - L2 = L2  do L2 := (L2 + 1); L3 := (L3 + 1)  (Repetir el paso de arriba para probar otro ejemplo)
-- if  -true & false then skip else skip  
{
module Main where
import Data.Char
}

%name calc
%tokentype { Token }
%error { parseError }

%token
      ':='        { Assign }
      if          { If }
      then        { Then }
      else        { Else }
      ';'         { Seq }
      while       { While }
      do          { Do }
      skip        { Skip }
      Boolean     { Boolean $$} -- CHECAR
      '='         { Equal }
      '&'         { And }
      '-'         { Not }
      Loc         { Loc $$ }    -- CHECAR
      Number      { Number $$ } -- CHECAR
      '('         { LP }
      ')'         { RP }
      '+'         { Sum }
%%

C   : E ':=' E                             { AssignASA $1 $3 }
    | if B then C else C                   { IfThenElse $2 $4 $6 }
    | C ';' C                              { SeqASA $1 $3 }
    | while B do C                         { WhileDo $2 $4 }
    | skip                                 { SkipASA }


B   : Boolean                              { BoolASA $1 }
    | E '=' E                              { EqualASA $1 $3 }
    | B '&' B                              { AndASA $1 $3 }
    | '-' B                                { NotASA $2 }

E   : Loc                                  { LocASA $1 }
    | Number                               { NumberASA $1 }
    | '(' E '+' E ')'                      { SumASA $2 $4 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

-- Definiciones del Lenguaje
data C 
      = AssignASA E E 
      | IfThenElse B C C 
      | SeqASA C C 
      | WhileDo B C 
      | SkipASA 
      deriving Show

data B 
      = BoolASA Bool 
      | EqualASA E E 
      | AndASA B B 
      | NotASA B 
      deriving Show

data E 
      = LocASA Int 
      | NumberASA Int 
      | SumASA E E 
      deriving Show

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

-- :::::::::::::: LEXER DE LA PRACTICA 3 ::::::::::::::
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | x == ' ' = lexer xs -- Caso Espacio
    | x == ':' = if (length((filter(==':')asignacion)) == 1 && length((filter(=='=')asignacion)) == 1)  then [Assign] ++ lexer (drop lAssign ([x]++xs)) else error "Asignacion mal formada :C"
    | x == '=' = [Equal] ++ lexer xs
    | x == '&' = [And] ++ lexer xs
    | x == '-' = [Not] ++ lexer xs
    | x == '(' = [LP] ++ lexer xs
    | x == ')' = [RP] ++ lexer xs
    | x == ';' = [Seq] ++ lexer xs
    | x == '+' = [Sum] ++ lexer xs
    | isDigit x = [Number digito] ++ lexer (drop(length(cadenaNumerica)) ([x]++xs))
    | isAlpha x = let (cadena, resto) = span isAlphaNum (x:xs)
                in case cadena of
                    "if"      -> [If] ++ lexer resto          -- Token If
                    "then"    -> [Then] ++ lexer resto        -- Token Then
                    "else"    -> [Else] ++ lexer resto        -- Token Else
                    "while"   -> [While] ++ lexer resto       -- Token While
                    "do"      -> [Do] ++ lexer resto          -- Token Do 
                    "skip"    -> [Skip] ++ lexer resto        -- Token Skip
                    "true"    -> Boolean True : lexer resto   -- Token Boolean True
                    "false"   -> Boolean False : lexer resto  -- Token Boolean False
                    ('L':num) -> Loc (read num) : lexer resto -- Token Loc Int
                    _         -> error "ERROR: Token no reconocido :C"
    where
        cadenaAssign = ([x]++ xs)
        asignacion = (getSubAssigment cadenaAssign 0)
        lAssign = length(asignacion)

        cadenaNumerica = getSubNum([x] ++ xs)
        digito = read(cadenaNumerica)

-- Función que ayuda en la identificación de la asignación en el lexer.
-- Aseguramos que se capturen correctamente ":" y "=", y se ignoren los espacios en blanco.
getSubAssigment :: String -> Int -> String
getSubAssigment [] _ = []
getSubAssigment x 2 = []
getSubAssigment (x:xs) i
    | x == ':' && i == 0  = [x] ++ getSubAssigment xs 1
    | x == '=' && i == 1  = [x] ++ getSubAssigment xs 2
    | x == ' ' && i /= 2  = [x] ++ getSubAssigment xs i
    | otherwise           = getSubAssigment xs 2

getSubNum :: String -> String
getSubNum [] = []
getSubNum (x:xs)
    | x /= ' ' && isDigit x = [x] ++ getSubNum xs
    | otherwise              = []


-- :::::::::::::: MAIN ::::::::::::::

--main = getContents >>= print . calc . lexer
main = do
    input <- getLine
    let tokens = lexer input
    print $ calc tokens

}
