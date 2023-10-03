-- PRÁCTICA: Análisis Léxico

-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::

-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos

import Data.Char

-- Considera el siguiente lenguaje conocido como WHILE:
--      Comp C ::= L := E | if B then C else C | C; C | while B do C | skip
--      Bool B ::= true | f alse | E = E | B&B | − B
--      Arith E ::= L | n | (E + E)

-- El objetivo de esta practica es profundizar en el proceso de análisis léxico. La práctica consiste en definir las siguientes funciones:

-- Considera la siguiente definición de Tokens:
data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show


----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- :- 5 pts -: Define la función lexer que recibe una cadena del lenguaje WHILE y devuelve una lista de sus tokens. Esta función es un lexer ad hoc del lenguaje.

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
