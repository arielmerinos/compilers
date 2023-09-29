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
    | x == '=' = [Equal] ++ lexer xs -- Token Equal
    | x == '&' = [And] ++ lexer xs   -- Token And
    | x == '-' = [Not] ++ lexer xs   -- Token Not
    | x == '(' = [LP] ++ lexer xs    -- Token LP
    | x == ')' = [RP] ++ lexer xs    -- Token RP
    | x == ';' = [Seq] ++ lexer xs   -- Token Seq
    | x == '+' = [Sum] ++ lexer xs   -- Token Sum
    -- Token Assign
    | x == ':' = if (length((filter(==':')asignacion)) == 1 && length((filter(=='=')asignacion)) == 1)  
                    then [Assign] ++ lexer (drop lAssign ([x]++xs)) 
                    else error "Asignacion mal formada :C"
    -- Token While
    | x == 'w' = if (length((filter(=='w')whilee)) == 1 && length((filter(=='h')whilee)) == 1 && length((filter(=='i')whilee)) == 1 && length((filter(=='l')whilee)) == 1 && length((filter(=='e')whilee)) == 1) 
                    then [While] ++ lexer (drop lWhile ([x]++xs)) 
                    else error "While mal formado :C"
    -- Token If
    | x == 'i' = if (length((filter(=='i')iff)) == 1 && length((filter(=='f')iff)) == 1)  
                    then [If] ++ lexer (drop lIf ([x]++xs)) 
                    else error "If mal formada :C"
    -- Token Do
    | x == 'd' = if (length((filter(=='d')doo)) == 1 && length((filter(=='o')doo)) == 1)  
                    then [Do] ++ lexer (drop lDo ([x]++xs)) 
                    else error "Do mal formada :C"
    -- Token Skip
    | x == 's' = if (length((filter(=='s')skipp)) == 1 && length((filter(=='k')skipp)) == 1 && length((filter(=='i')skipp)) == 1 && length((filter(=='p')skipp)) == 1) 
                    then [Skip] ++ lexer (drop lSkip ([x]++xs)) 
                    else error "Skip mal formado :C"
    -- Token Else
    | x == 'e' = if (length((filter(=='e')elsee)) == 2 && length((filter(=='l')elsee)) == 1 && length((filter(=='s')elsee)) == 1) 
                    then [Else] ++ lexer (drop lElse ([x]++xs)) 
                    else error "Else mal formado :C"
    -- Token False
    | x == 'f' = if (length((filter(=='f')falsee)) == 1 && length((filter(=='a')falsee)) == 1 && length((filter(=='l')falsee)) == 1 && length((filter(=='s')falsee)) == 1 && length((filter(=='e')falsee)) == 1) 
                    then [Boolean False] ++ lexer (drop lFalse ([x]++xs)) 
                    else error "Bool False mal formado :C"
    -- Token True y Then (ambos empiezan con 't')
    | x == 't' && siguienteCaracter 'r' xs =  
        if (length (filter (=='t') truee) == 1 && length (filter (=='r') truee) == 1 && length (filter (=='u') truee) == 1 && length (filter (=='e') truee) == 1) 
            then [Boolean True] ++ lexer (drop lTrue ([x] ++ xs)) 
            else error "Bool True mal formado :C"
    | x == 't' && siguienteCaracter 'h' xs =  
        if (length (filter (=='t') thenn) == 1 && length (filter (=='h') thenn) == 1 && length (filter (=='e') thenn) == 1 && length (filter (=='n') thenn) == 1) 
            then [Then] ++ lexer (drop lThen ([x] ++ xs)) 
            else error "Then mal formado :C"
    

    | isDigit x = [Number digito] ++ lexer (drop(length(cadenaNumerica)) ([x]++xs))
    | isAlpha x = let (cadena, resto) = span isAlphaNum (x:xs)
                  in case cadena of 
                    ('L':num) -> Loc (read num) : lexer resto -- Token Loc Int
                    _         -> error "Loc no válido :C"
    | otherwise = error "ERROR: Token no reconocido :C"


    where
        cadena = ([x]++ xs)

        asignacion = (getSubAssigment cadena 0)
        lAssign = length(asignacion)

        doo = (getSubDo cadena 0)
        lDo = length(doo)

        iff = (getSubIf cadena 0)
        lIf = length(iff)

        whilee = (getSubWhile cadena 0)
        lWhile = length(whilee)

        falsee = (getSubFalse cadena 0)
        lFalse = length(falsee)

        skipp = (getSubSkip cadena 0)
        lSkip = length(skipp)

        elsee = (getSubElse cadena 0)
        lElse = length(elsee)

        thenn = (getSubThen cadena 0)
        lThen = length(thenn)

        truee = (getSubTrue cadena 0)
        lTrue = length(truee)

        cadenaNumerica = getSubNum([x] ++ xs)
        digito = read(cadenaNumerica)


getSubNum :: String -> String
getSubNum [] = []
getSubNum (x:xs)
    | x /= ' ' && isDigit x = [x] ++ getSubNum xs
    | otherwise              = []



-- ----------------------- FUNCIONES AUXILIARES -----------------------


-- Función auxiliar que verifica si el siguiente carácter en la cadena es igual a un carácter dado 
-- Devuelve True si es así. También ignora los espacios en blanco.
siguienteCaracter :: Char -> String -> Bool
siguienteCaracter _ [] = False
siguienteCaracter c (x:xs)
    | x == c    = True
    | isSpace x = siguienteCaracter c xs
    | otherwise = False

-- Función que ayuda en la identificación de la asignación en el lexer.
-- Aseguramos que se capturen correctamente ":" y "=", y se ignoren los espacios en blanco.
-- Procesa cadenas como " :    =   " 
getSubAssigment :: String -> Int -> String
getSubAssigment [] _ = []
getSubAssigment x 2 = []
getSubAssigment (x:xs) i
    | x == ':' && i == 0  = [x] ++ getSubAssigment xs 1
    | x == '=' && i == 1  = [x] ++ getSubAssigment xs 2
    | x == ' ' && i /= 2  = [x] ++ getSubAssigment xs i
    | otherwise           = getSubAssigment xs 2

-- Función que ayuda en la identificación de "if" en el lexer.
-- Aseguramos que se capturen correctamente "i" y "f", y se ignoren los espacios en blanco.
-- Procesa cadenas como " i    f   " 
getSubIf :: String -> Int -> String
getSubIf [] _ = []
getSubIf x 2 = []
getSubIf (x:xs) i
    | x == 'i' && i == 0  = [x] ++ getSubIf xs 1
    | x == 'f' && i == 1  = [x] ++ getSubIf xs 2
    | x == ' ' && i /= 2  = [x] ++ getSubIf xs i
    | otherwise           = getSubIf xs 2

-- Función que ayuda en la identificación de "do" en el lexer.
-- Aseguramos que se capturen correctamente "d" y "o", y se ignoren los espacios en blanco.
-- Procesa cadenas como " d    o   " 
getSubDo :: String -> Int -> String
getSubDo [] _ = []
getSubDo x 2 = []
getSubDo (x:xs) i
    | x == 'd' && i == 0  = [x] ++ getSubDo xs 1
    | x == 'o' && i == 1  = [x] ++ getSubDo xs 2
    | x == ' ' && i /= 2  = [x] ++ getSubDo xs i
    | otherwise           = getSubDo xs 2

-- Función que ayuda en la identificación de "while" en el lexer.
-- Aseguramos que se capturen correctamente "w", "h", "i", "l" y "e", y se ignoren los espacios en blanco.
-- Procesa cadenas como " w   h   i   l   e   " 
getSubWhile :: String -> Int -> String
getSubWhile [] _ = []
getSubWhile x 5 = []
getSubWhile (x:xs) i
    | x == 'w' && i == 0  = [x] ++ getSubWhile xs 1
    | x == 'h' && i == 1  = [x] ++ getSubWhile xs 2
    | x == 'i' && i == 2  = [x] ++ getSubWhile xs 3
    | x == 'l' && i == 3  = [x] ++ getSubWhile xs 4
    | x == 'e' && i == 4  = [x] ++ getSubWhile xs 5
    | x == ' ' && i /= 5  = [x] ++ getSubWhile xs i
    | otherwise           = getSubWhile xs 5

-- Función que ayuda en la identificación de "false" en el lexer.
-- Aseguramos que se capturen correctamente "f", "a", "l", "s" y "e", y se ignoren los espacios en blanco.
-- Procesa cadenas como " f   a   l   s   e   " 
getSubFalse :: String -> Int -> String
getSubFalse [] _ = []
getSubFalse x 5 = []
getSubFalse (x:xs) i
    | x == 'f' && i == 0  = [x] ++ getSubFalse xs 1
    | x == 'a' && i == 1  = [x] ++ getSubFalse xs 2
    | x == 'l' && i == 2  = [x] ++ getSubFalse xs 3
    | x == 's' && i == 3  = [x] ++ getSubFalse xs 4
    | x == 'e' && i == 4  = [x] ++ getSubFalse xs 5
    | x == ' ' && i /= 5  = [x] ++ getSubFalse xs i
    | otherwise           = getSubFalse xs 5

-- Función que ayuda en la identificación de "skip" en el lexer.
-- Aseguramos que se capturen correctamente "s", "k", "i" y "p", y se ignoren los espacios en blanco.
-- Procesa cadenas como " s   k   i   p   " 
getSubSkip :: String -> Int -> String
getSubSkip [] _ = []
getSubSkip x 4 = []
getSubSkip (x:xs) i
    | x == 's' && i == 0  = [x] ++ getSubSkip xs 1
    | x == 'k' && i == 1  = [x] ++ getSubSkip xs 2
    | x == 'i' && i == 2  = [x] ++ getSubSkip xs 3
    | x == 'p' && i == 3  = [x] ++ getSubSkip xs 4
    | x == ' ' && i /= 4  = [x] ++ getSubSkip xs i
    | otherwise           = getSubSkip xs 4

-- Función que ayuda en la identificación de "else" en el lexer.
-- Aseguramos que se capturen correctamente "e", "l", "s" y "e", y se ignoren los espacios en blanco.
-- Procesa cadenas como " e   l   s   e   " 
getSubElse :: String -> Int -> String
getSubElse [] _ = []
getSubElse x 4 = []
getSubElse (x:xs) i
    | x == 'e' && i == 0  = [x] ++ getSubElse xs 1
    | x == 'l' && i == 1  = [x] ++ getSubElse xs 2
    | x == 's' && i == 2  = [x] ++ getSubElse xs 3
    | x == 'e' && i == 3  = [x] ++ getSubElse xs 4
    | x == ' ' && i /= 4  = [x] ++ getSubElse xs i
    | otherwise           = getSubElse xs 4

-- Función que ayuda en la identificación de "then" en el lexer.
-- Aseguramos que se capturen correctamente "t", "h", "e" y "n", y se ignoren los espacios en blanco.
-- Procesa cadenas como " t   h   e   n   " 
getSubThen :: String -> Int -> String
getSubThen [] _ = []
getSubThen x 4 = []
getSubThen (x:xs) i
    | x == 't' && i == 0  = [x] ++ getSubThen xs 1
    | x == 'h' && i == 1  = [x] ++ getSubThen xs 2
    | x == 'e' && i == 2  = [x] ++ getSubThen xs 3
    | x == 'n' && i == 3  = [x] ++ getSubThen xs 4
    | x == ' ' && i /= 4  = [x] ++ getSubThen xs i
    | otherwise           = getSubThen xs 4

-- Función que ayuda en la identificación de "true" en el lexer.
-- Aseguramos que se capturen correctamente "t", "r", "u" y "e", y se ignoren los espacios en blanco.
-- Procesa cadenas como " t   r   u   e   " 
getSubTrue :: String -> Int -> String
getSubTrue [] _ = []
getSubTrue x 4 = []
getSubTrue (x:xs) i
    | x == 't' && i == 0  = [x] ++ getSubTrue xs 1
    | x == 'r' && i == 1  = [x] ++ getSubTrue xs 2
    | x == 'u' && i == 2  = [x] ++ getSubTrue xs 3
    | x == 'e' && i == 3  = [x] ++ getSubTrue xs 4
    | x == ' ' && i /= 4  = [x] ++ getSubTrue xs i
    | otherwise           = getSubTrue xs 4