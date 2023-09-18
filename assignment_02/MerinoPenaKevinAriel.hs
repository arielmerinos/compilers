-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos
-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Cristóbal Morales Karen | 317180321 | @NerakCM

--- NO MODIFICAR LA FIRMA DE NINGUNA FUNCIÓN --- NO MODIFICAR LA FIRMA DE NINGUNA FUNCIÓN --- NO MODIFICAR LA FIRMA DE NINGUNA FUNCIÓN 

import Data.Char
import Data.List

-- Considera el siguiente lenguaje de expresiones aritméticas y booleanas con notación post-fija EAB:
-- S ::= AExp | BExp 
-- AExpa::= v | n | a_0 a_1+ | a_0 a_1− 
-- BExpb::= t | f | b_0 b_1&& | b_0 b_1|| | a_0 a_1 =
-- El objetivo de esta practica es acercamos al proceso de compilación traduciendo este lenguaje a lenguaje ensamblador. 
-- La practica consiste en definir las siguientes funciones:

-- El análisis léxico consiste en reunir secuencias de caracteres en unidades significativas llamadas tokens.
--  Considera la siguiente definición de Tokens:

data Token = Var String | Number Int | Boolean Bool | Sum | Subs | And | Or | Equal deriving Show

----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- :- 2 pts -: Define la función lexer que recibe una cadena del lenguaje EAB y devuelve una lista de sus tokens.
lexer :: String -> [Token]
lexer [] = [] 
lexer (x:xs) 
    | x == ' ' = lexer xs -- Caso Espacio Listo
    | x == '+' = [Sum] ++ lexer xs -- Caso Sum Listo
    | x == '-' = [Subs] ++ lexer xs -- Caso Subs Listo
    | x == '&' = if length (filter (== '&') (getSubAnd([x]++xs) 0 '&')) == 2 then [And] ++ lexer (drop (length (getSubAnd([x]++xs) 0 '&')) ([x]++xs)) else error "And suelto" -- Caso And Listo
    | x == '|' = if length (filter (== '|') (getSubOr([x]++xs) 0 '|')) == 2 then [Or] ++ lexer (drop (length (getSubOr([x]++xs) 0 '|')) ([x]++xs)) else error "Or suelto" -- Caso Or Listo
    | x == '=' = if length (filter (== '=') (getSubEqual([x]++xs) 0 '=')) == 2 then [Equal] ++ lexer (drop (length (getSubEqual([x]++xs) 0 '=')) ([x]++xs)) else error "Equal suelto" -- Caso Equal Listo
    | isDigit x = [Number digito] ++ lexer (drop(length(cadenaNumerica)) ([x]++xs))
    | x == 'f' = if length (filter (== 'f') (variable)) == 1 && isVoidString (getTail variable) then [Boolean False] ++ lexer (drop (length variable) ([x]++xs)) else [Var variable] ++ lexer (drop (length variable) ([x]++xs))
    | x == 't' = if length (filter (== 't') (variable)) == 1 && isVoidString (getTail variable) then [Boolean True] ++ lexer (drop (length variable) ([x]++xs)) else [Var variable] ++ lexer (drop (length variable) ([x]++xs))
    | isAlpha x = [Var variable] ++ lexer (drop(length(variable)) ([x]++xs))
    where
        cadenaNumerica = getSubNum([x]++xs)
        digito = read(cadenaNumerica)
        variable = getSubString(([x]++xs))


getSubAnd :: String -> Int -> Char -> String
getSubAnd x 2 _ = []
getSubAnd (x:xs) i '&'
    | x == '&' && i/= 2 = [x] ++ getSubAnd xs (i+1) '&'
    | x == ' ' && i/= 2 = [x] ++ getSubAnd xs i '&'
    | otherwise = []

getSubOr :: String -> Int -> Char -> String
getSubOr x 2 _ = []
getSubOr (x:xs) i '|'
    | x == '|' && i/= 2 = [x] ++ getSubOr xs (i+1) '|'
    | x == ' ' && i/= 2 = [x] ++ getSubOr xs i '|'
    | otherwise = []

getSubEqual :: String -> Int -> Char -> String
getSubEqual x 2 _ = []
getSubEqual (x:xs) i '='
    | x == '=' && i/= 2 = [x] ++ getSubEqual xs (i+1) '='
    | x == ' ' && i/= 2 = [x] ++ getSubEqual xs i '='
    | otherwise = []

getSubNum :: String -> String
getSubNum [] = []
getSubNum (x:xs)
    | x /= ' ' && isDigit x = [x] ++ getSubNum xs
    | otherwise = []

getSubString :: String -> String
getSubString [] = []
getSubString (x:xs)
    | x /= ' ' && isAlpha x = [x] ++ getSubString xs
    | otherwise = []

isVoidString :: String -> Bool
isVoidString [] = True
isVoidString (x:xs) = if isAlpha x then False else isVoidString xs

getTail :: String -> String
getTail [] = []
getTail (x:xs) = xs

-- { - Ejemplo -}
-- > lexer "22␣3␣+␣var␣==␣t␣&&"
-- > [Number 22,Number 3,Sum,Var "var",Equal,Boolean True,And]
-- > lexer "22␣+␣var␣var␣f␣t␣==␣&&"
-- > [Number 22,Sum,Var "var",Var "var",Boolean False,Boolean True,Equal,And]

-- Hay una función en haskell que ya pasa de string a número or ejemplo 
-- Podremos resolver toda la función con pattern matching, considerar los espacios vacíos

----------------------------------------------------- EJERCICIO 2 ---------------------------------------------------------------

-- Análisis Sintáctico
-- El análisis sintáctico consiste en determinar la estructura del programa, es decir, determina los elementos estructurales del programa 
-- así como sus relaciones. Considera la siguiente definición de ASA y Stack:

data ASA = VarASA String | NumberASA Int | BooleanASA Bool | Op Token ASA ASA deriving Show 
type Stack = [ASA]

-- 1.9 pts
-- Define la función scannerAux que recibe una lista de tokens y un Stack, y devuelve su árbol de sintaxis abstracta correspondiente.
-- Hint: La solución es análoga al algoritmo para evaluar expresiones en notación post-fija. ¿Qué debemos guardar en el stack en lugar 
-- del valor parcial?

scannerAux :: [Token] -> Stack -> ASA
scannerAux [] (asa_Final:_) = asa_Final 
scannerAux [] _ = error "Expresión mal formada."
scannerAux (t:tokens) stack =
    case t of
        Var v -> scannerAux tokens (VarASA v : stack)
        Number n -> scannerAux tokens (NumberASA n : stack)
        Boolean b -> scannerAux tokens (BooleanASA b : stack)
        Sum -> operacionBinaria Sum
        Subs -> operacionBinaria Subs
        And -> operacionBinaria And
        Or -> operacionBinaria Or
        Equal -> operacionBinaria Equal
  where
    operacionBinaria op =
        case stack of
            (operador1: operador2: pilaRestante) -> scannerAux tokens (Op op operador1 operador2 : pilaRestante)
            _ -> error "Expresión mal formada."

-- { - Ejemplo -}
-- > scannerAux [Number 22,Number 3,Sum,Var "var",Equal,Boolean True,And] [] > Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3)
-- (NumberASA 22)))
-- > scannerAux [Number 22,Sum,Var "var",Var "var",Boolean False,Boolean True ,Equal ,And] []
-- > Expresion mal formada.

-- :- 0.1 pts -:
-- Define la función scanner que recibe una lista de tokens y devuelve su árbol de sintaxis abstracta correspondiente.
scanner :: [Token] -> ASA
scanner tokens = scannerAux tokens []

-- { - Ejemplo -}
-- > scanner [Number 22,Number 3,Sum,Var "var",Equal,Boolean True,And]
-- > Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3)
-- (NumberASA 22)))
-- > scanner [Number 22,Sum,Var "var",Var "var",Boolean False,Boolean True,Equal,And] > Expresion mal formada.

----------------------------------------------------- EJERCICIO 3 ---------------------------------------------------------------

-- Análisis Semántico
-- El análisis semántico consiste en verificar que el significado del programa sea claro y consistente con la especificación del lenguaje. 
-- Considera los siguientes tipos y sus juicios:


data Type = Num | Bool deriving Show

-- :- 1.9 -: pts Define la función TypeCheckerAux que recibe un ASA y devuelve el tipo de la expresión únicamente si el tipado del programa 
-- es consistente. En otro caso arroja un error indicando el problema con el programa.

typeCheckerAux :: ASA -> Type
typeCheckerAux (VarASA _) = Num
typeCheckerAux (NumberASA _) = Num
typeCheckerAux (BooleanASA _) = Bool
typeCheckerAux (Op token expr1 expr2) = case token of
    And -> verificaBool expr1 expr2
    Or -> verificaBool expr1 expr2
    Equal -> verificaEqual expr1 expr2
    Sum -> verificaNum expr1 expr2
    Subs -> verificaNum expr1 expr2
    where
        verificaBool e1 e2 = case (typeCheckerAux e1, typeCheckerAux e2) of
            (Bool, Bool) -> Bool
            (x1, x2) -> error $ "El tipo de los argumentos " ++ show e1 ++ " y " ++ show e2 ++ " no son los esperados para el operador " ++ show token
        verificaEqual e1 e2 = case (typeCheckerAux e1, typeCheckerAux e2) of
            (Num, Num) -> Bool
            (x1, x2) -> error $ "El tipo de los argumentos " ++ show e1 ++ " y " ++ show e2 ++ " no son los esperados para el operador " ++ show token
        verificaNum e1 e2 = case (typeCheckerAux e1, typeCheckerAux e2) of
            (Num, Num) -> Num
            (x1, x2) -> error $ "El tipo de los argumentos " ++ show e1 ++ " y " ++ show e2 ++ " no son los esperados para el operador " ++ show token


-- { - Ejemplo -}
-- > typeCheckerAux (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum
-- (NumberASA 3) (NumberASA 22)))) > Bool
-- > typeCheckerAux (Op And (NumberASA 43) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22))))
-- > El tipo de los argumentos NumberASA 43 y Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)) no son los esperados para el operador And


-- :- 0.1 -: pts 
-- Define la función TypeChecker que recibe un ASA y devuelve dicho ASA si el tipado del programa es consistente.

typeChecker :: ASA -> ASA
typeChecker expr =
    case typeCheckerAux expr of
        Num -> expr
        Bool -> expr
        
-- { - Ejemplo -}
-- > typeChecker (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA
-- 3) (NumberASA 22))))
-- > Op And (BooleanASA False) (Op Equal (VarASA "var") (Op Sum (NumberASA 3)
-- (NumberASA 22)))
-- > typeChecker (Op And (NumberASA 43) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22))))
-- > El tipo de los argumentos NumberASA 43 y Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22)) no son los esperados para el operador And

-- Tenemos que notificar cuáles son los dos argumentos que está recibiendo 

----------------------------------------------------- EJERCICIO 4 ---------------------------------------------------------------
-- 5 Optimización de Código Fuente

-- :- 1 pto -: 
-- El plegado constante es una optimización que elimina expresiones cuyo valor se puede calcular previo a ejecutar el código. 
-- Define la función constantFolding que recibe un ASA y devuelve el ASA resultante de aplicarle plegado contante.

constantFolding :: ASA -> ASA
constantFolding (Op And (BooleanASA True) der) = constantFolding der
constantFolding (Op And izq (BooleanASA True)) = constantFolding izq
constantFolding (Op Or (BooleanASA False) der) = constantFolding der
constantFolding (Op Or izq (BooleanASA False)) = constantFolding izq
constantFolding (Op op izq der) =
  let
    foldIzq = constantFolding izq
    foldDer = constantFolding der
  in
    case (op, foldIzq, foldDer) of
      (Sum, NumberASA a, NumberASA b) -> NumberASA (a + b)
      (Subs, NumberASA a, NumberASA b) -> NumberASA (a - b)
      (And, BooleanASA a, BooleanASA b) -> BooleanASA (a && b)
      (Or, BooleanASA a, BooleanASA b) -> BooleanASA (a || b)
      (Equal, NumberASA x, NumberASA y) -> BooleanASA (x == y)
      _ -> Op op foldIzq foldDer
constantFolding atomico = atomico
-- { - Ejemplo -}
-- { - Ejemplo -}
-- > constantFolding (Op And (BooleanASA True) (Op Equal (VarASA "var") (Op Sum (NumberASA 3) (NumberASA 22))))
-- > Op Equal (VarASA "var") (NumberASA 25)
-- > constantFolding (Op And (Op Equal (VarASA "var") (VarASA "var")) (Op Equal (VarASA "var") (Op Sum (VarASA "var") (NumberASA 22))))
-- > Op And (Op Equal (VarASA "var") (VarASA "var")) (Op Equal (VarASA "var") (Op Sum (VarASA "var") (NumberASA 22)))


-- Devuelve el ASA resultante de aplicarle plegado contante
-- 


-- Considera las siguientes definiciones
data Value = N Int | B Bool | S String 
instance Show Value where
    show (N n) = show n 
    show (B b) = show b 
    show (S s) = show s
data ThreeAddress = Assign String Value| Operation String String Token String 
instance Show ThreeAddress where
    show (Assign t v) = show t ++ " = " ++ show v
    show (Operation t a op b) = show t ++ " = " ++ show a ++ tokenThreeAddress op ++ show b

-- :- 0.2 pts -: 
-- Define la función fresh que recibe una lista de enteros y devuelve el menor natural posible que no este en la lista.
fresh :: [Int] -> Int 
fresh l = freshAux  (sort l) 0 -- Si no ordenamos la lista y ponemos una lista descendente no obtendremos el número deseado
  where                        -- Por ejemplo fresh [2, 1, 0] = 1 (si no la ordenamos)  
    freshAux :: [Int] -> Int -> Int
    freshAux [] n = n
    freshAux (x:xs) n
      | x == n = freshAux xs (n + 1)
      | otherwise =  freshAux xs n
-- { - Ejemplo -}
-- > fresh [1 ,2 ,3]
-- > 0
-- > fresh [4 ,2 ,3 ,0] > 1

-- Generar tantas variables temporales como sean necesarias

-- :- 0.7 pts -:
-- Define la función threeAddressAux que recibe un ASA y la lista de los enteros que han sido usado en variables temporales, y 
-- devuelve una tripleta con la traducción correspondiente en código de tres direcciones, la variable temporal que almacena el 
-- resultado actual y los enteros que se han usado para variables temporales.

threeAddressAux :: ASA -> [Int] -> ([ThreeAddress],String,[Int]) 
threeAddressAux (VarASA v) ts = (c', temp, i:ts)
   where
      i = fresh ts
      temp = "t" ++ show i
      c' = [Assign temp (S v)]
threeAddressAux (NumberASA n) ts = (c', temp, i:ts)
   where
      i = fresh ts
      temp = "t" ++ show i
      c' = [Assign temp (N n)]
threeAddressAux (BooleanASA b) ts = (c', temp, i:ts)
   where
      i = fresh ts
      temp = "t" ++ show i
      c' = [Assign temp (B b)] 
threeAddressAux (Op op a b) ts = (a' ++ b' ++ c', temp, i:bs)
   where
      (a', adA, as) = threeAddressAux a ts
      (b', adB, bs) = threeAddressAux b as
      i = fresh bs
      temp =  "t" ++ show i
      c' = [Operation temp adA op adB]
-- { - Ejemplo -}
-- > threeAddressAux (Op Equal (VarASA "var") (NumberASA 25)) []
-- > (["t0" = "var","t1" = 25,"t2" = "t0" == "t1"],"t2",[2,1,0])
-- > threeAddressAux (Op Equal (NumberASA 50) (VarASA "var")) [] > (["t0" = 50,"t1" = "var","t2" = "t0" == "t1"],"t2",[2,1,0])

-- :- 0.1 pts -:
-- Define la función threeAddress que recibe un ASA y devuelve su traducción correspondiente en código de tres direcciones.
threeAddress :: ASA -> [ThreeAddress]
threeAddress asa = trad
   where (trad, temp, ts) = threeAddressAux asa []
-- { - Ejemplo -}
-- > threeAddress (Op Equal (VarASA "var") (NumberASA 25)) > ["t0" = "var","t1" = 25,"t2" = "t0" == "t1"]
-- > threeAddress (Op Equal (NumberASA 50) (VarASA "var")) > ["t0" = 50,"t1" = "var","t2" = "t0" == "t1"]

-- Este lo vamos a hacer juntos la siguiente clase
-- ESte arbol ya viene en orden
-- de <---- no he usado variables que no hayan estado definidas


----------------------------------------------------- EJERCICIO 6 ---------------------------------------------------------------

-- Generación de Código
-- La generación de código recibe una representación intermedia del programa y genera código en lenguaje maquina. 
-- Considera las siguientes nemotecnias de lenguaje ensamblador.
tokenThreeAddress :: Token -> String
tokenThreeAddress Sum = " + "
tokenThreeAddress Subs = " - "
tokenThreeAddress And = " & "
tokenThreeAddress Or = " | "
tokenThreeAddress Equal = " == "

-- Asignar el valor V en el registro R1 
-- MOV R1 V
-- Aplicar la operacion con los registros R2 y R3, y guardar el valor en el registro R1 
-- ADD R1 R2 R3
-- SUBS R1 R2 R3 
-- AND R1 R2 R3 
-- OR R1 R2 R3 
-- EQ R1 R2 R3

-- :- 2 pts -:
-- Define la función assembly que recibe un programa en código de tres direcciones y devuelve su traducción correspondiente a lenguaje ensamblador.
assembly :: [ThreeAddress] -> String
assembly prog = unlines $ map toAssembly prog

toAssembly :: ThreeAddress -> String
toAssembly (Assign t v) =
  case v of
    N n -> "MOV " ++ t ++ " " ++ show n
    S s -> "MOV " ++ t ++ " " ++ s
    B b -> "MOV " ++ t ++ " " ++ show b

toAssembly (Operation t a op b) =
  case op of
    Sum -> "ADD " ++ t ++ " " ++ a ++ " " ++ b
    Subs -> "SUBS " ++ t ++ " " ++ a ++ " " ++ b
    And -> "AND " ++ t ++ " " ++ a ++ " " ++ b
    Or -> "OR " ++ t ++ " " ++ a ++ " " ++ b
    Equal -> "EQ " ++ t ++ " " ++ a ++ " " ++ b 

-- { - Ejemplo -}
-- > assembly ["t0" = "var","t1" = 25,"t2" = "t0" == "t1"]
-- > MOV "t0" "var"
-- MOV "t1" 25
-- EQ "t2" "t0" "t1"
-- > assembly ["t0" = 50,"t1" = "var","t2" = "t0" == "t1"]
-- > MOV "t0" 50
-- MOV "t1" "var"
-- EQ "t2" "t0" "t1"

------------------------------------ PROBAR DE LA SIGUIENTE MANERA:
-- let entrada = [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"]
-- putStr (assembly entrada)

----------------------------------------------------- EJERCICIO 7 ---------------------------------------------------------------
-- :- 0.5 pts -:
-- Utilizando las funciones definidas anteriormente Define la función compile que recibe un programa en AEB y devuelve su 
-- traducción correspondiente a lenguaje ensamblador.
--compile :: String -> String 
-- { - Ejemplo -}
-- > compile "22␣3␣+␣var␣==␣t␣&&"
-- > MOV "t0" "var"
-- MOV "t1" 25
-- EQ "t2" "t0" "t1"
