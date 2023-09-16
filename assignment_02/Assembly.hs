----------------------------------------------------- EJERCICIO 6 ---------------------------------------------------------------

----------------- EJERCICIO GENERACION DE CODIGO 
-- Generación de Código
-- La generación de código recibe una representación intermedia del programa y genera código en lenguaje maquina. 
-- Considera las siguientes nemotecnias de lenguaje ensamblador.

-- Asignar el valor V en el registro R1 
-- MOV R1 V
-- Aplicar la operacion con los registros R2 y R3, y guardar el valor en el registro R1 
-- ADD R1 R2 R3
-- SUBS R1 R2 R3 
-- AND R1 R2 R3 
-- OR R1 R2 R3 
-- EQ R1 R2 R3

data Value = N Int | B Bool | S String
instance Show Value where
    show (N n) = show n
    show (B b) = show b
    show (S s) = show s

data Token = Var String | Number Int | Boolean Bool | Sum | Subs | And | Or | Equal deriving Show

data ThreeAddress = Assign String Value | Operation String String Token String
instance Show ThreeAddress where
    show (Assign t v) = show t ++ " = " ++ show v
    show (Operation t a op b) = show t ++ " = " ++ show a ++ tokenThreeAddress op ++ show b

tokenThreeAddress :: Token -> String
tokenThreeAddress Sum = " + "
tokenThreeAddress Subs = " - "
tokenThreeAddress And = " & "
tokenThreeAddress Or = " | "
tokenThreeAddress Equal = " == "

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

--- Probar de la siguiente manera:
-- let entrada = [Assign "t0" (S "var"), Assign "t1" (N 25), Operation "t2" "t0" Equal "t1"]
-- putStr (assembly entrada)