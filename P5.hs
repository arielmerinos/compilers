-- PRÁCTICA: Análisis Sintáctico LR

-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::

-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos

-- Considera el siguiente sublenguaje conocido como WHILE:
-- Comp C::= C';C |C'
-- C'::= L:= E | skip
-- Bool B:: B'& B | B' 
-- B'::= true | false | E = E | - (B)
-- Arith E:: = E' + E | E'
-- E' ::= L | n


-- Considera la definición de Tokens de la practica 3 y la siguiente definiciones:
data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show

data Content = T Token | S | C | B | E deriving Show
data State = Q Int deriving Show
type Input = [Token]
type Stack = [State]
type Symbols = [Content]

-- ************************ Pasos para probar: ******************************
-- 1) ghci <Archivo.hs> (desde terminal)

-- Ejemplos: 
-- 1) parserAux [ Loc 2 , Assign , Number 1 , Seq , Loc 3 , Assign , Number 0 , Seq , While , Not , Loc 2 , Equal , Loc 2 , Do , Loc 2 , Assign, LP , Loc 2 , Sum , Number 1 , Seq , Loc 3 , Assign , LP , Loc 3 , Sum , Number 1] [Q 0] [] 

-- 2) parserAux [ Loc 1 , Assign , Number 1 , Seq , Loc 2 , Assign , Number 2 , Seq , Skip ] [Q 0] []

-- 3) parser [ Loc 2 , Assign , Number 1 , Seq , Loc 3 , Assign , Number 0 , Seq , While , Not , Loc 2 , Equal , Loc 2 , Do , Loc 2 , Assign, LP , Loc 2 , Sum , Number 1 , Seq , Loc 3 , Assign , LP , Loc 3 , Sum , Number 1]

-- 4) parser [ Loc 1 , Assign , Number 1 , Seq , Loc 2 , Assign , Number 2 , Seq , Skip ]



----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- 9.9 pts Define la función parserAux que recibe una lista de tokens, el stack de estados y símbolos, y devuelve verdadero si
-- y solo si la lista de tokens pertenece al lenguaje con los stacks actuales. Esta función es un parser LR(1) ad hoc del lenguaje.

parserAux :: Input -> Stack -> Symbols -> Bool
parserAux [] (Q 0 : _) [] = True
parserAux (Loc _ : rest) stack symbols = parserAux rest (Q 0 : stack) (S : symbols)
parserAux (Assign : Number _ : Seq : rest) (Q 0 : stack) (S : symbols) = parserAux rest (Q 1 : Q 0 : stack) (C : symbols)
parserAux (Skip : rest) (Q 0 : stack) (C : symbols) | null symbols = parserAux rest (Q 0 : stack) symbols
parserAux (Skip : rest) (Q 0 : stack) (C : symbols) = parserAux rest (Q 0 : stack) (tail symbols)
parserAux (rest) (Q 1 : stack) (C : symbols) = parserAux rest stack symbols
parserAux _ _ _ = False


-- 0.1 pts Utilizando la función parserAux, define la función parser que recibe una lista de tokens WHILE y devuelve
-- verdadero si y solo si la lista de tokens pertenece al lenguaje.
parser :: Input -> Bool
parser tokens = parserAux tokens [Q 0] []

