-- PRÁCTICA: Análisis Sintáctico LR

-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::

-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos

-- ************************ Pasos para probar: ******************************
-- 1) ghci (desde terminal)
-- 2) :load <NombrDeleArchivo>.hs
-- 3) (Ingresar ejemplo) 
    -- parserAux [ Loc 2 , Assign , Number 1 , Seq , Loc 3 , Assign , Number 0 , Seq , While , Not , Loc

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
type Input = [ Token ]
type Stack = [ State ]
type Symbols = [ Content ]

----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- 9.9 pts Define la función parserAux que recibe una lista de tokens, el stack de estados y símbolos, y devuelve verdadero si
-- y solo si la lista de tokens pertenece al lenguaje con los stacks actuales. Esta función es un parser LR(1) ad hoc del lenguaje.

-- Comienza en el estado Q 0 y con una pila de estados vacía.
-- Si el token actual es:
        -- Loc, entonces el parser empuja el estado Q 1 en la pila de estados y el token T (Loc l) en la pila de símbolos.
        -- Assign, entonces el parser empuja el estado Q 2 en la pila de estados y el token Assign en la pila de símbolos.
        -- Number, entonces el parser empuja el estado Q 3 en la pila de estados y el token Number n en la pila de símbolos.
        -- Seq, entonces el parser empuja el estado Q 0 y el estado Q 4 en la pila de estados.
-- El parser continúa de esta manera hasta que encuentra el token Skip en  Q 6. Si esto ocurre, entonces el parser devuelve True. De lo contrario, el parser devuelve False.

parserAux :: Input -> Stack -> Symbols -> Bool
parserAux [] [Q q] [] = True
parserAux [] _ _ = False
parserAux (t:ts) (q:qs) syms =
  case (q, t) of
    (Q 0, Loc l) -> parserAux ts (Q 1 : qs) (T (Loc l) : syms)
    (Q 1, Assign) -> parserAux ts (Q 2 : qs) (Assign : syms)
    (Q 2, Number n) -> parserAux ts (Q 3 : qs) (Number n : syms)
    (Q 3, Seq) -> parserAux ts (Q 0 : Q 4 : qs) syms
    (Q 4, Loc l) -> parserAux ts (Q 5 : qs) (T (Loc l) : syms)
    (Q 5, Assign) -> parserAux ts (Q 6 : qs) (Assign : syms)
    (Q 6, Number n) -> parserAux ts qs (Number n : syms)
    (Q 6, Skip) -> parserAux ts qs syms
    (q, _) -> False


-- 0.1 pts Utilizando la función parserAux, define la función parser que recibe una lista de tokens WHILE y devuelve
-- verdadero si y solo si la lista de tokens pertenece al lenguaje.
parser :: Input -> Bool
parser tokens = parserAux tokens [Q 0] []
