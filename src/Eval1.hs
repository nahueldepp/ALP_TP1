module Eval1
  ( eval,
    State,
  )
where

import AST
import qualified Data.Map.Strict as M
import Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado vacío
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v state = state M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v x state = M.insert v x state

-- Evalúa un programa en el estado vacío
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comando en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip state = Skip :!: state
stepComm (Let x e) state =
  let (e' :!: state') = evalExp e state
   in Skip :!: ((update x e') state')
stepComm (Seq c1 c2) state = case c1 of
  Skip -> c2 :!: state
  _ ->
    let (c1' :!: state') = stepComm c1 state
     in (Seq c1' c2) :!: state'
stepComm (IfThenElse b c1 c2) state =
  let (b' :!: state') = evalExp b state
   in case b' of
        True -> c1 :!: state'
        False -> c2 :!: state'
stepComm (RepeatUntil c b) state =
  (Seq c (IfThenElse b Skip (RepeatUntil c b))) :!: state

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const i) state =(i :!: state)
evalExp (Var x) state = (lookfor x state) :!: state
evalExp (UMinus x) state =
  let (n :!: state') = evalExp x state
   in (-n) :!: state'
evalExp (Plus a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' + b') :!: state''
evalExp (Minus a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' - b') :!: state''
evalExp (Times a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' * b') :!: state''
evalExp (Div a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' `div` b') :!: state''
evalExp (Eq a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' == b') :!: state''
evalExp (Lt a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' < b') :!: state''
evalExp (Gt a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' > b') :!: state''
evalExp (NEq a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' /= b') :!: state''
evalExp BTrue state = True :!: state
evalExp BFalse state = False :!: state
evalExp (Not b) state =
  let (b' :!: state') = evalExp b state
   in (not b') :!: state'
evalExp (Or a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' || b') :!: state''
evalExp (And a b) state =
  let (a' :!: state') = evalExp a state
      (b' :!: state'') = evalExp b state'
   in (a' && b') :!: state''
evalExp (VarInc x) state =
  let v = lookfor x state
      x' = v + 1
   in x' :!: (update x x' state)
evalExp (VarDec x) state =
  let v = lookfor x state
      x' = v - 1
   in x' :!: (update x x' state)