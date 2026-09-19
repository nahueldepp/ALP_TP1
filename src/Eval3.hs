module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados 
type State = (M.Map Variable Int, String)

-- Estado vacío
-- Completar la definición
initState :: State
initState = (M.empty, [])

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (m, _) = case M.lookup v m of
  Nothing -> Left UndefVar
  Just n -> Right n

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update v x (m, t) = (M.insert v x m, t)

-- Agrega una traza dada al estado
-- Completar la definición
addTrace :: String -> State -> State
addTrace s (m, []) = (m, s)
addTrace s (m, t) = (m, (t ++ " ") ++ s)

-- Evalúa un programa en el estado vacío
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalúa múltiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do
  (c' :!: s') <- stepComm c s
  stepCommStar c' s'

-- Evalúa un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm Skip state = return (Skip :!: state)
stepComm (Let x e) state = do
  (e' :!: state') <- evalExp e state
  let msg = "Let " ++ x ++ " " ++ (show e')
  return (Skip :!: update x e' (addTrace msg state'))
stepComm (Seq c1 c2) state = case c1 of
  Skip -> return (c2 :!: state)
  _ -> do
    (c1' :!: state') <- stepComm c1 state
    return ((Seq c1' c2) :!: state')
stepComm (IfThenElse b c1 c2) state = do
  (b' :!: state') <- evalExp b state
  case b' of
    True -> return (c1 :!: state')
    False -> return (c2 :!: state')
stepComm (RepeatUntil c b) state =
  return ((Seq c (IfThenElse b Skip (RepeatUntil c b))) :!: state)

-- Evalúa una expresión
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const i) state = return (i :!: state)
evalExp (Var x) state = do
  n <- lookfor x state
  return (n :!: state)
evalExp (UMinus x) state = do
  (n :!: state') <- evalExp x state
  return ((-n) :!: state')
evalExp (Plus a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' + b') :!: state'')
evalExp (Minus a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' - b') :!: state'')
evalExp (Times a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' * b') :!: state'')
evalExp (Div a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  if b' == 0 then Left DivByZero else return ((a' `div` b') :!: state'')
evalExp (Eq a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' == b') :!: state'')
evalExp (Lt a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' < b') :!: state'')
evalExp (Gt a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' > b') :!: state'')
evalExp (NEq a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' /= b') :!: state'')
evalExp BTrue state = return (True :!: state)
evalExp BFalse state = return (False :!: state)
evalExp (Not b) state = do
  (b' :!: state') <- evalExp b state
  return ((not b') :!: state')
evalExp (Or a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' || b') :!: state'')
evalExp (And a b) state = do
  (a' :!: state') <- evalExp a state
  (b' :!: state'') <- evalExp b state'
  return ((a' && b') :!: state'')
evalExp (VarInc x) state = do
  v <- lookfor x state
  let nx = v + 1
  return (nx :!: (update x nx state))
evalExp (VarDec x) state = do
  v <- lookfor x state
  let nx = v - 1
  return (nx :!: (update x nx state))