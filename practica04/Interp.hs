module Interp where

import Grammars

data ASA
  = Id Nombre
  | Num Int
  | Boolean Bool
  | Add ASA ASA
  | Sub ASA ASA
  | Not ASA
  | Fun Nombre ASA
  | App ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 1: desazucarado ----------------------------------------------------

-- Convierte una lista no vacia de parametros distintos en funciones
-- unarias anidadas. El primer parametro queda en la funcion exterior.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | Just v <- curryFun xs e = Just (Fun x v)
  | otherwise = Nothing


-- Convierte una aplicacion con uno o mas argumentos en aplicaciones unarias
-- asociadas por la izquierda.
curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

-- Convierte dos o mas operandos en operaciones binarias asociadas por la
-- izquierda. El constructor recibido sera Add o Sub.
binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op (x:y:xs) = Just (foldl op (op x y) xs)
binaryOp _ _ = Nothing


-- Convierte las ligaduras de let* en let anidados y despues elimina cada let
-- mediante LetS x e1 e2 ==> App (Fun x e2') e1'. La primera ligadura debe
-- quedar en el let exterior para que las siguientes puedan usarla.
desugar :: SASA -> Maybe ASA
desugar (IdS x) = Just (Id x)
desugar (NumS n) = Just (Num n)
desugar (BooleanS b) = Just (Boolean b)
desugar (AddS xs)
  | Just xs' <- traverse desugar xs = binaryOp Add xs'
  | otherwise = Nothing
desugar (SubS xs)
  | Just xs' <- traverse desugar xs = binaryOp Sub xs'
  | otherwise = Nothing
desugar (NotS e)
  | Just e' <- desugar e = Just (Not e')
  | otherwise = Nothing

desugar (LetS x e1 e2)
  | Just e1' <- desugar e1
  , Just e2' <- desugar e2 = Just (App (Fun x e2') e1')
  | otherwise = Nothing

desugar (LetStarS [] e) = desugar e
desugar (LetStarS ((x1,e1):xs) e) = desugar (LetS x1 e1 (LetStarS xs e))

desugar (FunS xs e)
  | Just e' <- desugar e = curryFun xs e'
  | otherwise = Nothing

desugar (AppS e xs)
  | Just e' <- desugar e
  , Just xs' <- traverse desugar xs = curryApp e' xs'
  | otherwise = Nothing
  


-- RETO 2: evaluacion con cerraduras ---------------------------------------

-- Busca la asociacion mas reciente de un identificador.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((nom, val):xs)
  | x == nom = Just val
  | otherwise = lookupEnv x xs

-- Evalua con alcance estatico. Fun produce una cerradura con el ambiente
-- actual. App evalua primero la posicion de funcion, despues el argumento y
-- por ultimo el cuerpo en el ambiente guardado por la cerradura.
-- La aplicacion es ansiosa: el argumento se exige aunque el cuerpo no lo use.
-- Conserva la resta truncada y la convencion de que todo numero cuenta como
-- verdadero cuando aparece como operando de Not.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)

bigStep env (Add x y)
  | Just (NumV n) <- bigStep env x
  , Just (NumV m) <- bigStep env y = Just (NumV (n + m))
  | otherwise = Nothing

bigStep env (Sub x y)
  | Just (NumV n) <- bigStep env x
  , Just (NumV m) <- bigStep env y = Just (NumV (max 0 (n - m)))
  | otherwise = Nothing

bigStep env (Not e) = case bigStep env e of
  Just (BooleanV b) -> Just (BooleanV (not b))
  Just (NumV _) -> Just (BooleanV False)
  _ -> Nothing

bigStep env (Fun x e) = Just (ClosureV x e env)

bigStep env (App f a)
  | Just (ClosureV p body defEnv) <- bigStep env f
  , Just v <- bigStep env a = bigStep ((p, v):defEnv) body
  | otherwise = Nothing
