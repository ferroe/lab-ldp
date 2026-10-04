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
  | If ASA ASA ASA
  deriving (Eq, Show)

data Value
  = NumV Int
  | BooleanV Bool
  | ClosureV Nombre ASA Env
  | ExprV ASA Env
  deriving (Eq, Show)

type Env = [(Nombre, Value)]

-- RETO 3: desazucarado ----------------------------------------------------

-- Recupera estas funciones del laboratorio 4. Las funciones y aplicaciones
-- del nucleo siguen siendo unarias, y las operaciones siguen siendo binarias.
curryFun :: [Nombre] -> ASA -> Maybe ASA
curryFun [] _ = Nothing
curryFun [x] e = Just (Fun x e)
curryFun (x:xs) e
  | x `elem` xs = Nothing
  | Just v <- curryFun xs e = Just (Fun x v)
  | otherwise = Nothing

curryApp :: ASA -> [ASA] -> Maybe ASA
curryApp _ [] = Nothing
curryApp e xs = Just (foldl App e xs)

binaryOp :: (ASA -> ASA -> ASA) -> [ASA] -> Maybe ASA
binaryOp op (x:y:xs) = Just (foldl op (op x y) xs)
binaryOp _ _ = Nothing

-- Desazucara las clausulas ordinarias de cond en If anidados. La alternativa
-- else es el ultimo argumento y se conserva como la rama final.
desugarCond :: [(SASA, SASA)] -> SASA -> Maybe ASA
desugarCond [] e = desugar e
desugarCond ((c,r) : l) e
  | Just c' <- desugar c
  , Just r' <- desugar r
  , Just resto <- desugarCond l e = Just (If c' r' resto)
  | otherwise = Nothing

-- Elimina toda la sintaxis superficial. CondS se traduce a If anidados.
-- LetRecS f definicion cuerpo se traduce usando el identificador Y:
--
--   LetS f (AppS (IdS "Y") (FunS [f] definicion)) cuerpo
--
-- y despues se elimina tambien ese LetS. LetRecS no pertenece al nucleo.
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

desugar (IfS c t e)
  | Just c' <- desugar c
  , Just t' <- desugar t
  , Just e' <- desugar e = Just (If c' t' e')
  | otherwise = Nothing

desugar (CondS clau els) = desugarCond clau els

desugar (LetRecS f e c)
  | Just e' <- desugar e
  , Just c' <- desugar c = Just (App (Fun f c') (App (Id "Y") (Fun f e')))
  | otherwise = Nothing


-- RETO 4: evaluacion perezosa con alcance estatico ------------------------

-- Busca la asociacion mas reciente sin exigir su contenido.
lookupEnv :: Nombre -> Env -> Maybe Value
lookupEnv _ [] = Nothing
lookupEnv x ((nom, val):xs)
  | x == nom = Just val
  | otherwise = lookupEnv x xs

-- Exige una cerradura de expresion usando el ambiente guardado. Si al
-- evaluarla se obtiene otra ExprV, continua hasta producir otro valor.
strict :: Value -> Maybe Value
strict (NumV n) = Just (NumV n)
strict (BooleanV b) = Just (BooleanV b)
strict (ClosureV x e env) = Just (ClosureV x e env)
strict (ExprV e env) 
  | Just e' <- bigStep env e = strict e'
  | otherwise = Nothing

-- Semantica de paso grande con alcance estatico y evaluacion perezosa.
--
-- * Id devuelve directamente la asociacion encontrada.
-- * Fun produce ClosureV con el ambiente de definicion.
-- * App exige la posicion de funcion, pero liga el argumento como
--   ExprV argumento ambienteDeLaLlamada.
-- * Add, Sub y Not exigen sus operandos.
-- * If exige solamente la condicion y evalua una sola rama.
--
-- La resta sobre naturales permanece truncada en cero.
bigStep :: Env -> ASA -> Maybe Value
bigStep env (Id x) = lookupEnv x env
bigStep _ (Num n) = Just (NumV n)
bigStep _ (Boolean b) = Just (BooleanV b)

bigStep env (Add x y)
  | Just x' <- bigStep env x
  , Just y' <- bigStep env y
  , Just (NumV n) <- strict x'
  , Just (NumV m) <- strict y' = Just (NumV (n + m))
  | otherwise = Nothing

bigStep env (Sub x y)
  | Just x' <- bigStep env x
  , Just y' <- bigStep env y
  , Just (NumV n) <- strict x'
  , Just (NumV m) <- strict y' = Just (NumV (max 0 (n - m))) 
  | otherwise = Nothing

bigStep env (Not e)
  | Just e' <- bigStep env e
  , Just (BooleanV b) <- strict e' = Just (BooleanV (not b))
  | Just e' <- bigStep env e
  , Just (NumV _) <- strict e' = Just (BooleanV False)
  | otherwise = Nothing

bigStep env (Fun x e) = Just (ClosureV x e env)

bigStep env (App f a)
  | Just f' <- bigStep env f
  , Just (ClosureV p body defEnv) <- strict f' = bigStep ((p, (ExprV a env)):defEnv) body
  | otherwise = Nothing

bigStep env (If c t e)
  | Just c' <- bigStep env c
  , Just (BooleanV b) <- strict c' = if b then bigStep env t else bigStep env e
  | otherwise = Nothing
