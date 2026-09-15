module Interp where

import Grammars
import Data.List ((\\), nub)

-- RETO 3: sustitucion nominal que evita captura
freeVars :: ASA -> [String]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (Id x) = [x]
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (EqP e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Expt e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (And xs) = nub (concatMap freeVars xs)
freeVars (Or xs) = nub (concatMap freeVars xs)
freeVars (Add xs) = nub (concatMap freeVars xs)
freeVars (Sub xs) = nub (concatMap freeVars xs)
freeVars (Mul xs) = nub (concatMap freeVars xs)
freeVars (Div xs) = nub (concatMap freeVars xs)
freeVars (Lt xs) = nub (concatMap freeVars xs)
freeVars (Gt xs) = nub (concatMap freeVars xs)
freeVars (Le xs) = nub (concatMap freeVars xs)
freeVars (Ge xs) = nub (concatMap freeVars xs)
freeVars (Let bindings body) = nub (concatMap freeVars (map snd bindings) ++ ((freeVars body) \\ map fst bindings))
freeVars (LetStar [] body) = freeVars body
freeVars (LetStar ((x, e):bs) body) = nub (freeVars e ++ (freeVars (LetStar bs body) \\ [x]))

names :: ASA -> [String]
names (Num _) = []
names (Boolean _) = []
names (Id x) = [x]
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (EqP e1 e2) = nub (names e1 ++ names e2)
names (Expt e1 e2) = nub (names e1 ++ names e2)
names (And xs) = nub (concatMap names xs)
names (Or xs) = nub (concatMap names xs)
names (Add xs) = nub (concatMap names xs)
names (Sub xs) = nub (concatMap names xs)
names (Mul xs) = nub (concatMap names xs)
names (Div xs) = nub (concatMap names xs)
names (Lt xs) = nub (concatMap names xs)
names (Gt xs) = nub (concatMap names xs)
names (Le xs) = nub (concatMap names xs)
names (Ge xs) = nub (concatMap names xs)
names (Let bindings body) = nub (map fst bindings ++ concatMap names (map snd bindings) ++ names body)
names (LetStar bindings body) = nub (map fst bindings ++ concatMap names (map snd bindings) ++ names body)

freshName :: [String] -> String
freshName xs = head (filter (\c -> notElem c xs) libres)

libres :: [String]
libres = "z" : [ "z" ++ show n | n <- [1..] ]

sust :: ASA -> String -> ASA -> ASA
sust (Num n) _ _ = Num n
sust (Boolean b) _ _ = Boolean b
sust (Id x) y s
  | x == y = s
  | otherwise = Id x
sust (Not e) x s = Not (sust e x s)
sust (Add1 e) x s = Add1 (sust e x s)
sust (Sub1 e) x s = Sub1 (sust e x s)
sust (ZeroP e) x s = ZeroP (sust e x s)
sust (EqP e1 e2) x s = EqP (sust e1 x s) (sust e2 x s)
sust (Expt e1 e2) x s = Expt (sust e1 x s) (sust e2 x s)
sust (Add xs) y s = Add (map (\e -> sust e y s) xs)
sust (Sub xs) y s = Sub (map (\e -> sust e y s) xs)
sust (Mul xs) y s = Mul (map (\e -> sust e y s) xs)
sust (Div xs) y s = Div (map (\e -> sust e y s) xs)
sust (Lt xs) y s = Lt (map (\e -> sust e y s) xs)
sust (Gt xs) y s = Gt (map (\e -> sust e y s) xs)
sust (Le xs) y s = Le (map (\e -> sust e y s) xs)
sust (Ge xs) y s = Ge (map (\e -> sust e y s) xs)
sust (Let bindings body) x s = 
  let (nuevosBindings, nuevoBody) = sustBindings bindings body x s
  in Let nuevosBindings nuevoBody
sust (LetStar [] body) x s = LetStar [] (sust body x s)
sust (LetStar ((y, e):bs) body) x s
  | y == x = LetStar ((y, sust e x s) : bs) body
  | notElem y (freeVars s) = 
      let e' = sust e x s
          LetStar bs' body' = sust (LetStar bs body) x s
      in LetStar ((y, e') : bs') body'
  | otherwise = 
      let z = freshName ([x, y] ++ names (LetStar ((y, e):bs) body) ++ names s)
          e' = sust e x s
          bodyRenombrado = sust (LetStar bs body) y (Id z)
          LetStar bs' body' = sust bodyRenombrado x s
      in LetStar ((z, e') : bs') body'

sustBindings :: [Binding] -> ASA -> String -> ASA -> ([Binding], ASA)
sustBindings [] body x s = ([], sust body x s)
sustBindings ((y, e) : bs) body x s
  | y == x =
      let e' = sust e x s
      in ((y, e') : bs, body)
  | notElem y fvs =
      let e' = sust e x s
          (bs', body') = sustBindings bs body x s
      in ((y, e') : bs', body')
  | otherwise =
      let e' = sust e x s
          z = freshName ([x, y] ++ names body ++ names s ++ concatMap names (map snd ((y, e):bs)))
          body' = sust body y (Id z)
          (bs', body'') = sustBindings bs body' x s
      in ((z, e') : bs', body'')
  where
    fvs = freeVars s

sustMany :: ASA -> [Binding] -> ASA
sustMany expr [] = expr
sustMany expr binds =
  let (exprRenombrada, temporales) = foldl renombrar (expr, []) binds
      resultado = foldl (\e (temp, val) -> sust e temp val) exprRenombrada temporales
  in resultado
  where
    libresBinds = concatMap (freeVars . snd) binds
    renombrar (e, acc) (x, val) =
      let ocupados = names e ++ libresBinds ++ map fst acc
          z = freshName ocupados
      in (sust e x (Id z), (z, val) : acc)

-- RETO 4: semantica operacional de paso grande
bigStep :: ASA -> Maybe ASA
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)
bigStep (Id _) = Nothing
bigStep (Not e) = evaluaNot (bigStep e)
bigStep (Add1 e) = evaluaAdd1 (bigStep e)
bigStep (Sub1 e) = evaluaSub1 (bigStep e)
bigStep (ZeroP e) = evaluaZeroP (bigStep e)
bigStep (Expt e1 e2) = evaluaExpt (bigStep e1) (bigStep e2)
bigStep (EqP e1 e2) = evaluaEqP (bigStep e1) (bigStep e2)

bigStep (Add xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaAdd vals
  | otherwise = Nothing

bigStep (Sub xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaSub vals
  | otherwise = Nothing

bigStep (Mul xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaMul vals
  | otherwise = Nothing

bigStep (Div xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaDiv vals
  | otherwise = Nothing

bigStep (And xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaAnd vals
  | otherwise = Nothing

bigStep (Or xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaOr vals
  | otherwise = Nothing

bigStep (Lt xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaRelacion (<) vals
  | otherwise = Nothing

bigStep (Gt xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaRelacion (>) vals
  | otherwise = Nothing

bigStep (Le xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaRelacion (<=) vals
  | otherwise = Nothing

bigStep (Ge xs)
  | length xs >= 2
  , Just vals <- evaluaLista xs = evaluaRelacion (>=) vals
  | otherwise = Nothing

bigStep (Let bindings body)
  | sinDuplicados (map fst bindings)
  , Just vals <- evaluaLista (map snd bindings) =
      let nuevosBinds = zip (map fst bindings) vals
      in bigStep (sustMany body nuevosBinds)
  | otherwise = Nothing
  where
    sinDuplicados xs = length (nub xs) == length xs

bigStep (LetStar [] body) = bigStep body
bigStep (LetStar ((x, e):bs) body) = do
  val <- bigStep e
  let restoSustituido = sust (LetStar bs body) x val
  bigStep restoSustituido


evaluaLista :: [ASA] -> Maybe [ASA]
evaluaLista [] = Just []
evaluaLista (x:xs)
  | Just val <- bigStep x
  , Just vals <- evaluaLista xs = Just (val : vals)
  | otherwise = Nothing

evaluaNot :: Maybe ASA -> Maybe ASA
evaluaNot (Just (Boolean b)) = Just (Boolean (not b))
evaluaNot (Just (Num _)) = Just (Boolean False)
evaluaNot _ = Nothing

evaluaAdd1 :: Maybe ASA -> Maybe ASA
evaluaAdd1 (Just (Num n)) = Just (Num (n + 1))
evaluaAdd1 _ = Nothing

evaluaSub1 :: Maybe ASA -> Maybe ASA
evaluaSub1 (Just (Num n))
  | n <= 0 = Just (Num 0)
  | otherwise = Just (Num (n - 1))
evaluaSub1 _ = Nothing

evaluaZeroP :: Maybe ASA -> Maybe ASA
evaluaZeroP (Just (Num n)) = Just (Boolean (n == 0))
evaluaZeroP _ = Nothing

evaluaExpt :: Maybe ASA -> Maybe ASA -> Maybe ASA
evaluaExpt (Just (Num m)) (Just (Num n))
  | n >= 0 = Just (Num (m ^ n))
  | otherwise = Nothing
evaluaExpt _ _ = Nothing

evaluaEqP :: Maybe ASA -> Maybe ASA -> Maybe ASA
evaluaEqP (Just (Num m)) (Just (Num n)) = Just (Boolean (m == n))
evaluaEqP (Just (Boolean b)) (Just (Boolean c)) = Just (Boolean (b == c))
evaluaEqP _ _ = Nothing

evaluaAdd :: [ASA] -> Maybe ASA
evaluaAdd [] = Just (Num 0)
evaluaAdd (Num x : xs)
  | Just (Num suma) <- evaluaAdd xs = Just (Num (x + suma))
  | otherwise = Nothing
evaluaAdd _ = Nothing

evaluaMul :: [ASA] -> Maybe ASA
evaluaMul [] = Just (Num 1)
evaluaMul (Num x : xs)
  | Just (Num prod) <- evaluaMul xs = Just (Num (x * prod))
  | otherwise = Nothing
evaluaMul _ = Nothing

evaluaSub :: [ASA] -> Maybe ASA
evaluaSub (Num x : xs) = foldlM restarTruncada (Num x) xs
  where
    restarTruncada (Num acc) (Num n) = Just (Num (max 0 (acc - n)))
    restarTruncada _ _ = Nothing
    foldlM _ z [] = Just z
    foldlM f z (y:ys) = case f z y of
      Nothing -> Nothing
      Just z' -> foldlM f z' ys
evaluaSub _ = Nothing

evaluaDiv :: [ASA] -> Maybe ASA
evaluaDiv (Num x : xs) = foldlM dividirSegura (Num x) xs
  where
    dividirSegura (Num _) (Num 0) = Nothing
    dividirSegura (Num acc) (Num n) = Just (Num (acc `div` n))
    dividirSegura _ _ = Nothing
    foldlM _ z [] = Just z
    foldlM f z (y:ys) = case f z y of
      Nothing -> Nothing
      Just z' -> foldlM f z' ys
evaluaDiv _ = Nothing

evaluaAnd :: [ASA] -> Maybe ASA
evaluaAnd [] = Just (Boolean True)
evaluaAnd (Boolean b : xs)
  | Just (Boolean resto) <- evaluaAnd xs = Just (Boolean (b && resto))
  | otherwise = Nothing
evaluaAnd _ = Nothing

evaluaOr :: [ASA] -> Maybe ASA
evaluaOr [] = Just (Boolean False)
evaluaOr (Boolean b : xs)
  | Just (Boolean resto) <- evaluaOr xs = Just (Boolean (b || resto))
  | otherwise = Nothing
evaluaOr _ = Nothing

evaluaRelacion :: (Int -> Int -> Bool) -> [ASA] -> Maybe ASA
evaluaRelacion op xs = checaPares xs
  where
    checaPares (Num a : Num b : resto)
      | op a b = checaPares (Num b : resto)
      | otherwise = Just (Boolean False)
    checaPares [_] = Just (Boolean True)
    checaPares _ = Nothing