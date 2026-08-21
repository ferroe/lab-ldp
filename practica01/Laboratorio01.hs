module Laboratorio01 where

distanciaOrigen :: Double -> Double -> Double
distanciaOrigen x y = sqrt (x^2 + y^2)

sumaCuadradosPares :: [Int] -> Int
sumaCuadradosPares xs = sum (map (^2) (filter (even) (xs)))

aplicaTresVeces :: (a -> a) -> a -> a
aplicaTresVeces fun x =  fun (fun (fun x))

varianza2 :: Double -> Double -> Double
varianza2 x y = 
  let media = (x + y) / 2 in ((x - media)^2 + (y - media)^2) / 2

clasificaTemperatura :: Int -> String
clasificaTemperatura n
  | n < 1     = "frio extremo"
  | n <= 15   = "frio"
  | n <= 25   = "templado"
  | n <= 35   = "calido"
  | otherwise   = "calor extremo"

intercala :: a -> [a] -> [a]
intercala y []  = []
intercala y [x] = [x]
intercala y (x:xs) = x:[y] ++ intercala y xs

data Expr
  = Lit Int
  | Suma Expr Expr
  | Producto Expr Expr
  deriving (Eq, Show)

evalua :: Expr -> Int
evalua (Lit n) = n
evalua (Producto m n) = evalua m * evalua n
evalua (Suma m n) = evalua m + evalua n