-- Prática 04 de Haskell
-- Nome: 

faixaIdoso :: Int -> String
faixaIdoso idade = 
  if idade >= 80 then "IDO80"
  else if idade >= 75 then "IDO79"
  else if idade >= 70 then "IDO74"
  else if idade >= 65 then "IDO69"
  else if idade >= 60 then "IDO64"
  else "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos ltupla = [(nome, idade, faixaIdoso idade) | (nome, idade) <- ltupla]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' ltupla = zipWith (\(x,y) z -> (x,y,z)) ltupla (map faixaIdoso (map (\(_,x) -> x) ltupla)) 

strColor :: (Int,Int,Int) -> String
strColor tuple = (\(x,y,z) -> "rgb(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")") tuple

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs qtd (cx, cy) raio =  [(cqx, cy, raio) | cqx <- take qtd [cx,cx+raio^2..]]

genCircs' :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs' qtd (cx, cy) raio = take qtd [(cqx, cy, raio) | cqx <- (iterate (2+) cx)]
--take qtd (iterate (raio+) )

genReds :: Int -> [(Int,Int,Int)]
genReds num = [(80+i*5,0,0) | i <- take num [1,2..]]