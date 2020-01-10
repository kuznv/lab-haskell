-- 1)	Перестановки
-- С помощью функций concat и map реализуйте генерацию всех возможных перестановок элементов списка. Входной список содержит уникальные элементы.
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap (insert x) (perms xs) where
  insert :: a -> [a] -> [[a]]
  insert x [] = [[x]]
  insert x (y:ys) = [x:y:ys] ++ (map (y:) (insert x ys))

-- 2)	 Строгая свёртка
-- Реализуйте строгую версию левой свёртки списка с помощью оператора seq.
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f a [] = a
foldl'' f a (x:xs) = seq a foldl'' f (f a x) xs

-- 3) Две сворачивающие функции
-- За один проход свёртки вычислить и сумму, и произведение элементов списка.
sumProd :: Num a => [a] -> (a, a)
sumProd = foldl step (0, 1) where
  step :: Num a => (a, a) -> a -> (a, a)
  step (s, p) n = (s + n, p * n)
