-- 1)	Перестановки
-- Реализуйте перестановки из #1 лабораторной с помощью монады списка (оператором монадического связывания, либо через do-нотацию).
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = do {
   tmp <- perms xs;
   insert x tmp
} where
    insert :: a -> [a] -> [[a]]
    insert x []     = [[x]]
    insert x (y:ys) = [x:y:ys] ++ (map (y:) (insert x ys))
	
-- perms (x:xs) = (perms xs) >>= (insert x) where
    -- insert :: a -> [a] -> [[a]]
    -- insert x []     = [[x]]
    -- insert x (y:ys) = [x:y:ys] ++ (map (y:) (insert x ys))
	
-- perms (x:xs) = concatMap (insert x) (perms xs) 