-- 1) Группа
class Group a where
  id1 :: a
  assoc :: a -> a -> a
  inverse :: a -> a

-- 2) Кольцо
-- Определите класс типов Кольцо (с единицей), https://ru.wikipedia.org/wiki/Кольцо_(математика).
class (Group a) => Ring1 a where
  add1 :: a -> a -> a
  mul1 :: a -> a -> a

-- 3) Кольцо вычетов
-- Определите тип данных Кольцо вычетов по модулю n представителем класса типов Кольцо (с единицей), https://ru.wikipedia.org/wiki/Сравнение_по_модулю#Классы_вычетов.
class Ring a where
  id2 :: a -> a
  add :: a -> a -> a
  mul :: a -> a -> a

data RingMod = RM { a :: Integer, n :: Integer } deriving (Show, Eq)

rm :: Integer -> Integer -> RingMod
rm a n = RM (a `mod` n) n

instance Ring RingMod where
  id2 (RM a n) = RM 1 n
  add (RM a n) (RM b n2) = rm (a + b) n
  mul (RM a n) (RM b n2) = rm (a * b) n

-- 4) Двоичное дерево
-- Определите тип данных Двоичное дерево.
data Tree a = Null | Node (Tree a) a (Tree a) deriving(Show)

-- data List a = [] | a : List a

-- 5) DFS
-- Сделайте Двоичное дерево представителем класса Foldable, реализовав обход дерева в глубину.

-- instance Foldable Tree where
	-- foldMap dfs Null = mempty
	-- foldMap dfs (Node left x right) = foldMap dfs left `mappend` dfs x `mappend` foldMap dfs right

-- 6) BFS
-- Сделайте Двоичное дерево представителем класса Foldable, реализовав обход дерева в ширину.

instance Foldable Tree where
  foldMap f Null = mempty
  foldMap f (Node l x r) = f x `mappend` foldMap f l `mappend` foldMap f r

  foldr f z Null = z
  foldr f z (Node l v r) = foldr f z (bfs (Node l v r))

bfs :: Tree a -> [a]
bfs tree = visit [tree] where
  visit [] = []
  visit xs = map nodeValue xs ++ visit (concat (map nNodes xs)) where
    nodeValue (Node _ a _) = a
    nNodes (Node  Null _ Null) = []
    nNodes (Node a _ Null) = [a]
    nNodes (Node  Null _ b) = [b]
    nNodes (Node a _ b) = [a,b]

-- 7) DFS*3
-- Расширение задачи 5. Реализуете три варианта обхода дерева: preorder, in-order, postorder, https://en.m.wikipedia.org/wiki/Tree_traversal
newtype PreOrder a = PreOrder (Tree a) deriving(Show)
newtype InOrder a = InOrder (Tree a) deriving(Show)
newtype PostOrder a = PostOrder (Tree a) deriving(Show)

instance Foldable PostOrder where
	 foldMap dfs (PostOrder Null) = mempty
	 foldMap dfs (PostOrder (Node left x right)) = foldMap dfs (PostOrder left) `mappend` foldMap dfs (PostOrder right) `mappend` dfs x

instance Foldable InOrder where
	foldMap dfs (InOrder Null) = mempty
	foldMap dfs (InOrder (Node left x right)) = foldMap dfs (InOrder left) `mappend` dfs x `mappend` foldMap dfs (InOrder right)

instance Foldable PreOrder where
	foldMap dfs (PreOrder Null) = mempty
	foldMap dfs (PreOrder (Node left x right)) = dfs x `mappend` foldMap dfs (PreOrder left) `mappend` foldMap dfs (PreOrder right)

tree = Node	
  (Node Null 20 Null)
  30
  (Node (Node Null 40 Null) 10 (Node Null 50 Null))
