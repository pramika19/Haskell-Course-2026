import Data.Monoid (Sum(..))

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a)
  deriving (Show)
--1)Functor for Sequence
instance Functor Sequence where
    fmap :: (a -> b) -> Sequence a -> Sequence b
    fmap _ Empty        = Empty
    fmap f (Single x)   = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)


--2)Foldable for sequence
instance Foldable Sequence where
    foldMap :: Monoid m => (a -> m) -> Sequence a -> m
    foldMap _ Empty        = mempty
    foldMap f (Single x)   = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = foldr (:) []

seqLength :: Sequence a -> Int
seqLength = getSum . foldMap (const (Sum 1))


--3)Semigroup & Monoid for sequence
instance Semigroup (Sequence a) where
    (<>) Empty ys        = ys
    (<>) (Single x) ys   = Append (Single x) ys
    (<>) (Append l r) ys = Append l (r <> ys)

instance Monoid (Sequence a) where
    mempty :: Sequence a
    mempty = Empty


--4)Tail Recursion and Sequence Search
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem x seq0 = go [seq0]
  where
    go []                    = False
    go (Empty      : rest)   = go rest
    go (Single y   : rest)   = y == x || go rest
    go (Append l r : rest)   = go (l : r : rest)


--5)Tail Recursion and Sequence Flatten
tailToList :: Sequence a -> [a]
tailToList seq0 = go [seq0] []
  where
    go []                    acc = reverse acc
    go (Empty      : rest)   acc = go rest acc
    go (Single y   : rest)   acc = go rest (y : acc)
    go (Append l r : rest)   acc = go (l : r : rest) acc


--6)Tail Recursion and Reverse Polish Notation
data Token = TNum Int | TAdd | TSub | TMul | TDiv

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go []           [result] = Just result
    go []           _        = Nothing        
    go (TNum n : ts) stack   = go ts (n : stack)
    go (op     : ts) stack   =
        case stack of
            (b : a : rest) ->
                case applyOp op a b of
                    Nothing  -> Nothing         
                    Just res -> go ts (res : rest)
            _ -> Nothing                        

    applyOp TAdd a b = Just (a + b)
    applyOp TSub a b = Just (a - b)
    applyOp TMul a b = Just (a * b)
    applyOp TDiv a b
        | b == 0    = Nothing
        | otherwise = Just (a `div` b)


--7) Expressing functions via foldr and foldl
--a) reverse /foldl
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

--b) takeWhile/foldr
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x acc -> if p x then x : acc else []) []

--c) decimal
decimal :: [Int] -> Int
decimal = foldl (\acc d -> acc * 10 + d) 0


--8)Run-length encoding via folds
--a) encode/foldr
encode :: Eq a => [a] -> [(a, Int)]
encode = foldr step []
  where
    step x []                       = [(x, 1)]
    step x ((y, n) : rest)
        | x == y                    = (y, n + 1) : rest
        | otherwise                 = (x, 1) : (y, n) : rest

--b) decode/foldr
decode :: [(a, Int)] -> [a]
decode = foldr (\(x,n) acc -> foldr (:) acc (replicate n x)) []