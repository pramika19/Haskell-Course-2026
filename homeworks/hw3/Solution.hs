import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM,guard)
import Control.Monad.Writer
import Data.List (permutations)


--1)Maze navigation
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

--a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  neighbours <- Map.lookup pos maze
  Map.lookup dir neighbours

--b
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start dirs = foldM (move maze) start dirs

--c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start dirs = (start :) <$> go start dirs
  where
    go _   []     = Just []
    go pos (d:ds) = do
      next <- move maze pos d
      rest <- go next ds
      return (next : rest)


--2)Decoding message

type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`Map.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

--3)Seating aarangements

type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  perm <- permutations guests
  let pairs = zip perm (tail perm ++ [head perm]) 
  let hasConflict (a, b) = (a, b) `elem` conflicts || (b, a) `elem` conflicts
  guard (not $ any hasConflict pairs)
  return perm


--4)Result MOnad with warnings
data Result a = Failure String | Success a [String]
  deriving (Show)

--a)
instance Functor Result where
  fmap _ (Failure msg)       = Failure msg
  fmap f (Success val warns) = Success (f val) warns

instance Applicative Result where
  pure x = Success x []

  Failure msg       <*> _                 = Failure msg
  _                 <*> Failure msg       = Failure msg
  Success f warns1  <*> Success x warns2  = Success (f x) (warns1 ++ warns2)

instance Monad Result where
  return = pure

  Failure msg       >>= _ = Failure msg
  Success val warns >>= f =
    case f val of
      Failure msg        -> Failure msg
      Success val' warns' -> Success val' (warns ++ warns')

--b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

--c)
validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Age cannot be negative"
  | age > 150 = do
      warn "Age cannot be above 150 "
      return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

--5)Evaluator with simplification log
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show)

simplify :: Expr -> Writer [String] Expr

simplify (Add e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  simplifyAdd e1' e2'

simplify (Mul e1 e2) = do
  e1' <- simplify e1
  e2' <- simplify e2
  simplifyMul e1' e2'

simplify (Neg e) = do
  e' <- simplify e
  simplifyNeg e'

simplify lit = return lit

simplifyAdd :: Expr -> Expr -> Writer [String] Expr
simplifyAdd (Lit 0) e = do
  tell ["Add identity: 0 + e -> e"]
  return e
simplifyAdd e (Lit 0) = do
  tell ["Add identity: e + 0 -> e"]
  return e
simplifyAdd (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " + " ++ show b ++ " -> " ++ show (a+b)]
  return (Lit (a + b))
simplifyAdd e1 e2 = return (Add e1 e2)


simplifyMul :: Expr -> Expr -> Writer [String] Expr
simplifyMul (Lit 1) e = do
  tell ["Mul identity: 1 * e -> e"]
  return e
simplifyMul e (Lit 1) = do
  tell ["Mul identity: e * 1 -> e"]
  return e
simplifyMul (Lit 0) _ = do
  tell ["Zero absorption: 0 * e -> 0"]
  return (Lit 0)
simplifyMul _ (Lit 0) = do
  tell ["Zero absorption: e * 0 -> 0"]
  return (Lit 0)
simplifyMul (Lit a) (Lit b) = do
  tell ["Constant folding: " ++ show a ++ " * " ++ show b ++ " -> " ++ show (a*b)]
  return (Lit (a * b))
simplifyMul e1 e2 = return (Mul e1 e2)


simplifyNeg :: Expr -> Writer [String] Expr
simplifyNeg (Neg e) = do
  tell ["Double negation: --e -> e"]
  return e
simplifyNeg e = return (Neg e)


--6)ziplist
newtype ZipList a = ZipList { getZipList :: [a] } deriving (Show)

--a)
instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x  = ZipList (repeat x)  
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

--b)
test1=pure id <*> ZipList [1,2,3]
test2=pure (+) <*> ZipList [1,2,3] <*> ZipList [10,20,30]

--c)
{-ZipList cannot have a lawful Monad instance because its behavior depends on fixed positions while a Monad 
needs to combine results where each input can produce outputs of different lengths.

If a function returns ZipLists of different sizes, there is no clean way to align them
position by position. We would either have to drop values or invent new ones which breaks consistency.

Because of this we cannot define >>= in a way that satisfies the monad laws. -}
