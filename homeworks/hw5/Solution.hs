module StateMachines where

import Control.Monad.State
import Control.Monad (unless)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.IO (hFlush, stdout)

--stack machine
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)

execInstr POP = do
  stack <- get
  case stack of
    []     -> return ()         
    (_:xs) -> put xs

execInstr DUP = do
  stack <- get
  case stack of
    []     -> return ()
    (x:xs) -> put (x : x : xs)

execInstr SWAP = do
  stack <- get
  case stack of
    (x:y:xs) -> put (y : x : xs)
    _         -> return ()       

execInstr ADD = do
  stack <- get
  case stack of
    (x:y:xs) -> put (x + y : xs)
    _         -> return ()

execInstr MUL = do
  stack <- get
  case stack of
    (x:y:xs) -> put (x * y : xs)
    _         -> return ()

execInstr NEG = do
  stack <- get
  case stack of
    []     -> return ()
    (x:xs) -> put (negate x : xs)


execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg prog = execState (execProg prog) []

--expression evaluator with variable bindings
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr   
  | Seq  Expr Expr     
  deriving (Show)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = 
    return n

eval (Var name)   = do
  env <- get
  return (env Map.! name)          

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 + v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 * v2)

eval (Neg e) = do
  v <- eval e
  return (negate v)

eval (Assign name expr) = do
  v <- eval expr
  modify (Map.insert name v)
  return v

eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval expr = evalState (eval expr) Map.empty

--memoised edit distance
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int

editDistM xs ys i j = do
  cache <- get

  case Map.lookup (i, j) cache of
    Just v  -> return v
    Nothing -> do
      v <- compute
      modify (Map.insert (i, j) v)
      return v
  where
    compute
      | i == 0    = return j
      | j == 0    = return i
      | xs !! (i-1) == ys !! (j-1) = editDistM xs ys (i-1) (j-1)
      | otherwise = do
          del  <- editDistM xs ys (i-1) j
          ins  <- editDistM xs ys i     (j-1)
          sub  <- editDistM xs ys (i-1) (j-1)
          return (1 + minimum [del, ins, sub])

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty

--state and treasure hunters game simulation
data CellType
  = Normal
  | Decision [String]     
  | Obstacle Int          
  | IntermediateTreasure Int  
  | Trap Int              
  | Goal
  deriving (Show)

board :: Map Int CellType
board = Map.fromList
  [ (0,  Normal)
  , (1,  IntermediateTreasure 10)
  , (2,  Decision ["Forest path (1)", "Mountain pass (2)"])
  , (3,  Normal)
  , (4,  Obstacle 2)
  , (5,  Trap 15)
  , (6,  IntermediateTreasure 20)
  , (7,  Decision ["River route (1)", "Desert shortcut (2)"])
  , (8,  Normal)
  , (9,  Obstacle 3)
  , (10, IntermediateTreasure 30)
  , (11, Trap 10)
  , (12, Decision ["Narrow bridge (1)", "Hidden tunnel (2)"])
  , (13, Normal)
  , (14, Goal)
  ]

boardSize :: Int
boardSize = 15

data GameState = GameState
  { playerPos    :: Int
  , playerScore  :: Int
  , playerEnergy :: Int
  , turnNumber   :: Int
  , gameLog      :: [String]
  } deriving (Show)

initialState :: GameState
initialState = GameState
  { playerPos    = 0
  , playerScore  = 0
  , playerEnergy = 20
  , turnNumber   = 1
  , gameLog      = ["Game started! Reach cell 14 before energy runs out."]
  }


type AdventureGame a = StateT GameState IO a

logMsg :: String -> AdventureGame ()
logMsg msg = modify (\s -> s { gameLog = gameLog s ++ [msg] })


movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  gs <- get
  let pos     = playerPos gs
      newPos  = min (boardSize - 1) (pos + roll)
      moved   = newPos - pos
  put gs { playerPos    = newPos
          , playerEnergy = playerEnergy gs - 1
          , turnNumber   = turnNumber gs + 1
          }
  logMsg $ "Moved " ++ show moved ++ " space(s) to cell " ++ show newPos ++ "."
  return moved

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- lift $ getPlayerChoice options
  logMsg $ "Player chose: " ++ choice
  return choice

handleLocation :: AdventureGame Bool
handleLocation = do
  gs <- get
  let pos  = playerPos gs
      cell = Map.findWithDefault Normal pos board
  case cell of
    Goal -> do
      logMsg "Reached the treasure!!!"
      return True

    IntermediateTreasure bonus -> do
      modify (\s -> s { playerScore = playerScore s + bonus })
      logMsg $ "Found a treasure chest!  +" ++ show bonus ++ " points."
      return False

    Trap penalty -> do
      modify (\s -> s { playerScore = max 0 (playerScore s - penalty) })
      logMsg $ "Fell into a trap!  -" ++ show penalty ++ " points."
      return False

    Obstacle penalty -> do
      modify (\s -> s { playerEnergy = max 0 (playerEnergy s - penalty) })
      logMsg $ "An obstacle blocks way!  -" ++ show penalty ++ " energy."
      return False

    Decision paths -> do
      logMsg "Reached a decision point!"
      choice <- makeDecision paths
      let extraSteps = if choice == last paths then 2 else 1
      gs2 <- get
      let newPos = min (boardSize - 1) (playerPos gs2 + extraSteps)
      put gs2 { playerPos = newPos }
      logMsg $ "Path taken moves you to cell " ++ show newPos ++ "."
      return False

    Normal -> do
      logMsg "Nothing here, keep moving!"
      return False

playTurn :: AdventureGame Bool
playTurn = do
  gs <- get
  if playerEnergy gs <= 0
    then do
      logMsg "Ran out of energy.  Game over!"
      return True
    else do
      lift $ displayGameState gs
      roll <- lift getDiceRoll
      _    <- movePlayer roll
      done <- handleLocation
      return done

playGame :: AdventureGame ()
playGame = do
  done <- playTurn
  unless done playGame

getDiceRoll :: IO Int
getDiceRoll = do
  putStr "Roll the dice (enter 1-6): "
  hFlush stdout
  line <- getLine
  case reads line of
    [(n, "")] | n >= 1 && n <= 6 -> return n
    _ -> do
      putStrLn "  Invalid input.  Please enter a number between 1 and 6."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState gs = do
  putStrLn ""
  putStrLn (replicate 60 '=')
  putStrLn $ "  TURN " ++ show (turnNumber gs)
  putStrLn $ "  Position : " ++ show (playerPos gs) ++ " / " ++ show (boardSize - 1)
  putStrLn $ "  Score    : " ++ show (playerScore gs)
  putStrLn $ "  Energy   : " ++ show (playerEnergy gs)
  putStrLn ""
  let cells = [0 .. boardSize - 1]
      render i
        | i == playerPos gs = "[P]"
        | i == boardSize-1  = "[X]"
        | otherwise         = "[ ]"
  putStrLn $ "  " ++ concatMap render cells
  putStrLn ""
  let recent = drop (max 0 (length (gameLog gs) - 3)) (gameLog gs)
  mapM_ (\m -> putStrLn $ "  > " ++ m) recent
  putStrLn (replicate 60 '=')

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "  Choose path:"
  mapM_ (\(i, o) -> putStrLn $ "    " ++ show i ++ ") " ++ o)
        (zip [1..] options)
  putStr "  Your choice (number): "
  hFlush stdout
  line <- getLine
  case reads line of
    [(n, "")] | n >= 1 && n <= length options -> return (options !! (n - 1))
    _ -> do
      putStrLn "  Invalid choice.  Please try again."
      getPlayerChoice options


main :: IO ()
main = do
  putStrLn ""
  putStrLn "Stack Machine"
  print $ runProg [PUSH 3, PUSH 4, ADD, DUP, MUL,PUSH 5,DUP]              
  print $ runProg [ADD]                                  

  putStrLn "\nExpression Evaluator"
  let expr = Seq (Assign "x" (Num 2)) (Mul (Var "x") (Num 5))
  print $ runEval expr                                    

  putStrLn "\nEdit Distance"          
  print $ editDistance "sunday"  "monday"              
  print $ editDistance ""        "hello"                   

  putStrLn "\nTreasure Hunters Game"
  finalState <- execStateT playGame initialState
  putStrLn ""
  putStrLn "Result : "
  putStrLn $ "Score  : " ++ show (playerScore  finalState)
  putStrLn $ "Energy : " ++ show (playerEnergy finalState)
  putStrLn $ "Cell   : " ++ show (playerPos    finalState)