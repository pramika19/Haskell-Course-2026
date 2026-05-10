newtype Reader r a = Reader { runReader :: r -> a }

--funtor,applicative and monad instances
instance Functor (Reader r) where
  fmap f ra = Reader $ \env -> f (runReader ra env)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x
  liftA2 f ra rb = Reader $ \env -> f (runReader ra env) (runReader rb env)

instance Monad (Reader r) where
  ra >>= k = Reader $ \env -> runReader (k (runReader ra env)) env

--primitives
ask :: Reader r r
ask = Reader $ \env -> env

asks :: (r -> a) -> Reader r a
asks f = Reader $ \env -> f env

local :: (r -> r) -> Reader r a -> Reader r a
local modify ra = Reader $ \env -> runReader ra (modify env)

--bank eg
data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)



calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return $ truncate (fromIntegral (balance acc) * rate)


applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return $ acc { balance = balance acc - fee }


checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return $ balance acc >= minBal


processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  updatedAcc <- applyTransactionFee acc   
  interest   <- calculateInterest acc     
  meetsMin   <- checkMinimumBalance acc   
  return (updatedAcc, interest, meetsMin)