{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Generator.StReader where

import Control.Applicative 
import Control.Monad (liftM,ap)
import Control.Exception
import           Control.Monad.Catch          (MonadThrow, throwM)
--newtype State s a = State { runState :: s -> (a, s) }
--newtype Reader e a = Reader { runReader :: e -> a }

data InternalStReader m s e a = InternalStReader { runStReader :: s -> e -> m (a,s) }
type StReader a = forall m s e. (MonadThrow m) => InternalStReader m s e a

instance MonadThrow m => Functor (InternalStReader m s e) where
    fmap = liftM

instance MonadThrow m => Applicative (InternalStReader m s e) where
    pure = return
    (<*>) = ap

instance MonadThrow m => Monad (InternalStReader m s e) where
    return x = InternalStReader (\ s e -> return (x, s) )
    InternalStReader t >>= f = InternalStReader (\ st e -> t st e >>= \(a, st') -> runStReader (f a) st' e)
                                                --Nothing -> Nothing
                                                --Just (a, st') -> runInternalStReader (f a) st' e) 


--instance (MonadThrow m, Alternative m) => Alternative (InternalStReader m s e) where
--      empty = InternalStReader (\_ _ -> throwM . toException $ CustomException "Empty")
--      InternalStReader f <|> (InternalStReader g) = InternalStReader (\ s e -> f s e <|> g s e)  


throwExc :: (MonadThrow m) => SomeException -> InternalStReader m s e a
throwExc e = InternalStReader (\ _ _ -> throwM e)

ask :: MonadThrow m => InternalStReader m s e e
ask = InternalStReader (\ st env -> return (env, st))

put :: MonadThrow m => s -> InternalStReader m s e ()
put st = InternalStReader (\ _ _ -> return ((), st))

get :: MonadThrow m => InternalStReader m s e s
get = InternalStReader (\ st _ -> return (st, st))

local :: MonadThrow m => (env -> env) -> InternalStReader m s env a -> InternalStReader m s env a
local f (InternalStReader g) = InternalStReader (\s e -> g s $ f e)  



data CustomException = CustomException {
                            errorMessage :: String
                       } deriving (Show)

instance Exception CustomException where

--(\ st -> let (a, st') = s st 
--          in runState (f a) st'
  

-- f :: a -> State s b
-- s :: s -> (a, s)
-- runState :: State s a -> s -> (a,s)



--    Reader env >>= f = (\e -> runReader (f (env e)) e) 


-- f :: a -> Reader e b
-- env :: e -> a

