module Generator.StReader where

import Control.Applicative 
import Control.Monad (liftM,ap)
import Control.Exception
import Common.Exception
import           Control.Monad.Catch          (MonadThrow, throwM)

------------------------------------------------------------------------
-- StReader: una mónada utilizada durante el chequeo de tipos
------------------------------------------------------------------------

data StReader m s e a = StReader { runStReader :: s -> e -> m (a,s) }

instance MonadThrow m => Functor (StReader m s e) where
    fmap = liftM

instance MonadThrow m => Applicative (StReader m s e) where
    pure = return
    (<*>) = ap

instance MonadThrow m => Monad (StReader m s e) where
    return x = StReader (\ s e -> return (x, s) )
    StReader t >>= f = StReader (\ st e -> t st e >>= \(a, st') -> runStReader (f a) st' e)

throwExc :: (MonadThrow m) => SomeException -> StReader m s e a
throwExc e = StReader (\ _ _ -> throwM e)

ask :: MonadThrow m => StReader m s e e
ask = StReader (\ st env -> return (env, st))

put :: MonadThrow m => s -> StReader m s e ()
put st = StReader (\ _ _ -> return ((), st))

get :: MonadThrow m => StReader m s e s
get = StReader (\ st _ -> return (st, st))

local :: MonadThrow m => (env -> env) -> StReader m s env a -> StReader m s env a
local f (StReader g) = StReader (\s e -> g s $ f e)

throwCustomExceptionM :: (MonadThrow m) => String -> StReader m s e a
throwCustomExceptionM = throwExc . throwCustomException

