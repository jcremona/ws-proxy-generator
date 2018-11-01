module Generator.StReader where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM,ap)

--newtype State s a = State { runState :: s -> (a, s) }
--newtype Reader e a = Reader { runReader :: e -> a }

newtype StReader s e a = StReader { runStReader :: s -> e -> Maybe (a,s) }

instance Functor (StReader s e) where
    fmap = liftM

instance Applicative (StReader s e) where
    pure = return
    (<*>) = ap

instance Monad (StReader s e) where
    return x = StReader (\ s e -> Just (x, s) )
    StReader t >>= f = StReader (\ st e -> case t st e of
                                                Nothing -> Nothing
                                                Just (a, st') -> runStReader (f a) st' e) 


throwExc :: StReader s e a
throwExc = StReader (\ _ _ -> Nothing)

ask :: StReader s e e
ask = StReader (\ st env -> Just (env, st))

put :: s -> StReader s e ()
put st = StReader (\ _ _ -> Just ((), st))

get :: StReader s e s
get = StReader (\ st _ -> Just (st, st))

local :: (env -> env) -> StReader s env a -> StReader s env a
local f (StReader g) = StReader (\s e -> g s $ f e)  

--(\ st -> let (a, st') = s st 
--          in runState (f a) st'
  

-- f :: a -> State s b
-- s :: s -> (a, s)
-- runState :: State s a -> s -> (a,s)



--    Reader env >>= f = (\e -> runReader (f (env e)) e) 


-- f :: a -> Reader e b
-- env :: e -> a

