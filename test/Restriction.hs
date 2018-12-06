{-# LANGUAGE ExistentialQuantification #-}

data AA a = forall m . Monad m => AA (m a)

instance Monad m => Monad AA where
     return = AA . return    -- Me veo obligado a definir AA como "data AA m a"

