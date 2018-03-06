-- Q30.1 To prove that Monad is strictly more powerful than Functor, 
-- write a universal version of <$>, as in the preceding lesson’s exercise, 
-- called allFmapM, that defines <$> for all members of the Monad type class. 
-- Because it works for all instances of Monad, the only functions you can use are the methods required by the Monad type class 
-- (and lambda functions). To get you started, here’s your type signature:
allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM = (flip (>>=)) . (return . )

-- Q30.2 To prove that Monad is strictly more powerful than Applicative, 
-- write a universal version of <*>, called allApp, that defines <*> for all members of the Monad type class. 
-- Because it works for all instances of Monad, the only functions you can use are the methods required by the Monad 
-- type class (and lambda functions). To get you started, here’s your type signature:
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf mv = mf >>= 
  \f -> mv >>=
  \v -> return $ f v 

-- Q30.3 Implement a bind function which is the same as (>>=) for Maybe:
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind (Just v) k = k v
bind _ _ = Nothing 
