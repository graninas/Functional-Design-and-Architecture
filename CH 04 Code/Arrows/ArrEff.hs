{-# LANGUAGE Arrows #-}
module ArrEff where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Control.Monad.Free

type Prod eff b c = (c, ArrEff eff b c)
newtype ArrEff eff b c = ArrEff (b -> eff (Prod eff b c))

instance Monad eff => Category (ArrEff eff) where
    id = ArrEff (\b -> return (b, id))
    ArrEff g . ArrEff f = ArrEff arrFG
      where
        arrFG a = do
            fa <- f a
            feededF fa
        feededF (b, arr1) = do
            gb <- g b
            feededG arr1 gb
        feededG arr1 (c, arr2) = return (c, arr2 . arr1)

instance Monad eff => Arrow (ArrEff eff) where
    arr f = ArrEff (\b -> return (f b, arr f))
    first (ArrEff f) = ArrEff arrF
      where
        arrF (b, d) = do
            fb <- f b
            feededF fb d
        feededF (c, arr1) d = return ((c, d), first arr1)

instance Monad eff => Functor (ArrEff eff b) where
    fmap f (ArrEff r) = ArrEff (\b -> do
        (c, next) <- r b
        return (f c, fmap f next))


mArr mf = ArrEff (\b -> do
    c <- mf b
    return (c, mArr mf))

mConst mf = ArrEff (\_ -> do
    c <- mf
    return (c, mConst mf))

aConst c = arr (const c)

runArrEffList :: Monad m => [c] -> ArrEff m b c -> [b] -> m [c]
runArrEffList accum (ArrEff f) []     = return accum
runArrEffList accum (ArrEff f) (b:bs) = do
    (c, next) <- f b
    runArrEffList (c:accum) next bs

runArrEff :: Monad m => ArrEff m b c -> [b] -> m [c]
runArrEff = runArrEffList []

runArrEff1 :: Monad m => ArrEff m b c -> b -> m (c, ArrEff m b c)
runArrEff1 (ArrEff f) b = f b

timesA :: Monad m => Int -> ArrEff m b c -> ArrEff m b [c]
timesA 0 _  = aConst []
timesA n ar = ArrEff (\b -> do
    (c, next)   <- runArrEff1 ar b
    (cs, next') <- runArrEff1 (timesA (n-1) next) b
    return (c:cs, next'))

forEachA :: Monad m => ArrEff m b () ->  ArrEff m [b] ()
forEachA ar = ArrEff (\bs -> do
    mapM_ (runArrEff1 ar) bs
    return ((), aConst ()))

duplicateA :: Monad m => ArrEff m b (b, b)
duplicateA = arr $ \b -> (b, b)

takeFirstA :: Monad m => ArrEff m (b, c) b
takeFirstA = arr $ \(b, _) -> b

takeSecondA :: Monad m => ArrEff m (b, c) c
takeSecondA = arr $ \(_, c) -> c

------ Arrow for Free language --------------------------------------------------
type ArrEffFree f b c = ArrEff (Free f) b c

-- :t says:
-- (Monad m1, Monad m)
--      => (m (c, ArrEff m b c) -> m1 (b1, t))
--      -> ArrEff m b c
--      -> b
--      -> m1 b1
runFreeArr interpret ar v = do
    let p = runArrEff1 ar v
    (c, next) <- interpret p -- TODO: what to do with next?
    return c

