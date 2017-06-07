module Valuation where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Year = Int

type Value s a = ReaderT s (MaybeT Identity) a

type TimeValue s a = Year -> Value s a

runValuation :: s -> Value s a -> Maybe a
runValuation s v = runIdentity (runMaybeT (runReaderT v s))

year :: TimeValue s Year
year = return

previous :: TimeValue s a -> TimeValue s a
previous v t = v (t - 1)

next :: TimeValue s a -> TimeValue s a
next v t = v (t + 1)

discount :: TimeValue s Double -> Year -> TimeValue s Double -> TimeValue s Double
discount wacc t0 v t
    | t == t0 = v t
    | otherwise = do
        w <- wacc (t0 + 1)
        v' <- discount wacc (t0 + 1) v t
        return $ (1 / (1 + w)) * v'

npv :: TimeValue s Double -> TimeValue s Double -> TimeValue s Double
npv w v t0 = do
    v' <- next v t0
    w' <- next w t0
    n' <- npv w v (t0 + 1) <|> return 0
    return $ 1 / (1 + w') * (n' + v')

interpolate
    :: Fractional a
    => [(Year, a)] -> TimeValue s a
interpolate [(t0, v0)] y
    | y == t0 = return v0
    | otherwise = lift mzero
interpolate ((t0, v0):(t1, v1):ps) y
    | y < t0 = lift mzero
    | (t0 <= y) && (y <= t1) = return $ v0 + (v1 - v0) * fromIntegral (y - t0) / fromIntegral (t1 - t0)
    | otherwise = interpolate ((t1, v1) : ps) y
