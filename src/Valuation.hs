{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Valuation
    ( module Valuation
    ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Statistics.Distribution
import Statistics.Distribution.Normal

type Year = Int

newtype Scenario a = Scenario
    {
      scenario :: a
    }

newtype Value s a = Value
    { runValue :: ReaderT (Scenario s) (ExceptT String Identity) a
    } deriving
    (
        Monad,
        Applicative,
        Alternative,
        Functor,
        MonadReader (Scenario s),
        MonadError String
    )

instance Num a => Num (Value s a) where
    negate = fmap negate
    (+) = liftM2 (+)
    (*) = liftM2 (*)
    fromInteger = pure . fromInteger
    abs = fmap abs
    signum = fmap signum

instance Fractional a => Fractional (Value s a) where
    (/) = liftM2 (/)
    fromRational = pure . fromRational
    recip x = 1 / x

type TimeValue s a = Year -> Value s a

runValuation :: s -> Value s a -> Either String a
runValuation s v = runIdentity (runExceptT (runReaderT (runValue v) (Scenario s)))

runV :: (Show a, RealFrac a) => TimeValue () a -> [Year] -> IO ()
runV v (y:ys) = do
    putStr $ show y
    putStr "  "
    case runValuation () $ v y of
        Left s -> print s
        Right result -> print result
    runV v ys
runV _ [] = return ()

discount :: TimeValue s Double -> Year -> TimeValue s Double -> TimeValue s Double
discount r t0 v t
    | t == t0 = v t
    | otherwise = discount r (t0 + 1) v t / (1 + r (t0 + 1))

npv :: TimeValue s Double -> TimeValue s Double -> TimeValue s Double
npv r v t = v t + (npv r v (t+1) / (1 + r (t+1)) <|> return 0)

interpolate :: Fractional a => [(Year, a)] -> TimeValue s a

interpolate [] _                 = throwError "invalid year"

interpolate [(t0, v0)] t
    | t >= t0                    = return v0
    | otherwise                  = throwError "Invalid year"

interpolate ((t0, v0):(t1, v1):ps) t
    | t == t0            = return v0
    | t < t1             = return $ v0 + (v1 - v0) * fromIntegral (t - t0) / fromIntegral (t1 - t0)
    | otherwise          = interpolate ((t1, v1) : ps) t

linear :: Fractional a => TimeValue s a -> Year -> Year -> a -> TimeValue s a
linear v t0 t1 target t
    | t0 < t && t <= t1 = do
        base <- v t0
        return $ base + (target - base) * fromIntegral (t - t0) / fromIntegral (t1 - t0)
    | t1 < t            = return target


pvAnnuity :: Double -> Double -> Int -> Double
pvAnnuity r pmt n = pmt * (1 - (1+r)^^(-n)) / r

blackScholes :: Double -> Double -> Double -> Double -> Double -> Double
blackScholes sigma r t s k =
    let d1 = 1 / (sigma * sqrt t) * (log (s / k) + (r + sigma ** t / 2) * t)
        d2 = d1 - sigma * sqrt t
     in
    s * cumulative standard d1 - cumulative standard d2 * k * exp (-r * t)
