{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Valuation
    ( module Valuation
    ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

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
discount wacc t0 v y
    | y == t0 = v y
    | otherwise = discount wacc (t0 + 1) v y / (1 + wacc (t0 + 1))

npv :: TimeValue s Double -> TimeValue s Double -> TimeValue s Double
npv w v y = do
    pv <- v y
    fv <- npv w v y / (1 + w y) <|> return 0
    return $ pv + fv


interpolate :: Fractional a => [(Year, a)] -> TimeValue s a

interpolate [] _                 = throwError "invalid year"

interpolate [(t0, v0)] y
    | y >= t0                    = return v0
    | otherwise                  = throwError "Invalid year"

interpolate ((t0, v0):(t1, v1):ps) y
    | y == t0            = return v0
    | y < t1             = return $ v0 + (v1 - v0) * fromIntegral (y - t0) / fromIntegral (t1 - t0)
    | otherwise          = interpolate ((t1, v1) : ps) y


