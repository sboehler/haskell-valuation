{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Valuation
    ( module Valuation
    ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader

type Year = Int

data Scenario a = Scenario
    { year :: Int,
      terminalYear :: Int,
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

runValuation :: s -> Value s a -> Year -> Year -> Either String a
runValuation s v ty y = runIdentity (runExceptT (runReaderT (runValue v) (Scenario y ty s)))

runV :: (Show a, RealFrac a) => Value () a -> Year -> [Year] -> IO ()
runV v ty (y:ys) = do
    putStr $ show y
    putStr "  "
    case runValuation () v ty y of
        Left s -> print s
        Right result -> print result
    runV v ty ys
runV _ _ [] = return ()


previous :: Value s a -> Value s a
previous v = do
    y <- subtract 1 <$> asks year
    v <@> y

next :: Value s a -> Value s a
next v = do
    y <- (+ 1) <$> asks year
    v <@> y

(<@>) :: Value s a -> Year -> Value s a
(<@>) v y = local (\x -> x {year = y}) v

discount :: Value s Double -> Year -> Value s Double -> Value s Double
discount wacc t0 v = do
    y <- asks year
    case y of
      t | t == t0 -> v
        | otherwise -> discount wacc (t0 + 1) v / (1 + (wacc <@> (t0 + 1)))

npv :: Value s Double -> Value s Double -> Value s Double
npv w v = do
    pv <- v
    fv <- next (npv w v / (1 + w)) <|> return 0
    return $ pv + fv


interpolate :: Fractional a => [(Year, a)] -> Value s a
interpolate l = do
    y <- asks year
    case l of
        []                       -> throwError "Invalid year"
        [(t0, v0)]
            | y >= t0            -> return v0
            | otherwise          -> throwError "Invalid year"
        ((t0, v0):(t1, v1):ps)
            | y == t0            -> return v0
            | y < t1             -> return $ v0 + (v1 - v0) * fromIntegral (y - t0) / fromIntegral (t1 - t0)
            | otherwise          -> interpolate $ (t1, v1) : ps
