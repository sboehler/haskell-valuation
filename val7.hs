module Val where

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

type Year = Int

data Scenario
    = High
    | Low
    | Rate Double

type Value a = ReaderT Scenario (MaybeT Identity) a

type TimeValue a = Year -> Value a

year :: TimeValue Year
year = return

previous :: TimeValue a -> TimeValue a
previous v t = v (t - 1)

discount :: Double -> Year -> TimeValue Double -> TimeValue Double
discount r t0 v t = fmap (* (discountFactor ^^ dt)) (v t)
  where
    discountFactor = 1 / (1 + r)
    dt = t - t0

npv :: Double -> TimeValue Double -> TimeValue Double
npv r v t0 =
    let discounted = discount r t0
        values = mapM (discounted v) [t0 ..]
    in fmap (sum . take 1000) values

revenueGrowthRate :: Value Double
revenueGrowthRate = do
    scenario <- ask
    case scenario of
        Low -> return 0.02
        High -> return 0.05
        Rate x -> return x

revenue :: TimeValue Double
revenue 2013 = lift mzero
revenue 2014 = return 10.0
revenue 2015 = return 11.0
revenue t
    | 2016 <= t && t <= 2020 = do
        g <- revenueGrowthRate
        r <- previous revenue t
        return (r * (1 + g))
    | t >= 2020 = revenue 2020

opex :: TimeValue Double
opex = do
    r <- revenue
    return (fmap (0.9 *) r)

grossMargin :: TimeValue Double
grossMargin t = do
    r <- revenue t
    o <- opex t
    return (r - o)

assets :: TimeValue Double
assets 2014 = return 60.0
assets t = do
    a <- previous assets t
    g <- grossMargin t
    return (a + g)
