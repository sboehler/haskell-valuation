module Val where

type Year = Int

type Value a = Year -> a

year :: Value Year
year = id

previous :: Value a -> Value a
previous v t = v (t - 1)

discount :: Double -> Year -> Value Double -> Value Double
discount r t0 v t = (discountFactor ^^ dt) * v t
  where
    discountFactor = 1 / (1 + r)
    dt = t - t0

npv :: Double -> Value Double -> Value Double
npv r v t0 = (sum . take 1000 . map (discount r t0 v)) [t0 ..]

revenueGrowthRate :: Value Double
revenueGrowthRate = const 0.05

revenue :: Value Double
revenue 2014 = 10.0
revenue 2015 = 11.0
revenue t
    | t <= 2020 = revenue (t - 1) * (1 + revenueGrowthRate t)
    | otherwise = revenue 2020

opex :: Value Double
opex = (* 0.9) . revenue

grossMargin :: Value Double
grossMargin = (-) <$> revenue <*> opex

assets :: Value Double
assets 2014 = 60.0
assets t = previous assets t + grossMargin t
