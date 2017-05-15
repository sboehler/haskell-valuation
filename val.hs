module Val where

type Year = Int

type Value = Year -> Double

revenueGrowthRate :: Value
revenueGrowthRate = const 0.05

revenue :: Value
revenue 2014 = 10.0
revenue 2015 = 11.0
revenue t
    | t <= 2020 = revenue (t - 1) * (1 + revenueGrowthRate t)
    | otherwise = revenue 2020

opex :: Value
opex t = revenue t * 0.9

grossMargin :: Value
grossMargin t = revenue t - opex t

assets :: Value
assets 2014 = 60.0
assets t = assets (t - 1) + grossMargin t

discount :: Double -> Year -> Value -> Year -> Double
discount r y0 v y = v y / (1 + r) ^^ (y - y0)

data Value2 = Value2
    { year :: Int
    , value :: Double
    }
