module Val where

type Year = Int

data Scenario
    = High
    | Low
    | Rate Double

runScenario :: Scenario -> Value a -> Year -> a
runScenario s v = v s

type Value a = Scenario -> Year -> a

year :: Value Year
year _ = id

previous :: Value a -> Value a
previous v s t = v s (t - 1)

discount :: Double -> Year -> Value Double -> Value Double
discount r t0 v s t = (discountFactor ^^ dt) * v s t
  where
    discountFactor = 1 / (1 + r)
    dt = t - t0

npv :: Double -> Value Double -> Value Double
npv r v s t0 = (sum . take 1000 . map (discount r t0 v s)) [t0 ..]

revenueGrowthRate :: Value Double
revenueGrowthRate Low = const 0.02
revenueGrowthRate High = const 0.05
revenueGrowthRate (Rate x) = const x

revenue :: Value Double
revenue _ 2014 = 10.0
revenue _ 2015 = 11.0
revenue s t
    | t <= 2020 = revenue s (t - 1) * (1 + revenueGrowthRate s t)
    | otherwise = revenue s 2020

opex :: Value Double
opex s = (* 0.9) . revenue s

grossMargin :: Value Double
grossMargin s = (-) <$> revenue s <*> opex s

assets :: Value Double
assets _ 2014 = 60.0
assets s t = previous assets s t + grossMargin s t
