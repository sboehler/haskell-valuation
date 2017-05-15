module Val where

type Year = Int

newtype Value a = V
    { eval :: a
    }

instance Functor Value where
    fmap f (V a) = V (f . a)

    {- pure x = V $ const x -}
    {- f <*> x = V (\t -> eval f t (eval x t)) -}
    {- V -}
        {- (\t -> -}
             {- let factor = 1 / (1 + rate) -}
                 {- delta = t - year -}
                 {- f = (* factor ^^ delta) -}
             {- in eval (fmap f v) t) -}

{- instance Applicative Value where -}
{- discount :: Double -> Year -> Value Double -> Value Double -}
{- discount rate year v = -}
revenueGrowthRate :: Value (Year -> Double)
revenueGrowthRate = V $ const 0.05

revenue :: Value (Year -> Double)
revenue =
    let v 2014 = 10.0
        v 2015 = 11.0
        v t
            | t <= 2020 = eval revenue (t - 1) * (1 + eval revenueGrowthRate t)
            | otherwise = eval revenue 2020
    in V v

opex :: Value (Year -> Double)
opex = fmap (\f -> (* 0.9) . f) revenue

grossMargin :: Value (Year -> Double)
grossMargin = (-) <$> revenue <*> opex

previous :: Value (Year -> Double) -> Value (Year -> Double)
previous v = V (\t -> eval v (t - 1))

assets :: Value (Year -> Double)
assets =
    let v 2014 = 60.0
        v t = eval assets (t - 1) + eval grossMargin t
    in V v
