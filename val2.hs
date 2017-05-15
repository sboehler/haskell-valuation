module Val where

type Year = Int

newtype Value a = Value
    { getValue :: (a, Year)
    } deriving (Show, Eq)

instance Num a =>
         Num (Value a) where
    Value (a, t1) + Value (b, t2)
        | t1 == t2 = Value (a + b, t1)
        | otherwise = error "incompatible"
    Value (a, t1) * Value (b, t2)
        | t1 == t2 = Value (a * b, t1)
        | otherwise = error "Incompatible"
    negate (Value (a, t1)) = Value ((-1) * a, t1)
    abs (Value (a, t)) = Value (abs a, t)
    signum = error "Not implemented"
    fromInteger = error "fromInteger is not implemented"

toScalar :: Value a -> a
toScalar (Value (a, _)) = a

revenueGrowthRate :: Year -> Value Double
revenueGrowthRate y = Value (0.05, y)

revenue :: Year -> Value Double
revenue t
    | t == 2014 = Value (10.0, t)
    | t == 2015 = Value (11.0, t)
    | t <= 2020 = Value (toScalar (revenue (t - 1)) * (1 + toScalar (revenueGrowthRate t)), t)
    | otherwise = revenue 2020

opex :: Year -> Value Double
opex t = fmap (* 0.9) (revenue t)

grossMargin :: Year -> Value Double
grossMargin t = Value (toScalar (revenue t) - toScalar (opex t), t)

assets :: Year -> Value Double
assets 2014 = Value (60.0, 2014)
assets t = Value (toScalar (assets (t - 1)) + toScalar (grossMargin t), t)

discount :: Double -> Year -> Value Double -> Value Double
discount r y0 (Value (v, y)) = Value (v / (1 + r) ^^ (y - y0), y0)

instance Functor Value where
    fmap f (Value (x, y)) = Value (f x, y)
