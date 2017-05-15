module Val where

type Year = Int

newtype Value a = V
    { eval :: Year -> a
    }

instance Functor Value where
    fmap f (V a) = V (f . a)

instance Applicative Value where
    pure x = V $ const x
    f <*> x = V (\t -> eval f t (eval x t))

instance Monad Value where
    return = pure
    m >>= f = V (\t -> eval (f (eval m t)) t)

year :: Value Int
year = V id

discount :: Double -> Year -> Value Double -> Value Double
discount rate y v =
    V
        (\t ->
             let factor = 1 / (1 + rate)
                 delta = t - y
                 f = (* factor ^^ delta)
             in eval (fmap f v) t)

revenueGrowthRate :: Value Double
revenueGrowthRate = V $ const 0.05

revenue :: Value Double
revenue = do
    y <- year
    case y of
        2014 -> return 10.0
        2015 -> return 11.0
        _
            | y <= 2020 -> do
                r <- previous revenue
                g <- revenueGrowthRate
                return (r * (1 + g))
            | otherwise -> return (eval revenue 2020)

opex :: Value Double
opex = do
    y <- year
    case y of
        2013 -> return 19.0
        _ -> do
            r <- revenue
            return (0.9 * r)

grossMargin :: Value Double
grossMargin = do
    r <- revenue
    o <- opex
    return (r - o)

previous :: Value a -> Value a
previous v = V (\t -> eval v (t - 1))

assets :: Value Double
assets =
    let v 2014 = 60.0
        v t = eval assets (t - 1) + eval grossMargin t
    in V v

test :: Value Double
test = do
    r <- revenue
    a <- assets
    return (a + r)
