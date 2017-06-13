module AAPL.Options
    ( optionValue, adjustedOptionValue, outstandingOptionsValue, shares
    ) where

import Valuation

price :: Double
price = 128

strike :: Double
strike = 15.48

expiry :: Double
expiry = 3.7

sigma :: Double
sigma = 0.3

rate :: Double
rate = 0.0247

n :: Double
n = 1.1

shares :: Double
shares = 5336.17

optionValue :: Double
optionValue = blackScholes sigma rate expiry price strike

adjustedOptionValue :: Double
adjustedOptionValue = calc (-100) price
  where
    calc prev s =
        let c = blackScholes sigma rate expiry s strike
        in if abs (c - prev) < 0.0000001
               then c
               else let adj_s = (shares * price + n * c) / (n + shares)
                    in calc c adj_s

outstandingOptionsValue :: TimeValue () Double
outstandingOptionsValue 2016 = return $ adjustedOptionValue * n
