module AAPL (module AAPL) where

import Control.Monad
import Valuation

import Control.Monad.Trans

type Val = TimeValue () Double

revenue :: Val
revenue 2016 = return 218118
revenue t = do
    g <- revenueGrowthRate t
    r <- previous revenue t
    return $ (1 + g) * r

terminalYear :: Year
terminalYear = 2030

revenueGrowthRate :: Val
revenueGrowthRate = interpolate [(2017, 0.015), (2021, 0.015), (2026, 0.01), (terminalYear, 0.01)]

operatingMargin :: Val
operatingMargin = interpolate [(2016, 0.2918), (2026, 0.25), (terminalYear, 0.25)]

operatingIncome :: Val
operatingIncome t = do
    rev <- revenue t
    op <- operatingMargin t
    return $ rev * op

taxRate :: Val
taxRate = interpolate [(2017, 0.2601), (2021, 0.2601), (2026, 0.3), (terminalYear, 0.3)]

noplat :: Val
noplat t = do
    o <- operatingIncome t
    tax <- taxRate t
    return $ o * (1 - tax)

salesToCapitalRatio :: Double
salesToCapitalRatio = 1.6

reinvestment :: Val
reinvestment t
    | t < 2027 = do
        rev' <- previous revenue t
        rev <- revenue t
        return $ (rev - rev') / salesToCapitalRatio
    | t >= 2027 = do
        g <- revenueGrowthRate t
        w <- wacc t
        cf <- noplat t
        return $ (g / w) * cf

fcff :: Val
fcff t = do
    nopl <- noplat t
    reinv <- reinvestment t
    return $ nopl - reinv

terminalValue :: Val
terminalValue t = do
    cf <- next fcff t
    w <- next wacc t
    g <- next revenueGrowthRate t
    return $ cf / (w - g)

cashFlow :: Val
cashFlow t
    | 2017 <= t && t < terminalYear - 1 = fcff t
    | t == terminalYear - 1 = (+) <$> fcff t <*> terminalValue t
    | otherwise = lift mzero

wacc :: Val
wacc = interpolate [(2017, 0.0908), (2021, 0.0908), (2026, 0.0697), (terminalYear, 0.0697)]
