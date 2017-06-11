module AAPL
    ( module AAPL
    ) where

import Control.Monad.Except
import Valuation

type Val = TimeValue () Double

terminalYear :: Year
terminalYear = 2027

revenue :: Val
revenue 2016    = return 218118
revenue t
    | t == 2016 = return 218118
    | t >  2016 = (1 + revenueGrowthRate t) * revenue (t-1)
    | otherwise = throwError "invalid year"

revenueGrowthRate :: Val
revenueGrowthRate = interpolate [
    (2017, 0.015),
    (2021, 0.015),
    (2026, 0.01 )]

operatingMargin :: Val
operatingMargin = interpolate [
    (2016, 0.2918),
    (2026, 0.25  )]

operatingIncome :: Val
operatingIncome t = revenue t * operatingMargin t

taxRate :: Val
taxRate = interpolate [
    (2017, 0.2601),
    (2021, 0.2601),
    (2026, 0.3   )]

noplat :: Val
noplat t = (1 - taxRate t) * operatingIncome t

salesToCapitalRatio :: Val
salesToCapitalRatio = return 1.6

reinvestment :: Val
reinvestment t
    | t < 2027  = (revenue t - revenue (t-1)) / salesToCapitalRatio t
    | otherwise = revenueGrowthRate t / wacc t * noplat t

fcff :: Val
fcff t = noplat t - reinvestment t

terminalValue :: Val
terminalValue t
    | t == terminalYear = fcff t * (1 + wacc t) / (wacc t - revenueGrowthRate t)
    | otherwise         = return 0

cashFlow :: Val
cashFlow t
    | t <= 2016           = return 0
    | t <    terminalYear = fcff t
    | t ==   terminalYear = terminalValue t
    | otherwise           = throwError "invalid year"

wacc :: Val
wacc = interpolate [
    (2017, 0.0908),
    (2021, 0.0908),
    (2026, 0.0697)]


debt :: Val
debt 2016 = return 87549

cash :: Val
cash 2016 = return $ 245090 + 150000
