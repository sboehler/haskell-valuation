module AAPL
    ( module AAPL
    ) where

import Control.Monad.Except
import Control.Monad.Reader
import Valuation

type Val = Value () Double

revenue :: Val
revenue = asks year >>= f
    where
        f y | y == 2016 = return 218118
            | y >  2016 = (1 + revenueGrowthRate) * previous revenue
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
operatingIncome = revenue * operatingMargin

taxRate :: Val
taxRate = interpolate [
    (2017, 0.2601),
    (2021, 0.2601),
    (2026, 0.3   )]

noplat :: Val
noplat = (1 - taxRate) * operatingIncome

salesToCapitalRatio :: Val
salesToCapitalRatio = return 1.6

reinvestment :: Val
reinvestment = do
    y <- asks year
    if y < 2027
        then (revenue - previous revenue) / salesToCapitalRatio
        else revenueGrowthRate / wacc * noplat

fcff :: Val
fcff = noplat - reinvestment

terminalValue :: Val
terminalValue = do
    y <- asks year
    ty <- asks terminalYear
    if y == ty
        then fcff * (1 + wacc) / (wacc - revenueGrowthRate)
        else return 0

cashFlow :: Val
cashFlow = do
    y <- asks year
    ty <- asks terminalYear
    let
        cashFlow'
            | y <= 2016 = return 0
            | y <   ty  = fcff
            | y ==  ty  = terminalValue
            | otherwise = throwError "invalid year"
        in cashFlow'

wacc :: Val
wacc = interpolate [
    (2017, 0.0908),
    (2021, 0.0908),
    (2026, 0.0697)]


debt :: Val
debt = return 87549

cash :: Val
cash = return 245090 + 150000
