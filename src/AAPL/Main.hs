module AAPL.Main
    ( module AAPL.Main
    ) where

import AAPL.Types
import AAPL.RD
import AAPL.Leases
import AAPL.Options

terminalYear :: Year
terminalYear = 2027

revenue :: Val
revenue 2016    = 218118
revenue t
    | t >  2016 = (1 + revenueGrowthRate t) * revenue (t-1)
    | otherwise = throwError "invalid year"

revenueGrowthRate :: Val
revenueGrowthRate t
    | 2017 <= t && t <= 2021 = 0.015
    | 2022 <= t              = linear revenueGrowthRate 2021 2026 0.01 t

operatingMargin :: Val
operatingMargin 2016 = operatingIncome 2016 / revenue 2016
operatingMargin    t = linear operatingMargin 2016 2026 0.25 t

operatingIncome :: Val
operatingIncome 2016 = 59212 + leaseCommittments 2016 - depreciationOperatingLease 2016 +
                                    rdExpenses 2016 - rdAmortization 2016
operatingIncome t    = revenue t * operatingMargin t

taxRate :: Val
taxRate t
    | 2016 <= t && t <= 2021 = 0.2601
    | 2022 <= t              = linear taxRate 2021 2026 0.3 t

marginalTaxRate :: Val
marginalTaxRate 2016 = 0.3

noplat :: Val
noplat t = (1 - taxRate t) * operatingIncome t

salesToCapitalRatio :: Val
salesToCapitalRatio _ = 1.6

reinvestment :: Val
reinvestment t
    | t < 2027  = (revenue t - revenue (t-1)) / salesToCapitalRatio t
    | otherwise = revenueGrowthRate t * noplat t / wacc t

fcff :: Val
fcff t = noplat t - reinvestment t

terminalValue :: Val
terminalValue t
    | t == terminalYear = fcff t * (1 + wacc t) / (wacc t - revenueGrowthRate t)
    | otherwise         = 0

cashFlow :: Val
cashFlow t
    | t <= 2016           = 0
    | t <    terminalYear = fcff t
    | t ==   terminalYear = terminalValue t
    | otherwise           = throwError "invalid year"

wacc :: Val
wacc t
    | 2017 <= t && t <= 2021 = 0.0908
    | 2022 <= t              = linear wacc 2021 2026 0.0697 t

valueOperatingAssets :: Val
valueOperatingAssets = npv wacc cashFlow

debt :: Val
debt 2016 = 87549 + debtValueOfLeases 2016

cash :: Val
cash 2016 = 245090 - 150000 * (marginalTaxRate 2016 - 0.1)

distressProceeds :: Val
distressProceeds _ = 0.5


equity :: Val
equity t = valueOperatingAssets t - debt t + cash t

equityInCommonShares :: Val
equityInCommonShares t = equity t - outstandingOptionsValue t

value :: Val
value t = equityInCommonShares t / pure shares
