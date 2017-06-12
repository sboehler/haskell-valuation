module AAPL
    ( module AAPL
    ) where

import Control.Monad.Except
import Valuation

type Val = TimeValue () Double

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
    | 2022 <= t              = linearChange revenueGrowthRate 2021 2026 0.01 t

operatingMargin :: Val
operatingMargin 2016 = operatingIncome 2016 / revenue 2016
operatingMargin    t = linearChange operatingMargin 2016 2026 0.25 t

operatingIncome :: Val
operatingIncome 2016 = pure 59212 + (leaseCommittments 2016 - depreciationOperatingLease 2016) +
                                    (rdExpenses 2016 - rdAmortization 2016)
operatingIncome t    = revenue t * operatingMargin t

taxRate :: Val
taxRate t
    | 2016 <= t && t <= 2021 = 0.2601
    | 2022 <= t              = linearChange taxRate 2021 2026 0.3 t

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
    | 2022 <= t              = linearChange wacc 2021 2026 0.0697 t

valueOperatingAssets :: Val
valueOperatingAssets = npv wacc cashFlow

debt :: Val
debt 2016 = 87549 + debtValueOfLeases 2016

cash :: Val
cash 2016 = 245090 - 150000 * (marginalTaxRate 2016 - 0.1)

distressProceeds :: Val
distressProceeds _ = 0.5

leaseCommittments :: Val
leaseCommittments 2016       = 939
leaseCommittments 2017       = 929
leaseCommittments 2018       = 919
leaseCommittments 2019       = 915
leaseCommittments 2020       = 889
leaseCommittments 2021       = 836
leaseCommittments t
    | 2022 <= t && t <= 2024 = 3139 / 3
leaseCommittments _          = throwError "invalid year"

depreciationOperatingLease :: Val
depreciationOperatingLease 2016 = debtValueOfLeases 2016 / 8

preTaxCostOfDebt :: Val
preTaxCostOfDebt _ = 0.0327

debtValueOfLeases :: Val
debtValueOfLeases 2016 = npv preTaxCostOfDebt leaseCommittments 2017 / (1 + preTaxCostOfDebt 2017)

-- R&D expenses

rdExpenses :: Val
rdExpenses 2013 =  4475
rdExpenses 2014 =  6041
rdExpenses 2015 =  8067
rdExpenses 2016 = 10512

rdAmortization :: Val
rdAmortization 2016 = (rdExpenses 2013 + rdExpenses 2014 + rdExpenses 2015) / 3

rdAsset :: Val
rdAsset 2016 = (rdExpenses 2014 + rdExpenses 2015 * 2 + rdExpenses 2016 * 3) / 3

equity :: Val
equity t = valueOperatingAssets t - debt t + cash t
