module MSFT where

import Control.Monad
import Valuation

type Val = TimeValue () Double

productRevenue :: Val
productRevenue 2014 = return 72948
productRevenue 2015 = return 75956
productRevenue 2016 = return 61502

serviceRevenue :: Val
serviceRevenue 2014 = return 13885
serviceRevenue 2015 = return 17624
serviceRevenue 2016 = return 23818

revenue :: Val
revenue t = (+) <$> productRevenue t <*> serviceRevenue t

costOfProductRevenue :: Val
costOfProductRevenue 2014 = return (-16681)
costOfProductRevenue 2015 = return (-21410)
costOfProductRevenue 2016 = return (-17880)

costOfServiceRevenue :: Val
costOfServiceRevenue 2014 = return (-10397)
costOfServiceRevenue 2015 = return (-11628)
costOfServiceRevenue 2016 = return (-14900)

costOfRevenue :: Val
costOfRevenue t = (+) <$> costOfProductRevenue t <*> costOfServiceRevenue t

grossMargin :: Val
grossMargin t = (+) <$> revenue t <*> costOfRevenue t

researchAndDevelopment :: Val
researchAndDevelopment 2014 = return (-11381)
researchAndDevelopment 2015 = return (-12046)
researchAndDevelopment 2016 = return (-11988)

salesAndMarketing :: Val
salesAndMarketing 2014 = return (-15811)
salesAndMarketing 2015 = return (-15713)
salesAndMarketing 2016 = return (-14697)

generalAndAdministrative :: Val
generalAndAdministrative 2014 = return (-4677)
generalAndAdministrative 2015 = return (-4611)
generalAndAdministrative 2016 = return (-4563)

impairmentIntegrationRestructuring :: Val
impairmentIntegrationRestructuring 2014 = return (-127)
impairmentIntegrationRestructuring 2015 = return (-10011)
impairmentIntegrationRestructuring 2016 = return (-1110)

operatingIncome :: Val
operatingIncome t =
    sum <$>
    mapM
        ($ t)
        [grossMargin, researchAndDevelopment, salesAndMarketing, generalAndAdministrative, impairmentIntegrationRestructuring]

otherIncomeNet :: Val
otherIncomeNet 2014 = return 61
otherIncomeNet 2015 = return 346
otherIncomeNet 2016 = return (-431)

incomeBeforeIncomeTaxes :: Val
incomeBeforeIncomeTaxes t = liftM2 (+) (operatingIncome t) (otherIncomeNet t)

provisionForIncomeTaxes :: Val
provisionForIncomeTaxes 2014 = return (-5746)
provisionForIncomeTaxes 2015 = return (-6314)
provisionForIncomeTaxes 2016 = return (-2953)

netIncome :: Val
netIncome t = liftM2 (+) (incomeBeforeIncomeTaxes t) (provisionForIncomeTaxes t)
