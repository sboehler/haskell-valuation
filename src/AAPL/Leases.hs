module AAPL.Leases (debtValueOfLeases, leaseCommittments, depreciationOperatingLease) where

import Valuation
import Control.Monad.Except
type Val = TimeValue () Double

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

