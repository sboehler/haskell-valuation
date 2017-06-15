module AAPL.Leases (module AAPL.Leases) where

import AAPL.Types

leaseCommittments :: Val
leaseCommittments 2016 = 939
leaseCommittments 2017 = 929
leaseCommittments 2018 = 919
leaseCommittments 2019 = 915
leaseCommittments 2020 = 889
leaseCommittments 2021 = 836
-- spread over 3 years
leaseCommittments 2022 = 3139 / 3
leaseCommittments 2023 = 3139 / 3
leaseCommittments 2024 = 3139 / 3
leaseCommittments _ = throwError "invalid year"

preTaxCostOfDebt :: Val
preTaxCostOfDebt _ = 0.0327

debtValueOfLeases :: Val
debtValueOfLeases 2016 = npv preTaxCostOfDebt leaseCommittments 2016 - leaseCommittments 2016

depreciationOperatingLease :: Val
depreciationOperatingLease 2016 = debtValueOfLeases 2016 / 8
