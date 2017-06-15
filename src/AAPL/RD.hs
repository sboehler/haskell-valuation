
module AAPL.RD (rdExpenses, rdAmortization) where

import AAPL.Types

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
