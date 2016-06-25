import HW01Tests
import Data.Monoid

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main = defaultMainWithOpts
       hw01Tests
       mempty
