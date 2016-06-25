module HW01Tests(
                 hw01Tests
                 )
       where

import Test.HUnit (Assertion,(@?=))
import Test.Framework
import Test.Framework.Providers.HUnit

import HW01

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Assertion
testLastDigit (n, d) = lastDigit n @?= d

testDropLastDigit :: (Integer, Integer) -> Assertion
testDropLastDigit (n, d) = dropLastDigit n @?= d

ex1LastDigitTests :: [Assertion]
ex1LastDigitTests = map testLastDigit [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]

ex1DropLastDigitTests :: [Assertion]
ex1DropLastDigitTests = map testDropLastDigit [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]

-- Exercise 2 -----------------------------------------

testLuhn :: (Integer, Bool) -> Assertion
testLuhn (n,b) = luhn n @?= b

ex5LuhnTests :: [Assertion]
ex5LuhnTests = map testLuhn [(5594589764218858, True), (1234567898965432, False)]


tests :: [(String, [Assertion])]
tests = [ ("testLastDigits", ex1LastDigitTests)
        , ("testDropLastDigit", ex1DropLastDigitTests)
        , ("testLuhn", ex5LuhnTests)
        ]

hw01Tests :: [Test]
hw01Tests = concat $ map (\(name, ts) ->
                           zipWith (\t n -> testCase (name ++ show n) t) ts ([1..] :: [Integer])
                         ) tests
