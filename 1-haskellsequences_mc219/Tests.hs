module Tests where

import IC.TestSuite

import Sequences


maxOf2TestCases
  = [ (1, 2) ==> 2,
      (2, 1) ==> 2,
      (2, 2) ==> 2,
      (0, 6) ==> 6
    ]

maxOf3TestCases
  = [ (1, 2, 3) ==> 3,
      (2, 1, 3) ==> 3,
      (3, 3, 3) ==> 3,
      (1, 1, 9) ==> 9
    ]

isADigitTestCases
  = [ ('1') ==> True,
      ('9') ==> True,
      ('A') ==> False,
      ('!') ==> False
    ]

isAlphaTestCases
  = [ ('1') ==> False,
      ('A') ==> True,
      ('a') ==> True,
      ('!') ==> False
    ]

digitToIntTestCases
  = [ ('0') ==> 0,
      ('9') ==> 9,
      ('7') ==> 7,
      ('5') ==> 5
    ]

toUpperTestCases
  = [ ('a') ==> 'A',
      ('z') ==> 'Z',
      ('A') ==> 'A',
      ('z') ==> 'Z'
    ]

--
-- Sequences and series
--

arithmeticSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 100.0,
      (10.0, 0.0, 10) ==> 10.0
    ]

geometricSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 0.0
    ]

arithmeticSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 550.0,
      (10.0, 0.0, 10) ==> 110.0
    ]

geometricSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 10.0
    ]

-- You can add your own test cases above

sequencesTestCases
  = [ TestCase  "maxOf2"      (uncurry maxOf2)
                              maxOf2TestCases
     , TestCase "maxOf3"      (uncurry3 maxOf3)
                              maxOf3TestCases
     , TestCase "isADigit"    (isADigit)
                              isADigitTestCases
     , TestCase "isAlpha"     (isAlpha)
                              isAlphaTestCases
     , TestCase "digitToInt"  (digitToInt)
                              digitToIntTestCases
     , TestCase "toUpper"     (toUpper)
                              toUpperTestCases
     , TestCase "arithmeticSeq" (uncurry3 arithmeticSeq)
                                arithmeticSeqTestCases
     , TestCase "geometricSeq"  (uncurry3 geometricSeq)
                                geometricSeqTestCases
     , TestCase "arithmeticSeries"  (uncurry3 arithmeticSeries)
                                    arithmeticSeriesTestCases
     , TestCase "geometricSeries"   (uncurry3 geometricSeries)
                                    geometricSeriesTestCases
    ]

runTests = mapM_ goTest sequencesTestCases

main = runTests
