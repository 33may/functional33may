--------------- Week1/Pythagoras
--------------- Anton Novokhatskiy, Adrián Pacera

-- commands to test:    import Week1.Pyth exposing (..)
--                      allTests || allTestsPassed

module Week1.Pyth exposing (..)

-- Define a function 'sqr' that takes an integer and returns its square.
sqr: Int -> Int
sqr n =
  n * n

-- Define a function 'isTriple' that checks if three integers form a Pythagorean triple.
isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
  -- Check if the sum of squares of any two numbers equals the square of the third number.
  if ((sqr a) + (sqr b) == (sqr c)) ||  ((sqr a) + (sqr c) == (sqr b)) ||  ((sqr b) + (sqr c) == (sqr a)) then
    True
  else
    False


--Using the Pythagoras Theorem we could define leg1, leg2 and hyp of the right-angle triangle with formulas(x^2 − y^2, 2yx, x^2 + y^2)

leg1: Int -> Int -> Int
leg1 a b =
  abs ( (sqr a) - (sqr b) )

leg2: Int -> Int -> Int
leg2 a b =
  2 * a * b

hyp: Int -> Int -> Int
hyp a b =
  (sqr a) + (sqr b)

-- Define a function 'pythTriple' that takes a tuple of two integers and returns a tuple representing a Pythagorean triple.
pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple (a, b) =
  -- Calculate the Pythagorean triple using the leg1, leg2, and hyp functions.
  (leg1 a b, leg2 a b, hyp a b)

-- Define a function 'isTripleTuple' that checks if a tuple of three integers is a Pythagorean triple.
isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) =
  -- Use the 'isTriple' function to check the tuple.
  isTriple a b c

------------------------ Test
testSqr1: Bool
testSqr1 = (sqr 3 == 9)

testSqr2: Bool
testSqr2 = (sqr 0 == 0)

testIsTriple1: Bool
testIsTriple1 = (isTriple 3 4 5 == True)

testIsTriple2: Bool
testIsTriple2 = (isTriple 5 9 12 == False)

testLeg1: Bool
testLeg1 = (leg1 3 4 == 7)

testLeg2: Bool
testLeg2 = (leg2 3 4 == 24)

testHyp: Bool
testHyp = (hyp 3 4 == 25)

testPythTriple: Bool
testPythTriple = (pythTriple (3, 4) == (7, 24, 25))

testIsTripleTuple: Bool
testIsTripleTuple = (isTripleTuple (3, 4, 5) == True)

-- Aggregated test results
allTests: List Bool
allTests =
    [ testSqr1
    , testSqr2
    , testIsTriple1
    , testIsTriple2
    , testLeg1
    , testLeg2
    , testHyp
    , testPythTriple
    , testIsTripleTuple
    ]

-- Function to run all tests and check if all passed
allTestsPassed : Bool
allTestsPassed = List.all identity allTests

