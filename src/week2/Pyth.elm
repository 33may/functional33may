module Week2.Pyth exposing (..)


--------------- Week1/Pythagoras
--------------- Anton Novokhatskiy, Adrián Pacera

-- commands to test:    import Week1.Pyth exposing (..)
--                      allTests || allTestsPassed


sqr: Int -> Int
sqr n =
  n * n

-- Define a function 'isTriple' that checks if three integers form a Pythagorean triple.
isTriple: Int -> Int -> Int -> Bool
isTriple a b c =
  a > 0 && b > 0 && c > 0 &&
    ((sqr a) + (sqr b) == (sqr c) ||
     (sqr a) + (sqr c) == (sqr b) ||
     (sqr b) + (sqr c) == (sqr a))

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
    if a == 0 || b == 0 then
        (0, 0, 0) -- Return a placeholder or signal value indicating invalid input.
    else
        let
            n1 = abs a
            n2 = abs b
        in
        -- Calculate the Pythagorean triple using the leg1, leg2, and hyp functions.
        (leg1 n1 n2, leg2 n1 n2, hyp n1 n2)

-- Define a function 'isTripleTuple' that checks if a tuple of three integers is a Pythagorean triple.
isTripleTuple: (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) =
  isTriple a b c

pythTriplesMap: List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list =
    List.map pythTriple list

pythTriplesRec: List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of
        [] ->
            []
        x :: xs ->
            pythTriple x :: pythTriplesRec xs

arePythTriplesFilter: List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list =
    List.filter isTripleTuple list


arePythTriplesRec: List (Int, Int, Int) -> List (Int, Int,Int)
arePythTriplesRec list =
    case list of
        [] ->
            []
        x :: xs ->
            if isTripleTuple x then
                x :: arePythTriplesRec xs
            else
                arePythTriplesRec xs


------------------------ Test
testSqr1: Bool
testSqr1 = (sqr 3 == 9)

testSqr2: Bool
testSqr2 = (sqr 0 == 0)

testSqr3: Bool
testSqr3 = (sqr -4 == 16)

testIsTriple1: Bool
testIsTriple1 = (isTriple 3 4 5 == True)

testIsTriple2: Bool
testIsTriple2 = (isTriple 5 9 12 == False)

testIsTriple3: Bool
testIsTriple3 = (isTriple -3 4 5 == False)

testIsTriple4: Bool
testIsTriple4 = (isTriple 0 0 0 == False)

testIsTriple5: Bool
testIsTriple5 = (isTriple 0 3 4 == False)

testLeg1: Bool
testLeg1 = (leg1 3 4 == 7)

testLeg1Negative: Bool
testLeg1Negative = (leg1 -3 4 == 7)

testLeg2: Bool
testLeg2 = (leg2 3 4 == 24)

testLeg2Negative: Bool
testLeg2Negative = (leg2 -3 4 == -24)

testHyp: Bool
testHyp = (hyp 3 4 == 25)

testHypNegative: Bool
testHypNegative = (hyp -3 4 == 25)

testPythTriple: Bool
testPythTriple = (pythTriple (3, 4) == (7, 24, 25))

testPythTripleNegative: Bool
testPythTripleNegative = (pythTriple (-3, 4) == (7, 24, 25))

testPythTripleZero: Bool
testPythTripleZero = (pythTriple (0, 5) == (0, 0, 0))

testIsTripleTuple: Bool
testIsTripleTuple = (isTripleTuple (3, 4, 5) == True)

-- Testing isTripleTuple with zero
testIsTripleTupleZero: Bool
testIsTripleTupleZero = (isTripleTuple (0, 0, 0) == False)

testPythTriplesMap: Bool
testPythTriplesMap =
    pythTriplesMap [(3, 4), (5, 12)] == [(7, 24, 25), (119, 120, 169)]

testPythTriplesRec: Bool
testPythTriplesRec =
    pythTriplesRec [(3, 4), (5, 12)] == [(7, 24, 25), (119, 120, 169)]


testArePythTriplesFilter: Bool
testArePythTriplesFilter =
    arePythTriplesFilter [(3, 4, 5), (5, 9, 12), (8, 15, 17)] == [(3, 4, 5), (8, 15, 17)]

testArePythTriplesRec: Bool
testArePythTriplesRec =
    arePythTriplesRec [(3, 4, 5), (5, 9, 12), (8, 15, 17)] == [(3, 4, 5), (8, 15, 17)]


-- Aggregated test results including new tests
allTests: List Bool
allTests =
  [ testSqr1, testSqr2, testSqr3
  , testIsTriple1, testIsTriple2, testIsTriple3, testIsTriple4, testIsTriple5
  , testLeg1, testLeg1Negative, testLeg2, testLeg2Negative
  , testHyp, testHypNegative
  , testPythTriple, testPythTripleNegative, testPythTripleZero
  , testIsTripleTuple, testIsTripleTupleZero,
  testPythTriplesMap, testPythTriplesRec,
  testArePythTriplesFilter, testArePythTriplesRec
  ]

-- Function to run all tests and check if all passed
allTestsPassed : Bool
allTestsPassed = List.all identity allTests