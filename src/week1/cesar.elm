
--------------- Week1/cesar
--------------- Anton Novokhatskiy, Adrián Pacera

-- commands to test:    import Week1.Cesar exposing (..)
--                      allTests


module Week1.Cesar exposing (..)

import Char exposing (fromCode, toCode, isLower, isUpper)

-- Encodes a single character with a given offset.
encode: Int -> Char -> Char
encode offset letter =
  let
        -- Determine the base ASCII code for 'a' or 'A' depending on the case of the character.
        base = if isLower letter then toCode 'a' else toCode 'A'
        alphaLength = 26
    in
    -- Only encode alphabetical characters.
    if isLower letter || isUpper letter then
            -- Calculate the new character code with wrapping.
            -- (toCode char - base + offset) calculates the new position relative to the base.
            -- modBy alphaLength ensures the result is in matching alphabet range (lower or upper).
            -- + base shifts the character back to the correct ASCII range.
        fromCode (modBy alphaLength (toCode letter - base + offset) + base)
    else
        -- Non-alphabetical characters are unchanged.
        letter

-- Decodes a single character with a given offset.
decode: Int -> Char -> Char
decode offset letter =
  -- Decoding is simply encoding with a negative offset.
  encode (-offset) letter

-- Decodes a string with a given offset.
encrypt : Int -> String -> String
encrypt offset text =
    let
        list = String.toList text
    in
        String.fromList (encryptList offset list)

encryptList : Int -> List Char -> List Char
encryptList offset list =
    case list of
        [] ->
            []
        x :: xs ->
            encode offset x :: encryptList offset xs


decrypt: Int -> String -> String
decrypt offset text =
    let
        list = String.toList text
    in
    String.fromList (decryptList offset list)

decryptList : Int -> List Char -> List Char
decryptList offset list =
    encryptList -offset list

normalize : String -> String
normalize s =
    let
        list = String.toList s
    in
    String.fromList (normalizeList list)

normalizeList : List Char -> List Char
normalizeList list =
    case list of
        [] ->
            []
        x :: xs ->
            if x == ' ' then
                normalizeList xs
            else
                x :: normalizeList xs


-- Test functions for encoding
testEncode1: Bool
testEncode1 = (encode 5 'x' == 'c')

testEncode2: Bool
testEncode2 = (encode 7 'T' == 'A')

testEncode3: Bool
testEncode3 = (encode 56 '&'  == '&')

--Test functions for decoding
testDecode1:Bool
testDecode1 = (decode 5 'c' == 'x')

testDecode3: Bool
testDecode3 = (decode 23 '#'  == '#')

testDecode4: Bool
testDecode4 = (decode 0 'a'  == 'a')

-- Test functions for normalize
testNormalize1: Bool
testNormalize1 = (normalize "h e l l o" == "hello")

testNormalize2: Bool
testNormalize2 = (normalize "  Elm  " == "Elm")

testNormalize4: Bool
testNormalize4 = (normalize "" == "")

--Test functions for encrypt and decrypt
testEncrypt1: Bool
testEncrypt1 = (encrypt 5 "hello" == "mjqqt")

testEncrypt2: Bool
testEncrypt2 = (encrypt 1 "xyz" == "yza")

testDecrypt1: Bool
testDecrypt1 = (decrypt 5 "mjqqt" == "hello")

testDecrypt2: Bool
testDecrypt2 = (decrypt 1 "yza" == "xyz")


--Tests results
allTestsEncode: List Bool
allTestsEncode = [testEncode1, testEncode2, testEncode3]

allTestsDecode: List Bool
allTestsDecode = [testDecode1, testDecode3, testDecode4]

allTestsNormalize: List Bool
allTestsNormalize = [testNormalize1, testNormalize2, testNormalize4]

allTestsEncrypt: List Bool
allTestsEncrypt = [testEncrypt1, testEncrypt2]

allTestsDecrypt: List Bool
allTestsDecrypt = [testDecrypt1, testDecrypt2]

allTests: List (List Bool)
allTests = [allTestsEncode, allTestsDecode, allTestsNormalize, allTestsEncrypt, allTestsDecrypt]
