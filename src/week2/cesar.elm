module Week2.Cesar exposing (..)

import Char exposing (fromCode, toCode, isLower, isUpper)

-- Encodes a single character with a given offset.
encode: Int -> Char -> Char
encode shift letter =
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
        fromCode (modBy alphaLength (toCode letter - base + shift) + base)
    else
        -- Non-alphabetical characters are unchanged.
        letter

-- Decodes a single character with a given offset.
decode: Int -> Char -> Char
decode shift letter =
  -- Decoding is simply encoding with a negative offset.
  encode (-shift) letter

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
            encode 2 x :: normalizeList xs

