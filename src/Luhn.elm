module Luhn exposing (isValid, validate)

{-| This library allows you to validate data using the Luhn Algorithm.
The algorithm is often used to guard against simple mistakes when
a user enters something like a credit card or social security number.


# Validation

@docs validate, isValid

-}

import Helpers
    exposing
        ( checksum
        , doubleAtEvenIndices
        , keepUnder10
        , notEmpty
        , toListOfInt
        )


{-| Accepts a string and returns a `Result String String` indicating
whether the string was a valid number according to the Luhn Algorithm.

    validate "49927398716" == Ok "49927398716"

-}
validate : String -> Result String String
validate numberString =
    let
        numbers =
            numberString
                |> String.split ""
                |> List.reverse
                |> toListOfInt

        checkDigit =
            numbers
                |> Result.andThen
                    (Result.fromMaybe "Input string is empty" << List.head)
                |> Result.withDefault 0
    in
    numbers
        |> Result.andThen (Result.fromMaybe "Input string is empty" << List.tail)
        |> Result.andThen notEmpty
        |> Result.map doubleAtEvenIndices
        |> Result.map (List.map keepUnder10)
        |> Result.map List.sum
        |> Result.andThen (checksum checkDigit)
        |> Result.map (\_ -> numberString)


{-| Same as `validate` but returns a `Bool` instead, for easy use in
`if`-expressions.

    isValid "49927398716" == True

-}
isValid : String -> Bool
isValid numberString =
    case validate numberString of
        Ok _ ->
            True

        Err _ ->
            False
