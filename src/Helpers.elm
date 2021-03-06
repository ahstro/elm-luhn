module Helpers exposing
    ( checksum
    , doubleAtEvenIndices
    , keepUnder10
    , notEmpty
    , toListOfInt
    )


toListOfInt : List String -> Result String (List Int)
toListOfInt strings =
    strings
        |> List.reverse
        |> List.foldl
            (\numberString acc ->
                case String.toInt numberString of
                    Just num ->
                        Result.map ((::) num) acc

                    Nothing ->
                        Err <| "Not a number: " ++ numberString
            )
            (Ok [])


doubleAtEvenIndices : List Int -> List Int
doubleAtEvenIndices =
    List.indexedMap
        (\index num ->
            if modBy 2 index == 0 then
                num * 2

            else
                num
        )


keepUnder10 : Int -> Int
keepUnder10 num =
    if num >= 10 then
        num - 9

    else
        num


checksum : Int -> Int -> Result String Bool
checksum checkDigit sum =
    if modBy 10 (sum + checkDigit) == 0 then
        Ok True

    else
        Err "Incorrect checksum"


notEmpty : List a -> Result String (List a)
notEmpty nums =
    if List.isEmpty nums then
        Err "Input string is only check digit"

    else
        Ok nums
