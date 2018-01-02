module HelpersTests exposing (suite)

import Expect
import Helpers
import Result.Extra exposing (isErr, isOk)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Helpers"
        [ describe "doubleAtEvenIndices"
            [ test "Doubles the number at even indeices" <|
                \_ ->
                    Expect.equal
                        [ 0, 1, 4, 3, 8, 5, 12 ]
                        (Helpers.doubleAtEvenIndices [ 0, 1, 2, 3, 4, 5, 6 ])
            ]
        , describe "keepUnder10"
            [ test "Subtracts 9 if number is 10 or over" <|
                \_ ->
                    Expect.equal
                        [ 1, 2, 3 ]
                        (List.map Helpers.keepUnder10 [ 10, 11, 12 ])
            , test "Does nothing if number is under 10" <|
                \_ ->
                    Expect.equal
                        [ 1, 2, 3, 9 ]
                        (List.map Helpers.keepUnder10 [ 1, 2, 3, 9 ])
            ]
        , describe "toListOfInt"
            [ test "Turns a list of valid Int strings into a `Ok (List Int)`" <|
                \_ ->
                    Expect.true
                        "true"
                        (isOk (Helpers.toListOfInt [ "1", "2", "3" ]))
            , test "Turns a list of invalid Int strings into a `Err String`" <|
                \_ ->
                    Expect.true
                        "true"
                        (isErr (Helpers.toListOfInt [ "1.12", "2", "3" ]))
            , test "Keeps the order of valid lists" <|
                \_ ->
                    Expect.equal
                        (Helpers.toListOfInt [ "1", "2", "3", "4", "5" ])
                        (Ok [ 1, 2, 3, 4, 5 ])
            ]
        , describe "checksum"
            [ test "Ok if checksum is good with non-zero checkdigit" <|
                \_ ->
                    Expect.true
                        "Expected the checksum to be good"
                        (isOk (Helpers.checksum 1 9))
            , test "Ok if checksum is good with zero checkdigit" <|
                \_ ->
                    Expect.true
                        "Expected the checksum to be good"
                        (isOk (Helpers.checksum 0 20))
            , test "Err if checksum is bad" <|
                \_ ->
                    Expect.true
                        "Expected the checksum to be bad"
                        (isErr (Helpers.checksum 0 19))
            ]
        , describe "notEmpty"
            [ test "Err if list is empty" <|
                \_ ->
                    Expect.true
                        "Expected Err because list is empty"
                        (isErr (Helpers.notEmpty []))
            , test "Ok if list is not empty" <|
                \_ ->
                    Expect.true
                        "Expected Ok because list is not empty"
                        (isOk (Helpers.notEmpty [ 1 ]))
            ]
        ]
