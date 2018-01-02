module LuhnTests exposing (suite)

import Expect
import Luhn
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Luhn"
        [ describe "validate"
            [ describe "Valid"
                [ test "Valid numbers" <|
                    \_ ->
                        let
                            validNumbers =
                                [ "9405237190"
                                , "49927398716"
                                , "1234567812345670"
                                ]
                        in
                        Expect.equal
                            (List.map Ok validNumbers)
                            (List.map Luhn.validate validNumbers)
                ]
            , describe "Invalid"
                [ test "Empty string " <|
                    \_ ->
                        Expect.equal
                            (Err "Input string is empty")
                            (Luhn.validate "")
                , test "Input string is only check digit " <|
                    \_ ->
                        Expect.equal
                            (Err "Input string is only check digit")
                            (Luhn.validate "1")
                , test "Incorrect checksum" <|
                    \_ ->
                        Expect.equal
                            (Err "Incorrect checksum")
                            (Luhn.validate "49927398717")
                , test "Includes symbols" <|
                    \_ ->
                        Expect.equal
                            (Err "could not convert string ';' to an Int")
                            (Luhn.validate "123lkj;193")
                , test "Includes letters" <|
                    \_ ->
                        Expect.equal
                            (Err "could not convert string 'j' to an Int")
                            (Luhn.validate "123lkj")
                , test "Is a single invalid character" <|
                    \_ ->
                        Expect.equal
                            (Err "could not convert string 'k' to an Int")
                            (Luhn.validate "k")
                ]
            ]
        , describe "isValid"
            [ describe "Valid"
                [ test "Valid numbers" <|
                    \_ ->
                        let
                            validNumbers =
                                [ "9405237190"
                                , "49927398716"
                                , "1234567812345670"
                                ]
                        in
                        Expect.equal
                            (List.map (always True) validNumbers)
                            (List.map Luhn.isValid validNumbers)
                ]
            , describe "Invalid"
                [ test "Invalid numbers" <|
                    \_ ->
                        let
                            invalidNumbers =
                                [ ""
                                , "1"
                                , "49927398717"
                                , "123lkj;193"
                                , "123lkj"
                                , "k"
                                ]
                        in
                        Expect.equal
                            (List.map (always False) invalidNumbers)
                            (List.map Luhn.isValid invalidNumbers)
                ]
            ]
        ]
