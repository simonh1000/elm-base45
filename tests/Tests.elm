module Tests exposing (..)

import Base45 exposing (..)
import Bytes exposing (width)
import Bytes.Decode as Decode
import Bytes.Encode as Bytes
import Expect exposing (Expectation)
import Test exposing (..)


{-| Examples from IETF doc
-}
pairs =
    [ ( "AB", "BB8" )
    , ( "ietf!", "QED8WEX0" )
    , ( "Hello!!", hello_ )
    , ( "base-45", "UJCLQE7W581" )
    ]


hello_ =
    "%69 VD92EX0"


decodeTests : Test
decodeTests =
    let
        tester ( str, b45 ) =
            test str <|
                \_ ->
                    b45
                        |> decode
                        |> Result.toMaybe
                        |> Maybe.andThen bytesToString
                        |> Expect.equal (Just str)
    in
    pairs
        |> List.map tester
        |> describe "decoding"


bytesToString : Bytes.Bytes -> Maybe String
bytesToString bs =
    Decode.decode (Decode.string (width bs)) bs


encodeTests : Test
encodeTests =
    let
        tester ( str, b45 ) =
            test str <|
                \_ ->
                    str
                        |> stringToBytes
                        |> encode
                        |> Expect.equal (Just b45)
    in
    pairs
        |> List.map tester
        |> describe "encoding"


stringToBytes s =
    Bytes.encode <| Bytes.string s



--{-| Used during development based on the examples in the spec
---}
--stringTests : Test
--stringTests =
--    describe "tests from spec"
--        [ test "encode 'Hello!!'" <|
--            \_ ->
--                encodeString "Hello!!"
--                    |> Expect.equal hello_
--        , test "decode 'Hello!!' 1" <|
--            \_ ->
--                decCommon hello_
--                    |> Expect.equal (Ok [ 72, 101, 108, 108, 111, 33, 33 ])
--        , test "decode 'Hello!!' 2" <|
--            \_ ->
--                decodeString hello_
--                    |> Expect.equal (Ok "Hello!!")
--        ]
