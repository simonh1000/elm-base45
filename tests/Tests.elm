module Tests exposing (..)

import Base45 exposing (decode, decodeString, encode, encodeString)
import Bytes exposing (width)
import Bytes.Decode as Decode
import Bytes.Encode as Bytes
import Expect exposing (Expectation)
import Test exposing (..)


decodeTests : Test
decodeTests =
    describe "decoding"
        [ test "BB8 -> AB" <|
            \_ ->
                decode "BB8"
                    |> Result.toMaybe
                    |> Maybe.andThen bytesToString
                    |> Expect.equal (Just "AB")
        , test "QED8WEX0 -> ietf!" <|
            \_ ->
                decode "QED8WEX0"
                    |> Result.toMaybe
                    |> Maybe.andThen bytesToString
                    |> Expect.equal (Just "ietf!")
        , test "BB8 as string" <|
            \_ ->
                decodeString "BB8"
                    |> Expect.equal (Ok [ 65, 66 ])
        ]


bytesToString : Bytes.Bytes -> Maybe String
bytesToString bs =
    Decode.decode (Decode.string (width bs)) bs


encodeTests : Test
encodeTests =
    describe "encoding"
        [ test "AB" <|
            \_ ->
                "AB"
                    |> enc
                    |> encode
                    |> Expect.equal (Just "BB8")
        , test "Hello!! as string" <|
            \_ ->
                encodeString "Hello!!"
                    |> Expect.equal "%69VD92EX0"
        , test "Hello!!" <|
            \_ ->
                "Hello!!"
                    |> Bytes.string
                    |> Bytes.encode
                    |> encode
                    |> Expect.equal (Just "%69VD92EX0")
        ]


enc s =
    Bytes.encode <| Bytes.string s
