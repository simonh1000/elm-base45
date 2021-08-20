module Tests exposing (..)

import Base45 exposing (..)
import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Bytes.Extra as BE
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
    Encode.encode <| Encode.string s


testBadString : Test
testBadString =
    describe "bad strings"
        [ test "stop on first bad character(s)" <|
            \_ ->
                decode long
                    |> Expect.equal (Err "Could not reverse lookup 'd'")
        ]


long =
    "AAAAAAAAAdata:image/png;base64,iVBORw0KGgoAAAANSUhEUg"


binaryTests =
    describe "binary tests"
        [ --test "binary length" <|
          --    \_ ->
          --        binaryData
          --            |> String.dropLeft 4
          --            |> decode
          --            |> Result.map Bytes.width
          --            |> Expect.equal (Ok 345)
          --
          --,
          test "binary content" <|
            \_ ->
                binaryData
                    |> String.dropLeft 4
                    |> decode
                    |> Result.map BE.toByteValues
                    |> Expect.equal (Ok exp)
        ]


binaryData =
    "HC1:NCFOXN%TSMAHN-H%OCHOS80JS3NL73AG4O:5+T9ZKEOGICTC*JJM*4*AE *FCV4*XUA2PSGH.+H$NI4L6F$SYQ1WWB8VF*0S-$RUM06*J*TB9Z54OGXV1E*RLUFTWOK/N:PIWIG4SIBRU4SI.J9WVHWVH+ZE5%PAT1HRI1THRX3$*R/IE%TE6UG+ZE V1+GO9+PGF6Z6NC8P$WA3AAPEPBDSM+Q9I6O670C57Q4UYQD*O%+Q.SQBDOBKLP64-HQ/HQ3IRE+QJDO4A7E:7LYP:SQE20% KOHKSKE MCAOI8%MVYEQ KX*O%OKI%KI$N+CGDPIHG4OCGDPQIX0S%O+*P3-SY$N%VEY5LV39JDKM.SY$NPJGL8R$8RZQJ*8P/-3*4C 347CPFRMLNKNM8POCJPGG7HE61E3AA8O6IVVKVPQC%9FHRQV8USIJFQS /MXY6SCBIABV3C1X96LNS2W6NSS30%EVW*1EFD 1G/WAIXCVCU-G7WXMTSOPKPM:G/00A8B 0"


exp =
    [ 120, 218, 187, 212, 226, 187, 136, 81, 141, 197, 99, 74, 225, 197, 150, 83, 182, 146, 25, 11, 34, 126, 47, 97, 76, 114, 114, 101, 145, 74, 100, 17, 153, 201, 38, 149, 112, 167, 125, 166, 37, 35, 243, 66, 198, 37, 137, 101, 141, 171, 146, 146, 51, 43, 228, 12, 12, 157, 92, 195, 124, 34, 221, 163, 220, 61, 3, 252, 157, 44, 92, 67, 45, 189, 130, 204, 13, 125, 220, 124, 124, 2, 195, 149, 189, 146, 146, 243, 129, 218, 147, 82, 242, 24, 147, 82, 74, 178, 140, 12, 140, 12, 117, 13, 204, 116, 13, 12, 147, 50, 139, 147, 29, 163, 220, 147, 114, 19, 115, 253, 131, 220, 117, 13, 13, 128, 192, 208, 204, 210, 50, 41, 183, 32, 199, 53, 84, 223, 80, 223, 200, 80, 223, 208, 212, 200, 50, 169, 56, 133, 41, 169, 36, 61, 211, 194, 196, 192, 212, 216, 210, 192, 192, 44, 169, 172, 32, 203, 208, 208, 208, 210, 216, 192, 212, 192, 192, 52, 57, 37, 63, 41, 203, 208, 210, 194, 0, 104, 164, 174, 161, 73, 114, 94, 98, 238, 146, 164, 180, 188, 116, 215, 164, 162, 196, 212, 162, 146, 164, 244, 188, 130, 128, 212, 146, 212, 34, 133, 128, 196, 210, 28, 5, 223, 196, 162, 204, 196, 228, 180, 188, 146, 116, 87, 167, 32, 71, 215, 160, 144, 228, 244, 188, 146, 130, 0, 215, 16, 215, 32, 155, 0, 199, 80, 31, 27, 95, 199, 32, 79, 199, 228, 178, 212, 162, 84, 67, 61, 99, 61, 131, 8, 135, 196, 9, 5, 79, 175, 191, 74, 248, 103, 248, 218, 99, 151, 120, 98, 210, 122, 238, 213, 153, 145, 226, 29, 181, 185, 53, 145, 89, 59, 88, 215, 95, 146, 76, 255, 185, 166, 253, 150, 225, 141, 0, 163, 247, 211, 14, 228, 105, 134, 126, 225, 84, 229, 100, 203, 239, 137, 58, 88, 179, 243, 194, 225, 201, 94, 134, 98, 0, 43, 88, 117, 36 ]
