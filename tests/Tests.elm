module Tests exposing (..)

import Base45 exposing (..)
import Bytes exposing (Bytes, Endianness(..), width)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
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
        [ test "binary length" <|
            \_ ->
                binaryData
                    |> String.dropLeft 4
                    |> Debug.log "NCFO =="
                    |> decode
                    |> Result.map Bytes.width
                    |> Expect.equal (Ok 345)
        , test "binary content" <|
            \_ ->
                binaryData
                    |> String.dropLeft 4
                    |> decode
                    |> Result.map (debug >> Encode.encode)
                    |> Expect.equal (Err "345")
        ]


debug : Bytes -> Encode.Encoder
debug imageData =
    Encode.sequence
        [ Encode.signedInt16 BE (Bytes.width imageData)
        , Encode.bytes imageData
        ]


binaryData =
    "HC1:NCFOXN%TSMAHN-H%OCHOS80JS3NL73AG4O:5+T9ZKEOGICTC*JJM*4*AE *FCV4*XUA2PSGH.+H$NI4L6F$SYQ1WWB8VF*0S-$RUM06*J*TB9Z54OGXV1E*RLUFTWOK/N:PIWIG4SIBRU4SI.J9WVHWVH+ZE5%PAT1HRI1THRX3$*R/IE%TE6UG+ZE V1+GO9+PGF6Z6NC8P$WA3AAPEPBDSM+Q9I6O670C57Q4UYQD*O%+Q.SQBDOBKLP64-HQ/HQ3IRE+QJDO4A7E:7LYP:SQE20% KOHKSKE MCAOI8%MVYEQ KX*O%OKI%KI$N+CGDPIHG4OCGDPQIX0S%O+*P3-SY$N%VEY5LV39JDKM.SY$NPJGL8R$8RZQJ*8P/-3*4C 347CPFRMLNKNM8POCJPGG7HE61E3AA8O6IVVKVPQC%9FHRQV8USIJFQS /MXY6SCBIABV3C1X96LNS2W6NSS30%EVW*1EFD 1G/WAIXCVCU-G7WXMTSOPKPM:G/00A8B 0"
