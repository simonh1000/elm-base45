module Base45 exposing (decode, encode)

{-| Base45 Data Encoding

Encode/Decode binary data for/from QR codes


# API

@docs decode, encode

-}

import Array exposing (Array)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Decoder)
import Bytes.Encode as Encode exposing (Encoder)
import Dict exposing (Dict)


{-| Converts a Base45 string into Bytes

Fails if the string is not of length 3x or 3x + 2

-}
decode : String -> Result String Bytes
decode =
    decCommon
        >> Result.map (List.map Encode.unsignedInt8 >> Encode.sequence >> Encode.encode)


decodeString : String -> Result String String
decodeString =
    decCommon
        >> Result.map (List.map Char.fromCode >> String.fromList)


decCommon =
    String.toList
        >> List.map String.fromChar
        >> decode_


{-| Each 3 characters corresponds to 2 bytes.
A trailing 2 characters corresponds to 1 or 2 bytes
-}
decode_ : List String -> Result String (List Int)
decode_ s =
    case s of
        a :: b :: c :: tl ->
            Result.map2 (++) (dec3 a b c) (decode_ tl)

        [ a, b ] ->
            dec2 a b

        [ _ ] ->
            Err "Bad shape: single character encountered at end"

        [] ->
            Ok []


dec3 : String -> String -> String -> Result String (List Int)
dec3 a b c =
    Result.map3 (\a_ b_ c_ -> modRem_ <| a_ + b_ * base45 + c_ * base45Squared)
        (revLookup a)
        (revLookup b)
        (revLookup c)


dec2 : String -> String -> Result String (List Int)
dec2 a b =
    Result.map2 (\a_ b_ -> modRem_ <| a_ + b_ * base45)
        (revLookup a)
        (revLookup b)


modRem_ : Int -> List Int
modRem_ x =
    let
        ( a, b ) =
            modRem 256 x
    in
    if a == 0 then
        [ b ]

    else
        [ a, b ]



-- ---------------------------
-- Encoder
-- ---------------------------


{-| Converts Bytes into a Base45 string
-}
encode : Bytes -> Maybe String
encode bytes =
    Decode.decode (bytesDecoder bytes) bytes


bytesDecoder : Bytes -> Decoder String
bytesDecoder bs =
    Decode.loop ( Bytes.width bs, Nothing, [] ) decInner


type alias State =
    ( Int, Maybe Int, List String )


{-| Each 2 (or last 1) bytes, is converted into a 3 character Base45 string
-}
decInner : State -> Decoder (Decode.Step State String)
decInner ( len, mbPrev, lst ) =
    case ( len == 0, mbPrev ) of
        ( True, Nothing ) ->
            -- finished
            Decode.succeed <| Decode.Done <| String.concat <| List.reverse lst

        ( True, Just x ) ->
            -- finished. Encode the single, remaining byte
            Decode.succeed <| Decode.Done <| String.concat <| List.reverse <| encode1 x :: lst

        ( False, Nothing ) ->
            -- first byte of next pair
            Decode.map (\y -> Decode.Loop ( len - 1, Just y, lst )) Decode.unsignedInt8

        ( False, Just x ) ->
            -- we have a new pair
            Decode.map (\y -> Decode.Loop ( len - 1, Nothing, encode2 x y :: lst )) Decode.unsignedInt8



-- special case for strings


encodeString : String -> String
encodeString =
    let
        encode_ : List Char -> List String
        encode_ s =
            case s of
                a :: b :: tl ->
                    -- TODO make tail recursive
                    encode2 (Char.toCode a) (Char.toCode b) :: encode_ tl

                [ a ] ->
                    [ encode1 <| Char.toCode a ]

                [] ->
                    []
    in
    String.toList
        >> encode_
        >> String.concat



-- helpers


encode2 : Int -> Int -> String
encode2 a b =
    let
        ( p2, r2 ) =
            modRem base45Squared (a * 256 + b)
    in
    String.concat <| List.filterMap identity [ Just <| encode1 r2, lookup p2 ]


encode1 : Int -> String
encode1 v =
    let
        ( p1, r1 ) =
            modRem base45 v
    in
    String.concat <| List.filterMap identity [ lookup r1, lookup p1 ]


base45Squared : Int
base45Squared =
    45 ^ 2


base45 : number
base45 =
    45



-- Helpers


{-| Todo: make more efficient
-}
modRem : Int -> Int -> ( Int, Int )
modRem d v =
    ( v // d, modBy d v )



-- Lookups


lookup : Int -> Maybe String
lookup d =
    Array.get d lookupTable


revLookup : String -> Result String Int
revLookup d =
    Dict.get d revLookupTable
        |> Result.fromMaybe ("Could not reverse lookup '" ++ d ++ "'")



-- Lookup Tables


lookupTable : Array String
lookupTable =
    Array.fromList lookupList


revLookupTable : Dict String Int
revLookupTable =
    lookupList |> List.indexedMap (\idx a -> ( a, idx )) |> Dict.fromList


lookupList : List String
lookupList =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", " ", "$", "%", "*", "+", "-", ".", "/", ":" ]
