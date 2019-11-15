module Main exposing (..)

import Browser
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Decode.Field as Field
import Json.Decode.Pipeline exposing (optional, required)



--- CONSTANTS ---


apiUrl =
    "http://localhost:3000/words?spelling="



---- MODEL ----


type Model
    = Failure
    | Loading
    | Success BaseWordData


type Word
    = BaseWord BaseWordData
    | RelatedWord RelatedWordData


type alias BaseWordData =
    { id : Int
    , definition : WordDefiniton
    , spelling : String
    , language_id : Int
    , origins : List RelatedWordData
    , origin_ofs : List RelatedWordData
    , relations : List RelatedWordData
    , derivations : List RelatedWordData
    , derived_froms : List RelatedWordData
    , language : Language
    }


type alias RelatedWordData =
    { id : Int
    , definition : WordDefiniton
    , spelling : String
    , language_id : Int
    }


type WordDefiniton
    = MissingMessage String
    | Definition String


type alias Language =
    { id : Int
    , iso_code : String
    , name : String
    }



--- UPDATE ---


type Msg
    = GotWord (Result Http.Error BaseWordData)


init : ( Model, Cmd Msg )
init =
    ( Loading
    , getWord
    )


getWord : Cmd Msg
getWord =
    Http.get
        { url = apiUrl ++ "wheel"
        , expect = Http.expectJson GotWord baseWordDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWord result ->
            case result of
                Ok word ->
                    ( Success word, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "Failed to load word data"

        Loading ->
            text "Loading..."

        Success word ->
            div [] [ text (Debug.toString model) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }



--- DECODERS ---


baseWordDecoder : Decoder BaseWordData
baseWordDecoder =
    Decode.map11
        BaseWordData
        (field "id" int)
        (field "definition" definitonDecoder)
        (field "spelling" string)
        (field "language_id" int)
        (field "origins" relatedWordDataDecoder)
        (field "language" languageDecoder)
        (field "origin_ofs" relatedWordDataDecoder)
        (field "relations" relatedWordDataDecoder)
        (field "derivations" relatedWordDataDecoder)
        (field "derived_froms" relatedWordDataDecoder)


definitonDecoder : Decoder WordDefiniton
definitonDecoder =
    Field.attempt "definition" Decode.string <|
        \maybeDef ->
            case maybeDef of
                Just def ->
                    Definition def
                        |> Decode.succeed

                _ ->
                    Decode.succeed (MissingMessage "We seem to be missing this word's definition")


relatedWordDataDecoder : Decoder (List RelatedWordData)
relatedWordDataDecoder =
    Decode.list
        (Decode.map4
            RelatedWordData
            (field "id" int)
            (field "definition" definitonDecoder)
            (field "spelling" string)
            (field "language_id" int)
        )


languageDecoder : Decoder Language
languageDecoder =
    Decode.map3
        Language
        (field "id" int)
        (field "iso_code" string)
        (field "name" string)
