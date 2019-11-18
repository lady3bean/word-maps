module Main exposing (..)

import Browser
import Html exposing (Html, div, pre, text, ul, li)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Decode.Field as Field


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
    , definition : Maybe String
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
    , definition : Maybe String
    , spelling : String
    , language_id : Int
    }

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


view : Model -> Html msg
view model =
    case model of
        Failure ->
            text "Failed to load word data"

        Loading ->
            text "Loading..."

        Success word ->
            div [] [
                    div [] [text ("spelling: " ++ word.spelling)]
                    , div [] [text ("language: " ++ word.language.name)]
                    , div []
                        [text ("definition: " ++ (Maybe.withDefault "We seem to be missing this word's definition" word.definition))]
                    , div []
                        [
                        text "origins: "
                        , renderRelatedWordList word.origins
                        ]
                    , div []
                        [
                        text "origin of: "
                        , renderRelatedWordList word.origin_ofs
                        ]
                    , div []
                        [
                        text "relations: "
                        , renderRelatedWordList word.relations
                        ]
                    , div []
                        [
                        text "derivations: "
                        , renderRelatedWordList word.derivations
                        ]
                    , div []
                        [
                        text "derived from: "
                        , renderRelatedWordList word.derived_froms
                        ]
                    ]



renderRelatedWordSpelling : RelatedWordData -> Html msg
renderRelatedWordSpelling word =
    li [] [ text word.spelling ]

renderRelatedWordList : List RelatedWordData -> Html msg
renderRelatedWordList list =
    let
        listItems = List.map renderRelatedWordSpelling list
    in
        ul [] listItems

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
        Field.require "id" int <| \id ->
        Field.attempt "definition" string <| \definition ->
        Field.require "spelling" string <| \spelling ->
        Field.require "language_id" int <| \language_id ->
        Field.require "origins" relatedWordDataDecoder <| \origins ->
        Field.require "language" languageDecoder <| \language ->
        Field.require "origin_ofs" relatedWordDataDecoder <| \origin_ofs ->
        Field.require "relations" relatedWordDataDecoder <| \relations ->
        Field.require "derivations" relatedWordDataDecoder <| \derivations ->
        Field.require "derived_froms" relatedWordDataDecoder <| \derived_froms ->

        Decode.succeed
            { id = id
            , definition = definition
            , spelling = spelling
            , language_id = language_id
            , origins = origins
            , origin_ofs = origin_ofs
            , relations = relations
            , derivations = derivations
            , derived_froms = derived_froms
            , language = language
            }


relatedWordDataDecoder : Decoder (List RelatedWordData)
relatedWordDataDecoder =
    Decode.list(
        Field.require "id" int <| \id ->
        Field.attempt "definition" string <| \definition ->
        Field.require "spelling" string <| \spelling ->
        Field.require "language_id" int <| \language_id ->

        Decode.succeed
            { id = id
            , definition = definition
            , spelling = spelling
            , language_id = language_id
        }
    )

languageDecoder : Decoder Language
languageDecoder =
    Decode.map3
        Language
        (field "id" int)
        (field "iso_code" string)
        (field "name" string)
