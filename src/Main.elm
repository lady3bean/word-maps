module Main exposing (..)

import Browser
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)
import Json.Decode.Field as Field
import Json.Decode.Pipeline exposing (optional, required, requiredAt)



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
        Field.require "id" int <| \id ->
        Field.require "definition" definitonDecoder <| \definition ->
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
