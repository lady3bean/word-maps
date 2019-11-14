module Main exposing (..)

import Browser
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string)



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

    -- , definition : WordDefiniton
    , spelling : String
    , language_id : Int

    -- , origins : RelatedWords
    -- , origin_ofs : RelatedWords
    -- , relations : RelatedWords
    -- , derivations : RelatedWords
    -- , derived_froms : RelatedWords
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
    | Present String


type RelatedWords
    = NoDataMessage String
    | Relateds (List Word)


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
    Decode.map4
        BaseWordData
        (field "id" int)
        -- (field "definition" definitonDecoder)
        (field "spelling" string)
        (field "language_id" int)
        (field "language" languageDecoder)



-- (field "origins" relatedWordsDecoder)
-- (field "origin_ofs" relatedWordsDecoder)
-- (field "relations" relatedWordsDecoder)
-- (field "derivations" relatedWordsDecoder)
-- (field "derived_froms" relatedWordsDecoder)
-- definitonDecoder : WordDefiniton -> Decoder String
-- definitonDecoder def =
--     case Decode.decodeString def of
--         Ok definition ->
--             definition
--         Err _ ->
--             "We seem to be missing the definition for this word"
-- relatedWordsDecoder : Decoder RelatedWords
-- relatedWordsDecoder =
--     Decode.list RelatedWordData


languageDecoder : Decoder Language
languageDecoder =
    Decode.map3
        Language
        (field "id" int)
        (field "iso_code" string)
        (field "name" string)
