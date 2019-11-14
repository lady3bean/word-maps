module Main exposing (..)

import Browser
import Html exposing (Html, pre, text)
import Html.Attributes exposing (src)
import Http
import Json.Decode exposing (Decoder, field, string)



--- CONSTANTS ---


apiUrl =
    "http://localhost:3000/words?spelling="



---- MODEL ----


type Model
    = Failure
    | Loading
    | Success String


type Word
    = BaseWord BaseWordData
    | RelatedWord RelatedWordData


type alias BaseWordData =
    { id : Int
    , definition : WordDefiniton
    , spelling : String
    , language_id : Int
    , origins : RelatedWords
    , origin_ofs : RelatedWords
    , relations : RelatedWords
    , derivations : RelatedWords
    , derived_froms : RelatedWords
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


init : ( Model, Cmd Msg )
init =
    ( Loading
    , getWord
    )


getWord : Cmd Msg
getWord =
    Http.get
        { url = apiUrl ++ "wheel"
        , expect = Http.expectJson GotWord spellingDecoder
        }


spellingDecoder : Decoder String
spellingDecoder =
    field "spelling" string


type Msg
    = GotWord (Result Http.Error String)


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
            pre [] [ text word ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
