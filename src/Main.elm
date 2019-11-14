module Main exposing (..)

import Browser
import Html exposing (Html, pre, text)
import Html.Attributes exposing (src)
import Http



--- CONSTANTS ---


apiUrl =
    "http://localhost:3000/words?spelling="



---- MODEL ----


type Model
    = Failure
    | Loading
    | Success String


type Word
    = BaseWord WordData
    | RelatedWord RelatedWordData


type alias WordData =
    { id : Int
    , definition : String
    , spelling : String
    , language_id : Int
    , origins : List Word
    , origin_ofs : List Word
    , relations : List Word
    , derivations : List Word
    , derived_froms : List Word
    , language : Language
    }


type alias RelatedWordData =
    { id : Int
    , definition : String
    , spelling : String
    , language_id : Int
    }


type alias Language =
    { id : Int
    , iso_code : String
    , name : String
    }


init : ( Model, Cmd Msg )
init =
    ( Loading
    , Http.get
        { url = apiUrl ++ "wheel"
        , expect = Http.expectString GotText
        }
    )


buildWordTree : Word -> Word
buildWordTree word =
    case word of
        BaseWord data ->
            BaseWord data

        RelatedWord data ->
            RelatedWord data



---- UPDATE ----


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
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
