module Main exposing (..)

import Browser
import Browser.Events
import Color
import Force exposing (State)
import Graph exposing (Edge, Graph, Node, NodeContext, NodeId)
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Events exposing (on, onClick, onInput)
import Html.Events.Extra.Mouse as Mouse
import Http
import Json.Decode as Decode exposing (Decoder, int, string)
import Json.Decode.Field as Field
import Time
import TypedSvg exposing (circle, g, line, svg, title)
import TypedSvg.Attributes exposing (class, fill, stroke, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Attribute, Svg, text)
import TypedSvg.Types exposing (Fill(..))



--- CONSTANTS ---


apiUrl : String
apiUrl =
    "http://localhost:3000/words?spelling="


w : Float
w =
    990


h : Float
h =
    504



---- MODEL ----


type alias Model =
    { lookupValue : String
    , pageState : PageState
    , graph : Graph Entity ()
    , simulation : Force.State NodeId
    }



-- initialModel : Model
-- initialModel =
--     { lookupValue = ""
--     , pageState = Loading
--     , graph =
--     }


type PageState
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


type alias Entity =
    Force.Entity NodeId { value : String }



--- UPDATE ---


type Msg
    = FetchWord
    | GotWord (Result Http.Error BaseWordData)
    | UpdateLookupValue String
    | RelatedWordLookup String


initializeNode : NodeContext String () -> NodeContext Entity ()
initializeNode ctx =
    { node = { label = Force.entity ctx.node.id ctx.node.label, id = ctx.node.id }
    , incoming = ctx.incoming
    , outgoing = ctx.outgoing
    }


init : ( Model, Cmd Msg )
init =
    let
        graph =
            Graph.mapContexts initializeNode wordGraph

        link { from, to } =
            ( from, to )

        forces =
            [ Force.links <| List.map link <| Graph.edges graph
            , Force.manyBody <| List.map .id <| Graph.nodes graph
            , Force.center (w / 2) (h / 2)
            ]
    in
    ( Model "test" Loading graph (Force.simulation forces), Cmd.none )


fetchWord : String -> Cmd Msg
fetchWord word =
    Http.get
        { url = apiUrl ++ word
        , expect = Http.expectJson GotWord baseWordDecoder
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchWord ->
            ( { model | pageState = Loading }, fetchWord model.lookupValue )

        GotWord result ->
            case result of
                Ok word ->
                    ( { model | pageState = Success word }, Cmd.none )

                Err _ ->
                    ( { model | pageState = Failure }, Cmd.none )

        UpdateLookupValue value ->
            ( { model | lookupValue = value }, Cmd.none )

        RelatedWordLookup word ->
            ( { model | pageState = Loading }, fetchWord word )



---- VIEW ----
-- view : Model -> Html Msg
-- view model =
--     case model.pageState of
--         Failure ->
--             div []
--                 [ Html.text "Failed to load word data. Please look up another word."
--                 , wordLookupInput
--                 , wordLookupButton
--                 ]
--         Loading ->
--             div []
--                 [ Html.text "Loading... If you'd like, you can look up another word"
--                 , wordLookupInput
--                 , wordLookupButton
--                 ]
--         Success word ->
--             div []
--                 [ div [] [ Html.text ("spelling: " ++ word.spelling) ]
--                 , div [] [ Html.text ("language: " ++ word.language.name) ]
--                 , div []
--                     [ Html.text ("definition: " ++ Maybe.withDefault "We seem to be missing this word's definition" word.definition) ]
--                 , div []
--                     [ Html.text "origins: "
--                     , renderRelatedWordList word.origins
--                     ]
--                 , div []
--                     [ Html.text "origin of: "
--                     , renderRelatedWordList word.origin_ofs
--                     ]
--                 , div []
--                     [ Html.text "relations: "
--                     , renderRelatedWordList word.relations
--                     ]
--                 , div []
--                     [ Html.text "derivations: "
--                     , renderRelatedWordList word.derivations
--                     ]
--                 , div []
--                     [ Html.text "derived from: "
--                     , renderRelatedWordList word.derived_froms
--                     ]
--                 , div []
--                     [ wordLookupInput
--                     , wordLookupButton
--                     ]
--                 ]


renderRelatedWordSpelling : RelatedWordData -> Html Msg
renderRelatedWordSpelling word =
    li [ onClick (RelatedWordLookup word.spelling) ] [ Html.text word.spelling ]


renderRelatedWordList : List RelatedWordData -> Html Msg
renderRelatedWordList list =
    let
        listItems =
            List.map renderRelatedWordSpelling list
    in
    ul [] listItems


wordLookupInput : Html Msg
wordLookupInput =
    div []
        [ input [ onInput UpdateLookupValue ] []
        ]


wordLookupButton : Html Msg
wordLookupButton =
    button [ onClick FetchWord ] [ Html.text "Look up" ]



---- GRAPHING ----
-- wordGraph : BaseWordData -> List RelatedWordData -> List ( NodeId, NodeId ) -> Graph n
-- wordGraph baseWord relatedWords =


wordGraph : Graph String ()
wordGraph =
    let
        labels =
            [ "bean", "been" ]

        edges =
            [ ( 0, 0 )
            , ( 1, 0 )
            ]
    in
    Graph.fromNodeLabelsAndEdgePairs labels edges


linkElement graph edge =
    let
        source =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.from graph

        target =
            Maybe.withDefault (Force.entity 0 "") <| Maybe.map (.node >> .label) <| Graph.get edge.to graph
    in
    line
        [ strokeWidth 1
        , stroke (Color.rgb255 170 170 170)
        , x1 source.x
        , y1 source.y
        , x2 target.x
        , y2 target.y
        ]
        []


nodeElement node =
    circle
        [ r 2.5
        , fill (Fill Color.black)
        , stroke (Color.rgba 0 0 0 0)
        , strokeWidth 7
        , Mouse.onDown node.id
        , cx node.label.x
        , cy node.label.y
        ]
        [ title [] [ Html.text node.label.value ] ]


view : Model -> Svg Msg
view model =
    svg [ viewBox 0 0 w h ]
        [ Graph.edges model.graph
            |> List.map (linkElement model.graph)
            |> g [ class [ "links" ] ]
        , Graph.nodes model.graph
            |> List.map nodeElement
            |> g [ class [ "nodes" ] ]
        ]



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
    Field.require "id" int <|
        \id ->
            Field.attempt "definition" string <|
                \definition ->
                    Field.require "spelling" string <|
                        \spelling ->
                            Field.require "language_id" int <|
                                \language_id ->
                                    Field.require "origins" relatedWordDataDecoder <|
                                        \origins ->
                                            Field.require "language" languageDecoder <|
                                                \language ->
                                                    Field.require "origin_ofs" relatedWordDataDecoder <|
                                                        \origin_ofs ->
                                                            Field.require "relations" relatedWordDataDecoder <|
                                                                \relations ->
                                                                    Field.require "derivations" relatedWordDataDecoder <|
                                                                        \derivations ->
                                                                            Field.require "derived_froms" relatedWordDataDecoder <|
                                                                                \derived_froms ->
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
    Decode.list
        (Field.require "id" int <|
            \id ->
                Field.attempt "definition" string <|
                    \definition ->
                        Field.require "spelling" string <|
                            \spelling ->
                                Field.require "language_id" int <|
                                    \language_id ->
                                        Decode.succeed
                                            { id = id
                                            , definition = definition
                                            , spelling = spelling
                                            , language_id = language_id
                                            }
        )


languageDecoder : Decoder Language
languageDecoder =
    Field.require "id" int <|
        \id ->
            Field.require "iso_code" string <|
                \iso_code ->
                    Field.require "name" string <|
                        \name ->
                            Decode.succeed
                                { id = id
                                , iso_code = iso_code
                                , name = name
                                }
