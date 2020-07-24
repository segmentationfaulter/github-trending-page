module Main exposing (main)

import Browser
import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region exposing (heading, mainContent)
import Html exposing (Html)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    El.layout [] elmUIView


elmUIView : Element Msg
elmUIView =
    column
        [ mainContent
        , El.width El.fill
        , El.spacing 40
        ]
        [ headerView
        , trendingView
        ]


headerView : Element Msg
headerView =
    el
        [ El.width El.fill, El.height <| El.px 174, Background.color <| El.rgb255 250 251 252 ]
    <|
        El.textColumn
            [ El.centerY, El.width El.fill ]
            [ el [ El.centerX, heading 1, Font.size 40 ] <| text "Trending"
            , El.paragraph [ El.centerX ] [ text "See what the GitHub community is most excited about today." ]
            ]


trendingView : Element Msg
trendingView =
    row
        [ El.width El.fill
        ]
        [ el [ El.width <| fillPortion 1 ] El.none
        , el
            [ El.width <| fillPortion 3
            ]
            trendingViewControls
        , el [ El.width <| fillPortion 1 ] El.none
        ]


trendingViewControls : Element Msg
trendingViewControls =
    row
        [ Border.width 1
        , Border.color <| El.rgb255 225 228 232
        , Border.roundEach {topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0}
        , El.width El.fill
        ]
        [ text "contorls view" ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
