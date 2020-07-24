module Main exposing (..)

import Browser
import Element as El exposing (Element, column, el, row, text)
import Element.Background as Background
import Element.Font as Font
import Element.Region exposing (heading)
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
    El.layout [] header


header : Element Msg
header =
    el
        [ El.width El.fill, El.height <| El.px 174, Background.color <| El.rgb255 250 251 252 ]
    <|
        El.textColumn
            [ El.centerY, El.width El.fill ]
            [ el [ El.centerX, heading 1, Font.size 40 ] <| text "Trending"
            , El.paragraph [ El.centerX ] [ text "See what the GitHub community is most excited about today." ]
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
