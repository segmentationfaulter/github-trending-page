module Main exposing (main)

import Browser
import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as InputEl
import Element.Region exposing (mainContent)
import Html exposing (Html)
import Http
import Views.Header exposing (headerView)
import Views.TrendingReposList exposing (TrendingReposList, trendingReposListDecoder)



---- MODEL ----


type Model
    = ReposView
    | DevsView


init : ( Model, Cmd Msg )
init =
    ( ReposView, fetchTrendingRepos )


fetchTrendingRepos : Cmd Msg
fetchTrendingRepos =
    Http.get
        { url = "https://ghapi.huchen.dev/repositories"
        , expect = Http.expectJson GotTrendingRepos trendingReposListDecoder
        }



---- UPDATE ----


type Msg
    = SwitchToRepos
    | SwitchToDevs
    | GotTrendingRepos (Result Http.Error TrendingReposList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SwitchToRepos ->
            ( ReposView, Cmd.none )

        SwitchToDevs ->
            ( DevsView, Cmd.none )

        GotTrendingRepos result ->
            case result of
                Ok trendingRepos ->
                    ( ReposView, Cmd.none )

                Err httpError ->
                    ( ReposView, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    El.layout [] <| elmUIView model


elmUIView : Model -> Element Msg
elmUIView model =
    column
        [ mainContent
        , El.width El.fill
        , El.spacing 40
        ]
        [ headerView
        , trendingView model
        ]


trendingView : Model -> Element Msg
trendingView model =
    row
        [ El.width El.fill
        ]
        [ el [ El.width <| fillPortion 1 ] El.none
        , el
            [ El.width <| fillPortion 3
            ]
          <|
            trendingViewControls model
        , el [ El.width <| fillPortion 1 ] El.none
        ]


trendingViewControls : Model -> Element Msg
trendingViewControls model =
    let
        renderButton : String -> Msg -> Element Msg
        renderButton label msg =
            let
                isActive : Bool
                isActive =
                    case model of
                        DevsView ->
                            if label == "Developers" then
                                True

                            else
                                False

                        ReposView ->
                            if label == "Repositories" then
                                True

                            else
                                False

                activeStateColor : El.Color
                activeStateColor =
                    El.rgb255 3 102 214

                roundedBorders : El.Attribute Msg
                roundedBorders =
                    if label == "Repositories" then
                        Border.roundEach { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                    else
                        Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }

                borderWidth : El.Attribute Msg
                borderWidth =
                    if isActive then
                        Border.width 1

                    else
                        case label of
                            "Repositories" ->
                                Border.widthEach { left = 1, top = 1, bottom = 1, right = 0 }

                            _ ->
                                Border.widthEach { left = 0, top = 1, bottom = 1, right = 1 }

                borderColor : El.Attribute Msg
                borderColor =
                    if isActive then
                        Border.color activeStateColor

                    else
                        Border.color <| El.rgb255 225 228 232

                backgroundColor : El.Color
                backgroundColor =
                    if isActive then
                        activeStateColor

                    else
                        El.rgb255 246 248 250

                fontColor : El.Color
                fontColor =
                    if isActive then
                        El.rgb255 255 255 255

                    else
                        El.rgb255 0 0 0
            in
            InputEl.button
                [ Background.color backgroundColor
                , El.paddingXY 16 5
                , Font.size 14
                , Font.color fontColor
                , roundedBorders
                , borderWidth
                , borderColor
                ]
                { onPress = Just msg, label = text label }
    in
    row
        [ Border.width 1
        , Border.color <| El.rgb255 225 228 232
        , Border.roundEach { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }
        , El.width El.fill
        , El.height <| El.px 66
        , El.padding 16
        , Background.color <| El.rgb255 246 248 250
        ]
        [ renderButton "Repositories" SwitchToRepos
        , renderButton "Developers" SwitchToDevs
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
