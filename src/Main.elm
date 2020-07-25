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
import Views.TrendingReposList exposing (TrendingReposList, trendingReposListDecoder, trendingReposView)



---- MODEL ----


type Model
    = Loading
    | ReposView TrendingReposList
    | DevsView
    | Failure Http.Error


init : ( Model, Cmd Msg )
init =
    ( Loading, fetchTrendingRepos )


fetchTrendingRepos : Cmd Msg
fetchTrendingRepos =
    Http.get
        { url = "https://ghapi.huchen.dev/repositories"
        , expect = Http.expectJson GotTrendingRepos trendingReposListDecoder
        }



---- UPDATE ----


type Msg
    = Noop
    | GotTrendingRepos (Result Http.Error TrendingReposList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrendingRepos result ->
            case result of
                Ok trendingRepos ->
                    ( ReposView trendingRepos, Cmd.none )

                Err httpError ->
                    ( Failure httpError, Cmd.none )

        Noop ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        selectedView : Element Msg
        selectedView =
            case model of
                Loading ->
                    el [ Font.size 40, El.centerX, El.centerY ] <| text "Loading..."

                ReposView trendingRepos ->
                    elmUIView model trendingRepos

                DevsView ->
                    el [ Font.size 40, El.centerX, El.centerY ] <| text "Loading..."

                Failure error ->
                    failureView error
    in
    El.layout [] <| selectedView


failureView : Http.Error -> Element Msg
failureView error =
    case error of
        Http.BadUrl errorMsg ->
            el [] <| text errorMsg

        Http.Timeout ->
            el [] <| text "It is taking too long, try refreshing"

        Http.NetworkError ->
            el [] <| text "Looks like you don't have internet"

        Http.BadStatus _ ->
            el [] <| text "We got an error reponse from server"

        Http.BadBody errorMsg ->
            el [] <| text errorMsg


elmUIView : Model -> TrendingReposList -> Element Msg
elmUIView model repos =
    column
        [ mainContent
        , El.width El.fill
        , El.spacing 40
        ]
        [ headerView
        , trendingView model repos
        ]


trendingView : Model -> TrendingReposList -> Element Msg
trendingView model repos =
    row
        [ El.width El.fill
        ]
        [ el [ El.width <| fillPortion 1 ] El.none
        , column
            [ El.width <| fillPortion 5
            ]
            [ trendingViewControls model
            , trendingReposView repos
            , el [El.height <| El.px 50] El.none
            ]
        , el [ El.width <| fillPortion 1 ] El.none
        ]


trendingViewControls : Model -> Element Msg
trendingViewControls model =
    let
        reposButtonLabel : String
        reposButtonLabel =
            "Repositories"

        devsButtonLabel : String
        devsButtonLabel =
            "Developers"

        renderButton : String -> Msg -> Element Msg
        renderButton label msg =
            let
                isActive : Bool
                isActive =
                    case model of
                        ReposView _ ->
                            if label == reposButtonLabel then
                                True

                            else
                                False

                        DevsView ->
                            if label == devsButtonLabel then
                                True

                            else
                                False

                        Loading ->
                            False

                        Failure _ ->
                            False

                activeStateColor : El.Color
                activeStateColor =
                    El.rgb255 3 102 214

                roundedBorders : El.Attribute Msg
                roundedBorders =
                    if label == reposButtonLabel then
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
                , El.height <| El.px 32
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
        [ renderButton reposButtonLabel Noop
        , renderButton devsButtonLabel Noop
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
