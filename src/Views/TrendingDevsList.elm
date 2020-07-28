module Views.TrendingDevsList exposing (TrendingDevsList, trendingDevsListDecoder, trendingDevsView)

import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region exposing (heading)
import Html exposing (label)
import Json.Decode as Decode
import Views.Icons as Icons


type alias TrendingDev =
    { username : String
    , name : String
    , url : String
    , sponsorUrl : Maybe String
    , avatar : String
    , repo : Maybe PopularRepo
    }


type alias PopularRepo =
    { name : String
    , description : String
    , url : String
    }


type alias TrendingDevsList =
    List TrendingDev


trendingDevsListDecoder : Decode.Decoder TrendingDevsList
trendingDevsListDecoder =
    let
        usernameDecoder =
            Decode.field "username" Decode.string

        nameDecoder =
            Decode.field "name" Decode.string

        urlDecoder =
            Decode.field "url" Decode.string

        sponsorUrlDecoder =
            Decode.maybe <| Decode.field "sponsorUrl" Decode.string

        avatarDecoder =
            Decode.field "avatar" Decode.string

        repoDecoder : Decode.Decoder (Maybe PopularRepo)
        repoDecoder =
            Decode.maybe <|
                Decode.field "repo"
                    (Decode.map3
                        (\name description url -> { name = name, description = description, url = url })
                        (Decode.field "name" Decode.string)
                        (Decode.field "description" Decode.string)
                        (Decode.field "url" Decode.string)
                    )
    in
    Decode.list <|
        Decode.map6
            (\username name url sponsorUrl avatar repo -> { username = username, name = name, url = url, sponsorUrl = sponsorUrl, avatar = avatar, repo = repo })
            usernameDecoder
            nameDecoder
            urlDecoder
            sponsorUrlDecoder
            avatarDecoder
            repoDecoder


trendingDevItem : Int -> TrendingDev -> Element msg
trendingDevItem index dev =
    let
        leftAlignedElements : Element msg
        leftAlignedElements =
            let
                rowNumber : Element msg
                rowNumber =
                    el [ Font.size 12, El.alignTop ] (El.text <| String.fromInt index)

                avatar : Element msg
                avatar =
                    el
                        [ El.width <| El.px 48
                        , Border.rounded 24
                        , Border.width 1
                        , Border.color <| El.rgb255 255 255 255
                        , El.clip
                        ]
                    <|
                        El.image
                            [ El.width <| El.px 48
                            , El.height <| El.px 48
                            ]
                            { src = dev.avatar
                            , description = "avatar of " ++ dev.name
                            }

                devName : Element msg
                devName =
                    column
                        [ El.spacing 4
                        ]
                        [ el
                            [ El.height <| El.px 20
                            , Font.semiBold
                            , Font.color <| El.rgb255 3 102 214
                            , heading 1
                            ]
                          <|
                            text dev.name
                        , El.paragraph
                            [ Font.size 16
                            , El.spacing 8
                            , Font.alignLeft
                            ]
                            [ text dev.username ]
                        ]
            in
            row
                [ El.spacing 16
                , El.width <| El.fillPortion 1
                , El.alignLeft
                ]
                [ rowNumber
                , avatar
                , devName
                ]

        centerAlignedElements : Element msg
        centerAlignedElements =
            let
                popularRepoText : Element msg
                popularRepoText =
                    El.paragraph
                        [ Font.size 12
                        , El.alignTop
                        , El.spacing 6
                        ]
                        [ Icons.fireIcon
                        , text " POPULAR REPO"
                        ]

                repo : Element msg
                repo =
                    case dev.repo of
                        Nothing ->
                            El.none

                        Just popularRepo ->
                            El.paragraph
                                [ Font.size 16
                                , El.spacing 4
                                , Font.color <| El.rgb255 3 102 214
                                , Font.semiBold
                                ]
                                [ Icons.repoIcon
                                , text <| " " ++ popularRepo.name
                                ]

                descriptionText : Element msg
                descriptionText =
                    case dev.repo of
                        Nothing ->
                            El.none

                        Just { description } ->
                            El.paragraph
                                [ Font.size 12
                                , El.spacing 6
                                ]
                                [ text description ]
            in
            column
                [ El.width <| El.fillPortion 1
                , El.alignTop
                , El.centerX
                , Font.alignLeft
                , El.spacing 8
                ]
                [ popularRepoText
                , repo
                , descriptionText
                ]

        rightAlignedElements : Element msg
        rightAlignedElements =
            let
                buttonAttributes : List (El.Attribute msg)
                buttonAttributes =
                    [ El.paddingXY 12 3
                    , Background.color <| El.rgb255 250 251 252
                    , Border.width 1
                    , Border.rounded 6
                    , Border.color <| El.rgba255 27 31 35 0.15
                    , El.height <| El.px 28
                    , El.alignRight
                    ]

                followButton : Element msg
                followButton =
                    let
                        button : Element msg
                        button =
                            el
                                []
                            <|
                                text "Follow"
                    in
                    El.newTabLink
                        buttonAttributes
                        { url = dev.url, label = button }

                sponsorButton : Element msg
                sponsorButton =
                    let
                        button : Element msg
                        button =
                            El.row
                                []
                                [ Icons.heartIcon
                                , text "  Sponsor"
                                ]
                    in
                    case dev.sponsorUrl of
                        Nothing ->
                            El.none

                        Just sponsorUrl ->
                            El.newTabLink
                                buttonAttributes
                                { url = sponsorUrl, label = button }
            in
            row
                [ El.width <| fillPortion 1
                , El.alignTop
                , Font.size 12
                , El.spacing 8
                ]
                [ sponsorButton
                , followButton
                ]
    in
    row
        [ El.padding 16
        , El.spaceEvenly
        , Font.color <| El.rgb255 36 41 46
        , Border.color <| El.rgb255 225 228 232
        , Border.widthEach { left = 1, bottom = 1, right = 1, top = 0 }
        , El.width El.fill
        ]
        [ leftAlignedElements
        , centerAlignedElements
        , rightAlignedElements
        ]


trendingDevsView : TrendingDevsList -> Element msg
trendingDevsView trendingDevs =
    column
        [ El.width El.fill
        ]
    <|
        List.indexedMap trendingDevItem trendingDevs
