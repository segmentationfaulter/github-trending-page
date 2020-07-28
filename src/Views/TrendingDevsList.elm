module Views.TrendingDevsList exposing (TrendingDevsList, trendingDevsListDecoder, trendingDevsView)

import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region exposing (heading)
import Json.Decode as Decode


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
            column
                [
                    El.width <| El.fillPortion 1
                ]
                [text "center"]

        rightAlignedElements : Element msg
        rightAlignedElements =
            row
                [
                    El.width <| fillPortion 1
                    , El.alignRight
                ]
                [text "right"]
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
