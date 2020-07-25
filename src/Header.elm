module Header exposing(headerView)

import Element as El exposing (Element, el, text)
import Element.Region exposing (heading)
import Element.Background as Background
import Element.Font as Font

headerView : Element msg
headerView =
    el
        [ El.width El.fill, El.height <| El.px 174, Background.color <| El.rgb255 250 251 252 ]
    <|
        El.textColumn
            [ El.centerY, El.width El.fill ]
            [ el [ El.centerX, heading 1, Font.size 40 ] <| text "Trending"
            , El.paragraph [ El.centerX ] [ text "See what the GitHub community is most excited about today." ]
            ]

