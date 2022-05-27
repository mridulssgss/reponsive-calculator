module View exposing (..)

import Html exposing (Html)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE



{-
   Possible Tasks
   1. Change the layout dynamically, based on number of rows and columns
   2. Add configurable types like
       1. Width
       2. height
       3. Background Color
       4. roundedness
       5. Border
       6. style


-}


height =
    20


width =
    30


viewButtons : ( Float, Float ) -> String -> msg -> Svg msg
viewButtons ( x, y ) label msg =
    let
        ( transX, transY ) =
            ( x + width / 2, y + height / 2 )
    in
    S.g
        [ SE.onClick msg
        ]
        [ S.rect
            [ SA.x (String.fromFloat x)
            , SA.y (String.fromFloat y)
            , SA.height (String.fromFloat height)
            , SA.width (String.fromFloat width)
            , SA.rx "15"
            , SA.fillOpacity "0.5"
            ]
            []
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            ]
            [ S.text label ]
        ]


splitAtEvery : Int -> List a -> List (List a)
splitAtEvery index lst =
    case lst of
        [] ->
            []

        _ ->
            List.append [ List.take index lst ] (splitAtEvery index (List.drop index lst))


getCoordinatedList : List (List ( String, msg )) -> List ( ( Float, Float ), ( String, msg ) )
getCoordinatedList lst =
    let
        p =
            10

        assignXCoord y list =
            List.foldl (\e ( x, fe ) -> ( x + width + p, List.append fe [ ( ( x, y ), e ) ] )) ( 0, [] ) list
                |> Tuple.second

        ( _, yLists ) =
            List.foldl (\l ( y, ls ) -> ( y + height + p, List.append ls [ ( y, l ) ] )) ( 0, [] ) lst
    in
    List.map (\( y, l ) -> assignXCoord y l) yLists
        |> List.concat


viewCalc : String -> String -> List ( String, msg ) -> Html msg
viewCalc history answer buttons =
    let
        cols =
            4

        buttonGroups =
            splitAtEvery cols buttons

        coordButtons =
            getCoordinatedList buttonGroups
    in
    S.svg
        [ SA.viewBox "0 0 500 500"
        ]
        (List.map
            (\( c, ( l, m ) ) -> viewButtons c l m)
            coordButtons
        )
