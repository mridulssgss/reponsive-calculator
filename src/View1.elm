module View exposing (viewCalc)

import Html exposing (Html)
import Main exposing (Msg)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import View.Attributes exposing (Attribute)



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


type alias CalcConfig =
    { height : Float
    , width : Float
    }


defCalcConfig : CalcConfig
defCalcConfig =
    { height = 200
    , width = 200
    }


height =
    20


width =
    30



{-
   genRectangleButton : ( Float, Float ) -> String -> msg -> Svg Msg
   genRectangleButton ( x, y ) label msg =
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
               , SA.rx "2"
               , SA.style "fill:rgb(143 143 237);stroke-width:0.5;stroke:rgb(0,0,0)"
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
-}
{-
   genCircleButton : ( Float, Float ) -> String -> msg -> Svg Msg
   genCircleButton ( x, y ) label msg =
       let
           ( transX, transY ) =
               ( x + width / 2, y + height / 2 )
       in
       S.g
           [ SE.onClick msg
           ]
           [ S.circle
               [ SA.cx (String.fromFloat x)
               , SA.cy (String.fromFloat y)
               , SA.r 50
               , SA.style "fill:rgb(143 143 237);stroke-width:0.5;stroke:rgb(0,0,0)"
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
-}
{-
   viewButtons ( x, y ) label msg =
       let
           buttonShape =
               "Circle"
       in
       case buttonShape of
           "Circle" ->
               genCircleButton ( x, y ) label msg

           _ ->
               genRectangleButton ( x, y ) label msg

-}


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
            , SA.rx "2"
            , SA.style "fill:rgb(143 143 237);stroke-width:0.5;stroke:rgb(0,0,0)"
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


viewDisplay : ( Float, Float ) -> ( Float, Float ) -> String -> Svg msg
viewDisplay ( x, y ) ( w, h ) str =
    let
        ( transX, transY ) =
            ( x + w / 2, y + h / 2 )
    in
    S.g
        []
        [ S.rect
            [ SA.x (String.fromFloat x)
            , SA.y (String.fromFloat y)
            , SA.height (String.fromFloat h)
            , SA.width (String.fromFloat w)
            , SA.rx "1"
            , SA.style "fill:rgb(143 143 237);stroke-width:0.5;stroke:rgb(0,0,0)"
            , SA.fillOpacity "0"
            ]
            []
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            ]
            [ S.text str ]
        ]



{-
   This is the main view function for calculator
   It takes all the data required to show a calculator
   1. The attributes like height, width etc
   2. The history
   3. The final answer
   4. All the buttons to be shown

-}


viewCalc : List (Attribute CalcConfig) -> String -> String -> List ( String, msg ) -> Html msg
viewCalc edits history answer buttons =
    let
        config =
            List.foldl (\f a -> f a) defCalcConfig edits

        cols =
            4

        buttonGroups =
            splitAtEvery cols buttons

        coordButtons =
            getCoordinatedList 50 buttonGroups

        maxW =
            (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                |> Maybe.withDefault 500
            )
                + width
    in
    S.svg
        [ SA.viewBox ("0 0 " ++ String.fromFloat config.width ++ " " ++ String.fromFloat config.height)
        , SA.height "80vh"
        , SA.style "border-style:solid;padding:10px;"
        ]
        (viewDisplay ( 0, 0 ) ( maxW, 20 ) history
            :: viewDisplay ( 0, 25 ) ( maxW, 20 ) answer
            :: List.map
                (\( c, ( l, m ) ) -> viewButtons c l m)
                coordButtons
        )



-- I N T E R N A L      H E L P E R S


splitAtEvery : Int -> List a -> List (List a)
splitAtEvery index lst =
    case lst of
        [] ->
            []

        _ ->
            List.append [ List.take index lst ] (splitAtEvery index (List.drop index lst))


getCoordinatedList : Float -> List (List ( String, msg )) -> List ( ( Float, Float ), ( String, msg ) )
getCoordinatedList initY lst =
    let
        p =
            10

        assignXCoord y list =
            List.foldl (\e ( x, fe ) -> ( x + width + p, List.append fe [ ( ( x, y ), e ) ] )) ( 0, [] ) list
                |> Tuple.second

        ( _, yLists ) =
            List.foldl (\l ( y, ls ) -> ( y + height + p, List.append ls [ ( y, l ) ] )) ( initY, [] ) lst
    in
    List.map (\( y, l ) -> assignXCoord y l) yLists
        |> List.concat
