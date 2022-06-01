module View.Attributes exposing (..)


type alias Attribute c =
    c -> c



{- Function to set the width attribute of CalcConfig -}


width : Float -> Attribute c
width _ =
    \cc ->
        cc



{- Function to set the height attribute of CalcConfig -}


height : Float -> Attribute c
height _ =
    \cc ->
        cc
