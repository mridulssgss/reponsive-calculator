module View.Attributes exposing (..)


type alias Attribute c =
    c -> c



{- Function to set the width attribute of CalcConfig -}


width : Float -> Attribute { c | width : Float }
width f =
    \cc ->
        { cc | width = f }



{- Function to set the height attribute of CalcConfig -}


height : Float -> Attribute { c | height : Float }
height f =
    \cc ->
        { cc | height = f }
