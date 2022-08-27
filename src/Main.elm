module Main exposing (..)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import View as V
import View.Attributes as VA


type Op
    = Plus
    | Minus
    | Div
    | Multiply


type Term
    = Operand Int
    | Operator Op


type alias Model =
    { res : Maybe Int
    , stack : List Op
    , exp : String
    , postfix : List Term
    , malformed : Bool
    , showResult : Bool
    }


init =
    { res = Nothing
    , stack = []
    , exp = ""
    , postfix = []
    , malformed = False
    , showResult = False
    }


type Msg
    = Digit Int
    | Opertr Op
    | GetRes
    | AllClear


opToString : Op -> String
opToString op =
    case op of
        Plus ->
            " + "

        Minus ->
            " - "

        Multiply ->
            " * "

        Div ->
            " / "



{-
   check precedence of op1 with op2
   Op 1 > Op 2  => GT
   Op 1 <= Op 2  => LT

-}


preced : Op -> Op -> Order
preced op1 op2 =
    case ( op1, op2 ) of
        ( Plus, _ ) ->
            LT

        ( Minus, _ ) ->
            LT

        ( Multiply, Multiply ) ->
            LT

        ( Multiply, Div ) ->
            LT

        ( Multiply, _ ) ->
            GT

        ( Div, Div ) ->
            LT

        ( Div, Multiply ) ->
            LT

        ( Div, _ ) ->
            GT


popPrecedence : Op -> ( List Op, List Op ) -> ( List Op, List Op )
popPrecedence op ( stack, popped ) =
    case stack of
        [] ->
            ( op :: [], popped )

        x :: xs ->
            if preced op x == LT then
                popPrecedence op ( xs, List.append popped [ x ] )

            else
                ( op :: stack, popped )


addOp : Int -> Op -> ( List Term, List Op ) -> ( List Term, List Op )
addOp num op ( postfix, stack ) =
    let
        ( newStack, popped ) =
            popPrecedence op ( stack, [] )
    in
    ( List.append postfix (Operand num :: List.map Operator popped), newStack )


calc : Op -> List Int -> List Int
calc op stack =
    case stack of
        a :: b :: abs ->
            case op of
                Plus ->
                    b + a :: abs

                Minus ->
                    b - a :: abs

                Multiply ->
                    b * a :: abs

                Div ->
                    b // a :: abs

        _ ->
            stack


evalPostFix : ( List Term, List Int ) -> ( List Term, List Int )
evalPostFix ( postfix, stack ) =
    case postfix of
        [] ->
            ( postfix, stack )

        x :: xs ->
            case x of
                Operand n ->
                    evalPostFix ( xs, n :: stack )

                Operator op ->
                    evalPostFix ( xs, calc op stack )


evaluate : Int -> ( List Term, List Op ) -> Int
evaluate num ( postfix, stack ) =
    let
        finPostfix =
            List.append postfix (Operand num :: List.map Operator stack)

        ( _, evalStack ) =
            evalPostFix ( finPostfix, [] )
    in
    Maybe.withDefault 0 (List.head evalStack)



{-
   This is the main update function. It functions as follows
   1. Converts the Infix expression to Postfix expression while the input is being given
   2. Pops stack and finalises the Postfix expression and then evaluates to show the result.
-}


update : Msg -> Model -> Model
update msg model =
    case msg of
        Digit d ->
            { model
                | res = Just (Maybe.withDefault 0 model.res * 10 + d)
                , exp =
                    if model.showResult then
                        String.fromInt (Maybe.withDefault 0 model.res * 10 + d)

                    else
                        model.exp ++ String.fromInt d
                , showResult = False
            }

        Opertr op ->
            case model.res of
                Nothing ->
                    { model | malformed = True }

                Just num ->
                    let
                        ( postfix, stack ) =
                            addOp num op ( model.postfix, model.stack )
                    in
                    { model
                        | res = Nothing
                        , stack = stack
                        , postfix = postfix
                        , exp = model.exp ++ opToString op
                    }

        GetRes ->
            case model.res of
                Nothing ->
                    { model | malformed = True }

                Just num ->
                    { model
                        | res = Just (evaluate num ( model.postfix, model.stack ))
                        , stack = []
                        , postfix = []
                        , showResult = True
                    }

        AllClear ->
            init


controls =
    [ ( "9", Digit 9 )
    , ( "8", Digit 8 )
    , ( "7", Digit 7 )
    , ( "AC", AllClear )
    , ( "6", Digit 6 )
    , ( "5", Digit 5 )
    , ( "4", Digit 4 )
    , ( "+", Opertr Plus )
    , ( "3", Digit 3 )
    , ( "2", Digit 2 )
    , ( "1", Digit 1 )
    , ( "-", Opertr Minus )
    , ( "0", Digit 0 )
    , ( "X", Opertr Multiply )
    , ( "/", Opertr Div )
    , ( "=", GetRes )
    ]


view : Model -> Html Msg
view model =
    H.div
        [ HA.style "height" "100%"
        , HA.style "display" "flex"
        , HA.style "justify-content" "center"
        , HA.style "padding" "10px"
        ]
        [ V.viewCalc
            [ VA.width 150
            , VA.height 200
            ]
            model.exp
            (String.fromInt (Maybe.withDefault 0 model.res))
            controls
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
