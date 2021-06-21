module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, disabled, id, value)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { display : String
    , shouldAppendNum : Bool
    , tape : TapeState
    }


type TapeState
    = Nada
    | FirstState { firstNum : Float, firstOp : Operator }
    | SecondState { firstNum : Float, firstOp : Operator, secondNum : Float, secondOp : Operator }


init : Model
init =
    { display = "0", shouldAppendNum = False, tape = Nada }


type Msg
    = Digit Int
    | Decimal
    | Operation Operator
    | Eq
    | Clear


type Operator
    = Add
    | Sub
    | Mul
    | Div


view : Model -> Html Msg
view model =
    div [ id "calculator" ]
        [ div [ id "response-pane" ] [ text model.display ]
        , div [ id "modifier-buttons" ]
            [ button
                [ id "ac", class "modifiers", onClick Clear ]
                [ text "AC" ]
            , button
                [ class "modifiers", disabled True ]
                [ text <| String.fromChar <| Char.fromCode 177 ]
            , button
                [ class "modifiers", disabled True ]
                [ text "%" ]
            ]
        , div [ id "operation-buttons" ]
            [ button
                [ class "operations", value "division", onClick <| Operation Div ]
                [ text <| String.fromChar <| Char.fromCode 247 ]
            , button
                [ class "operations", value "multiplication", onClick <| Operation Mul ]
                [ text <| String.fromChar <| Char.fromCode 215 ]
            , button
                [ class "operations", value "subtraction", onClick <| Operation Sub ]
                [ text "-" ]
            , button
                [ class "operations", value "addition", onClick <| Operation Add ]
                [ text "+" ]
            , button
                [ class "operations", value "answer", onClick Eq ]
                [ text "=" ]
            ]
        , div [ id "digit-buttons" ]
            [ button [ class "digits", onClick <| Digit 7 ] [ text "7" ]
            , button [ class "digits", onClick <| Digit 8 ] [ text "8" ]
            , button [ class "digits", onClick <| Digit 9 ] [ text "9" ]
            , button [ class "digits", onClick <| Digit 4 ] [ text "4" ]
            , button [ class "digits", onClick <| Digit 5 ] [ text "5" ]
            , button [ class "digits", onClick <| Digit 6 ] [ text "6" ]
            , button [ class "digits", onClick <| Digit 1 ] [ text "1" ]
            , button [ class "digits", onClick <| Digit 2 ] [ text "2" ]
            , button [ class "digits", onClick <| Digit 3 ] [ text "3" ]
            , button [ class "digits", onClick <| Digit 0 ] [ text "0" ]
            , button [ class "digits", onClick Decimal ] [ text "." ]
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Operation op ->
            doMath model op

        Clear ->
            init

        Digit int ->
            if String.length model.display >= 10 then
                model

            else
                let
                    newDisplay =
                        if model.shouldAppendNum == True then
                            model.display ++ String.fromInt int

                        else
                            String.fromInt int
                in
                { model | display = newDisplay, shouldAppendNum = True }

        Decimal ->
            case String.contains "." model.display of
                True ->
                    model

                False ->
                    { model | display = model.display ++ "." }

        Eq ->
            -- TODO [7, -, =] doesn't work correctly
            -- TODO [3, + 2, =, =, =] doesn't work correctly
            doMath model Add


doMath : Model -> Operator -> Model
doMath model op =
    case model.tape of
        Nada ->
            let
                newTape =
                    { firstNum = Maybe.withDefault 0 <| String.toFloat model.display
                    , firstOp = op
                    }
            in
            { model | tape = FirstState newTape, shouldAppendNum = False }

        -- N is any number, m is division or multiplication, and a is addition or
        -- subtraction. There are a total of 5 possible scenarios, as listed below.
        FirstState tape ->
            -- This accounts for scenarios [N, m, N, m], [N, m, N, a], and [N, a, N, a]
            if (tape.firstOp == Mul || tape.firstOp == Div) || (op == Add || op == Sub) then
                let
                    result =
                        operatorToMath tape.firstOp tape.firstNum <|
                            Maybe.withDefault 0 <|
                                String.toFloat model.display
                in
                { model
                    | display = validateDisplay result
                    , tape = FirstState { firstNum = result, firstOp = op }
                    , shouldAppendNum = False
                }

            else
                let
                    newTape =
                        { firstNum = tape.firstNum
                        , firstOp = tape.firstOp
                        , secondNum = Maybe.withDefault 0 <| String.toFloat model.display
                        , secondOp = op
                        }
                in
                { model
                    | tape = SecondState newTape
                    , shouldAppendNum = False
                }

        SecondState tape ->
            -- This accounts for scenario [N, a, N, m, N, m]
            let
                result1 =
                    operatorToMath tape.secondOp tape.secondNum <|
                        Maybe.withDefault 0 <|
                            String.toFloat model.display
            in
            if op == Mul || op == Div then
                { model
                    | display = validateDisplay result1
                    , tape = SecondState { firstNum = tape.firstNum, firstOp = tape.firstOp, secondNum = result1, secondOp = op }
                    , shouldAppendNum = False
                }

            else
                -- This account for scenario [N, a, N, m, N, a]
                let
                    result2 =
                        operatorToMath tape.firstOp tape.firstNum result1
                in
                { model
                    | display = validateDisplay result2
                    , tape = FirstState { firstNum = result2, firstOp = op }
                    , shouldAppendNum = False
                }


validateDisplay : Float -> String
validateDisplay float =
    if isInfinite float then
        "Undefined"

    else if String.length (String.fromFloat float) > 10 then
        "Error"

    else
        String.fromFloat float


operatorToMath : Operator -> (Float -> Float -> Float)
operatorToMath op =
    case op of
        Add ->
            (+)

        Sub ->
            (-)

        Mul ->
            (*)

        Div ->
            (/)
