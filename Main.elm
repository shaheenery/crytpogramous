module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    String


model : Model
model =
    "Spicy jalapeno bacon ipsum dolor amet porchetta ham hock shank filet mignon brisket meatball tongue frankfurter fatback strip steak. Capicola ham bacon pork belly.  The quick brown fox jumped over the lazy dog."


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- UPDATE


type Msg
    = EditPuzzle String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditPuzzle puzzle ->
            puzzle



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ puzzleInputView model
        , puzzleBoardView model
        ]


puzzleInputView : String -> Html Msg
puzzleInputView puzzleString =
    input
        [ type_ "text"
        , class "form-control puzzle-input"
        , placeholder "Enter the puzzle"
        , onInput (EditPuzzle)
        , Html.Attributes.value puzzleString
        ]
        []


puzzleBoardView : Model -> Html Msg
puzzleBoardView model =
    let
        words =
            String.split " " model
    in
        div [ class "puzzle-board" ] (List.map (\word -> wordView word) words)


wordView : String -> Html Msg
wordView word =
    let
        chars =
            String.toList word
    in
        div [ class "word" ] (List.map (\char -> charView char) chars)


charView : Char -> Html Msg
charView char =
    let
        displayChar =
            case char of
                ' ' ->
                    " "

                _ ->
                    String.fromChar char
    in
        span [] [ text displayChar ]
