module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- MODEL


type alias Model =
    { puzzle : String
    , selected : Maybe Char
    }


model : Model
model =
    { puzzle = "Spicy jalapeno bacon ipsum dolor amet porchetta ham hock shank filet mignon brisket meatball tongue frankfurter fatback strip steak. Capicola ham bacon pork belly.  The quick brown fox jumped over the lazy dog."
    , selected = Nothing
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- UPDATE


type Msg
    = EditPuzzle String
    | Highlight (Maybe Char)


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditPuzzle puzzle ->
            { model | puzzle = puzzle }

        Highlight letter ->
            { model | selected = letter }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ puzzleInputView model
        , puzzleBoardView model
        ]


puzzleInputView : Model -> Html Msg
puzzleInputView model =
    div [ class "form-row" ]
        [ div [ class "col-sm-11" ] [ puzzleTexboxView model ]
        , div [ class "col-sm-1" ] [ puzzleResetView ]
        ]


puzzleTexboxView : Model -> Html Msg
puzzleTexboxView model =
    input
        [ type_ "text"
        , class "form-control puzzle-input"
        , placeholder "Enter the puzzle"
        , onInput (EditPuzzle)
        , Html.Attributes.value model.puzzle
        ]
        []


puzzleResetView : Html Msg
puzzleResetView =
    button
        [ onClick (EditPuzzle "")
        , class "form-control"
        ]
        [ text "Clear" ]


puzzleBoardView : Model -> Html Msg
puzzleBoardView model =
    let
        words =
            String.split " " model.puzzle
    in
        div [ class "puzzle-board" ] (List.map (\word -> wordView word model.selected) words)


wordView : String -> Maybe Char -> Html Msg
wordView word selected =
    let
        chars =
            String.toList word
    in
        div [ class "word" ] (List.map (\char -> charView char selected) chars)


charView : Char -> Maybe Char -> Html Msg
charView char selected =
    let
        displayChar =
            case char of
                ' ' ->
                    " "

                _ ->
                    String.fromChar char

        classAttrs =
            if (anySelected char selected) then
                [ class "highlight" ]
            else
                []

        events =
            [ onMouseEnter (Highlight (Just char))
            , onMouseLeave (Highlight Nothing)
            ]

        attrs =
            classAttrs ++ events
    in
        span attrs [ text displayChar ]


anySelected : Char -> Maybe Char -> Bool
anySelected aChar selected =
    case selected of
        Just selChar ->
            (aChar == selChar)

        Nothing ->
            False
