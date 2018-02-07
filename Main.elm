module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Dict exposing (..)


-- MODEL


type alias Model =
    { puzzle : String
    , selected : Maybe Char
    , mapping : Dict Char Char
    , solution : String
    }


alphas : Dict Char Char
alphas =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        |> List.map (\char -> ( char, ' ' ))
        |> Dict.fromList


model : Model
model =
    { puzzle = "Spicy jalapeno bacon ipsum dolor amet porchetta ham hock shank filet mignon brisket meatball tongue frankfurter fatback strip steak. Capicola ham bacon pork belly.  The quick brown fox jumped over the lazy dog."
    , selected = Nothing
    , mapping = alphas
    , solution = ""
    }


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- UPDATE


type Msg
    = EditPuzzle String
    | Highlight (Maybe Char)
    | UpdateMapping Char String


update : Msg -> Model -> Model
update msg model =
    case msg of
        EditPuzzle puzzle ->
            { model | puzzle = puzzle }

        Highlight letter ->
            { model | selected = letter }

        UpdateMapping letter otherLetter ->
            let
                toMap =
                    case (String.uncons otherLetter) of
                        Just ( aChar, _ ) ->
                            aChar

                        Nothing ->
                            ' '

                newDict =
                    (Dict.insert letter toMap model.mapping)
            in
                { model | mapping = newDict }



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
        , Attr.value model.puzzle
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
        div [ class "puzzle-board" ] (List.map (\word -> wordView word model.selected model.mapping) words)


wordView : String -> Maybe Char -> Dict Char Char -> Html Msg
wordView word selected mapping =
    let
        chars =
            String.toList word
    in
        div [ class "word" ] (List.map (\char -> charView char selected mapping) chars)



--this just feels bad, readability kind of stinks


charView : Char -> Maybe Char -> Dict Char Char -> Html Msg
charView char selected mapping =
    let
        displayChar =
            case char of
                ' ' ->
                    " "

                _ ->
                    String.fromChar char

        classAttrs =
            if (anySelected char selected) then
                [ class "highlight letter-space" ]
            else
                [ class "letter-space" ]

        events =
            [ onMouseEnter (Highlight (Just char))
            , onMouseLeave (Highlight Nothing)
            ]

        mappedChar =
            case (Dict.get char mapping) of
                Just ' ' ->
                    ""

                Just aChar ->
                    String.fromChar aChar

                _ ->
                    ""

        attrs =
            classAttrs ++ events
    in
        div attrs
            [ div [ class "proposed" ]
                [ input
                    [ type_ "text"
                    , value mappedChar
                    , onInput (UpdateMapping char)
                    , maxlength 1
                    , Attr.size 1
                    ]
                    []
                ]
            , div
                []
                [ text displayChar ]
            ]


anySelected : Char -> Maybe Char -> Bool
anySelected aChar selected =
    case selected of
        Just selChar ->
            (aChar == selChar)

        Nothing ->
            False
