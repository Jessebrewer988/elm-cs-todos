port module Main exposing (..)

import Browser
import Dialog
import Html exposing (Attribute, Html, button, div, h1, hr, input, label, li, menu, p, span, text, ul)
import Html.Attributes exposing (checked, class, classList, for, id, placeholder, style, type_, value)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode as Json



-- MAIN


port saveTodos : List Todo -> Cmd msg


port loadTodos : (List Todo -> msg) -> Sub msg


type alias Model =
    { title : String
    , text : String
    , todos : List Todo
    , deleteCandidate : Maybe Int
    }


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }



--UPDATE


type Msg
    = ToggleIsDone Int Bool
    | AddTodo
    | UpdateText String
    | Keydown Int
    | LoadTodos (List Todo)
    | RequestDelete Int
    | CancelDelete
    | ConfirmDelete Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleIsDone id isChecked ->
            let
                newModel =
                    { model | todos = List.map (toggleWith id isChecked) model.todos }
            in
            ( newModel, saveTodos newModel.todos )

        AddTodo ->
            if String.trim model.text /= "" then
                let
                    newModel =
                        addTodo model
                in
                ( newModel, saveTodos newModel.todos )

            else
                ( model, Cmd.none )

        Keydown key ->
            if key == 13 then
                if String.trim model.text /= "" then
                    let
                        newModel =
                            addTodo model
                    in
                    ( newModel, saveTodos newModel.todos )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        LoadTodos todos ->
            ( { model | todos = todos }, saveTodos todos )

        UpdateText text ->
            ( { model | text = text }, Cmd.none )

        RequestDelete id ->
            ( { model | deleteCandidate = Just id }, Cmd.none )

        CancelDelete ->
            ( { model | deleteCandidate = Nothing }, Cmd.none )

        ConfirmDelete id ->
            ( { model
                | todos = List.filter (\t -> t.id /= id) model.todos
                , deleteCandidate = Nothing
              }
            , saveTodos (List.filter (\t -> t.id /= id) model.todos)
            )


addTodo : Model -> Model
addTodo model =
    let
        newId =
            case List.maximum (List.map .id model.todos) of
                Just maxId ->
                    maxId + 1

                Nothing ->
                    0
    in
    { model
        | todos = model.todos ++ [ { id = newId, title = String.trim model.text, completed = False } ]
        , text = ""
    }


toggleWith : Int -> Bool -> Todo -> Todo
toggleWith id isChecked todo =
    if todo.id == id then
        { todo | completed = isChecked }

    else
        todo


onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
    on "keydown" (Json.map tagger keyCode)



-- VIEW


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ class "todo" ]
        [ div [ class "cs-checkbox" ]
            [ input
                [ id ("checkbox" ++ String.fromInt todo.id)
                , type_ "checkbox"
                , checked todo.completed
                , onCheck (ToggleIsDone todo.id)
                , class "cs-checkbox"
                ]
                []
            , label [ for ("checkbox" ++ String.fromInt todo.id), class "cs-checkbox__label" ]
                [ span
                    [ classList [ ( "completed", todo.completed ) ] ]
                    [ text todo.title ]
                ]
            , button
                [ class "cs-btn delete-btn"
                , onClick (RequestDelete todo.id)
                ]
                [ text "X" ]
            , hr [ class "cs-hr" ] []
            ]
        ]


viewConfirmDialog : Model -> Html Msg
viewConfirmDialog model =
    case model.deleteCandidate of
        Nothing ->
            text ""

        Just id ->
            Dialog.view (Just (deleteDialogConfig id))


deleteDialogConfig : Int -> Dialog.Config Msg
deleteDialogConfig id =
    { closeMessage = Just CancelDelete
    , containerClass = Just "cs-dialog"
    , header =
        Just <|
            div [ class "heading" ]
                [ div [ class "wrapper" ]
                    [ div [ class "icon" ] []
                    , p [ class "text" ] [ text "Confirm Delete" ]
                    ]
                ]
    , body =
        Just <|
            div [ class "content" ]
                [ text "Are you sure you want to delete this todo? This action cannot be undone." ]
    , footer =
        Just <|
            menu [ class "footer-btns" ]
                [ button
                    [ class "cs-btn"
                    , onClick (ConfirmDelete id)
                    ]
                    [ text "OK" ]
                , button
                    [ class "cs-btn"
                    , onClick CancelDelete
                    ]
                    [ text "Cancel" ]
                ]
    }


viewDeleteModal : Model -> Html Msg
viewDeleteModal model =
    case model.deleteCandidate of
        Nothing ->
            text ""

        Just id ->
            Dialog.view (Just (deleteDialogConfig id))


viewProgress : Model -> Html Msg
viewProgress model =
    let
        total =
            List.length model.todos

        completed =
            List.length (List.filter .completed model.todos)

        percentage =
            if total == 0 then
                0

            else
                (completed * 100) // total

        statusMessage =
            if total == 0 then
                "Add todo to see your progress"

            else if percentage == 100 then
                "All tasks complete - great job!"

            else
                "Keep up the good work!"
    in
    div [ class "cs-progress-container" ]
        [ p [ class "cs-progress-text" ] [ text statusMessage ]
        , div [ class "cs-progress-bar" ]
            [ div
                [ class "bars"
                , style "width" (String.fromInt percentage ++ "%")
                ]
                []
            ]
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "footer" ]
        [ viewProgress model
        , text "Made with Elm and CS16.css"
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ] [ text model.title ]
        , ul [ class "todos" ] (List.map viewTodo model.todos)
        , viewDeleteModal model
        , span [ class "input-container" ]
            [ input [ id "input", class "cs-input", type_ "text", placeholder "Add todo", value model.text, onInput UpdateText, onKeydown Keydown ] []
            , button
                [ onClick AddTodo
                , class "cs-btn"
                ]
                [ text "Add" ]
            ]
        , viewFooter model
        ]



-- MAIN


init : Maybe (List Todo) -> ( Model, Cmd Msg )
init maybeTodos =
    ( { title = "Elm Todos"
      , text = ""
      , todos = maybeTodos |> Maybe.withDefault [ { id = 0, completed = False, title = "Learn Elm" } ]
      , deleteCandidate = Nothing
      }
    , Cmd.none
    )


main : Program (Maybe (List Todo)) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> loadTodos LoadTodos
        }
