module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, input, p, section, span, text)
import Html.Attributes exposing (class, classList, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> ( init, Cmd.none ), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Task =
    { id : Int, description : String }


type alias Error =
    { state : Bool, message : String }


type alias Model =
    { tasks : List Task, newTaskDescription : String, errorState : Error }


init : Model
init =
    { tasks = [], newTaskDescription = "", errorState = { state = False, message = "" } }



-- UPDATE


type Msg
    = CreateTask
    | UpdateNewTaskDescription String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTask ->
            case model.newTaskDescription of
                "" ->
                    ( { model | errorState = { state = True, message = "Test" } }, Cmd.none )

                _ ->
                    let
                        taskId =
                            List.length model.tasks + 1

                        -- This is a temporary solution to generate unique IDs. I will change it later when i implemented the delete function.
                        newTask =
                            Task taskId model.newTaskDescription
                    in
                    ( { model | tasks = newTask :: model.tasks, newTaskDescription = "", errorState = { state = False, message = "" } }, Cmd.none )

        UpdateNewTaskDescription newTask ->
            ( { model | newTaskDescription = newTask }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "content" ]
            [ section []
                [ h1 [] [ text "ToDo List" ]
                , div [ class "input-group" ]
                    [ input
                        [ placeholder "Your next task is..."
                        , type_ "text"
                        , id "new-task-input"
                        , classList [ ( "input-error", model.errorState.state ) ]
                        , name "new-task-input"
                        , value model.newTaskDescription
                        , onInput UpdateNewTaskDescription
                        ]
                        []
                    , button [ onClick CreateTask ] [ text "+" ]
                    ]
                ]
            , div [ id "divide" ] []
            , renderTasks <| model.tasks
            ]
        ]


renderTasks : List Task -> Html Msg
renderTasks tasks =
    if List.isEmpty tasks then
        div [ class "empty-state" ]
            [ p [] [ text "You don't have any tasks yet.\nAdd some and they will appear here." ]
            ]

    else
        section [] (List.map taskCardView <| tasks)


taskCardView : Task -> Html Msg
taskCardView task =
    div [ class "task-card" ]
        [ span [] [ text "ícone" ]
        , p [] [ text task.description ]
        ]
