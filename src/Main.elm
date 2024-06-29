module Main exposing (..)

import Browser
import FontAwesome as Icon exposing (Icon)
import FontAwesome.Attributes as Icon
import FontAwesome.Solid as Icon
import Html exposing (Html, button, div, h1, i, img, input, p, section, span, text)
import Html.Attributes exposing (alt, class, classList, id, name, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = \_ -> ( init, Cmd.none ), update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL
-- Change the id type to String to use the UUID library


type alias Task =
    { id : Int, description : String, isDone : Bool }


type alias Error =
    { state : Bool, message : String }


type alias Model =
    { tasks : List Task, newTaskDescription : String, errorState : Error }


deleteIconPath : String
deleteIconPath =
    "../assets/icons/delete-icon.png"


init : Model
init =
    { tasks =
        [ Task 1 "Task 1" True
        , Task 2 "Task 2" False
        , Task 3 "Task 3" True
        ]
    , newTaskDescription = ""
    , errorState = Error False ""
    }



-- UPDATE


type Msg
    = CreateTask
    | UpdateNewTaskDescription String
    | DeleteTask Int
    | UpdateTask Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTask ->
            case model.newTaskDescription of
                "" ->
                    ( { model | errorState = Error True "It is not possible to insert a new blank task." }, Cmd.none )

                _ ->
                    let
                        -- This is a temporary solution to generate unique IDs. I will change it later when i implemented the delete function.
                        taskId =
                            List.length model.tasks + 1

                        newTask =
                            Task taskId model.newTaskDescription False
                    in
                    ( { model
                        | tasks = newTask :: model.tasks
                        , newTaskDescription = ""
                        , errorState = Error False ""
                      }
                    , Cmd.none
                    )

        UpdateNewTaskDescription newTask ->
            ( { model | newTaskDescription = newTask }, Cmd.none )

        DeleteTask taskId ->
            ( { model | tasks = filterTaskToDelete taskId model.tasks }, Cmd.none )

        UpdateTask taskId ->
            let
                completedTask =
                    updateTask taskId model.tasks
            in
            ( { model | tasks = completedTask }, Cmd.none )


filterTaskToDelete : Int -> List Task -> List Task
filterTaskToDelete taskId =
    List.filter (\task -> task.id /= taskId)


updateTask : Int -> List Task -> List Task
updateTask taskId =
    List.map
        (\task ->
            if task.id == taskId then
                { task | isDone = not task.isDone }

            else
                task
        )



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
                , renderErrorMessage model.errorState.message
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


renderErrorMessage : String -> Html Msg
renderErrorMessage message =
    if message == "" then
        text ""

    else
        p [ class "error-message" ] [ text message ]


taskCardView : Task -> Html Msg
taskCardView task =
    div [ class "task-card" ]
        [ div []
            [ i [ onClick (UpdateTask task.id), classList [ ( "task-done", task.isDone ) ] ]
                [ if task.isDone then
                    Icon.view Icon.checkCircle

                  else
                    Icon.view Icon.circle
                ]
            , p [ classList [ ( "task-done", task.isDone ) ] ] [ text task.description ]
            ]
        , i [ onClick <| DeleteTask task.id, alt "delete task icon", class "remove-task-icon" ] [ Icon.view Icon.trashCan ]
        ]
