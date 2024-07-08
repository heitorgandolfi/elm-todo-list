port module Main exposing (..)

import Browser
import FontAwesome as Icon
import FontAwesome.Attributes as Icon
import FontAwesome.Solid as Icon
import Html exposing (Html, button, div, h1, i, input, p, section, span, text)
import Html.Attributes exposing (alt, class, classList, id, name, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Random
import UUID exposing (generator, toString)



-- MAIN


main : Program (List Task) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = \_ -> Sub.none }



-- MODEL


type alias Task =
    { id : String, description : String, isDone : Bool }


type alias Error =
    { state : Bool, message : String }


type alias Model =
    { tasks : List Task, newTaskDescription : String, errorState : Error, randomNumber : Int, completedTasksCount : Int }


init : List Task -> ( Model, Cmd Msg )
init flags =
    ( { initialModel
        | tasks = flags
        , completedTasksCount = updateCompletedTasksCount <| flags
      }
    , generateRandomIntCmd
    )


initialModel : Model
initialModel =
    { tasks = []
    , newTaskDescription = ""
    , errorState = Error False ""
    , randomNumber = 0
    , completedTasksCount = 0
    }



-- UPDATE


port storeTasks : Encode.Value -> Cmd msg


encodeTask : Task -> Encode.Value
encodeTask task =
    Encode.object
        [ ( "id", Encode.string task.id )
        , ( "description", Encode.string task.description )
        , ( "isDone", Encode.bool task.isDone )
        ]


saveTasksOnLocalStorage : List Task -> Cmd msg
saveTasksOnLocalStorage tasks =
    Encode.list encodeTask tasks
        |> storeTasks


generateRandomIntCmd : Cmd Msg
generateRandomIntCmd =
    Random.generate RandomNumberGenerated (Random.int 1 100000)


type Msg
    = CreateTask
    | UpdateNewTaskDescription String
    | DeleteTask String
    | UpdateTask String
    | RandomNumberGenerated Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateTask ->
            if String.isEmpty model.newTaskDescription then
                ( { model | errorState = Error True "Cannot add a new task with an empty description." }, Cmd.none )

            else if String.length model.newTaskDescription > 200 then
                ( { model | errorState = Error True "Task description must be at most 200 characters long." }, Cmd.none )

            else
                let
                    taskId =
                        Random.step generator (Random.initialSeed model.randomNumber)
                            |> Tuple.first
                            |> toString

                    newTask =
                        Task taskId model.newTaskDescription False

                    updatedTasks =
                        newTask :: model.tasks
                in
                ( { model | tasks = updatedTasks, newTaskDescription = "", errorState = Error False "" }
                , Cmd.batch [ saveTasksOnLocalStorage updatedTasks, generateRandomIntCmd ]
                )

        UpdateNewTaskDescription newTask ->
            ( { model | newTaskDescription = newTask }, Cmd.none )

        DeleteTask taskId ->
            let
                updatedTasks =
                    filterTaskToDelete taskId model.tasks
            in
            ( { model | tasks = updatedTasks }, saveTasksOnLocalStorage updatedTasks )

        UpdateTask taskId ->
            let
                updatedTasks =
                    updateTask taskId model.tasks

                completedTasks =
                    updateCompletedTasksCount updatedTasks
            in
            ( { model | tasks = updatedTasks, completedTasksCount = completedTasks }, saveTasksOnLocalStorage updatedTasks )

        RandomNumberGenerated number ->
            ( { model | randomNumber = number }, Cmd.none )


filterTaskToDelete : String -> List Task -> List Task
filterTaskToDelete taskId tasks =
    List.filter (\task -> task.id /= taskId) tasks


updateTask : String -> List Task -> List Task
updateTask taskId tasks =
    List.map
        (\task ->
            if task.id == taskId then
                { task | isDone = not task.isDone }

            else
                task
        )
        tasks


updateCompletedTasksCount : List Task -> Int
updateCompletedTasksCount tasks =
    List.length (List.filter .isDone tasks)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "content" ]
            [ renderNewTaskInput <| model
            , div [ id "divide" ] []
            , renderTasks <| model.tasks
            ]
        ]


renderNewTaskInput : Model -> Html Msg
renderNewTaskInput model =
    section []
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
        , div [ class "task-counter" ]
            [ p [] [ text "Total Tasks: ", span [ class "total-tasks-counter" ] [ text (String.fromInt <| List.length model.tasks) ] ]
            , p [] [ text "Completed Tasks: ", span [ class "completed-tasks-counter" ] [ text (String.fromInt model.completedTasksCount) ] ]
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
