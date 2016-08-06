module Scorekeeper exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (..)
import String
import Bootstrap.Html exposing (..)


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    , isEdit : Bool
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Save ->
            if (String.isEmpty model.name) then
                model
            else
                save model

        Cancel ->
            cancel model

        Input nm ->
            Debug.log "Input updateded model"
                { model | name = nm }

        Score player points ->
            score model player points

        Edit player ->
            select model player

        DeletePlay play ->
            deletePlay model play


cancel : Model -> Model
cancel model =
    let
        newPlayers =
            List.map
                (\p -> { p | isEdit = False })
                model.players
    in
        { model | name = "", playerId = Nothing, players = newPlayers }


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays

        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - 1 * play.points }
                    else
                        player
                )
                model.players
    in
        { model | plays = newPlays, players = newPlayers }


select : Model -> Player -> Model
select model player =
    let
        newPlayers =
            List.map
                (\p ->
                    if p.id == player.id then
                        { p | isEdit = True }
                    else
                        p
                )
                model.players
    in
        { model | name = player.name, playerId = Just player.id, players = newPlayers }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
        { model | players = newPlayers, plays = play :: model.plays }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map (changeNamePlayers model.name id) model.players

        newPlays =
            List.map (changeNamePlays model.name id) model.plays
    in
        { model | players = newPlayers, plays = newPlays }


changeNamePlayers : String -> Int -> Player -> Player
changeNamePlayers nm id player =
    if player.id == id then
        { player | name = nm }
    else
        player


changeNamePlays : String -> Int -> Play -> Play
changeNamePlays nm id play =
    if play.id == id then
        { play | name = nm }
    else
        play


add : Model -> Model
add model =
    let
        player =
            Player (List.length model.players) model.name 0 False

        newPlayers =
            player :: model.players
    in
        { model | players = newPlayers, name = "" }


view : Model -> Html Msg
view model =
    container_
        [ h1 [] [ text "Score Keeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    div [ class "panel-heading row" ]
        [ div [ class "col-md-9" ] [ text "Plays" ]
        , div [ class "col-md-3" ] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    List.map play model.plays |> div []


play : Play -> Html Msg
play play =
    div [ class "row" ]
        [ div [ class "col-md-1 glyphicon glyphicon-remove", onClick (DeletePlay play) ] []
        , div [ class "col-md-8" ] [ text play.name ]
        , div [ class "col-md-3 text-left" ] [ span [] [ text (toString play.points) ] ]
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    div [ class "panel-heading row" ]
        [ div [ class "col-md-11" ] [ text "Name" ]
        , div [ class "col-md-1" ] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    List.sortBy .name model.players
        |> List.map player
        |> div []


player : Player -> Html Msg
player player =
    div
        [ class
            ("row"
                ++ if player.isEdit then
                    " edit-row"
                   else
                    ""
            )
        ]
        [ div [ class "col-md-1 glyphicon glyphicon-edit", onClick (Edit player) ] []
        , div [ class "col-md-8" ] [ text player.name ]
        , div [ class "col-md-1" ] [ button [ type' "button", onClick (Score player 2) ] [ text "2pt" ] ]
        , div [ class "col-md-1" ] [ button [ type' "button", onClick (Score player 3) ] [ text "3pt" ] ]
        , div [ class "col-md-1" ] [ span [] [ text (toString player.points) ] ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.sum (List.map (\play -> play.points) model.plays)
    in
        div [ class "row" ]
            [ div [ class "col-md-11 text-right" ] [ text "Total:" ]
            , div [ class "col-md-1" ] [ text (toString total) ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ class "form-inline", onSubmit Save ]
        [ div
            [ class "form-group"
            ]
            [ input
                [ type' "text"
                , class
                    (case model.playerId of
                        Nothing ->
                            ""

                        Just id ->
                            "edit-input"
                    )
                , placeholder "Add/Edit Player ..."
                , onInput Input
                , value model.name
                ]
                []
            ]
        , btnPrimary' "pull-right" { btnParam | label = Just "Cancel" } Cancel
        , btnSubmitPrimary' "pull-right" { btnParam | label = Just "Save" }
        ]


main : Program Never
main =
    Html.App.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
