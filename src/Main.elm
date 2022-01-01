module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Random.Extra



-- MAIN


main : Program () Model Msg
main =
  Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }



-- MODEL


type alias Level =
  { word: String
  , americanImageName: String
  , britishImageName: String
  }


allLevels : Array Level
allLevels =
  Array.fromList
  [ Level "Chips" "potato_chips.jpg" "french_fries.jpg"
  ]


type Answer
  = American
  | British


answerString : Answer -> String
answerString answer =
  case answer of
    American ->
      "American"
    British ->
      "British"


type alias Model =
  { levelIndex: Int
  , correctAnswer: Answer
  , score : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 0 American 0
  , Random.generate NewAnswer ( Random.Extra.choice American British )
  )


-- UPDATE


type Msg
  = NewAnswer Answer
  | UserAnswer Answer
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewAnswer answer ->
      ( { model | correctAnswer = answer }
      , Cmd.none
      )
    UserAnswer answer ->
      ( case answer of
          American ->
            { model
            | levelIndex = model.levelIndex + 1
            , score = model.score + 1
            }
          British ->
            { model
            | levelIndex = model.levelIndex + 1
            , score = model.score - 1
            }
      , Random.generate NewAnswer ( Random.Extra.choice American British )
      )

    Reset ->
      init ()


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
  [ div [] [ text ( "Score: " ++ String.fromInt model.score ) ]
  , case ( Array.get model.levelIndex allLevels ) of
      Just level ->
        div []
          [ div [] [text ( "Choose the " ++ ( answerString model.correctAnswer ) ++ " meaning" )]
          , viewImage level.americanImageName American
          , div [] [text level.word]
          , viewImage level.britishImageName British
          ]

      Nothing ->
        div [] [ text "Game over!" ]
  , button [ onClick Reset ] [ text "Reset" ]
  ]

viewImage : String -> Answer -> Html Msg
viewImage imageName answer =
  img
    [ src ( "resources/" ++ imageName )
    , onClick ( UserAnswer answer )
    , height 100
    ] []
