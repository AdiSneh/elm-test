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
  [ Level "chips" "potato_chips.jpg" "french_fries.jpg"
  , Level "torch" "torch.jpg" "flashlight.jpg"
  , Level "football" "football.jpg" "soccer_ball.jpg"
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
  , Random.generate NewCorrectAnswer (Random.Extra.choice American British)
  )


-- UPDATE


type Msg
  = NewCorrectAnswer Answer
  | UserAnswer Answer
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewCorrectAnswer answer ->
      ( { model | correctAnswer = answer }
      , Cmd.none
      )
    UserAnswer answer ->
      ( { model
        | levelIndex = model.levelIndex + 1
        , score = model.score + getLevelScore model.correctAnswer answer
        }
      , Random.generate NewCorrectAnswer (Random.Extra.choice American British)
      )

    Reset ->
      init ()


getLevelScore : Answer -> Answer -> Int
getLevelScore correctAnswer userAnswer =
  if userAnswer == correctAnswer then
    1
  else
    -1


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
  [ div [] [ text ("Score: " ++ String.fromInt model.score) ]
  , case (Array.get model.levelIndex allLevels) of
      Just level ->
        div []
          [ div [] [ text ("Choose the " ++ (answerString model.correctAnswer) ++ " meaning for " ++ level.word) ]
          , viewImage level.americanImageName American
          , viewImage level.britishImageName British
          ]

      Nothing ->
        div [] [ text "Game over!" ]
  , button [ onClick Reset ] [ text "Start over" ]
  ]


viewImage : String -> Answer -> Html Msg
viewImage imageName answer =
  img
    [ src ("/resources/" ++ imageName)
    , onClick (UserAnswer answer)
    , height 100
    ] []
