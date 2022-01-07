module Main exposing (..)

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


allLevels : List Level
allLevels =
  [ Level "chips" "potato_chips.jpg" "french_fries.jpg"
  , Level "torch" "torch.jpg" "flashlight.jpg"
  , Level "football" "football.jpg" "soccer_ball.jpg"
  , Level "biscuit" "biscuit_bread.jpg" "biscuit_cookie.jpg"
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
  { levelNumber: Int
  , levels: List Level
  , correctAnswer: Answer
  , score: Int
  , lives: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1 allLevels American 0 3
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
      ( updateWithUserAnswer model answer
          |> updateLevels
          |> updateLevelNumber
      , Random.generate NewCorrectAnswer (Random.Extra.choice American British)
      )

    Reset ->
      init ()


updateWithUserAnswer : Model -> Answer -> Model
updateWithUserAnswer model userAnswer =
  if userAnswer == model.correctAnswer then
    { model | score = model.score + 1 }
  else
    { model | lives = model.lives - 1 }


updateLevels : Model -> Model
updateLevels model =
  if model.lives > 0 then
    case List.tail model.levels of
      Just new_levels ->
        { model | levels = new_levels }
      Nothing ->
        model
  else
    model


updateLevelNumber : Model -> Model
updateLevelNumber model =
  if List.isEmpty model.levels || model.lives <= 0 then
    model
  else
    { model | levelNumber = model.levelNumber + 1 }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
  [ div [] [ text ("Level " ++ String.fromInt model.levelNumber) ]
  , div [] [ text ("Score: " ++ String.fromInt model.score) ]
  , div [] [ text ("Lives: " ++ String.fromInt model.lives) ]
  , if model.lives == 0 then
      div [] [ text "You lose :(" ]
    else
      case List.head model.levels of
        Just level ->
          div []
            [ div [] [ text ("Choose the " ++ (answerString model.correctAnswer) ++ " meaning for " ++ level.word) ]
            -- TODO: Randomize the order of the images
            , viewImage level.americanImageName American
            , viewImage level.britishImageName British
            ]
        Nothing ->
          div [] [ text "You win!" ]
  , button [ onClick Reset ] [ text "Start over" ]
  ]


viewImage : String -> Answer -> Html Msg
viewImage imageName answer =
  img
    [ src ("/resources/" ++ imageName)
    , onClick (UserAnswer answer)
    , height 100
    ] []
