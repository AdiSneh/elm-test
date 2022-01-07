module Main exposing (..)

import Browser
import Dict exposing (Dict)
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
  , imageNames: Dict String String
  }


allLevels : List Level
allLevels =
  [ Level "chips"
    ( Dict.fromList
      [ (answerString American, "potato_chips.jpg")
      , (answerString British, "french_fries.jpg")
      ]
    )
  , Level "torch"
    ( Dict.fromList
      [ (answerString American, "torch.jpg")
      , (answerString British, "flashlight.jpg")
      ]
    )
  , Level "football"
    ( Dict.fromList
      [ (answerString American, "football.jpg")
      , (answerString British, "soccer_ball.jpg")
      ]
    )
  , Level "biscuit"
    ( Dict.fromList
      [ (answerString American, "biscuit_bread.jpg")
      , (answerString British, "biscuit_cookie.jpg")
      ]
    )
  ]


type Answer
  = American
  | British


-- TODO: Code smell
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
  , answerOrder: (Answer, Answer)
  , score: Int
  , lives: Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  -- TODO: I shouldn't have to pass initial values for variables that will be randomly generated
  ( Model 1 allLevels American (American, British) 0 3
  , Random.generate NewCorrectAnswer (Random.pair (Random.Extra.choice American British) (Random.Extra.choice American British))
  )


-- UPDATE


type Msg
  = NewCorrectAnswer (Answer, Answer)
  | UserAnswer Answer
  | Reset


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewCorrectAnswer (correctAnswer, firstAnswer) ->
      ( { model
        | correctAnswer = correctAnswer
        , answerOrder = getAnswerOrder firstAnswer
        }
      , Cmd.none
      )
    UserAnswer answer ->
      ( updateWithUserAnswer model answer
          |> updateLevels
          |> updateLevelNumber
      , Random.generate NewCorrectAnswer (Random.pair (Random.Extra.choice American British) (Random.Extra.choice American British))
      )

    Reset ->
      init ()


getAnswerOrder : Answer -> (Answer, Answer)
getAnswerOrder firstAnswer =
  case firstAnswer of
    American ->
      (American, British)
    British ->
      (British, American)


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
            -- TODO: Duplication
            , viewImage level.imageNames (Tuple.first model.answerOrder)
            , viewImage level.imageNames (Tuple.second model.answerOrder)
            ]
        Nothing ->
          div [] [ text "You win!" ]
  , button [ onClick Reset ] [ text "Start over" ]
  ]


viewImage : Dict String String -> Answer -> Html Msg
viewImage imageNames answer =
  case (Dict.get (answerString answer) imageNames) of
    Just imageName ->
      img
        [ src ("/resources/" ++ imageName)
        , onClick (UserAnswer answer)
        , height 100
        ] []
    -- TODO: Code smell
    Nothing ->
      div [] [text "An error has occurred"]
