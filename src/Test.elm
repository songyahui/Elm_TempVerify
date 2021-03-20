-- Press a button to generate a random number between 1 and 6.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/random.html
--

init = 1

import Browser

import Browser
import Html as F exposing (Html (..), text, pre) 
import Http
import Html.Events exposing (..)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, placeholder, value)
import Html.Events exposing (onClick, onDoubleClick, onInput, onSubmit)


{-
import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random
import Process
import Task



-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL



type alias Model =
  { dieFace : Int
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model 1
  , Cmd.none
  )



-- UPDATE


type Msg
  = Roll
  | NewFace Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Roll ->
      ( model
      , Process.sleep 1000
        |> Task.perform (\_ ->  NewFace (model.dieFace + 1))
      )

    NewFace newFace ->
      ( Model newFace
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (String.fromInt model.dieFace) ]
    , button [ onClick Roll ] [ text "Roll" ]
    ]
-}