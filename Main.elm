module Main (..) where

import Debug exposing (..)
import Effects exposing (Never, Effects)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Html
import Time exposing (..)
import Task
import Signal exposing (..)
import StartApp
import Window


-- MODEL


type alias Model =
  { mario : Mario
  , screenWidth : Int
  , screenHeight : Int
  }


type alias Mario =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  }


type Direction
  = Left
  | Right


type alias Keys =
  { x : Int, y : Int }


mario : Model
mario =
  { screenWidth = 1276
  , screenHeight = 446
  , mario =
      { x = 0
      , y = 0
      , vx = 0
      , vy = 0
      , dir = Right
      }
  }



-- UPDATE


type Action
  = Input ( Float, Keys )
  | WindowResize ( Int, Int )


step : Action -> Model -> ( Model, Effects Action )
step action model =
  case action of
    Input ( dt, keys ) ->
      ( { model
          | mario =
              model.mario
                |> gravity dt
                |> jump keys
                |> walk keys
                |> physics dt
        }
      , Effects.none
      )

    WindowResize ( w, h ) ->
      ( { model | screenWidth = Debug.log "w" w, screenHeight = Debug.log "h" h }
      , Effects.none
      )


jump : Keys -> Mario -> Mario
jump keys mario =
  if keys.y > 0 && mario.vy == 0 then
    { mario | vy = 6.0 }
  else
    mario


gravity : Float -> Mario -> Mario
gravity dt mario =
  { mario
    | vy =
        if mario.y > 0 then
          mario.vy - dt / 4
        else
          0
  }


physics : Float -> Mario -> Mario
physics dt mario =
  { mario
    | x = mario.x + dt * mario.vx
    , y = max 0 (mario.y + dt * mario.vy)
  }


walk : Keys -> Mario -> Mario
walk keys mario =
  { mario
    | vx = toFloat keys.x
    , dir =
        if keys.x < 0 then
          Left
        else if keys.x > 0 then
          Right
        else
          mario.dir
  }



-- VIEW


display : Signal.Address Action -> Model -> Element
display address model =
  let
    mario =
      model.mario

    ( w, h ) =
      ( toFloat model.screenWidth, toFloat model.screenHeight )

    verb =
      if mario.y > 0 then
        "jump"
      else if mario.vx /= 0 then
        "walk"
      else
        "stand"

    dir =
      case mario.dir of
        Left ->
          "left"

        Right ->
          "right"

    src =
      "imgs/mario/" ++ verb ++ "/" ++ dir ++ ".gif"

    marioImage =
      image 35 35 src

    groundY =
      62 - h / 2
  in
    collage
      model.screenWidth
      model.screenHeight
      [ rect w h
          |> filled (rgb 174 238 238)
      , rect w 50
          |> filled (rgb 74 167 43)
          |> move ( 0, 24 - h / 2 )
      , marioImage
          |> toForm
          |> move ( mario.x, mario.y + groundY )
      ]



-- SIGNALS


input : Signal ( Float, Keys )
input =
  let
    delta =
      map (\t -> t / 20) (fps 25)

    deltaArrows =
      map2 (,) delta Keyboard.arrows
  in
    sampleOn delta deltaArrows


app =
  let
    delta =
      map (\t -> t / 20) (fps 25)
  in
    StartApp.start
      { init = ( mario, Effects.none )
      , update = step
      , view = \a m -> Html.fromElement (display a m)
      , inputs =
          [ map WindowResize Window.dimensions
          , map Input input
          ]
      }


main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
