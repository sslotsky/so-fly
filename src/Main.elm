port module Main exposing (..)
import Json.Encode as E
import Time


-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html exposing (Html, node)
-- import Html.Events exposing (onClick)
import Html exposing (canvas)
import Browser.Events exposing (onAnimationFrame)
import Html.Attributes exposing (height)
import Html.Attributes exposing (width)



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions _ =
  Time.every 1 Tick

port sendMessage : E.Value -> Cmd msg

-- MODEL

type alias Position = (Int, Int)
type alias Hero =
  { position: Position }
type alias Model =
  { height: Int
  , width: Int
  , hero: Hero
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    hero = Model 200 600 { position = (5, 50) }
  in
    ( hero
    , Cmd.none
    --, sendMessage (encodeGame hero)
    )


-- UPDATE


type Msg
  = Tick Time.Posix


-- update : Msg -> Model -> Model
update msg model =
  case msg of
    Tick _ ->
      (model, sendMessage (encodeGame model))


-- VIEW

encodeHero : Hero -> E.Value
encodeHero {position} = 
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.int x)
      , ("y", E.int y)
      ]
encodeGame model =
  E.object
    [ ("height", E.int model.height)
    , ("width", E.int model.width)
    , ("hero", encodeHero model.hero)
    ]

view : Model -> Html Msg
view model =
  node "game-canvas" [height model.height, width model.width] []
  -- button [ onClick Decrement ] [ text "-" ]
  -- div []
  --   [ button [ onClick Decrement ] [ text "-" ]
  --   , div [] [ text (String.fromInt model) ]
  --   , button [ onClick Increment ] [ text "+" ]
  --   ]
