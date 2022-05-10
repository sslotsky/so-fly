port module Main exposing (..)
import Json.Encode as E
import Time
import Browser
import Html exposing (Html, node)
import Html.Attributes exposing (height)
import Html.Attributes exposing (width)



-- MAIN


main : Program () Model Msg
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

subscriptions : a -> Sub Msg
subscriptions _ =
  Sub.batch 
    [ Time.every 1 Tick
    , bodyKeyPress BodyKeyPress
    , bodyKeyUp BodyKeyUp
    ]

port sendMessage : E.Value -> Cmd msg
port bodyKeyPress : (Int -> msg) -> Sub msg
port bodyKeyUp : (Int -> msg) -> Sub msg


-- MODEL

type alias Position = (Float, Float)
type alias Velocity = (Float, Float)

type alias MoveController =
  { right: Bool
  , left: Bool
  , up: Bool
  , down: Bool
  }

type alias Hero =
  { position: Position, velocity: Velocity }

type alias Model =
  { height: Int
  , width: Int
  , hero: Hero
  , controller: MoveController
  }

init : () -> (Model, Cmd Msg)
init _ =
  let ((height, width), position, velocity) = ((400, 600), (5, 50), (0, 0)) in
  let controller = { right = False, up = False, down = False, left = False } in

  let hero = { position = position, velocity = velocity } in
  let model = Model height width hero controller
  in
    (model, Cmd.none)


-- UPDATE


type Msg
  = Tick Time.Posix
  | BodyKeyPress Int
  | BodyKeyUp Int

move: Hero -> Float -> Float -> Hero
move hero x y =
  { hero | position = (x, y)}

type Direction = Left | Right | Up | Down

decelerate : Float -> Float -> Float
decelerate speed resistance =
  if abs speed <= 0.01 then 0 else speed * (1 - resistance / 60)

withFriction : Hero -> Hero
withFriction hero =
  let (vx, vy) = hero.velocity in
  let friction = 0.1 in
  { hero | velocity = (decelerate vx friction, decelerate vy friction) }

push : Hero -> Direction -> Model -> Hero
push hero direction {height, width} =
  let (x, y) = hero.velocity in
  let acc = 0.02 in
  let (px, py) = hero.position in
    case direction of
      Left -> 
        let speed = if px > 0 then x - acc else 0 in
          { hero | velocity = (speed, y)}
      Right ->
        let speed = if px < toFloat width then x + acc else 0 in
          { hero | velocity = (speed, y)}
      Up ->
        let speed = if py > 0 then y - acc else 0 in
          { hero | velocity = (x, speed)}
      Down ->
        let speed = if py < toFloat height then y + acc else 0 in
        { hero | velocity = (x, speed)}

adjustVelocity : Model -> Hero
adjustVelocity model =
  let
    {controller} = model
    adjust : Direction -> Hero -> Hero
    adjust direction hero =
      case direction of
        Left -> if controller.left then model |> push hero Left else hero
        Right -> if controller.right then model |> push hero Right else hero
        Up -> if controller.up then model |> push hero Up else hero
        Down -> if controller.down then model |> push hero Down else hero
  in
    model.hero |> adjust Left |> adjust Right |> adjust Up |> adjust Down

clamped : Float -> Float -> Position -> Position 
clamped maxX maxY (x, y) =
  (clamp 0 maxX x, clamp 0 maxY y)

nextState : Model -> Model
nextState model =
  let
    hero = adjustVelocity model
    {position, velocity} = hero
    ((x, y), (vx, vy)) = (position, velocity)
    (nextX, nextY) = clamped (toFloat model.width) (toFloat model.height) (x + vx, y + vy)
  in
    { model | hero = move hero nextX nextY |> withFriction }

moveToggle : Bool -> Direction -> Model -> Model
moveToggle on direction model =
  let {controller} = model in
    case direction of
      Left -> { model | controller = { controller | left = on }}
      Right -> { model | controller = { controller | right = on }}
      Up -> { model | controller = { controller | up = on }}
      Down -> { model | controller = { controller | down = on }}

startMoving = moveToggle(True)
stopMoving = moveToggle(False)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    Tick _ ->
      let next = nextState model in
        (next, sendMessage (encodeGame next))
    BodyKeyPress keyCode ->
      case keyCode of
        37 -> (model |> startMoving Left, Cmd.none)
        38 -> (model |> startMoving Up, Cmd.none)
        39 -> (model |> startMoving Right, Cmd.none)
        40 -> (model |> startMoving Down, Cmd.none)
        _ -> (model, Cmd.none)
    BodyKeyUp keyCode ->
      case keyCode of
        37 -> (model |> stopMoving Left, Cmd.none)
        38 -> (model |> stopMoving Up, Cmd.none)
        39 -> (model |> stopMoving Right, Cmd.none)
        40 -> (model |> stopMoving Down, Cmd.none)
        _ -> (model, Cmd.none)

-- VIEW

encodeHero : Hero -> E.Value
encodeHero {position} = 
  let
    (x, y) = position
  in
    E.object
      [ ("x", E.float x)
      , ("y", E.float y)
      ]
encodeGame : { a | height : Int, width : Int, hero : Hero } -> E.Value
encodeGame model =
  E.object
    [ ("height", E.int model.height)
    , ("width", E.int model.width)
    , ("hero", encodeHero model.hero)
    ]

view : Model -> Html Msg
view model =
  node "game-canvas" [height model.height, width model.width] []