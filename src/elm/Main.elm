module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , coloredSquare : Int
  , currentMove : Key
  , length : Int
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 0 Space 40
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
updateSquare : Model -> Key -> Model
updateSquare model choice =
  if (isGoodStep model choice (position model model.coloredSquare)) then 
    case choice of
      ArrowRight ->
        model.coloredSquare + 1
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
      ArrowLeft ->
        model.coloredSquare - 1
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
      ArrowDown -> 
        model.coloredSquare + model.length
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
      ArrowUp ->
        model.coloredSquare - model.length
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
      Space ->
        model.coloredSquare
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
  else
    model.coloredSquare
    |> modBy (model.length*model.length)
    |> Setters.setColoredSquareIn model

position : Model -> Int -> (Int, Int)
position model i = 
  ((i // model.length), modBy model.length i)

isGoodStep : Model -> Key -> (Int, Int)-> Bool
isGoodStep model key pos= 
  case key of
      ArrowRight ->  let (a, b) = pos in b < 39
      ArrowLeft ->  let (a, b) = pos in b > 0
      ArrowUp -> let (a, b) = pos in a > 0
      ArrowDown ->  let (a, b) = pos in a < 39
      Space -> True
      

updateCurrentMove : Model -> Key -> Model
updateCurrentMove model currentMove =
  case currentMove of 
    Space -> { model | currentMove = currentMove }
    ArrowLeft -> if(model.currentMove /= ArrowRight) then { model | currentMove = currentMove } else model
    ArrowRight -> if(model.currentMove /= ArrowLeft) then { model | currentMove = currentMove } else model
    ArrowUp -> if(model.currentMove /= ArrowDown) then { model | currentMove = currentMove } else model
    ArrowDown -> if(model.currentMove /= ArrowUp) then { model | currentMove = currentMove } else model

toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  not gameStarted
  |> Setters.setGameStartedIn model
  |> Update.none

keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> 
      update ToggleGameLoop model
    ArrowLeft -> 
      updateCurrentMove model key
      |> Update.none
    ArrowRight -> 
      updateCurrentMove model ArrowRight
      |> Update.none
    ArrowUp -> 
      updateCurrentMove model ArrowUp
      |> Update.none
    ArrowDown -> 
      updateCurrentMove model ArrowDown
      |> Update.none

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 150 then
    updateSquare model model.currentMove
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> Update.none
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model

{-| Manage all your view functions here. -}
cell : Int -> Int -> Html msg
cell index active =
  let class = if active == index then "cell active" else "cell" in
  Html.div [ Attributes.class class ] []

generateCells : Model -> Int -> List (Html msg)
generateCells model n =
  case n of
    0 -> []
    _ -> generateCells model (n-1) ++ [cell (n-1) model.coloredSquare]

movingSquare : Model -> Html msg
movingSquare model =
  Html.div [ Attributes.class "grid" ]
    (generateCells model (model.length*model.length))

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
