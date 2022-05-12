module Main exposing (..)

import Browser
import Random
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
  , grid : List String
  , coloredSquare : Int
  , snake : List Int
  , apple : Int
  , currentMove : Key
  , length : Int

  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time (initGrid [] 0) 0 [0] 150 Space 40
  |> Update.none

initGrid : List String -> Int -> List String
initGrid grid n = 
  if (n /= 400) then
    grid
  else
    initGrid grid n

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | CollisionAppleSnake
  | NewAppleRandomPosition Int


updateApple : Model -> Model
updateApple ({ apple, snake } as model) =
  case snake of
    header::rest ->
      if (apple == header) then
          {model | apple = model.apple}
      else 
        {model | apple = model.apple}
    [] -> {model | apple = model.apple}

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
updateSquare : Model -> Key -> Model
updateSquare model choice =
  case choice of
    ArrowRight ->
        if(modBy (model.length-1) model.coloredSquare == 0 && model.coloredSquare /= 0) then 
          model.coloredSquare
          |> modBy (model.length*model.length)
          |> Setters.setColoredSquareIn model
        else
          model.coloredSquare + 1
          |> modBy (model.length*model.length)
          |> Setters.setColoredSquareIn model
    ArrowLeft ->
        if(modBy model.length model.coloredSquare == 0) then 
          model.coloredSquare
          |> modBy (model.length*model.length)
          |> Setters.setColoredSquareIn model
        else
        model.coloredSquare - 1
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
    ArrowDown -> 
      if(model.coloredSquare >= (model.length * model.length) - (1+model.length)) then 
          model.coloredSquare
          |> modBy (model.length*model.length)
          |> Setters.setColoredSquareIn model
      else
        model.coloredSquare + model.length
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
    ArrowUp ->
      if(model.coloredSquare <= (model.length-1)) then 
          model.coloredSquare
          |> modBy (model.length*model.length)
          |> Setters.setColoredSquareIn model
      else
        model.coloredSquare - model.length
        |> modBy (model.length*model.length)
        |> Setters.setColoredSquareIn model
    Space ->
      model.coloredSquare
      |> modBy (model.length*model.length)
      |> Setters.setColoredSquareIn model

updateCurrentMove : Model -> Key -> Model
updateCurrentMove model currentMove =
  case currentMove of 
    Space -> { model | currentMove = currentMove }
    ArrowLeft -> if(model.currentMove /= ArrowRight) then { model | currentMove = currentMove } else model
    ArrowRight -> if(model.currentMove /= ArrowLeft) then { model | currentMove = currentMove } else model
    ArrowUp -> if(model.currentMove /= ArrowDown) then { model | currentMove = currentMove } else model
    ArrowDown -> if(model.currentMove /= ArrowUp) then { model | currentMove = currentMove } else model

isSnakePart: Int -> List Int -> Bool
isSnakePart elem snake =
  case snake of
      [] -> False
      head::rest ->
        if (elem == head) then
          True
        else
          isSnakePart elem rest

randomPosition : Cmd Msg
randomPosition =
    let r = Random.int 1 400 in
    Random.generate NewAppleRandomPosition r

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
    CollisionAppleSnake -> (model,randomPosition)
    NewAppleRandomPosition pos -> 
        case model.snake of
          [] -> {model | apple = model.apple } |> Update.none
          s ->
            if (isSnakePart pos s) then 
              (model,randomPosition) 
            else
              {model | apple = pos } |> Update.none


{-| Manage all your view functions here. -}
cell : Int -> Model -> Html msg 
cell index ({ coloredSquare, apple } as model) =
  let class = if index == coloredSquare then 
                "cell active" 
              else if apple == index then 
                "cell activeApple"
              else "cell"
  in
  Html.div [ Attributes.class class ] []

generateCells : Model -> Int -> List (Html msg)
generateCells model n =
  case n of
    0 -> []
    _ -> generateCells model (n-1) ++ [cell (n-1) model]

movingSquare : Model -> Html msg
movingSquare coloredSquare =
  Html.div [ Attributes.class "grid" ]
   (generateCells coloredSquare (40*40))

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
