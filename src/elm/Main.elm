module Main exposing (..)

import Browser
import Random
import Setters
import Update
import Browser.Events
import Html exposing (option)
import Html.Attributes exposing (value)
import Html exposing (text)
import Debug exposing (toString)
import Html exposing (Html)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (..)
import Html.Events as Events
import Time exposing (Posix)
import Json.Decode as Decode
import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (targetValue)

{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , snake : Array Int
  , apple : Int
  , currentMove : Key
  , length : Int
  , score : Int
  , wall : Bool
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time (Array.fromList [3, 2, 1, 0]) 84 ArrowRight 40 0 False
  |> changeApple

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | NewAppleRandomPosition Int
  | ChangeGridSize Int

snakeHead: Array Int -> Int
snakeHead snake = 
  case (Array.get 0 snake) of 
    Just a-> a
    Nothing -> 0

snakeBody: Array Int -> Array Int
snakeBody snake = Array.slice 1 ( ( Array.length snake ) - 1 ) snake

snakeMove: Array Int -> Int -> Array Int
snakeMove snake nextStep =
  let 
    newHeadPosition = Array.repeat 1 nextStep
    body = Array.slice 0 ( ( Array.length snake ) - 1 ) snake
  in
  Array.append newHeadPosition body


addSnakePart : Array Int -> Array Int
addSnakePart snake =
  let 
    newPartPosition = Array.get ( (Array.length snake) - 1 ) snake 
  in
  case newPartPosition of 
  Nothing -> snake
  Just a -> (Array.push a snake)

updateScore : Int -> Model -> Model
updateScore scoreToAdd model = 
  {model | score = (model.score + scoreToAdd)}


scoreForPassedTime : Posix -> Model -> Model
scoreForPassedTime time model = 
  let 
      time_ = Time.posixToMillis time
      timeInSecond = (modBy 60 (time_ // 1000))
      lastTimeInSecond = (modBy 60 (model.lastUpdate // 1000))
  in
  if (timeInSecond - lastTimeInSecond >= 1) then
    updateScore 10 model
  else
    model

collisionApple : Model -> Model
collisionApple ({ snake } as model) =
  if isCollisionApple model then
    {model | snake = addSnakePart snake } |> updateScore 100
  else 
    {model | snake = model.snake}


collisionBody : Model -> Model
collisionBody ({ snake } as model) =
  if isCollisionBody (snakeBody snake) (snakeHead snake) then
    {model | gameStarted = False }
  else 
    {model | gameStarted = True }

changeApple : Model -> (Model,Cmd Msg)
changeApple model = 
  if isCollisionApple model then
    model
    |> Update.withCmd (randomPositionApple model.length)
  else
    model |> Update.none

    
isCollisionApple : Model -> Bool
isCollisionApple ({ apple, snake } as model) =
  snakeHead snake == apple

isCollisionBody : Array Int -> Int -> Bool
isCollisionBody body head =
  let bodyParts = Debug.log "" body
  in
  case Array.toList body of 
    [] -> False
    part::rest -> if part == head then True else False || isCollisionBody (Array.fromList rest) head 


{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
updateSquare : Model -> Key -> Model
updateSquare model choice =
  let snakeH = (snakeHead model.snake) in
  if (isGoodStep model choice (position model snakeH)) then
    case choice of
      ArrowRight -> 
          snakeH + 1
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowLeft ->
        if (modBy model.length snakeH == 0) then 
          snakeH
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
        else
          snakeH- 1
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowDown -> 
          snakeH + model.length
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      ArrowUp ->
          snakeH  - model.length
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
      Space ->
          snakeH
          |> modBy (model.length*model.length)
          |> snakeMove model.snake
          |> Setters.setColoredSquareIn model
  else
    snakeH
    |> modBy (model.length*model.length)
    |> snakeMove model.snake
    |> Setters.setColoredSquareIn model

position : Model -> Int -> (Int, Int)
position model i = 
  ((i // model.length), modBy model.length i)

isGoodStep : Model -> Key -> (Int, Int)-> Bool
isGoodStep model key pos= 
  case key of
      ArrowRight ->  let (a, b) = pos in b < (model.length - 1)
      ArrowLeft ->  let (a, b) = pos in b > 0
      ArrowUp -> let (a, b) = pos in a > 0
      ArrowDown ->  let (a, b) = pos in a < (model.length - 1)
      Space -> True
      

updateCurrentMove : Model -> Key -> Model
updateCurrentMove model currentMove =
  case currentMove of 
    Space -> { model | currentMove = currentMove }
    ArrowLeft -> if (model.currentMove /= ArrowRight) then { model | currentMove = currentMove } else model
    ArrowRight -> if (model.currentMove /= ArrowLeft) then { model | currentMove = currentMove } else model
    ArrowUp -> if (model.currentMove /= ArrowDown) then { model | currentMove = currentMove } else model
    ArrowDown -> if (model.currentMove /= ArrowUp) then { model | currentMove = currentMove } else model

isSnakePart: Int -> List Int -> Bool
isSnakePart elem snake =
  case snake of
      [] -> False
      head::rest ->
        if (elem == head) then
          True
        else
          isSnakePart elem rest

randomPositionApple : Int -> Cmd Msg
randomPositionApple size =
    let 
      r = Random.int 1 (size*size) 
    in
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
  let 
    time_ = Time.posixToMillis time
  in
  if time_ - model.lastUpdate >= 150 then
    updateSquare model model.currentMove
    |> collisionApple
    |> collisionBody
    |> scoreForPassedTime time 
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> changeApple
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
    ChangeGridSize gridSize -> 
      case gridSize of 
      10  -> Model False model.lastUpdate  model.time (initialize 4 (\n -> n+1)) 84 Space gridSize 0 model.wall |> Update.none
      20 -> Model False model.lastUpdate model.time (initialize 4 (\n -> n+1)) 152 Space gridSize 0 model.wall  |> Update.none 
      40 -> Model False model.lastUpdate model.time (initialize 4 (\n -> n+1)) 150 Space gridSize 0 model.wall |> Update.none 
      _-> model |> Update.none
    NewAppleRandomPosition pos ->
      if (isCollisionApple model) then
        if (pos == (snakeHead model.snake) ) then
          model     
          |> Update.withCmd (randomPositionApple model.length)
        else
          {model | apple = pos } |> Update.none

      else 
        {model | apple = model.apple } |> Update.none


isSnakeCell : List Int -> Int -> Bool
isSnakeCell snake index = 
  case snake of 
    [] -> False
    head::rest ->
      if (head == index) then
        True
      else 
        isSnakeCell rest index


{-| Manage all your view functions here. -}
cell : Int -> Model -> Html msg 
cell index ({ snake , apple } as model) =
  let 
    snakeH = snakeHead snake
    class = if apple == index then 
                "cell activeApple"
              else 
                  if isSnakeCell (Array.toList snake) index then
                    "cell active"
                  else 
                    "cell"
    
  in

  Html.div [ Attributes.class class ] []

generateCells : Model -> Int -> List (Html msg)
generateCells model n =
  case n of
    0 -> []
    _ -> generateCells model (n-1) ++ [cell (n-1) model]

movingSquare : Model -> Html msg
movingSquare coloredSquare =
  Html.div ([ Attributes.class "grid" ] ++ gridStyle (String.fromInt coloredSquare.length) )
   (generateCells coloredSquare (coloredSquare.length*coloredSquare.length))

actualTime : Model -> Html msg
actualTime { score } =
  Html.div [ Attributes.class "actual-score" ]
    [ Html.text "SCORE"
    , score
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
      [ Html.text (String.join " " [word, "game"]) ]
    ]
targetValueRoleDecoder : Decode.Decoder Int 
targetValueRoleDecoder = 
  targetValue |> Decode.andThen (\val -> 
    case val of 
    "40" -> Decode.succeed 40 
    "20" -> Decode.succeed 20 
    "10" -> Decode.succeed  10
    _-> Decode.fail ("Invalid Role: " ++ val) ) 

gameParameters : Model -> Html Msg
gameParameters model = 
  Html.div [ Attributes.class "game-parameters" ]  
   [ Html.div[] 
      [ Html.text "Active border wall" , input[ type_ "checkbox" ] [] ]
      ,Html.div[] 
      [ Html.text "Active wall obstacle" , input[ type_ "checkbox" ] [] ]
      ,Html.div [] 
             [ Html.text "Grid size ",
             select
                [ Events.on "change" (Decode.map ChangeGridSize targetValueRoleDecoder) 
                ]
                [   viewOption 40
                    , viewOption 20
                    , viewOption 10
                  ]
              ]
          ]


viewOption : Int -> Html Msg
viewOption length =
  option
    [ value <| toString length ]
    [ text <| toString length ]
      

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [
    explanations model,
    gameParameters model,
    movingSquare model
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


gridStyle : String -> List (Attribute msg)
gridStyle size =
    [ style "display" "grid"
    , style "width" "600px"
    , style "height" "600px"
    , style "grid-template-rows" ("repeat("++size++" , 1fr)")
    , style "grid-template-columns" ("repeat("++size++" , 1fr)")
    , style "margin" "24px auto"
    , style "border" "1px solid #888"
    , style "background" "white"
    ]

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    
    }
