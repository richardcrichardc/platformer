module Main exposing (main)

import Browser
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Texture exposing (..)
import Color
import Html exposing (Html, div, p)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array
import Maybe
import Http
import Debug
import Json.Decode as Decode
import Browser.Events
import Tuple exposing (second)

-- MAIN


main =
  Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL

scale = 3
width = 15
height = 10



type alias TileSet =
  { texture : Texture
  , tilesPerRow : Int
  , tileWidth : Int
  , tileHeight : Int
  }

loadTileSet : Texture -> Int -> Int -> TileSet
loadTileSet texture tileWidth tileheight =
  let
    dims = dimensions texture
  in
    { texture = texture
    , tileWidth = tileWidth
    , tileHeight = tileheight
    , tilesPerRow = (round dims.width) // tileWidth
    }


type alias TileMap =
  { width : Int
  , height : Int
  , tiles : Array.Array Int
  , tileWidth : Int
  , tileHeight : Int
  , tileSet : Maybe TileSet
  }

mapDecoder : Decode.Decoder TileMap
mapDecoder =
    Decode.map6 TileMap
      (Decode.field "width" Decode.int)
      (Decode.field "height" Decode.int)
      (Decode.field "layers" (Decode.index 0 (Decode.field "data" (Decode.array Decode.int))))
      (Decode.field "tilewidth" Decode.int)
      (Decode.field "tileheight" Decode.int)
      (Decode.succeed Nothing)

type Key
  = Left
  | Right
  | Up
  | Down
  | Other

keyDecoder : Decode.Decoder Key
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Key
toDirection string =
  case string of
    "ArrowLeft" -> Left
    "ArrowRight" -> Right
    "ArrowUp" -> Up
    "ArrowDown" -> Down
    _ -> Other


type alias Point =
  { x : Int
  , y : Int
  }


type alias Model =
  { map : Maybe TileMap
  , status : String
  , offset : Point
  , keysDown : List Key
  }

type Msg
  = TilesLoaded Int Int (Maybe Texture)
  | MapLoaded (Result Http.Error TileMap)
  | KeyDown Key
  | KeyUp Key
  | Tic Float

init : () -> (Model, Cmd Msg)
init _ =
  ({ map = Nothing
   , status = "Loading..."
   , offset = { x = 0, y = 0 }
   , keysDown = []
   }
   , Http.get
    { url = "/assets/tilemap.tmj"
    , expect = Http.expectJson MapLoaded mapDecoder
    })


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
    , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
    , Browser.Events.onAnimationFrameDelta Tic ]

-- UPDATE




update : Msg -> Model -> (Model, Cmd msg)
update msg model =
  case msg of
    TilesLoaded tileWidth tileHeight maybeTexture ->
      case (model.map, maybeTexture) of
          (Just map, Just texture) ->
            ({ model |
                map = Just { map |
                             tileSet = Just (loadTileSet texture tileWidth tileHeight) } }
            , Cmd.none)
          _ ->
            (model, Cmd.none)

    MapLoaded result ->
      case result of
          Ok tileMap ->
            ({ model | map = Just tileMap, status = ("Map loaded. ")}, Cmd.none)
          Err err ->
            ({ model | status = Debug.toString err }, Cmd.none)

    KeyDown key ->
      ({ model | keysDown = key :: (remove model.keysDown key) }, Cmd.none)

    KeyUp key ->
      ({ model | keysDown = (remove model.keysDown key) }, Cmd.none)

    Tic time->
      case model.map of
        Just map ->
          let
            maxX = map.tileWidth * (map.width - width)
            maxY = map.tileHeight * (map.height - height)

            offsets = List.map keyOffsets model.keysDown
            dx = List.map Tuple.first offsets |> List.sum
            dy = List.map Tuple.second offsets |> List.sum

            newX = model.offset.x + dx
            newY = model.offset.y + dy
          in
            ({ model |
                offset = { x = min (max 0 newX) maxX,
                            y = min (max 0 newY) maxY }}
            , Cmd.none)
        Nothing ->
          (model, Cmd.none)


remove list val = List.filter (\v -> v /= val) list

keyOffsets key =
  case key of
    Up ->
      (0, -1)
    Down ->
      (0, 1)
    Left ->
      (-1, 0)
    Right ->
      (1, 0)
    Other ->
      (0, 0)

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ gameView model
    , p [ ] [ Html.text model.status ]
    , p [ ] [ Html.text (Debug.toString model.keysDown)
            , Html.text " "
            , Html.text (Debug.toString model.offset.x)
            , Html.text " "
            , Html.text (Debug.toString model.offset.y) ]
    ]

gameView : Model -> Html Msg
gameView model =
  case model.map of
    Nothing ->
      div [] []
    Just map ->
      let
          gameWidth = width * map.tileWidth
          gameHeight = height * map.tileHeight
          canvasWidth = String.fromInt (gameWidth * scale)
      in
        div [ style "width" canvasWidth ]
          [ Canvas.toHtmlWith
              { width = gameWidth
              , height = gameHeight
              , textures = [ loadFromImageUrl "./assets/tiles.png" (TilesLoaded map.tileWidth map.tileHeight) ]
              }
              [ style "border" "1px solid black"
              , style "display" "block" ]
              ([ shapes [ fill Color.white ] [ rect ( 0, 0 ) (toFloat gameWidth) (toFloat gameHeight) ] ]
               ++ (renderTiles map model.offset)
               ++ renderMan map)


          ]

renderMan : TileMap -> List Renderable
renderMan map =
  let
    x = (width * (toFloat map.tileWidth)) / 2
    y = (height * (toFloat map.tileHeight)) / 2
    personWidth = 15
    personHeight = 40
  in
    [ shapes [ fill Color.red ] [ rect (x, y) personWidth personHeight ] ]

renderTiles : TileMap -> Point -> List Renderable
renderTiles tileMap offset =
  case tileMap.tileSet of
    Just tileSet ->
      let
        mapTile mapIndex tileIndex =
          case tileIndex of
            0 ->
              Nothing
            _ ->
              let
                x = modBy tileMap.width mapIndex
                y = mapIndex // tileMap.width
              in
                Just (tile tileSet offset x y tileIndex)
      in
        List.indexedMap mapTile (Array.toList tileMap.tiles)
        |> List.filterMap identity

    _ -> []

tile tileSet offset x y ii =
  let
    i = ii - 1
    tileRect =
        { width = toFloat tileSet.tileWidth
        , height = toFloat tileSet.tileHeight
        , x = toFloat (tileSet.tileWidth * (modBy tileSet.tilesPerRow i))
        , y = toFloat (tileSet.tileWidth * (i // tileSet.tilesPerRow))
        }
    tileTexture = sprite tileRect tileSet.texture
    position = (toFloat (x * tileSet.tileWidth - offset.x),
                toFloat (y * tileSet.tileHeight - offset.y))
  in
    texture [] position tileTexture
