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

type alias TileSet =
  { texture : Texture
  , tileWidth : Int
  , tileHeight : Int
  , tilesPerRow : Int
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
  }

mapDecoder : Decode.Decoder TileMap
mapDecoder =
  Decode.field "layers"
    (Decode.index 0
      (Decode.map3 TileMap
        (Decode.field "width" Decode.int)
        (Decode.field "height" Decode.int)
        (Decode.field "data" (Decode.array Decode.int))))


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
  { tiles : Maybe TileSet
  , map : Maybe TileMap
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
  ({ tiles = Nothing
   , map = Nothing
            {-Just { width = 10
               , height = 10
               , tiles = Array.fromList
                          [0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 4, 4, 4, 4, 4,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            4, 4, 4, 5, 0, 0, 0, 43, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 3, 4, 4, 4,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                            4, 4, 4, 4, 4, 4, 4, 4, 4, 4]
                } -}
   , status = "Loading..."
   , offset = { x = 50, y = 70 }
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
      case maybeTexture of
          Just texture ->
            ({ model |
              tiles = Just (loadTileSet texture tileWidth tileHeight) }
            , Cmd.none)
          Nothing ->
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
      let
        offsets = List.map keyOffsets model.keysDown
        dx = List.map Tuple.first offsets |> List.sum
        dy = List.map Tuple.second offsets |> List.sum
      in
        ({ model |
             offset = { x = model.offset.x + dx,
                        y = model.offset.y + dy }}
        , Cmd.none)


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
    , p [ ] [ Html.text (Debug.toString model.keysDown) ]
    ]
gameView : Model -> Html Msg
gameView model =
    let
        scale = 5
        width = 15
        height = 10
        tileWidth = 16
        tileHeight = 16
        gameWidth = width * tileWidth
        gameHeight = height * tileHeight
        canvasWidth = String.fromInt (gameWidth * scale)

    in
      div [ style "width" canvasWidth ]
        [ Canvas.toHtmlWith
            { width = gameWidth
            , height = gameHeight
            , textures = [ loadFromImageUrl "./assets/tiles.png" (TilesLoaded 16 16) ]
            }
            [ style "border" "1px solid black"
            , style "display" "block" ]
            ( shapes [ fill Color.white ] [ rect ( 0, 0 ) gameWidth gameHeight ]
              :: (renderTiles model)
            )
        ]


renderSquare : Renderable
renderSquare =
  shapes [ fill (Color.rgba 0 0 0 1) ]
      [ rect (0, 0) 100 50 ]


renderTiles : Model -> List Renderable
renderTiles model =
  case (model.tiles, model.map) of
    (Just tileSet, Just tileMap) ->
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
                Just (tile tileSet model.offset x y tileIndex)
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
