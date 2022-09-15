module Main exposing (main)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


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
import Json.Decode exposing (Decoder, map3, field, int, array, index)

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

mapDecoder : Decoder TileMap
mapDecoder =
  field "layers"
    (index 0
      (map3 TileMap
        (field "width" int)
        (field "height" int)
        (field "data" (array int))))


type alias Model =
  { tiles : Maybe TileSet
  , map : Maybe TileMap
  , status : String
  }



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
   }
   , Http.get
    { url = "/assets/tilemap.tmj"
    , expect = Http.expectJson MapLoaded mapDecoder
    })


subscriptions : Model -> Sub msg
subscriptions _ = Sub.none

-- UPDATE


type Msg
  = TilesLoaded Int Int (Maybe Texture)
  | MapLoaded (Result Http.Error TileMap)


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


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ gameView model
    , p [ ]
        [ Html.text model.status ]
    ]
gameView : Model -> Html Msg
gameView model =
    let
        scale = 3
        width = 30
        height = 20
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
            (renderTiles model)
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
                Just (tile tileSet x y tileIndex)
      in
        List.indexedMap mapTile (Array.toList tileMap.tiles)
        |> List.filterMap identity

    _ -> []

tile tileSet x y ii =
  let
    i = ii - 1
    tileRect =
        { width = toFloat tileSet.tileWidth
        , height = toFloat tileSet.tileHeight
        , x = toFloat (tileSet.tileWidth * (modBy tileSet.tilesPerRow i))
        , y = toFloat (tileSet.tileWidth * (i // tileSet.tilesPerRow))
        }
    tileTexture = sprite tileRect tileSet.texture
    position = (toFloat (x * tileSet.tileWidth),
                toFloat (y * tileSet.tileHeight))
  in
    texture [] position tileTexture
