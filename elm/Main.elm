module Main where

import Window
import Http
import Html
import Signal
import List
import Graphics.Element
import Text
import Json.Decode
import Json.Decode ((:=))

type alias Post = {title : String, score : Int}

posts = 
  let postJson = Json.Decode.object2 (\t s -> {title = t, score = s}) 
                   ("title" := Json.Decode.string) ("score" := Json.Decode.int)
      readResp r =
        case r of
          Http.Success s ->
            case Json.Decode.decodeString (Json.Decode.list postJson) s of
              Ok ps -> ps
              _      -> []
          _         -> []
  in
  Http.sendGet (Signal.constant "http://localhost:3000/foo")
  |> Signal.map readResp

renderPosts : List Post -> Html.Html
renderPosts = Html.ol [] << List.map renderPost

renderPost p = Html.text (p.title ++ ", " ++ toString p.score)

upvotes : Channel

-- scene : List Post -> (Int, Int) -> Element
scene ps (w,h) = Html.toElement w h (renderPosts ps)

main = Signal.map2 scene posts Window.dimensions

