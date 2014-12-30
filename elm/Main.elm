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

type alias Post = {title : String, score : Int, vote : Vote}

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

voteButtons : String -> Html.Html
voteButtons title =
  let upVote   = Html.button [on
      downVote = 

renderPost p = Html.text (p.title ++ ", " ++ toString p.score)

type Vote = Up | Down | None

-- How is username transmitted?!
type Action
  = SetVote Title Vote

actions : Channel Action
actions = Signal.channel (SetVote "" None)

-- scene : List Post -> (Int, Int) -> Element
scene ps (w,h) = Html.toElement w h (renderPosts ps)

main = Signal.map2 scene posts Window.dimensions

