module Main where

import Util(..)
import Window
import Http
import Html
import Html.Events
import Signal
import List
import Graphics.Element
import Text
import Json.Decode
import Json.Decode ((:=))
import Maybe
import Dict
import Debug

-- Model
type alias ID    = Int
type Vote        = Up | Down | None
type alias Post  = {title : String, score : Int, vote : Vote, idNum : ID}
type alias State =
  { postDB : Dict.Dict Int Post
  }

-- User input
type Action = NoOp | ClickUp ID | ClickDown ID

actions : Signal.Channel Action
actions = Signal.channel NoOp

-- Messages from server
type Update
  = SetVote ID Vote
  | SetDB (Dict.Dict Int Post)
  | Error String

-- Parsing messages
parseVote s = case s of
  "Up"   -> Up
  "Down" -> Down
  _      -> None

postJson =
  Json.Decode.object4 (\t s n v -> {title = t, score = s, idNum = n, vote = v})
    ("title" := Json.Decode.string)
    ("score" := Json.Decode.int)
    ("idNum" := Json.Decode.int)
    ("vote"  := Json.Decode.map parseVote Json.Decode.string)

updateJson =
  Json.Decode.oneOf
  [ Json.Decode.object2 (\idNum v -> SetVote idNum v)
      ("idNum" := Json.Decode.int)
      ("vote"  := Json.Decode.map parseVote Json.Decode.string)
  , Json.Decode.map (SetDB << Dict.fromList << List.map (\p -> (p.idNum, p)))
      (Json.Decode.list postJson)
  ]

decodeUpdate : Http.Response String -> Update
decodeUpdate = 
  let decode r =
        case r of
          Http.Success s   -> Json.Decode.decodeString updateJson s
          Http.Waiting     -> Err "Waiting"
          Http.Failure _ s -> Err s
      fromResult ex =
        case ex of
          Ok x  -> x
          Err s -> Error s
  in
  fromResult << decode

-- Messages from server
postDBUpdates : Signal Update
postDBUpdates = 
  Http.sendGet (Signal.constant "http://localhost:3000/posts")
  |> Signal.map decodeUpdate

updates : Signal.Signal Update
updates =
  let encode a =
        case a of
          NoOp            -> Http.get "/noop" -- No filterMap for signals...
          ClickUp idNum   -> Http.get ("/up/" ++ toString idNum)
          ClickDown idNum -> Http.get ("/down/" ++ toString idNum)
  in
  Http.send (Signal.map encode (Signal.subscribe actions))
  |> Signal.map decodeUpdate
  |> Signal.merge postDBUpdates

-- Display
render : State -> Html.Html
render = Html.ol [] << List.map renderPost << List.sortBy .score << Dict.values << .postDB

voteButtons : Post -> Html.Html
voteButtons p =
  let voteButton f s = Html.button [Html.Events.onClick (Signal.send actions (f p.idNum))] [Html.text p.title]
      upVote         = voteButton ClickUp "+"
      downVote       = voteButton ClickDown "-"
  in
  Html.div [] [upVote, downVote]

renderPost p = Html.li [] [Html.text (p.title ++ ", " ++ toString p.score), voteButtons p]

update : Update -> State -> State
update u s = case u of
  SetVote idNum v ->
    {s | postDB <- Dict.update idNum (Maybe.map (\p -> {p | vote <- v})) s.postDB}

  SetDB db -> {s | postDB <- db}

  Error e -> Debug.log e s

-- scene : State -> (Int, Int) -> Element
scene s (w,h) =
  Html.body []
  [ Html.h1 [] [Html.text "Vote"]
  , Html.main' [] [render s]
  ]
  |> Html.toElement w h

state = Signal.foldp update {postDB = Dict.empty} updates

main = Signal.map2 scene state Window.dimensions

