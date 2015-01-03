module Main where

import Util(..)
import Window
import Http
import Html
import Html.Attributes (class, href)
import Html.Events
import Signal
import List
import List ((::))
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

type PostContentChunk = Text String | Link { text : String, url : String }

type alias Post  =
  { title : String
  , score : Int
  , vote : Vote
  , idNum : ID
  , content : List PostContentChunk
  }
type alias State =
  { postDB : Dict.Dict Int Post
  , cookie : Maybe String
  }

port cookie : Signal.Signal (Maybe String)

-- User input
type Action = NoOp | ClickUp ID | ClickDown ID

actions : Signal.Channel Action
actions = Signal.channel NoOp

-- Messages from server
type Update
  = SetVote ID Vote
  | SetDB (Dict.Dict ID Post)
  | Error String

-- Parsing messages
parseVote s = case s of
  "Up"   -> Up
  "Down" -> Down
  _      -> None

postJson =
  let postContentChunkJson =
        Json.Decode.oneOf
        [ Json.Decode.map Text Json.Decode.string
        , Json.Decode.object2 (\t u -> Link {text = t, url = u})
            ("text" := Json.Decode.string)
            ("url"  := Json.Decode.string)
        ]
  in
  Json.Decode.object5 (\t c s n v -> {title = t, content = c, score = s, idNum = n, vote = v})
    ("title" := Json.Decode.string)
    ("content" := Json.Decode.list postContentChunkJson)
    ("score" := Json.Decode.int)
    ("idNum" := Json.Decode.int)
    ("vote"  := Json.Decode.map parseVote Json.Decode.string)

updateJson =
  Json.Decode.oneOf
  [ Json.Decode.object2 (\v idNum -> SetVote idNum v)
      ("vote"  := Json.Decode.map parseVote Json.Decode.string)
      ("idNum" := Json.Decode.int)
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
  Http.sendGet (Signal.constant "/posts")
  |> Signal.map decodeUpdate

updates : Signal.Signal Update
updates =
  let encode a =
        case a of
          NoOp            -> Http.get "/actions/noop" -- No filterMap for signals...
          ClickUp idNum   -> Http.get ("/actions/up/" ++ toString idNum)
          ClickDown idNum -> Http.get ("/actions/down/" ++ toString idNum)
  in
  Http.send (Signal.map encode (Signal.subscribe actions))
  |> Signal.map decodeUpdate
  |> Signal.merge postDBUpdates

-- Display
render : State -> Html.Html
render =
  let negate x = -x in
  Html.ol []
  << List.map renderPost
  << List.sortBy (negate << .score)
  << Dict.values
  << .postDB

voteButtons : Post -> Html.Html
voteButtons p =
  let voteButton f s =
    Html.button [Html.Events.onClick (Signal.send actions (f p.idNum))] [Html.text s]
      upVote =
        Html.span
        [ Html.Events.onClick (Signal.send actions (ClickUp p.idNum))
        , class 
          ("glyphicon glyphicon-chevron-up " 
            ++ if p.vote == Up then "up-active" else "up-inactive")
        ]
        []
      downVote = 
        Html.span
        [ Html.Events.onClick (Signal.send actions (ClickDown p.idNum))
        , class 
          ("glyphicon glyphicon-chevron-down "
           ++ if p.vote == Down then "down-active" else "down-inactive")
        ]
        []
  in
  Html.div [class "vote-buttons"]
  [ Html.div [class "votebutton up"] [upVote]
  , Html.div [] [Html.text (toString p.score)]
  , Html.div [class "votebutton down"] [downVote]
  ]

renderContent c = case c of
  Text t           -> Html.text t
  Link {text, url} -> Html.a [href url] [Html.text text]

renderPost p = Html.li []
  [ voteButtons p
  , Html.div [class "post-container"] 
    [ Html.p [class "post-title"] [Html.text p.title]
    , Html.div [class "well well-sm"] (List.map renderContent p.content)
    ]
  ]

voteToInt v = case v of
  None -> 0
  Up -> 1
  Down -> -1

-- update : Update -> State -> State
update u s = case u of
  SetVote idNum v ->
    {s | postDB <- Dict.update idNum
      (Maybe.map (\p -> 
        {p | vote <- v, score <- p.score - voteToInt p.vote + voteToInt v}))
      s.postDB}

  SetDB db -> {s | postDB <- db}

  Error e -> s -- Debug.log e s

-- scene : State -> (Int, Int) -> Element
scene s (w,h) =
  let actionButton = case s.cookie of
        Nothing ->
          Html.a [href "/login.html"] 
            [Html.button [class "btn btn-default"] [Html.text "Login"]]
        Just _  ->
          Html.a [href "/submit.html"]
            [Html.button [class "btn btn-default"] [Html.text "Submit post"]]
  in
  Html.div [class "container"]
  [ Html.div [class "page-header"]
    [ Html.h1 [] [Html.text "CMSC 22300: Vote"]
    , actionButton 
    ]
  , Html.main' [] [render s]
  ]
  |> Html.toElement w h

state =
  Signal.map2 (\s c -> {s | cookie = c})
    (Signal.foldp update {postDB = Dict.empty} updates)
    cookie

main = Signal.map2 scene state Window.dimensions

