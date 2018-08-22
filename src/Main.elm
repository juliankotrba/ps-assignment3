import Html exposing (Html, div, input, textarea, text, span, button)
import Html.Events exposing(onInput, onClick)
import Html.Attributes exposing (..)
import Json.Decode
import Parser exposing (SyntaxComponent(..))
import Http

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

-- Model

type alias Model =
  { plainSourceCode: String
  , parsedSourceCode: List SyntaxComponent
  , url: String
  }

init : (Model, Cmd Msg)
init =
  ({ plainSourceCode = ""
   , parsedSourceCode = []
   , url = ""
  }
  , Cmd.none
  )

-- Update

type Msg
  = Reset
  | OnSpanClick String
  | OnCodeInput String
  | OnLoadCode
  | OnCodeLoaded (Result Http.Error String)
  | OnUrlInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      ({ model | plainSourceCode = "", parsedSourceCode = [] }, Cmd.none)

    OnSpanClick innerText ->
      ({ model | parsedSourceCode = (markSameOccurrences innerText (parse model.plainSourceCode)) }, Cmd.none)

    OnCodeInput changedCode ->
      ({ model | plainSourceCode = changedCode, parsedSourceCode = (parse changedCode) } , Cmd.none)

    OnUrlInput url ->
      ({ model | url = url }, Cmd.none)

    OnLoadCode ->
      (model, Http.send OnCodeLoaded (Http.getString model.url))

    OnCodeLoaded (Ok code) ->
      update (OnCodeInput code) model

    OnCodeLoaded (Err _) ->
      ({ model | plainSourceCode = "Loading failed" }, Cmd.none)


splitSourceCode : String -> List String
splitSourceCode s = String.split " " s
                 |> List.map (\s -> s++" ")

markSameOccurrences : String -> (List SyntaxComponent) -> (List SyntaxComponent)
markSameOccurrences markedText scs = List.map (\sc -> if (unwrap sc)==markedText then Marked (unwrap sc) else sc ) scs

unwrap : SyntaxComponent -> String
unwrap sc =
  case sc of
    Default s -> s
    Variable s -> s
    Literal s -> s
    Name s -> s
    Symbol s -> s
    Marked s -> s
    Error s -> s

createMarkedSpan : String -> Html Msg
createMarkedSpan t = span [ onSpanClick OnSpanClick, markedSpanStyle ] [text t]

createUnmarkedSpan : String -> Html Msg
createUnmarkedSpan t = span [ onSpanClick OnSpanClick, unmarkedSpanStyle ] [text t]

-- VIEW

view : Model -> Html Msg
view model = Html.div [ mainContainerStyle ] [
  div [ containerStyle ]
  [
    div [  ] [
      div [] [
        input [ urlInputStyle, placeholder "Load a file over http ..", onInput OnUrlInput] [ ],
        button [ loadButtonStyle , onClick OnLoadCode ] [ text "Load" ]
      ],
      textarea [  textareaStyle, rows 10, onKeyUp OnCodeInput ] [ text model.plainSourceCode ]
    ],
    div [ formattedCodeContainerStyle ] (List.map syntaxComponentToSpan model.parsedSourceCode)
  ]
 ]

onSpanClick : (String -> msg) -> Html.Attribute msg
onSpanClick tagger =
  Html.Events.on "click" (Json.Decode.map tagger decodeInnerTextAttr)

onKeyUp : (String -> msg) -> Html.Attribute msg
onKeyUp tagger =
  Html.Events.on "keyup" (Json.Decode.map tagger decodeValueAttr)

decodeInnerTextAttr : Json.Decode.Decoder String
decodeInnerTextAttr =
  Json.Decode.at ["target", "innerText"] Json.Decode.string

decodeValueAttr : Json.Decode.Decoder String
decodeValueAttr =
  Json.Decode.at [ "target", "value" ] Json.Decode.string

-- CSS styles

textareaStyle : Html.Attribute msg
textareaStyle =
  style
   [ ("resize", "none")
   , ("width", "100%")
   ]

mainContainerStyle : Html.Attribute msg
mainContainerStyle =
  style
    [ ("text-align", "center")
    , ("backgroundColor", "#263238")
    , ("height", "100%")
    , ("width", "100%")
    ]

containerStyle : Html.Attribute msg
containerStyle =
  style
    [ ("height", "100%")
    , ("width", "400px")
    , ("margin-top", "50px")
    , ("display", "inline-block")
    ]

formattedCodeContainerStyle : Html.Attribute msg
formattedCodeContainerStyle =
  style
    [ ("text-align", "left")
    , ("color", "white")
    ]

markedSpanStyle : Html.Attribute msg
markedSpanStyle =
  style
    [ ("color", "#FF1744")
    , ("font-weight", "bold")
    , ("font-family", "monospace")
    ]

unmarkedSpanStyle : Html.Attribute msg
unmarkedSpanStyle =
  style
    [ ("color", "white")
    , ("font-family", "monospace")
    ]

urlContainerStyle : Html.Attribute msg
urlContainerStyle =
  style
    [ {- TODO -} ]

urlInputStyle : Html.Attribute msg
urlInputStyle =
  style
    [ ("width", "250px")
    , ("float", "left")
    ]

loadButtonStyle : Html.Attribute msg
loadButtonStyle =
    style
      [ ("width", "140px")
      , ("float", "right")
      ]

-- Helper functions

parse s
  =  splitAndKeep "." s
  |> List.map String.trim
  |> List.map String.toList
  |> List.map (\r-> Parser.rule r)
  |> List.map parsedRuleToSyntaxComponents
  |> List.foldr (++) []

parsedRuleToSyntaxComponents r
  = case r of
      Ok p ->
        case p of
          (sc, _)::_ -> sc
          _ -> []
      _ -> []

syntaxComponentToSpan : SyntaxComponent -> Html Msg
syntaxComponentToSpan sc
  = case sc of
      Default s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Variable s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Literal s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Name s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Symbol s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Error s -> span [ onSpanClick OnSpanClick, {- TODO -} unmarkedSpanStyle ] [text s]
      Marked s -> span [ onSpanClick OnSpanClick, {- TODO -} markedSpanStyle ] [text s]

splitIntoRules s = List.map String.trim <| splitAndKeep "." s

splitAndKeep : String -> String -> List String
splitAndKeep d s
  = String.split d s |> List.filter (\s-> (s /= "") && (s /= "\n")) |> List.map (\s->s++d)

-- TODO: Remove later
rules = splitIntoRules code
code ="""
main-:findFiles-(*files):showFiles(*files)-.
showFiles(+x*x)-:showFile(+x)-:showFiles(*x)-.
showFiles()-.
findFiles-(*f):$(ls \\*\\.pdf)()-(*f)(*x).
showFile(+f)-:$(xpdf+f)()-(*a)(+b).
"""
