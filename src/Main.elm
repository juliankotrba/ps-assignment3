import Html exposing (Html, div, input, textarea, text, span, button, a)
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
  , parsedRules: List SyntaxComponent
  , url: String
  }

init : (Model, Cmd Msg)
init =
  ({ plainSourceCode = ""
   , parsedRules = []
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
      ({ model | plainSourceCode = "", parsedRules = [] }, Cmd.none)

    OnSpanClick innerText ->
      ({ model | parsedRules = (markSameOccurrences innerText (parse model.plainSourceCode)) }, Cmd.none)

    OnCodeInput changedCode ->
      ({ model | plainSourceCode = changedCode, parsedRules = (parse changedCode) } , Cmd.none)

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
    Unparsed s -> s
    Error s -> s
    Linebreak -> ""

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
    div [ formattedCodeContainerStyle ] (List.map syntaxComponentToSpan model.parsedRules),
    div [  ]
            [ a
                [ type_ "button"
                , href <| "data:text/plain;charset=utf-8," ++ model.plainSourceCode
                , downloadAs "code.txt"
                ]
                [ button [ downloadButtonStyle ] [ text "Save" ] ]
            ]
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

-- Helper functions

parse s
  =  String.lines s
  |> List.map String.toList
  |> List.map (\r-> Parser.rule r)
  |> List.map parsedRuleToSyntaxComponents
  |> List.map (\scs -> List.append scs [Linebreak])
  |> List.foldr (++) []

parsedRuleToSyntaxComponents r
  = case r of
      Ok p ->
        case p of
          (sc, np)::_ -> sc ++ [Unparsed <| String.fromList np]
          _ -> []
      Err (errMsg, p, np) ->
        case p of
          Just parsedSyntaxComponents ->
            parsedSyntaxComponents ++
              [Error <| (" // " ++ errMsg ++ if List.isEmpty np then "" else " -> " ++ String.fromList np)]

          Nothing -> []

syntaxComponentToSpan : SyntaxComponent -> Html Msg
syntaxComponentToSpan sc
  = case sc of
      Default s -> span [ onSpanClick OnSpanClick, defaultSpanStyle ] [text s]
      Variable s -> span [ onSpanClick OnSpanClick, variableSpanStyle ] [text s]
      Literal s -> span [ onSpanClick OnSpanClick, literalSpanStyle ] [text s]
      Name s -> span [ onSpanClick OnSpanClick, nameSpanStyle ] [text s]
      Symbol s -> span [ onSpanClick OnSpanClick, symbolSpanStyle ] [text s]
      Unparsed s -> span [ onSpanClick OnSpanClick, unparsedSpanStyle ] [text s]
      Error s -> span [ onSpanClick OnSpanClick, errorSpanStyle ] [text s]
      Marked s -> span [ onSpanClick OnSpanClick, markedSpanStyle ] [text s]
      Linebreak -> span [ linebreakSpanStyle ] [ ]

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

downloadButtonStyle : Html.Attribute msg
downloadButtonStyle =
  style
    [  ("width", "400px")
    , ("margin-top", "25px")
    ]

markedSpanStyle : Html.Attribute msg
markedSpanStyle =
  style
    [ ("color", "#FF1744")
    , ("font-weight", "bold")
    , ("font-family", "monospace")
    ]

defaultSpanStyle : Html.Attribute msg
defaultSpanStyle =
  style
    [ ("color", "white")
    , ("font-family", "monospace")
    ]

variableSpanStyle : Html.Attribute msg
variableSpanStyle =
  style
    [ ("color", "#FF9800")
    , ("font-family", "monospace")
    ]

symbolSpanStyle : Html.Attribute msg
symbolSpanStyle =
  style
    [ ("color", "#F48FB1")
    , ("font-family", "monospace")
    ]

literalSpanStyle : Html.Attribute msg
literalSpanStyle =
  style
    [ ("color", "#0091EA")
    , ("font-family", "monospace")
    ]

nameSpanStyle : Html.Attribute msg
nameSpanStyle =
  style
    [ ("color", "#00C853")
    , ("font-family", "monospace")
    ]

errorSpanStyle : Html.Attribute msg
errorSpanStyle =
  style
    [ ("color", "red")
    , ("font-family", "monospace")
    ]

unparsedSpanStyle : Html.Attribute msg
unparsedSpanStyle =
  style
    [ ("color", "white")
    , ("font-family", "monospace")
    ]

linebreakSpanStyle : Html.Attribute msg
linebreakSpanStyle =
  style
    [ ("display", "block") ]

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
