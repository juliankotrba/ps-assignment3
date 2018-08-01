import Html exposing (Html, div, input, textarea, text, span)
import Html.Events exposing(onInput)
import Html.Attributes exposing (..)
import Json.Decode

main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

--vtype CodeSequence = Wrapped String | Normal String

type alias Model =
  { plainSourceCode: String
  , splitSourceCode: List (Html Msg)
  }

model : Model
model = Model "" []

-- UPDATE

type Msg
  = Reset
  | OnSpanClick String
  | OnCodeInput String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> { model | plainSourceCode = "", splitSourceCode = [] }
    OnSpanClick innerText -> { model | splitSourceCode = (markSameOccurrences innerText (splitSourceCode model.plainSourceCode )) }
    OnCodeInput changedCode -> { model | plainSourceCode = changedCode, splitSourceCode = List.map (\s->createUnmarkedSpan s) (splitSourceCode model.plainSourceCode)}


splitSourceCode : String -> List String
splitSourceCode s = String.split " " s
            |> List.map (\s -> s++" ")
            --|> List.map (\s -> span [ onSpanClick OnSpanClick ] [text (s++" ")])

markSameOccurrences : String -> List String -> List (Html Msg)
markSameOccurrences markedText l = List.map (\s -> if s==markedText then createMarkedSpan s else createUnmarkedSpan s) l

createMarkedSpan : String -> Html Msg
createMarkedSpan t = span [ onSpanClick OnSpanClick, markedSpanStyle ] [text t]

createUnmarkedSpan : String -> Html Msg
createUnmarkedSpan t = span [ onSpanClick OnSpanClick ] [text t]

-- VIEW

view : Model -> Html Msg
view model = Html.div [ mainContainerStyle ] [
  div [ containerStyle ]
  [
    div [  ] [
      textarea [  textareaStyle, rows 10, cols 50, onTextAreaInput OnCodeInput ] [  ]
    ],
    div [ formattedCodeContainerStyle ] (model.splitSourceCode)
  ]
 ]


onTextAreaInput : (String -> msg) -> Html.Attribute msg
onTextAreaInput tagger =
  Html.Events.on "input" (Json.Decode.map tagger decodeValueAttr)

onSpanClick : (String -> msg) -> Html.Attribute msg
onSpanClick tagger =
  Html.Events.on "click" (Json.Decode.map tagger decodeInnerTextAttr)

decodeInnerTextAttr : Json.Decode.Decoder String
decodeInnerTextAttr =
  Json.Decode.at ["target", "innerText"] Json.Decode.string

decodeValueAttr : Json.Decode.Decoder String
decodeValueAttr =
  Json.Decode.at [ "target", "value" ] Json.Decode.string


-- CSS STYLES

textareaStyle : Html.Attribute msg
textareaStyle =
  style
   [
     ("resize", "none")
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
    , ("width", "300px")
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
