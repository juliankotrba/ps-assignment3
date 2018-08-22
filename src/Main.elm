import Html exposing (Html, div, input, textarea, text, span, button)
import Html.Events exposing(onInput, onClick)
import Html.Attributes exposing (..)
import Json.Decode
import Parser
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
  , splitSourceCode: List (Html Msg)
  , url: String
  }

init : (Model, Cmd Msg)
init =
  ({ plainSourceCode = ""
   , splitSourceCode = []
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
    Reset -> ({ model | plainSourceCode = "", splitSourceCode = [] }, Cmd.none)
    OnSpanClick innerText ->
      ({ model | splitSourceCode = [] {- (markSameOccurrences innerText (splitSourceCode model.plainSourceCode )) -} }, Cmd.none)

    OnCodeInput changedCode ->
      ({ model | plainSourceCode = changedCode, splitSourceCode = [(createMarkedSpan changedCode)]
        {-List.map (\s->createUnmarkedSpan s) (splitSourceCode model.plainSourceCode)-} } , Cmd.none)

    OnUrlInput url ->  ({ model | url = url }, Cmd.none)
    OnLoadCode -> (model, Http.send OnCodeLoaded (Http.getString model.url))
    OnCodeLoaded (Ok code) -> update (OnCodeInput code) model -- ({ model | plainSourceCode = code }, Cmd.none)
    OnCodeLoaded (Err _) -> ({ model | plainSourceCode = "Loading failed" }, Cmd.none)


splitSourceCode : String -> List String
splitSourceCode s = String.split " " s
                 |> List.map (\s -> s++" ")

markSameOccurrences : String -> List String -> List (Html Msg)
markSameOccurrences markedText l = List.map (\s -> if s==markedText then createMarkedSpan s else createUnmarkedSpan s) l

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
    div [ formattedCodeContainerStyle ] (model.splitSourceCode)
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

splitIntoRules s = splitAndKeep "." s

splitAndKeep : String -> String -> List String
splitAndKeep d s
  = String.split d s |> List.filter ((/=) "") |> List.map (\s->s++d)

code ="""
main-:findFiles-(*files):showFiles(*files)-.
showFiles(+x*x)-:showFile(+x)-:showFiles(*x)-.
showFiles()-.
findFiles-(*f):$(ls \\*\\.pdf)()-(*f)(*x).
showFile(+f)-:$(xpdf+f)()-(*a)(+b).
"""
