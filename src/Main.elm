import Html exposing (Html, div, input, textarea, text, span)
import Html.Events
import Html.Attributes exposing (..)
import Json.Decode

main = Html.beginnerProgram { model = model, view = view, update = update }

-- MODEL

--vtype CodeSequence = Wrapped String | Normal String

type alias Model = { plainSourceCode: String  }

model : Model
model = Model "fun isFunny() = False"

-- UPDATE

type Msg = Reset | OnSpanClick String

update : Msg -> Model -> Model
update msg model =
  case msg of
    Reset -> { model | plainSourceCode = "..." }
    OnSpanClick innerText -> { model | plainSourceCode = innerText }


-- VIEW

view : Model -> Html Msg
view model = Html.div [ mainContainerStyle ] [
  div [ containerStyle ]
  [
    div [  ] [
      textarea [ textareaStyle, rows 10, cols 50, placeholder model.plainSourceCode ] []
    ],
    div [ formattedCodeContainerStyle ] [
      span [ onSpanClick OnSpanClick ] [ text "code sequence .."],
      span [ onSpanClick OnSpanClick ] [ text ".. code sequence .."],
      span [ onSpanClick OnSpanClick ] [ text ".. code sequence .."],
      span [ onSpanClick OnSpanClick] [ text ".. code sequence .."]
    ]
  ]
 ]


onSpanClick : (String -> msg) -> Html.Attribute msg
onSpanClick tagger =
  Html.Events.on "click" (Json.Decode.map tagger decodeInnerTextAttr)

decodeInnerTextAttr : Json.Decode.Decoder String
decodeInnerTextAttr =
  Json.Decode.at ["target", "innerText"] Json.Decode.string

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
