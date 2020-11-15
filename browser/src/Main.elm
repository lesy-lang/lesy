port module Main exposing (main)

{-|Editor for lesy
-}

import Task

import Browser
import Browser.Events
import Browser.Navigation

import Url

import Element as Ui
import Element.Background as UiBackground
import Element.Font as UiFont
import Element.Input as UiInput
import Element.Border as UiBorder

import Svg exposing (Svg)
import Svg.Events
import Use.Svg

import Html
import Html.Attributes as HtmlAttr

import Html.Events.Extra.Mouse as Mouse

import Use.Math exposing (..)
import Use.Translate exposing (..)
import Use.Color exposing (..)

import Time exposing (..)
import Use.Animate exposing (pulse)

import File exposing (File)
import File.Download
import File.Select

import Json.Encode
import Json.Decode
import Use.Encode

import Use.List
import ZipList exposing (ZipList)
import Ids exposing (..)


main: Program () Model Msg
main=
  Browser.document
    { init= init
    , view= view
    , update= update
    , subscriptions= subscriptions
    }


type alias Morph=
  Translated
    { name: String
    , input: Group
    , paddedTextWidth: Float
    }
encodeMorph: Morph ->Json.Encode.Value
encodeMorph {translate, name, input, paddedTextWidth}=
  Json.Encode.object
    [ ( "translate", Use.Translate.encode translate )
    , ( "name", Json.Encode.string name )
    , ( "input", encodeGroup input )
    , ( "paddedTextWidth"
      , Json.Encode.float paddedTextWidth
      )
    ]
decodeMorph: Json.Decode.Decoder Morph
decodeMorph=
  Json.Decode.map4
    (\off name input width->
      { translate= off, name= name, input= input
      , paddedTextWidth= width
      }
    )
    (Json.Decode.field
      "translate" Use.Translate.decode
    )
    (Json.Decode.field
      "name" Json.Decode.string
    )
    (Json.Decode.field
      "input" decodeGroup
    )
    (Json.Decode.field
      "paddedTextWidth" Json.Decode.float
    )

type alias Group=
  Translated 
    { input: IdDict (Maybe Id) }
encodeGroup: Group ->Json.Encode.Value
encodeGroup group=
  Json.Encode.object
    [ ( "translate"
      , Use.Translate.encode (.translate group)
      )
    , ( "input"
      , Ids.encodeDict
          (Use.Encode.encodeMaybe Ids.encodeId)
          (.input group)
      )
    ]


decodeGroup: Json.Decode.Decoder Group
decodeGroup=
  Json.Decode.map2
    (\off input-> { translate= off, input= input })
    (Json.Decode.field
      "translate" Use.Translate.decode
    )
    (Json.Decode.field
      "input"
      (Ids.decodeDict
        (Json.Decode.nullable Ids.decodeId
        )
      )
    )


move:
  Translate ->Translated a ->Translated a
move off=
  updateTranslate (combine (+) off)

type Selectable=
    NothingSelected
  | MorphSelected Id
  | EditorSelected (Maybe Id)

type Pressable=
    MorphPressed Id
  | NoMorphPressed (Id ->Msg)
  | GroupPressed Id
  | NotPressed

type Drag=
    Drag
  | NoDrag

{-would like to eliminate this-}
type EditorPressed=
    EditorPressed
  | EditorNotPressed

type alias Sizes=
  SizesInMorphSelected
  (SizesInMorph
  (SizesInNoMorph
  (SizesInGroup
  (SizesInConnection
    { defaultMorphTranslate: Translate
    , trunkTranslate: Translate
    , defaultGroupTranslate: Translate
    }
  ))))

scaleSizes: (Float ->Float) ->Sizes
scaleSizes scale=
  let fontSize= 22|>scale
      textHeight= Use.Svg.computeHeight fontSize
      pad= (*) 0.4 fontSize
      boxHeight= textHeight|>padded pad
      triangleWidth=
        equilateralTriangleHeight ((/) boxHeight 2)
  in
  { fontSize= fontSize
  , textHeight= textHeight
  , halfBoxHeight= (/) boxHeight 2
  , triangleWidth= triangleWidth
  , triangleCircumradius=
      (*) triangleWidth ((/) 2 3)
  , pad= pad
  , lineWidth= 3.9|>scale
  , defaultMorphTranslate=
      { x= 64, y= 0 }|>mapXY scale
  , trunkTranslate=
      { x= 140, y= 300 }|>mapXY scale
  , defaultGroupTranslate=
      { x= 48, y= 0 }|>mapXY scale
  }

type alias Project=
  { name: String
  , selected: Selectable
  , trunkMorph: Maybe Id
  , morphs: IdDict Morph
  }

type alias Model=
  { ticks: Int
  
  , pressed: Pressable
  , drag: Drag
  , belowMouse: Pressable
  , editorPressed: EditorPressed

  , scale: Float ->Float
  
  , projects: ZipList Project
  , waitingForComputedTextWidth: List Id
  , sizes: Sizes
  }

init: () ->( Model, Cmd Msg )
init flags=
  ( { ticks= 0

    , pressed= NotPressed
    
    , drag= NoDrag
    , editorPressed= EditorNotPressed
    , belowMouse= NotPressed

    , projects=
        ZipList.singleton blankProject
    , waitingForComputedTextWidth= []

    , scale= (*) 1
    , sizes= scaleSizes ((*) 1)
    }

  , generateId PlaceTrunk
    --dummy morph for testing
  )

blankProject: Project
blankProject=
  { name= "not-named"
  , morphs= Ids.empty
  , trunkMorph= Nothing
  , selected= NothingSelected
  }


type Msg=
    FrameTick
  | Save
  | Select
  | RecieveSelectFile File
  | RecieveSelect String
  | AddBlankProject
  | SelectProject Int
  | Move Translate
  | Press Pressable
  | PressEditor
  | MouseOn Pressable
  | Lift
  | Scale Float
  | SelectMorph Id
  | RenameMorph String
  | PlaceTrunk Id
  | AddInput Id Id
  | RecieveComputedTextWidth Float

encodeState:
    Maybe Id-> IdDict Morph
  ->Json.Encode.Value
encodeState trunk morphs=
  Json.Encode.object
    [ ( "trunk"
      , trunk
        |>Maybe.map
            Ids.encodeId
        |>Maybe.withDefault
            (Json.Encode.string "")
      )
    , ( "morphs"
      , Ids.encodeDict
          encodeMorph morphs
      )
    ]
decodeState: String ->Model ->Model
decodeState content model=
  let orFail=
        Result.withDefault model
        --failed. maybe a dialog?
  in
  Json.Decode.decodeString
    (Json.Decode.field
      "morphs" (Ids.decodeDict decodeMorph)
    )
    content
  |>Result.map
      (\morphs->
        Json.Decode.decodeString
          (Json.Decode.field
            "trunk" (Json.Decode.maybe Ids.decodeId)
          )
          content
        |>Result.map
            (\trunk->
              {model
              | projects=
                  ZipList.insert
                    {blankProject
                    | trunkMorph= trunk
                    , morphs= morphs
                    }
                    (.projects model)
              }
            )
        |>orFail
      )
  |>orFail

update: Msg ->Model ->( Model, Cmd Msg )
update msg ({sizes} as model)=
  case msg of
    FrameTick->
      ( {model
        | ticks= (+) 1 (.ticks model)
        }
      , Cmd.none
      )
    
    Save->
      ( model
      , File.Download.string
          "not-named.lesy.json" "application/json"
          (let project= ZipList.current (.projects model)
          in
          encodeState
            (.trunkMorph project) (.morphs project)
          |>Json.Encode.encode 4
          )
      )
    
    Select->
      ( model
      , File.Select.file [ "application/json" ]
          RecieveSelectFile
      )
    
    RecieveSelectFile file->
      case
        String.endsWith ".lesy.json"
        (File.name file)
        of
        True->
          ( model
          , Task.perform
              RecieveSelect (File.toString file)
          )
        False->
          ( model, Cmd.none )
    
    RecieveSelect content->
      ( decodeState content model
      , Cmd.none
      )
    
    AddBlankProject->
      ( {model
        | projects=
            ZipList.insert blankProject
              (.projects model)
        }
      , generateId PlaceTrunk
      )
    
    SelectProject index->
      ( {model
        | projects=
            ZipList.goToIndex index (.projects model)
            |>Maybe.withDefault (.projects model)
        }
      , Cmd.none
      )
    
    Move off->
      ( let dragMorph id=
              inDrag Drag
              <|(updateMorph id (move off)
                |>forSelectedProject
                )
              <|model
        in
        case .pressed model of
          MorphPressed id->
            dragMorph id
          
          NotPressed->
            case .editorPressed model of
              EditorPressed->
                .trunkMorph (ZipList.current (.projects model))
                |>Maybe.map dragMorph
                |>Maybe.withDefault model

              EditorNotPressed->
                model
          
          NoMorphPressed _->
            model
          
          GroupPressed output->
            inDrag Drag
            <|(forSelectedProject
                (updateMorph output
                  (\morph->
                    {morph
                    | input= move off (.input morph)
                    }
                  )
                )
              )
            <|model

      , Cmd.none
      )
    
    Lift->
      let upModel=
            inDrag NoDrag
            <|inPressed NotPressed
            <|{model
              | editorPressed= EditorNotPressed
              }
      in
      case .drag model of
        NoDrag->
          case .pressed model of
            NotPressed->
              ( case .editorPressed model of
                  EditorPressed->
                    upModel
                    |>forSelectedProject
                        (inSelect
                          (EditorSelected
                            (case .selected (ZipList.current (.projects model)) of
                              EditorSelected maybeId-> maybeId
                              MorphSelected id-> Just id
                              NothingSelected-> Nothing
                            )
                          )
                        )

                  EditorNotPressed->
                    upModel
              , Cmd.none
              )
            
            MorphPressed id->
              ( upModel
                |>(forSelectedProject
                    (inSelect (MorphSelected id))
                  )
              , Cmd.none
              )
            
            NoMorphPressed createMsg->
              ( upModel
                |>(inSelect NothingSelected
                  |>forSelectedProject
                  )
              , Ids.generateId createMsg
              )
            
            GroupPressed output->
              ( upModel, Cmd.none )
                
        Drag->
          ( upModel, Cmd.none )
    
    Press mouseFirstDown->
      ( case .pressed model of
          NotPressed->
            inPressed mouseFirstDown model
          
          _-> model
      , Cmd.none
      )
    
    PressEditor->
      ( {model| editorPressed= EditorPressed }
      , Cmd.none
      )
    
    MouseOn part->
      ( {model| belowMouse= part }
      , Cmd.none
      )
    
    Scale factor->
      ( forSelectedProject
          (updateMorphs
            (mapEach
                (\_ morph->
                  updateTranslate
                    (mapXY ((*) factor))
                    {morph
                    | paddedTextWidth= 
                        (*) factor (.paddedTextWidth morph)
                    , input=
                        updateTranslate (mapXY ((*) factor))
                          (.input morph)
                    }
                )
            )
          )
        <|let lastFactor= (.scale model) 1
              scale= (*) ((*) factor lastFactor)
          in
          {model
          | scale= scale
          , sizes= scaleSizes scale
          }
      , Cmd.none
      )
    
    SelectMorph to->
      ( (inSelect (MorphSelected to)
         |>forSelectedProject
        )
        <|model
      , Cmd.none
      )
    
    RenameMorph to->
      ( case .selected (ZipList.current (.projects model)) of
          MorphSelected id->
            forSelectedProject
              (updateMorph id 
                (\morph-> {morph | name= to })
              )
            <|{model
              | waitingForComputedTextWidth=
                  id::(.waitingForComputedTextWidth model)
              }
          
          _ ->model
      , computeMorphTextWidth to
      )
    
    PlaceTrunk id->
      addMorph id (.trunkTranslate sizes)
      <|(inTrunk (Just id)|>forSelectedProject)
      <|model
    
    AddInput toId ownId->
      model
      |>forSelectedProject
          (updateMorph toId
              (\morph->
                let morphInput= .input morph
                    groupInput= .input morphInput
                in
                {morph
                | input=
                    {morphInput
                    | input=
                        Ids.update
                          toId (\_-> Just ownId)
                        <|groupInput
                    }
                }
              )
          )
      |>addMorph ownId
          (.defaultMorphTranslate sizes)
    
    RecieveComputedTextWidth width->
      ( List.head (.waitingForComputedTextWidth model)
        |>Maybe.map
          (\id->
            {model
            | waitingForComputedTextWidth=
                Use.List.removeHead
                  (.waitingForComputedTextWidth model)
            }
            |>forSelectedProject
                (updateMorph id
                  (\morph->
                    { morph
                    | paddedTextWidth=
                        ((*) width (.fontSize sizes))
                        |>padded (.pad sizes)
                    }
                  )
                )
          )
          |>Maybe.withDefault model
      , Cmd.none
      )

inDrag: Drag ->Model ->Model
inDrag drag model=
  {model | drag= drag }

inPressed: Pressable ->Model ->Model
inPressed mouseDownOn model=
  {model | pressed= mouseDownOn }

forSelectedProject:
  (Project ->Project) ->Model ->Model
forSelectedProject change model=
  {model
  | projects=
      Use.List.updateSelected
      change (.projects model)
  }

inTrunk: Maybe Id ->Project ->Project
inTrunk trunk project=
  {project | trunkMorph= trunk }

updateMorphs:
    (IdDict Morph ->IdDict Morph)
  ->Project ->Project
updateMorphs change project=
  {project | morphs= change (.morphs project) }

inSelect: Selectable ->Project ->Project
inSelect selected project=
  {project | selected= selected }

addMorph:
    Id ->Translate
  ->Model ->( Model, Cmd Msg )
addMorph
  id translate
  ({scale, sizes} as model)
  =
  let name= "🥗 ← 🍅 🥬"
  in
  ( {model
    | waitingForComputedTextWidth=
        id::(.waitingForComputedTextWidth model)
    }
    |>forSelectedProject
        (updateMorphs
          (add id
            { name= name
            , translate= translate
            , input=
                { input= Ids.empty
                , translate= (.defaultGroupTranslate sizes)
                }
            , paddedTextWidth=
                scale
                (Use.Svg.aproximateMonospacedWidth
                  (.fontSize sizes) name
                )
                |>padded (.pad sizes)
            }
          )
        >>inSelect (MorphSelected id)
        )

  , computeMorphTextWidth name
  )

updateMorph:
  Id ->(Morph ->Morph) ->Project ->Project
updateMorph id change=
  updateMorphs (Ids.update id change)
  

subscriptions: Model ->Sub Msg
subscriptions _=
  Sub.batch
    [ Time.every ((/) 1000 60) (\_ ->FrameTick)
    , recieveComputedTextWidth
        RecieveComputedTextWidth
    ]

port recieveComputedTextWidth:
  (Float ->msg) ->Sub msg
port computeTextWidth:
  { text: String, font: String }
  ->Cmd msg

computeMorphTextWidth: String ->Cmd msg
computeMorphTextWidth text=
  computeTextWidth
    { font= "1px "++morphFont, text= text }


view: Model ->Browser.Document Msg
view model=
  { title= "lesy"
  , body=
      [ Html.div
          [ HtmlAttr.style "height" "200%" ]
          [ Ui.layoutWith
              { options=
                  [ Ui.focusStyle noChangeOnFocus
                  ]
              }
              [] (viewPage model)
          ]
      ]
  }
noChangeOnFocus: Ui.FocusStyle
noChangeOnFocus=
  { borderColor= Nothing
  , backgroundColor= Nothing
  , shadow= Nothing
  }
  

viewPage: Model ->Ui.Element Msg
viewPage model=
  Ui.column
    [ Ui.height Ui.fill
    , Ui.width Ui.fill
    ]
    ([ viewTitleScreen
    , viewEditorScreen model
    ]
    |>List.map
      (Ui.el
        [ Ui.height (Ui.fillPortion 1)
        , Ui.width Ui.fill
        ]
      )
    )

viewTitleScreen: Ui.Element Msg
viewTitleScreen= 
  Ui.column
    [ Ui.width Ui.fill
    , Ui.height Ui.fill
    ]
    [ Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fillPortion 6)
        ]
        viewTitle
    , Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fillPortion 1)
        , UiBackground.color
            (attentionColor.rgb|>toRgb Ui.rgb)
        , UiFont.color (Ui.rgb 1 1 1)
        , Ui.paddingXY 20 0
        ]
        (Ui.el
          [ Ui.centerY ]
          (Ui.text catchPhrase)
        )
    ]

viewTitle: Ui.Element msg
viewTitle=
  Ui.column
    [ Ui.width Ui.fill
    , Ui.height Ui.fill
    ]
    [ Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fillPortion 4)
        , UiBackground.color editorColor
        , UiFont.color (Ui.rgb 1 1 1)
        , Ui.padding 10
        , UiFont.size 50
        , UiFont.family [ UiFont.monospace ]
        ]
        (Ui.el
          [ Ui.centerX, Ui.alignBottom ]
          (Ui.text "lesy")
        )
    , Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fillPortion 2)
        , UiBackground.color (Ui.rgb 0.063 0.067 0.047)
        , UiFont.color (Ui.rgb 0.59 0.63 0.078)
        ]
        (Ui.el
          [ Ui.centerX ]
          (Ui.text "has  a s c e n d e d")
        )
    ]

attentionColor: Rgba
attentionColor=
  { red= 0.0, green= 0.22, blue= 0.17 }
  |>withAlpha 0.51

catchPhrase: String
catchPhrase=
  """
    view friendly code.
simple & fun
  """

viewEditorScreen: Model ->Ui.Element Msg
viewEditorScreen
  { projects
  , drag, belowMouse
  , sizes, ticks
  }
  =
  let selectedProject= ZipList.current projects
      searchMorph id=
        search id (.morphs selectedProject)
  in
  Ui.column
    [ Ui.width Ui.fill
    , Ui.height Ui.fill
    , UiBackground.color (Ui.rgb 0 0 0)
    ]
    [ viewEditor
        (.trunkMorph selectedProject)
        searchMorph
        (.selected selectedProject)
        ticks
        (case drag of
          Drag-> Use.Svg.hand
          NoDrag->
            case belowMouse of
              NotPressed-> Use.Svg.arrow
              _-> Use.Svg.finger
        )
        sizes

    , case .selected selectedProject of
        EditorSelected maybeId->
          viewEditorProperties
            (.morphs selectedProject) maybeId

        MorphSelected id->
          (searchMorph id)
          |>Maybe.map
              (.name >>viewMorphProperties)
          |>Maybe.withDefault Ui.none
          
        NothingSelected-> Ui.none

    , viewOptions projects
    ]

editorColor: Ui.Color
editorColor=
  Ui.rgb 0.039 0.035 0.031

viewEditor:
    Maybe Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Use.Svg.Cursor
  ->Sizes
  ->Ui.Element Msg
viewEditor
  trunkId searchMorph
  selectable ticks cursor
  sizes
  =
  Ui.el
    [ Ui.height Ui.fill
    , Ui.width Ui.fill
    , Ui.height (Ui.fillPortion 9)
    , UiBackground.color editorColor
    , UiFont.color (Ui.rgb 0.14 0.14 0.14)
    ]
    (Ui.html
      (Svg.svg
        [ Use.Svg.width (Use.Svg.relative 1)
        , Use.Svg.height (Use.Svg.relative 1)
        , Use.Svg.cursor cursor
        , Mouse.onUp (\_->Lift)
        , Mouse.onDown (\_->PressEditor)
        , Use.Svg.mouseMoves Move
        ]
        [ viewTree
            trunkId searchMorph
            selectable ticks sizes
        ]
      )
    )

viewTree:
    Maybe Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Sizes
  ->Svg Msg
viewTree
  trunk searchMorph
  selected ticks sizes
  =
  trunk
  |>Maybe.map
      (\id->
        viewMorph
          id searchMorph
          selected ticks sizes
      )
  |>Maybe.withDefault
      (viewNoMorph PlaceTrunk sizes)

expect:
    Maybe v ->Use.Svg.Size ->(v ->Svg msg)
  ->Svg msg
expect maybeExists fontSize toSvg=
  maybeExists
  |>Maybe.map toSvg
  |>Maybe.withDefault
      (viewLabel
        fontSize
        "morph not found..."
      )


padded: number ->number ->number
padded by length=
  by+ length +by

viewMorph:
    Id ->(Id ->Maybe Morph)
  ->Selectable ->Int
  ->SizesInMorphSelected
    (SizesInMorph
      {a
      | textHeight: Float
      , lineWidth: Float
      , triangleCircumradius: Float
      , defaultMorphTranslate: Translate
      }
    )
  ->Svg Msg
viewMorph
  morphId searchMorph
  selected ticks
  ({ fontSize
    , halfBoxHeight
    , triangleWidth
    }
    as sizes
  )
  =
  expect (searchMorph morphId)
    (Use.Svg.px fontSize)
    (\{ name, translate, paddedTextWidth
      , input
      }->
      let afterTextX=
           (+) triangleWidth paddedTextWidth
          fullWidth= (+) afterTextX triangleWidth
      in
      Use.Svg.group
        [ Use.Svg.translate translate ]
        ((case selected of
            MorphSelected id->
              if (==) morphId id
              then
                [ viewMorphSelectedShadow
                    paddedTextWidth sizes ticks
                ]
              else []
            _ ->[]
          )
          ++
          [ Use.Svg.group
              [ Use.Svg.translate { x= afterTextX, y= 0 }
              ]
              (let begin= morphInputLineStart sizes
                   end= .translate input
              in
              [ viewConnection groupColor
                  begin end sizes
              , viewGroup
                  morphId searchMorph
                  selected ticks
                  (rotation (combine (-) end begin)) sizes
                  input
              ]
              )
          , viewMorphShape
              morphId name
              (morphColor|>(toRgba Use.Svg.rgba))
              paddedTextWidth sizes
          ]
        )
    )

viewGroup:
    Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Float
  ->SizesInGroup
    (SizesInNoMorph
    (SizesInConnection
      {a
      | fontSize: Float
      , defaultMorphTranslate: Translate
      }
    ))
  ->Group ->Svg Msg
viewGroup
  outputId searchMorph
  selected ticks turn
  ({ lineWidth, fontSize
    , defaultMorphTranslate
    }
    as sizes
  )
  { input, translate }
  =
  Use.Svg.group
    [ Use.Svg.translate translate ]
    ((viewGroupShape outputId turn sizes)
    ::
    (input
    |>Ids.values
    |>List.map
        (\morph->
          let begin= morphInputLineStart sizes
          in
          morph
          |>Maybe.map
              (\id->
                [ expect (id|>searchMorph)
                    (Use.Svg.px fontSize)
                    (\inputMorph->
                      let end= 
                            .translate inputMorph
                            |>mapX ((+) (moveInsideTriangle lineWidth))
                      in
                      viewConnection morphColor
                        begin end sizes
                    )
                , viewMorph
                    id searchMorph
                    selected ticks sizes
                ]
              )
          |>Maybe.withDefault
              [ viewConnection noMorphColor
                  begin defaultMorphTranslate
                  sizes 
              , Use.Svg.group
                  [ Use.Svg.translate defaultMorphTranslate
                  ]
                  [ viewNoMorph
                      (AddInput outputId) sizes
                  ]
              ]
        )
    |>List.concat
    )
  )

type alias SizesInGroup a=
  {a
  | triangleCircumradius: Float
  , halfBoxHeight: Float
  }
viewGroupShape:
    Id-> Float ->SizesInGroup a
  -> Svg Msg
viewGroupShape
  output turn
  { triangleCircumradius, halfBoxHeight }
  =
  Use.Svg.group
    [ drags (GroupPressed output) ]
    [ Use.Svg.circle 
        halfBoxHeight
        [ Use.Svg.fillColor
            (groupColor|>toRgba Use.Svg.rgba)
        ]
    , Use.Svg.polygon
        (regularPoly 5
          triangleCircumradius
          ((+) turn ((*) ((/) 1 5) pi))
        )
        [ Use.Svg.fillColor
            (connectionColorFromMorph groupColor)
        ]
    ]


type alias SizesInMorphSelected a=
  {a
  | triangleWidth: Float
  , halfBoxHeight: Float
  }
viewMorphSelectedShadow:
    Float ->SizesInMorphSelected a
  ->Int ->Svg msg
viewMorphSelectedShadow
  paddedTextWidth
  { triangleWidth, halfBoxHeight }
  ticks
  =
  let puls=
        pulse
          { min= 5.1, max= 8.1
          , loopFrames= 90
          }
          ticks
      afterTextX=
        (+) paddedTextWidth triangleWidth
      bottom= (+) halfBoxHeight puls
  in
  Use.Svg.polygon
    [ x0y0
    , { x= triangleWidth
      , y= bottom
      }
    , { x= afterTextX
      , y= bottom
      }
    , { x= (+) afterTextX triangleWidth
      , y= 0
      }
    , { x= afterTextX
      , y= halfBoxHeight
      }
    , { x= triangleWidth
      , y= halfBoxHeight
      }
    ]
    [ Use.Svg.fillColor 
        (attentionColor|>toRgba Use.Svg.rgba)
    ]

drags: Pressable ->Svg.Attribute Msg
drags=
  Svg.Events.onMouseDown <<Press

clickable:
  Pressable ->List (Svg.Attribute Msg)
clickable part=
  [ drags part
  , Svg.Events.onMouseOver (MouseOn part)
  , Svg.Events.onMouseOut (MouseOn NotPressed)
  ]

type alias SizesInMorph a=
  {a
  | fontSize: Float
  , triangleWidth: Float
  , halfBoxHeight: Float
  , pad: Float
  }
viewMorphShape:
    Id
  ->String ->Use.Svg.Color
  ->Float ->SizesInMorph a
  ->Svg Msg
viewMorphShape
  morphId name color
  textWidth
  { fontSize
  , triangleWidth, halfBoxHeight, pad
  }
  =
  Use.Svg.group
    (clickable (MorphPressed morphId))
    [ Use.Svg.polygon
        (boxPoints
          triangleWidth textWidth halfBoxHeight
        )
        [ Use.Svg.fillColor color
        ]
    , Use.Svg.group
        [ Use.Svg.translate
            { x= (+) triangleWidth pad
            , y= (-) halfBoxHeight pad
            }
        ]
        [ viewLabel
            (Use.Svg.px fontSize)
            name
        ]
    ]

boxPoints:
  Float ->Float ->Float ->List Translate
boxPoints triangleWidth textWidth halfHeight=
  let afterTextX= (+) triangleWidth textWidth
      fullWidth= (+) afterTextX triangleWidth
  in
  [ x0y0
  , { x= triangleWidth, y= -halfHeight }
  , { x= afterTextX,    y= -halfHeight }
  , { x= fullWidth,     y= 0 }
  , { x= afterTextX,    y= halfHeight }
  , { x= triangleWidth, y= halfHeight }
  ]


morphColor: Rgba
morphColor=
  { red= 0.0, green= 0.3, blue= 0.134 }
  |>withAlpha 0.6

groupColor: Rgba
groupColor=
  { red= 0.425, green= 0.2, blue= 0.3 }
  |>withAlpha 0.6

noMorphColor: Rgba
noMorphColor=
  { red= 0.8, green= 0.28, blue= 0.16 }
  |>withAlpha 0.34

connectionColorFromMorph: Rgba ->Use.Svg.Color
connectionColorFromMorph=
  .rgb
  >>mapRed ((+) 0.35)
  >>mapGreen ((+) 0.21)
  >>mapBlue ((+) -0.11)
    >>toRgb Use.Svg.rgb

moveInsideTriangle: Float ->Float
moveInsideTriangle lineWidth=
  (/) lineWidth (sqrt 2)

morphInputLineStart:
    {a
    | triangleWidth: Float
    , lineWidth: Float
    }
  ->Translate
morphInputLineStart { triangleWidth, lineWidth }=
  x0y0|>mapX
      (\_->
        (-) triangleWidth
          (moveInsideTriangle lineWidth)
      )

type alias SizesInConnection a=
  {a
  | triangleWidth: Float
  , triangleCircumradius: Float
  , lineWidth: Float
  }
viewConnection:
    Rgba ->Translate ->Translate
  ->SizesInConnection a
  ->Svg msg
viewConnection
  baseColor begin end
  ({ triangleWidth, triangleCircumradius, lineWidth }
    as sizes
  )
  =
  Use.Svg.group
    [ Use.Svg.translate begin ]
    (let delta= combine (-) end begin
         color= connectionColorFromMorph baseColor
    in
    [ Use.Svg.polygon
      (regularPoly 3
        triangleCircumradius
        ((+) (1/3* pi) (rotation delta))
      )
      [ Use.Svg.fillColor color
      , Use.Svg.translate
          (delta|>mapXY (\xy ->(/) xy 2))
      ]
    , Use.Svg.line
        [ x0y0, delta ]
        [ Use.Svg.fillColor Use.Svg.transparent
        , Use.Svg.lineColor color
        , Use.Svg.lineWidth
            (Use.Svg.px lineWidth)
        , Use.Svg.linecap Use.Svg.roundCap
        ]
    ]
    )
  

type alias SizesInNoMorph a=
  {a
  | textHeight: Float
  , lineWidth: Float
  , pad: Float
  }
viewNoMorph:
  (Id ->Msg) ->SizesInNoMorph a
  ->Svg Msg
viewNoMorph
  generatedMsg
  { textHeight, pad, lineWidth }
  =
  let plusArmLength= (/) textHeight 2
      radius= (+) pad plusArmLength
  in
  Use.Svg.group
    (clickable (NoMorphPressed generatedMsg))
    [ Use.Svg.circle radius
        [ Use.Svg.translate { x= radius, y= 0 }
        , Use.Svg.fillColor
            (noMorphColor |>toRgba Use.Svg.rgba)
        ]
    , Use.Svg.line
        (plusPoints plusArmLength)
        [ Use.Svg.translate { x= radius, y= 0 }
        , Use.Svg.lineWidth
            (Use.Svg.px lineWidth)
        , Use.Svg.linecap Use.Svg.roundCap
        , Use.Svg.linejoin Use.Svg.roundJoin
        , Use.Svg.lineColor
            (connectionColorFromMorph noMorphColor)
        , Use.Svg.fillColor Use.Svg.transparent
        ]
    ]
  
plusPoints: Float ->List Translate
plusPoints midDist=
  [ { x= 0, y= -midDist }
  , { x= 0, y= midDist }
  , x0y0
  , { x= -midDist, y= 0 }
  , { x= midDist, y= 0 }
  ]

morphFont: String
morphFont=
  "Noto Sans"

viewLabel: Use.Svg.Size ->String ->Svg msg
viewLabel fontSize name=
  Use.Svg.text name
    [ Use.Svg.fillColor (Use.Svg.rgb 1 1 1)
    , Use.Svg.fontFamily morphFont
    , Use.Svg.fontSize fontSize
    , noTextSelect
    ]

noTextSelect: Html.Attribute msg
noTextSelect=
  HtmlAttr.style
    "-webkit-user-select" "none"


viewTabRow:
  List (Ui.Element msg) ->Ui.Element msg
viewTabRow=
  viewInput []
  <<Ui.wrappedRow
      [ Ui.spacing 8
      , Ui.height Ui.fill
      , Ui.width Ui.fill
      ]

viewMorphTabs:
  IdDict Morph ->Maybe Id ->Ui.Element Msg
viewMorphTabs morphs selected=
  viewTabRow
    (let tab=
          selected
          |>Maybe.map
              (\selectedId->
                (\id->
                  (case (==) id selectedId of
                    True->
                      viewSelectedTab
                    False->
                      viewUnselectedTab
                  )
                  <|SelectMorph id
                )
              )
          |>Maybe.withDefault
              (viewUnselectedTab <<SelectMorph)
    in
    morphs
    |>mapEach
        (\id morph->
          tab id (.name morph)
            (morphColor|>toRgba Ui.rgba)
        )
    |>values
    )

viewTab:
    Int ->Ui.Color ->msg ->String ->Ui.Color
  ->Ui.Element msg
viewTab padY fontColor onPress name bgColor=
  UiInput.button 
    [ UiBackground.color bgColor
    , UiFont.color fontColor
    , Ui.paddingXY 4 padY
    , Ui.height Ui.fill
    ]
    { onPress= Just onPress,
      label=
        Ui.el
          [ Ui.centerY, Ui.centerX ]
          (Ui.text name)
    }
viewUnselectedTab:
  msg ->String ->Ui.Color ->Ui.Element msg
viewUnselectedTab=
  viewTab 4 (Ui.rgb 0.55 0.55 0.55)

viewSelectedTab:
  msg ->String ->Ui.Color ->Ui.Element msg
viewSelectedTab=
  viewTab 10 (Ui.rgb 1 1 1)

viewInput:
    List (Ui.Attribute msg)
  ->Ui.Element msg
  ->Ui.Element msg
viewInput attrs inputElement=
  Ui.column
    ([ Ui.paddingXY 0 5 ]
    ++attrs
    )
    [ Ui.el
        [ Ui.height Ui.fill ]
        (Ui.el
          [ UiFont.color (Ui.rgb 1 1 1)
          , Ui.padding 10
          ]
          inputElement
        )
    , Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fill)
        , Ui.paddingXY 5 0
        ]
        (Ui.html
          (Svg.svg
            [ Use.Svg.height (Use.Svg.px 2)
            , Use.Svg.width (Use.Svg.relative 1)
            ]
            [ Svg.rect
                [ Use.Svg.height (Use.Svg.relative 1)
                , Use.Svg.width (Use.Svg.relative 1)
                , Use.Svg.fillColor (Use.Svg.rgba 1 0.36 0 0.7)
                ] []
            ]
          )
        )
    ]


viewMorphProperties: String ->Ui.Element Msg
viewMorphProperties name=
  Ui.row
    [ Ui.width Ui.fill
    ]
    [ viewInput [ Ui.width Ui.fill ]
        (UiInput.search
          [ UiBackground.color (Ui.rgba 0 0 0 0)
          , UiBorder.color (Ui.rgba 0 0 0 0)
          , Ui.width Ui.fill
          , Ui.height Ui.fill
          , UiFont.family [ UiFont.typeface morphFont ]
          --, selection color attentionColor
          ]
          { onChange= RenameMorph
          , text= name
          , placeholder= Nothing
          , label= UiInput.labelHidden "name"
          }
        )
    ]

viewEditorProperties:
  IdDict Morph ->Maybe Id ->Ui.Element Msg
viewEditorProperties morphs selected=
  Ui.row
    [ Ui.width Ui.fill
    , Ui.spacing 4
    ]
    [ Ui.row
        [ Ui.width Ui.fill
        ]
        [ viewScaleButton "⌃" (Scale ((/) 11 10))
        , viewScaleButton "⌄" (Scale ((/) 10 11))
        ]
    , viewMorphTabs morphs selected
    ]

viewScaleButton: String ->msg ->Ui.Element msg
viewScaleButton text msg=
  viewInput [ UiFont.family [ UiFont.monospace ] ]
    (UiInput.button []
      { label= Ui.text text
      , onPress= Just msg
      }
    )


viewOptions: ZipList Project ->Ui.Element Msg
viewOptions projects=
  Ui.row
    [ Ui.width Ui.fill
    ]
    ([ viewSaveButton
    , viewSelectButton
    , viewAddBlankProjectButton
    , viewProjectTabs projects
    ]
    |>List.map (Ui.el [ Ui.alignBottom ])
    )

viewSaveButton: Ui.Element Msg
viewSaveButton=
  viewInput []
    (UiInput.button []
      { label=
          Ui.el
            [ UiFont.color (Ui.rgb 1 1 1)
            , UiFont.family [ UiFont.sansSerif ]
            , Ui.centerX
            , Ui.centerY
            ]
            (Ui.text "↧")
      , onPress= Just Save
      }
    )

viewSelectButton: Ui.Element Msg
viewSelectButton=
  viewInput []
    (UiInput.button []
      { label=
          Ui.el
            [ UiFont.color (Ui.rgb 1 1 1)
            , UiFont.family [ UiFont.sansSerif ]
            , Ui.centerX
            , Ui.centerY
            ]
            (Ui.text "↥")
      , onPress= Just Select
      }
    )

viewAddBlankProjectButton: Ui.Element Msg
viewAddBlankProjectButton=
  viewInput []
    (UiInput.button []
      { label=
          Ui.el
            [ UiFont.color (Ui.rgb 1 1 1)
            , UiFont.family [ UiFont.sansSerif ]
            , Ui.centerX
            , Ui.centerY
            ]
            (Ui.text "+")
      , onPress= Just AddBlankProject
      }
    )

viewProjectTabs: ZipList Project ->Ui.Element Msg
viewProjectTabs projects=
  viewTabRow
    (projects
    |>ZipList.indexedSelectedMap
      (\index isSelected project->
        case isSelected of
          True->
            viewSelectedTab (SelectProject index)
              (.name project)
              editorColor
          False->
            viewUnselectedTab (SelectProject index)
              (.name project)
              editorColor
      )
    |>ZipList.toList
    )

