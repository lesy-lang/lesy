port module Main exposing (main)

{-|Editor for lesy
-}

import Browser
import Browser.Events
import Browser.Navigation as Nav

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


import MorphIds exposing (..)


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

type Dock=
    Connected Id
  | NotConnected

type alias Group=
  Translated 
    { input: IdDict Dock }

move:
  Translate ->Translated a ->Translated a
move off=
  updateTranslate (combine (+) off)

type Selectable=
    NothingSelected
  | MorphSelected Id
  | EditorSelected

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


type alias Model=
  { ticks: Int
  
  , pressed: Pressable
  , selected: Selectable
  , drag: Drag
  , belowMouse: Pressable
  , editorPressed: EditorPressed

  , scale: Float ->Float
  
  , trunkMorph: Maybe Id
  , waitingForComputedTextWidth: List Id
  , morphs: IdDict Morph
  , sizes: Sizes
  }

init: () ->( Model, Cmd Msg )
init flags=
  ( { ticks= 0

    , pressed= NotPressed
    , selected= NothingSelected
    , drag= NoDrag
    , editorPressed= EditorNotPressed
    , belowMouse= NotPressed

    , morphs= MorphIds.empty
    , trunkMorph= Nothing
    , waitingForComputedTextWidth= []

    , scale= (*) 1
    , sizes= scaleSizes ((*) 1)
    }

  , generateId PlaceTrunk
    --dummy morph for testing
  )


type Msg=
    FrameTick
  | Move Translate
  | Press Pressable
  | PressEditor
  | MouseOn Pressable
  | Lift
  | Scale Float
  | ClickTab Id
  | RenameMorph String
  | PlaceTrunk Id
  | AddInput Id Id
  | RecieveComputedTextWidth Float


update: Msg ->Model ->( Model, Cmd Msg )
update msg ({sizes} as model)=
  case msg of
    FrameTick->
      ( {model
        | ticks= (+) 1 (.ticks model)
        }
      , Cmd.none
      )
    
    Move off->
      ( let dragMorph id=
              inDrag Drag
              <|updateMorph id (move off)
              <|model
        in
        case .pressed model of
          MorphPressed id->
            dragMorph id
          
          NotPressed->
            case .editorPressed model of
              EditorPressed->
                .trunkMorph model
                |>Maybe.map dragMorph
                |>Maybe.withDefault model

              EditorNotPressed->
                model
          
          NoMorphPressed _->
            model
          
          GroupPressed output->
            inDrag Drag
            <|updateMorph output
                (\morph->
                  {morph
                  | input= move off (.input morph)
                  }
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
                    upModel|>inSelect EditorSelected

                  EditorNotPressed->
                    upModel
              , Cmd.none
              )
            
            MorphPressed id->
              ( upModel|>inSelect (MorphSelected id)
              , Cmd.none
              )
            
            NoMorphPressed createMsg->
              ( upModel|>inSelect NothingSelected
              , MorphIds.generateId createMsg
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
      ( {model | editorPressed= EditorPressed }
      , Cmd.none
      )
    
    MouseOn part->
      ( {model | belowMouse= part }
      , Cmd.none
      )
    
    Scale factor->
      ( updateMorphs
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
        <|let lastFactor= (.scale model) 1
              scale= (*) ((*) factor lastFactor)
          in
          {model
          | scale= scale
          , sizes= scaleSizes scale
          }
      , Cmd.none
      )
    
    ClickTab to->
      ( inSelect (MorphSelected to) model
      , Cmd.none
      )
    
    RenameMorph to->
      ( case .selected model of
          MorphSelected id->
            updateMorph id 
              (\morph-> { morph | name= to })
            <|{model
              | waitingForComputedTextWidth=
                  id::(.waitingForComputedTextWidth model)
              }
          
          _ ->model
      , computeMorphTextWidth to
      )
    
    PlaceTrunk id->
      addMorph id (.trunkTranslate sizes)
      <|inTrunk (Just id)
      <|model
    
    AddInput toId ownId->
      model
      |>updateMorph toId
          (\morph->
            let morphInput= .input morph
                groupInput= .input morphInput
            in
            {morph
            | input=
                {morphInput
                | input=
                    MorphIds.update
                      toId (\_-> Connected ownId)
                    <|groupInput
                }
            }
          )
      |>addMorph ownId
          (.defaultMorphTranslate sizes)
    
    RecieveComputedTextWidth width->
      ( List.head (.waitingForComputedTextWidth model)
        |>Maybe.map
          (\id->
            { model
            | waitingForComputedTextWidth=
                removeHead
                  (.waitingForComputedTextWidth model)
            }
            |>updateMorph id
                (\morph->
                  { morph
                  | paddedTextWidth=
                      ((*)
                        (.fontSize sizes)
                        width
                      )
                      |>padded (.pad sizes)
                  }
                )
          )
          |>Maybe.withDefault model
      , Cmd.none
      )

removeHead: List a ->List a
removeHead=
  List.drop 1

inDrag: Drag ->Model ->Model
inDrag drag model=
  { model | drag= drag }

inPressed: Pressable ->Model ->Model
inPressed mouseDownOn model=
  { model | pressed= mouseDownOn }

inTrunk: Maybe Id ->Model ->Model
inTrunk trunk model=
  { model | trunkMorph= trunk }

updateMorphs:
  (IdDict Morph ->IdDict Morph) ->Model ->Model
updateMorphs change ({morphs} as model)=
  { model | morphs= change morphs }

inSelect: Selectable ->Model ->Model
inSelect selected model=
  { model | selected= selected }

addMorph:
    Id ->Translate
  ->Model ->( Model, Cmd Msg )
addMorph
  id translate
  ({scale, sizes} as model)
  =
  let name= "🥗 ← 🍅 🥬"
  in
  ( { model
    | waitingForComputedTextWidth=
        id::(.waitingForComputedTextWidth model)
    }
    |>updateMorphs
        (add id
            { name= name
            , translate= translate
            , input=
                { input= MorphIds.empty
                , translate= (.defaultGroupTranslate sizes)
                }
            , paddedTextWidth=
                (Use.Svg.aproximateMonospacedWidth
                  (.fontSize sizes) name
                )
                |>padded (.pad sizes)
            }
        )
      |>inSelect (MorphSelected id)

  , computeMorphTextWidth name
  )

updateMorph:
  Id ->(Morph ->Morph) ->Model ->Model
updateMorph id change=
  updateMorphs (MorphIds.update id change)
  

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
                  [ Ui.focusStyle focusStyle
                  ]
              }
              [] (makePage model)
          ]
      ]
  }

focusStyle: Ui.FocusStyle
focusStyle=
  { borderColor= Nothing
  , backgroundColor= Nothing
  , shadow=
      Just
        { color= attentionColor|>toRgba Ui.rgba
        , offset= (0, 0)
        , blur= 0
        , size= 3
        }
  }
  

makePage: Model ->Ui.Element Msg
makePage model=
  Ui.column
    [ Ui.height Ui.fill
    , Ui.width Ui.fill
    ]
    ([ makeTitleScreen
    , makeProperties model
    ]
    |>List.map
      (Ui.column
        [ Ui.height (Ui.fillPortion 1)
        , Ui.width Ui.fill
        ]
      )
    )

makeProperties: Model ->List (Ui.Element Msg)
makeProperties
  { trunkMorph, morphs, selected
  , drag, belowMouse
  , sizes, ticks
  }
  =
  let searchMorph id= search id morphs
  in
  [ makeEditor
      trunkMorph searchMorph
      selected ticks
      (case drag of
        Drag-> Use.Svg.hand
        NoDrag->
          case belowMouse of
            NotPressed-> Use.Svg.arrow
            _-> Use.Svg.finger
      )
      sizes
  , makeTabs morphs selected
  , case selected of
      EditorSelected->
        makeEditorProperties

      MorphSelected id->
        (searchMorph id)
        |>Maybe.map
            (.name >>makeMorphProperties)
        |>Maybe.withDefault Ui.none
        
      NothingSelected-> Ui.none
  ]

editorColor: Ui.Color
editorColor=
  Ui.rgb 0.039 0.035 0.031

makeTitle: Ui.Element msg
makeTitle=
  Ui.column
    [ Ui.width Ui.fill
    , Ui.height (Ui.fillPortion 6)
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

makeTitleScreen: List (Ui.Element Msg)
makeTitleScreen= 
  [ makeTitle
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

catchPhrase: String
catchPhrase=
  """
    Make friendly code.
simple & fun
  """


makeEditor:
    Maybe Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Use.Svg.Cursor
  ->Sizes
  ->Ui.Element Msg
makeEditor
  trunkId searchMorph
  selectable ticks cursor
  sizes
  =
  Ui.el
    [ Ui.height (Ui.fillPortion 48)
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
        [ makeTree
            trunkId searchMorph
            selectable ticks sizes
        ]
      )
    )

makeTree:
    Maybe Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Sizes
  ->Svg Msg
makeTree
  trunk searchMorph
  selected ticks sizes
  =
  trunk
  |>Maybe.map
      (\id->
        makeMorph
          id searchMorph
          selected ticks sizes
      )
  |>Maybe.withDefault
      (makeNoMorph PlaceTrunk sizes)

expect:
    Maybe v ->Use.Svg.Size ->(v ->Svg msg)
  ->Svg msg
expect maybeExists fontSize toSvg=
  maybeExists
  |>Maybe.map toSvg
  |>Maybe.withDefault
      (makeLabel
        fontSize
        "morph not found..."
      )


padded: number ->number ->number
padded by length=
  by+ length +by

makeMorph:
    Id ->(Id ->Maybe Morph)
  ->Selectable ->Int
  ->SizesInMorphSelected
    (SizesInMorph
    {a| textHeight: Float
      , lineWidth: Float
      , triangleCircumradius: Float
      , defaultMorphTranslate: Translate
    }
    )
  ->Svg Msg
makeMorph
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
    (Use.Svg.absolute fontSize)
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
                [ makeMorphSelected
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
              [ makeConnection groupColor
                  begin end sizes
              , makeGroup
                  morphId searchMorph
                  selected ticks
                  (rotation (combine (-) end begin)) sizes
                  input
              ]
              )
          , makeMorphShape
              morphId name
              (morphColor|>(toRgba Use.Svg.rgba))
              paddedTextWidth sizes
          ]
        )
    )

makeGroup:
    Id ->(Id ->Maybe Morph)
  ->Selectable ->Int ->Float
  ->SizesInGroup
    (SizesInNoMorph
    (SizesInConnection
      {a| fontSize: Float
        , defaultMorphTranslate: Translate
      }
    ))
  ->Group ->Svg Msg
makeGroup
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
    ((makeGroupShape outputId turn sizes)
    ::
    (input
    |>MorphIds.values
    |>List.map
        (\dock->
          let begin= morphInputLineStart sizes
          in
          case dock of
            Connected id->
              [ expect (id|>searchMorph)
                  (Use.Svg.absolute fontSize)
                  (\inputMorph->
                    let end= 
                          .translate inputMorph
                          |>mapX ((+) (moveInsideTriangle lineWidth))
                    in
                    makeConnection morphColor
                      begin end sizes
                  )
              , makeMorph
                  id searchMorph
                  selected ticks sizes
              ]
            
            NotConnected->
              [ makeConnection noMorphColor
                  begin defaultMorphTranslate
                  sizes 
              , Use.Svg.group
                  [ Use.Svg.translate defaultMorphTranslate
                  ]
                  [ makeNoMorph
                      (AddInput outputId) sizes
                  ]
              ]
        )
    |>List.concat
    )
  )

type alias SizesInGroup a=
  {a| triangleCircumradius: Float
    , halfBoxHeight: Float
  }
makeGroupShape:
    Id-> Float ->SizesInGroup a
  -> Svg Msg
makeGroupShape
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
makeMorphSelected:
    Float ->SizesInMorphSelected a
  ->Int ->Svg msg
makeMorphSelected
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
makeMorphShape:
    Id
  ->String ->Use.Svg.Color
  ->Float ->SizesInMorph a
  ->Svg Msg
makeMorphShape
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
        [ makeLabel
            (Use.Svg.absolute fontSize)
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
  x0y0
  |>mapX
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
makeConnection:
    Rgba ->Translate ->Translate
  ->SizesInConnection a
  ->Svg msg
makeConnection
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
            (Use.Svg.absolute lineWidth)
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
makeNoMorph:
  (Id ->Msg) ->SizesInNoMorph a
  ->Svg Msg
makeNoMorph
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
            (Use.Svg.absolute lineWidth)
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

makeLabel: Use.Svg.Size ->String ->Svg msg
makeLabel fontSize name=
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


makeTabs:
  IdDict Morph ->Selectable ->Ui.Element Msg
makeTabs morphs selected=
  Ui.row
    [ Ui.spacing 8
    , UiBackground.color (Ui.rgb 0 0 0)
    , Ui.height (Ui.fillPortion 1)
    , Ui.width (Ui.fillPortion 16)
    ]
    (let tab=
          case selected of
            MorphSelected selectedId->
              (\id->
                case (==) id selectedId of
                  True->
                    makeSelectedTab id
                  False->
                    makeUnselectedTab id
              )
            
            _-> makeUnselectedTab
    in
    morphs
    |>mapEach
        (\id morph->
          tab id (.name morph)
            (morphColor|>toRgba Ui.rgba)
        )
    |>values
    )

makeTab:
    Id ->String ->Ui.Color ->Int ->Ui.Color
  ->Ui.Element Msg
makeTab id name bgColor padY fontColor=
  UiInput.button 
    [ UiBackground.color editorColor
    , UiFont.color fontColor
    , Ui.paddingXY 4 padY
    , Ui.alignTop
    ]
    { onPress= Just (ClickTab id),
      label=
        Ui.el
          [ Ui.centerY, Ui.centerX ]
          (Ui.text name)
    }

makeUnselectedTab:
  Id ->String ->Ui.Color ->Ui.Element Msg
makeUnselectedTab id name bgColor=
  makeTab id name bgColor
    4 (Ui.rgb 0.55 0.55 0.55)

makeSelectedTab:
  Id ->String ->Ui.Color ->Ui.Element Msg
makeSelectedTab id name bgColor=
  makeTab id name bgColor
    11 (Ui.rgb 1 1 1)


makeMorphProperties: String ->Ui.Element Msg
makeMorphProperties name=
  UiInput.search
    [ UiBackground.color editorColor
    , UiFont.color (Ui.rgb 0.78 1 0.9)
    , UiBorder.color editorColor
    ]
    { onChange= RenameMorph
    , text= name
    , placeholder= Nothing
    , label= UiInput.labelHidden "name"
    }

makeScaleButton: String ->msg ->Ui.Element msg
makeScaleButton text msg=
  UiInput.button
    [ UiBackground.color editorColor
    , UiFont.color (Ui.rgb 1 1 1)
    , Ui.padding 10
    ]
    { label= Ui.text text, onPress= Just msg }

makeEditorProperties: Ui.Element Msg
makeEditorProperties=
  Ui.row
    [ Ui.width Ui.fill
    , UiBackground.color editorColor
    ]
    [ makeScaleButton "+" (Scale ((/) 11 10))
    , makeScaleButton "-" (Scale ((/) 10 11))
    , makeDownloadButton
    ]

makeDownloadButton: Ui.Element msg
makeDownloadButton=
  Ui.el
    [ UiBackground.color editorColor
    , UiFont.color (Ui.rgb 1 1 1)
    , Ui.padding 10
    ]
    (Ui.html
      (Html.a
        [ HtmlAttr.href "https://raw.githubusercontent.com/indique/indique.github.io/master/ungi.png"
        , HtmlAttr.download "not-named.lesy"
        ]
        [ Html.text "⤓"
        ]
      )
    )

