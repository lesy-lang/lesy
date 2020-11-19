port module Main exposing (main)

{-|Editor for lesy

- title screen
- editor screen
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

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Use.Json exposing (..)

import Dict exposing (Dict)
import Use.Collection
import ZipList exposing (ZipList)
import Id exposing (..)
import Use.Misc exposing
  (oneAs100Percent, noTextSelect)
import ZipList
import Id
import Array exposing (Array)
import Use.Json exposing (decodeDict)
import Svg exposing (view)
import Json.Decode exposing (index)


main: Program () Model Msg
main=
  Browser.document
    { init= init
    , view= view
    , update= update
    , subscriptions= subscriptions
    }


type alias BranchingOffIndex=
  Int

encodeBranchingOffIndex: BranchingOffIndex ->Encode.Value
encodeBranchingOffIndex=
  Encode.int
decodeBranchingOffIndex: Decoder BranchingOffIndex
decodeBranchingOffIndex=
  Decode.int

type alias DefinitionId=
  Id.Id

encodeDefinitionId: Id -> Encode.Value
encodeDefinitionId=
  Id.encode
decodeDefinitionId: Decoder Id
decodeDefinitionId=
  Id.decode

type BranchGap=
  BranchingFurther BranchingOffIndex
  | BranchMissing BranchingOffIndex DefinitionId

encodeBranchGap gap=
  Encode.object
    [ case gap of
        BranchingFurther index->
          ( "BranchingFurther"
          , Encode.object 
              [ ( "branchingOffIndex"
                , encodeBranchingOffIndex index
                )
              ]
          )
        BranchMissing index id->
          ( "BranchMissing"
          , Encode.object
              [ ( "branchingOffIndex"
                , encodeBranchingOffIndex index
                )
              , ( "definitionId", encodeDefinitionId id )
              ]
          )
    ]
decodeBranchGap: Decoder BranchGap
decodeBranchGap=
  Decode.oneOf
    [ Decode.field "BranchingFurther"
        (Decode.map BranchingFurther
          (Decode.field
            "branchingOffIndex" decodeBranchingOffIndex
          )
        )
    , Decode.field "BranchMissing"
        (Decode.map2 BranchMissing
          (Decode.field
            "branchingOffIndex" decodeBranchingOffIndex
          )
          (Decode.field
            "definitionId" decodeDefinitionId
          )
        )
    ]

{-| all branch-indexes
- leading or
- not leading

to (or from) a `BranchOff`.
-}
type alias BranchGaps=
  Dict DefinitionId BranchGap

encodingReadability: Int
encodingReadability= 3

encodeBranchGaps: BranchGaps ->Encode.Value
encodeBranchGaps=
  encodeDict encodingReadability
    encodeDefinitionId encodeBranchGap

decodeBranchGaps: Decoder BranchGaps
decodeBranchGaps=
  decodeDict
    decodeDefinitionId decodeBranchGap

type alias BranchOff a=
  Translated
    {a
    | definitionIndex: BranchingOffIndex
    , incomingBranchGaps: BranchGaps
    , outgoingBranchGaps: BranchGaps
    }
type alias Definition=
  BranchOff 
    { id: DefinitionId
    , name: String--will be in a lower level definition
    , textWidth: Float
    }
type BranchingOff=
  Defined (BranchOff {})
  | Definition Definition

encodeBranchingOff: BranchingOff ->Encode.Value
encodeBranchingOff branchingOff=
  let sharedFields
        { translate, definitionIndex
        , incomingBranchGaps, outgoingBranchGaps
        }
        =
        [ ( "translate"
          , Use.Translate.encode translate
          )
        , ( "definitionIndex"
          , encodeBranchingOffIndex definitionIndex
          )
        , ( "incomingBranchGaps"
          , encodeBranchGaps incomingBranchGaps
          )
        , ( "outgoingBranchGaps"
          , encodeBranchGaps outgoingBranchGaps
          )
        ]
  in
  Encode.object
    (case branchingOff of
      Defined branchOff->
        sharedFields branchOff
      
      Definition
        ({ id, name, textWidth } as definition)
        ->
        (sharedFields definition)
        ++
        [ ( "id", encodeDefinitionId id )
        , ( "name", Encode.string name )
        , ( "textWidth"
          , Encode.float textWidth
          )
        ]
    )
decodeBranchingOff: Decoder BranchingOff
decodeBranchingOff=
  let translateField=
        (Decode.field
          "translate" Use.Translate.decode
        )
      definitionIndexField=
        (Decode.field
          "definitionIndex" decodeBranchingOffIndex
        )
      incomingBranchGapsField=
        (Decode.field
          "incomingBranchGaps" decodeBranchGaps
        )
      outgoingBranchGapsField=
        (Decode.field
          "outgoingBranchGaps" decodeBranchGaps
        )
  in
  Decode.oneOf
    [ Decode.map4
        (\off defIndex incoming outgoing->
          Defined
            { translate= off
            , definitionIndex= defIndex
            , incomingBranchGaps= incoming
            , outgoingBranchGaps= outgoing
            }
        )
        translateField
        definitionIndexField
        outgoingBranchGapsField
        incomingBranchGapsField

    , Decode.map7
        (\off defIndex incoming outgoing
          id name width
          ->
          Definition
            { translate= off
            , definitionIndex= defIndex
            , incomingBranchGaps= incoming
            , outgoingBranchGaps= outgoing
            , id= id
            , name= name
            , textWidth= width
            }
        )
        translateField
        definitionIndexField
        outgoingBranchGapsField
        incomingBranchGapsField
        (Decode.field
          "id" decodeDefinitionId
        )
        (Decode.field
          "name" Decode.string
        )
        (Decode.field
          "textWidth" Decode.float
        )
    ]

move:
  Translate ->BranchingOff ->BranchingOff
move off branchingOff=
  let moveOff= updateTranslate (combine (+) off)
  in
  case branchingOff of
    Defined defined->
      moveOff defined |>Defined
    
    Definition definition->
      moveOff definition |>Definition
      

type Selected=
  NothingSelected
  | BranchingOffSelected BranchingOffIndex
  | EditorSelected (Maybe BranchingOffIndex)

encodeSelected: Selected ->Encode.Value
encodeSelected selected=
  case selected of
    NothingSelected->
      Encode.string "NothingSelected"

    BranchingOffSelected index->
      Encode.object
        [ ( "BranchingOffSelected"
          , Encode.object
              [ ( "branchingOffIndex"
                , encodeBranchingOffIndex index
                )
              ]
          )
        ]
    EditorSelected potentialSelectedBranch->
      Encode.object
        [ ( "EditorSelected"
          , Encode.object
              [ ( "potentialSelectedbranchingOffIndex"
                , encodeNullable encodeBranchingOffIndex
                    potentialSelectedBranch
                )
              ]
          )
        ]
decodeSelected: Decoder Selected
decodeSelected=
  Decode.oneOf
    [ Decode.field
        "NothingSelected" (Decode.succeed NothingSelected)
    , Decode.field
        "EditorSelected"
        (Decode.map EditorSelected
          (Decode.field
            "potentialSelectedbranchingOffIndex"
            (Decode.nullable decodeBranchingOffIndex)
          )
        )
    , Decode.field
        "BranchingOffSelected"
        (Decode.map BranchingOffSelected
          (Decode.field
            "branchingOffIndex" decodeBranchingOffIndex
          )
        )
    ]

type Pressed=
  BranchingOffPressed BranchingOffIndex
  | MissingBranchingOffPressed
      BranchingOffIndex DefinitionId
  | NotPressed

type Drag=
  Drag
  | NoDrag

--would like to eliminate this
type EditorPressed=
  EditorPressed
  | EditorNotPressed

type alias Sizes=
  SizesInBranchOffSelected
  (SizesInBranchOff
  (SizesInMissingBranchOff
  (SizesInBranchOff
  (SizesInConnection
    { defaultTranslate: Translate
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
  , defaultTranslate=
      { x= 64, y= 0 }|>mapXY scale
  }

type alias Project=
  { name: String
  , selected: Selected
  , branchingOffs: Dict BranchingOffIndex BranchingOff
  }
trunkIndex: BranchingOffIndex
trunkIndex= 0

appDefinitionId: DefinitionId
appDefinitionId=
  "򦸔󼼐򙖞񠴔򍻕򿂽𢘞񬆺"|>String.toList--todo

{-| **TODO.**
Will contain mostly native and core definitions, e.g.
- character
- rational / irational number
- group (a typesafe dict)

To create those, we would need a seperated program
to generate ids and compute the text width.
-}
basicDefinitions:
  {a| defaultTranslate: Translate }
  ->Dict BranchingOffIndex BranchingOff
basicDefinitions { defaultTranslate }=
  Dict.empty
  |>Dict.insert trunkIndex
      (Definition
        { translate= defaultTranslate
        , definitionIndex= trunkIndex--todo, there must be a definition
        , incomingBranchGaps= Dict.empty--todo
        , outgoingBranchGaps= Dict.empty
        , id= appDefinitionId
        , name= "app"
        , textWidth= 20--todo
        }
      )

blankProject:
  {a| defaultTranslate: Translate } ->Project
blankProject ({defaultTranslate} as sizes)=
  { name= "not-named"
  , selected= NothingSelected
  , branchingOffs=
      basicDefinitions sizes
  }
  
encodeProject: Project ->Encode.Value
encodeProject
  { name, selected, branchingOffs }
  =
  Encode.object
    [ ( "name", Encode.string name )
    , ( "selected", encodeSelected selected )
    , ( "branchingOffs"
      , encodeDict encodingReadability
          encodeBranchingOffIndex encodeBranchingOff
          branchingOffs
      )
    ]
decodeProject: Decoder Project
decodeProject=
  Decode.map3 Project
    (Decode.field
      "name" Decode.string
    )
    (Decode.field
      "selected" decodeSelected 
    )
    (Decode.field
      "branchingOffs"
      (decodeDict
        decodeBranchingOffIndex decodeBranchingOff
      )
    )
  

type alias Model=
  { ticks: Int
  
  , pressed: Pressed
  , drag: Drag
  , belowMouse: Pressed
  , editorPressed: EditorPressed

  , scale: Float ->Float
  
  , projects: ZipList Project
  , waitingForComputedTextWidth:
      Maybe (ZipList BranchingOffIndex)
  , sizes: Sizes
  }

init: () ->( Model, Cmd Msg )
init flags=
  ( let scale= (*) 1
        sizes= scaleSizes scale
    in
    { ticks= 0

    , pressed= NotPressed
    
    , drag= NoDrag
    , editorPressed= EditorNotPressed
    , belowMouse= NotPressed

    , projects=
        ZipList.singleton
          (blankProject sizes)
    , waitingForComputedTextWidth= Nothing

    , scale= scale
    , sizes= sizes
    }

  , Cmd.none
  )
type alias ProjectIndex=
  Int

type Msg=
  FrameTick
  | Save
  | Select
  | RecieveSelectFile File
  | RecieveSelect String
  | AddBlankProject
  | RenameProject String
  | SelectProject ProjectIndex
  | Move Translate
  | Press Pressed
  | PressEditor
  | MouseOn Pressed
  | Lift
  | Scale Float
  | SelectBranchingOff BranchingOffIndex
  | RenameBranchingOff String
  | AddIncomingBranchingOff BranchingOff
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
    
    Save->
      ( model
      , let project= ZipList.current (.projects model)
        in
        File.Download.string
          ((.name project)++".lesy.json")
          "application/json"
          (encodeProject project
          |>Encode.encode encodingReadability
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
      ( Decode.decodeString decodeProject content
        |>Result.map
            (\project->
              {model
              | projects=
                  ZipList.insert project
                    (.projects model)
              }
            )
        |>Result.withDefault 
            model--failed. a dialog?
      , Cmd.none
      )
    
    AddBlankProject->
      ( {model
        | projects=
            ZipList.insert
              (blankProject (.sizes model))
              (.projects model)
        }
      , Cmd.none
      )
    
    RenameProject name->
      ( {model
        | projects=
            Use.Collection.updateSelected
              (\project-> {project | name= name })
              (.projects model)
        }
      , Cmd.none
      )
    
    SelectProject index->
      ( {model
        | projects=
            ZipList.goToIndex index (.projects model)
            |>Maybe.withDefault
                (.projects model)
        }
      , Cmd.none
      )
    
    Move off->
      ( case .pressed model of
          BranchingOffPressed index->
            inDrag Drag
            <|(forSelectedProject
                (updateBranchingOff index
                  (Maybe.map (move off))
                )
              )
            <|model
          
          NotPressed->
            case .editorPressed model of
              EditorPressed->
                inDrag Drag
                <|(forSelectedProject
                    (updateBranchingOff trunkIndex
                      (Maybe.map (move off))
                    )
                  )
                <|model

              EditorNotPressed->
                model
          
          MissingBranchingOffPressed _ _->
            model

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
                              BranchingOffSelected id-> Just id
                              NothingSelected-> Nothing
                            )
                          )
                        )

                  EditorNotPressed->
                    upModel
              , Cmd.none
              )
            
            BranchingOffPressed index->
              ( upModel
                |>(forSelectedProject
                    (inSelect (BranchingOffSelected index))
                  )
              , Cmd.none
              )
            
            MissingBranchingOffPressed index definition->
              ( upModel
                |>forSelectedProject
                    (inSelect NothingSelected)
              , Cmd.none
              )
                
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
      ( let lastFactor= (.scale model) 1
            scale= (*) ((*) factor lastFactor)
        in
        {model
        | scale= scale
        , sizes= scaleSizes scale
        }
        |>forSelectedProject
            (updateBranchingOffs
              (Dict.map
                (\_ branchingOff->
                  let scaleTranslate= 
                        updateTranslate
                          (mapXY ((*) factor))
                  in
                  case branchingOff of
                    Defined branchOff->
                      scaleTranslate branchOff
                      |>Defined
                      
                    Definition definition->
                      scaleTranslate definition
                      |>Definition
                      
                )
              )
            )
      , Cmd.none
      )
    
    SelectBranchingOff branchingOff->
      ( (forSelectedProject
          (inSelect (BranchingOffSelected branchingOff)
          )
        )
        <|model
      , Cmd.none
      )
    
    RenameBranchingOff to->
      ( case .selected (ZipList.current (.projects model))
          of
          BranchingOffSelected branchingOffIndex->
            {model
            | waitingForComputedTextWidth=
                .waitingForComputedTextWidth model
                |>Maybe.map
                    (ZipList.insertAfter branchingOffIndex)
                |>Maybe.withDefault
                    (ZipList.singleton branchingOffIndex)
                |>Just
            }
            |>forSelectedProject
                (updateBranchingOff branchingOffIndex
                  (Maybe.map
                    (\mustBeDef->
                      case mustBeDef of
                        Definition def->
                          Definition {def | name= to }
                          
                        Defined defined->
                          Defined defined
                    )
                  )
                )
          
          _-> model

      , computeBranchOffTextWidth to
      )
    
    AddIncomingBranchingOff branchOff->
      ( model
        |>forSelectedProject
            (addAndSelectBranchOff
              branchOff-- (.sizes model)
            )
      , Cmd.none
      )
    
    RecieveComputedTextWidth width->
      let waiting= .waitingForComputedTextWidth model
      in
      waiting
      |>Maybe.map
          (\alreadySomeWaiting->
            ( let removed= ZipList.current alreadySomeWaiting
                  without=
                    ZipList.remove alreadySomeWaiting
              in
              {model
              | waitingForComputedTextWidth= without
              }
              |>forSelectedProject
                  (updateBranchingOff removed
                    (Maybe.map
                      (\shouldBeDef->
                        case shouldBeDef of
                          Definition def->
                            Definition
                              {def
                              | textWidth= (*) width (.fontSize sizes)
                              }
                          Defined defined->
                            Defined defined
                      )
                    )
                  )
            , Cmd.none
            )
          )
      |>Maybe.withDefault
          ( model, Cmd.none )

nextFreeIndex:
  Dict Int v ->Int
nextFreeIndex=
  Dict.keys >>List.foldr max 0

inDrag: Drag ->Model ->Model
inDrag drag model=
  {model | drag= drag }

inPressed: Pressed ->Model ->Model
inPressed mouseDownOn model=
  {model | pressed= mouseDownOn }

forSelectedProject:
  (Project ->Project) ->Model ->Model
forSelectedProject change model=
  {model
  | projects=
      Use.Collection.updateSelected
      change (.projects model)
  }

inSelect: Selected ->Project ->Project
inSelect selected project=
  {project | selected= selected }

addDefinition:
  BranchingOffIndex
  ->BranchingOffIndex
  ->Dict BranchingOffIndex v
  ->{a
    | fontSize: Float
    , defaultTranslate: Translate
    }
  ->Model ->( Model, Cmd Msg )
addDefinition
  definitionIndex
  ingoingIndex branchingOffs
  { fontSize, defaultTranslate }
  ({ scale, sizes, projects } as model)
  =
  let name= "🥗 ← 🍅 🥬"
      index=
        nextFreeIndex
          (.branchingOffs (ZipList.current projects))
  in
  ( {model
    | waitingForComputedTextWidth=
        .waitingForComputedTextWidth model
        |>Maybe.map
            (ZipList.insertAfter index)
        |>Maybe.withDefault
            (ZipList.singleton index)
        |>Just
    }
    |>forSelectedProject
        (addAndSelectBranchingOffAt index
          (Definition
            { definitionIndex= definitionIndex
            , incomingBranchGaps= Dict.empty
            , outgoingBranchGaps= Dict.empty
            , translate= defaultTranslate
            , name= name
            , textWidth=
                Use.Svg.aproximateMonospacedWidth fontSize
                  name
            , id= "noconect"|>String.toList
              --this id will be replaced in the next update
            }
          )
        )
    
  , computeBranchOffTextWidth name
  )

updateBranchingOffs:
  (Dict BranchingOffIndex BranchingOff
  ->Dict BranchingOffIndex BranchingOff
  )
  ->Project ->Project
updateBranchingOffs change project=
  {project
  | branchingOffs= change (.branchingOffs project)
  }

updateBranchingOff:
  BranchingOffIndex
  ->(Maybe BranchingOff ->Maybe BranchingOff)
  ->Project ->Project
updateBranchingOff index=
  updateBranchingOffs <<Dict.update index


addAndSelectBranchOff:
  BranchingOff ->Project ->Project
addAndSelectBranchOff branchingOff project=
  addAndSelectBranchingOffAt
    (nextFreeIndex (.branchingOffs project))
    branchingOff project

addAndSelectBranchingOffAt:
  BranchingOffIndex ->BranchingOff
  ->Project ->Project
addAndSelectBranchingOffAt index branchingOff=
  updateBranchingOffs
    (Dict.insert index branchingOff)
  >>inSelect
      (BranchingOffSelected index)
  

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

computeBranchOffTextWidth: String ->Cmd msg
computeBranchOffTextWidth text=
  computeTextWidth
    { font= "1px "++defaultFont, text= text }


view: Model ->Browser.Document Msg
view model=
  { title= "lesy"
  , body=
      [ Html.div
          [ HtmlAttr.style "height" (oneAs100Percent 2) ]
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
    make friendly code.
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
      branchingOffs= .branchingOffs selectedProject
      searchBranchOff index=
        Dict.get index (.branchingOffs selectedProject)
  in
  Ui.column
    [ Ui.width Ui.fill
    , Ui.height Ui.fill
    ]
    [ viewEditor
        (.branchingOffs selectedProject)
        (.selected selectedProject)
        ticks
        (case drag of
          Drag->
            Use.Svg.hand

          NoDrag->
            case belowMouse of
              NotPressed-> Use.Svg.arrow
              _-> Use.Svg.finger
        )
        sizes

    , Ui.column
        [ Ui.width Ui.fill
        , Ui.height Ui.fill
        , Ui.spacing 9
        , UiBackground.color (Ui.rgb 0 0 0)
        ]
        [ case .selected selectedProject of
            EditorSelected maybeIndex->
              viewEditorProperties
                branchingOffs maybeIndex

            BranchingOffSelected index->
              Dict.get index branchingOffs
              |>Maybe.map
                  (\branchingOff->
                    expectDefinition
                      (val .definitionIndex branchingOff) branchingOffs
                    |>Maybe.map
                        (.name >>viewBranchOffProperties)
                    |>Maybe.withDefault
                        definitionNotFoundUi
                  )
              |>Maybe.withDefault
                  branchingOffNotFoundUi
                
              
            NothingSelected-> Ui.none

        , viewOptions projects
        ]
    ]
val: (BranchOff {} ->b) ->BranchingOff ->b
val valueOf=
  valueOf <<toCommonBaseBranchOff
toCommonBaseBranchOff: BranchingOff ->BranchOff {}
toCommonBaseBranchOff branchingOff=
  case branchingOff of
    Defined defined-> defined

    Definition 
      { translate, definitionIndex
      , incomingBranchGaps, outgoingBranchGaps
      }
      ->
      { translate= translate
      , definitionIndex= definitionIndex
      , incomingBranchGaps= incomingBranchGaps
      , outgoingBranchGaps= outgoingBranchGaps
      }
  

editorColor: Ui.Color
editorColor=
  Ui.rgb 0.039 0.035 0.031

viewEditor:
  Dict BranchingOffIndex BranchingOff
  ->Selected ->Int ->Use.Svg.Cursor
  ->Sizes
  ->Ui.Element Msg
viewEditor
  branchingOffs
  selected ticks cursor
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
        [ viewBranchOff (BranchingFurther trunkIndex)
            branchingOffs
            selected ticks sizes
        ]
      )
    )

branchingOffNotFoundSvg:
  Use.Svg.Size ->Svg msg
branchingOffNotFoundSvg fontSize=
  viewLabel
    fontSize "branching-off not found..."

branchingOffNotFoundUi: Ui.Element msg
branchingOffNotFoundUi=
  Ui.el
    [ UiFont.family [ UiFont.typeface defaultFont ]
    ]
    (Ui.text "branching-off not found...")

padded: number ->number ->number
padded by length=
  by+ length +by


justDefinition: BranchingOff ->Maybe Definition
justDefinition branchingOff=
  case branchingOff of
    Definition def->
      Just def

    _-> Nothing

expectDefinition:
  BranchingOffIndex
  ->Dict BranchingOffIndex BranchingOff
  ->Maybe Definition
expectDefinition defIndex branchingOffs=
  Dict.get defIndex branchingOffs
  |>Maybe.andThen justDefinition

definitionNotFoundSvg: Use.Svg.Size ->Svg msg
definitionNotFoundSvg fontSize=
  viewLabel fontSize "where is my definition?"

definitionNotFoundUi: Ui.Element msg
definitionNotFoundUi=
  Ui.el
    [ UiFont.family [ UiFont.typeface defaultFont ]
    ]
    (Ui.text "where is my definition?")


viewBranchOff:
  BranchGap
  ->Dict BranchingOffIndex BranchingOff
  ->Selected ->Int
  ->SizesInBranchOffSelected
    (SizesInBranchOff
      {a
      | textHeight: Float
      , lineWidth: Float
      , triangleCircumradius: Float
      , defaultTranslate: Translate
      }
    )
  ->Svg Msg
viewBranchOff
  gap branchingOffs
  selected ticks
  ({ fontSize
    , halfBoxHeight, triangleWidth
    , defaultTranslate
    }
    as sizes
  )
  =
  let begin= branchOffInputLineStart sizes
      selectIfSelected index textWidth=
        case selected of
          BranchingOffSelected selectedIndex->
            case (==) index selectedIndex of
              True->
                [ viewBranchOffSelectedShadow
                    textWidth sizes ticks
                ]
              False-> []

          _-> []
      viewShape index branchingOff name textWidth=
        case branchingOff of
          Defined defined->
            [ viewBranchOffShape
                index name
                (branchOffColor|>toRgba Use.Svg.rgba)
                textWidth sizes
            ]
          Definition definition->
            [ viewConnection definitionColor
                begin (.translate definition) sizes
            , viewBranchOffShape
                index name
                (definitionColor|>toRgba Use.Svg.rgba)
                textWidth sizes
            ]
      viewMissingBranchingOff index defId=
        Use.Svg.group
          [ Use.Svg.translate defaultTranslate ]
          [ viewConnection missingBranchOffColor
              begin defaultTranslate sizes
          , viewMissingBranchOff index defId sizes
          ]
      goDownTree=
        .incomingBranchGaps
        >>Dict.values
        >>List.map
            (\incomingMaybeIndex->
              viewBranchOff
                incomingMaybeIndex branchingOffs
                selected ticks sizes
            )
  in
  case gap of
    BranchingFurther index->
      Dict.get index branchingOffs
      |>Maybe.map
          (\branchingOff->
            expectDefinition
              (val .definitionIndex branchingOff)
              branchingOffs
            |>Maybe.map
                (\{ name, textWidth, id }->
                    Use.Svg.group
                      [ Use.Svg.translate (val .translate branchingOff) ]
                      ((selectIfSelected index textWidth)
                      ++
                      [ Use.Svg.group
                          [ Use.Svg.translate
                              { x= (+) triangleWidth textWidth, y= 0 }
                          ]
                          ((goDownTree (toCommonBaseBranchOff branchingOff))
                          ++
                          (viewShape index branchingOff name textWidth)
                          )
                      ]
                    )
                  )
            |>Maybe.withDefault
                (definitionNotFoundSvg (Use.Svg.px fontSize))
          )
      |>Maybe.withDefault
          (branchingOffNotFoundSvg (Use.Svg.px fontSize))
      
    BranchMissing index id->
      viewMissingBranchingOff index id


type alias SizesInBranchOffSelected a=
  {a
  | triangleWidth: Float
  , halfBoxHeight: Float
  }
viewBranchOffSelectedShadow:
  Float ->SizesInBranchOffSelected a
  ->Int ->Svg msg
viewBranchOffSelectedShadow
  textWidth
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
        (+) textWidth triangleWidth
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

drags: Pressed ->Svg.Attribute Msg
drags=
  Svg.Events.onMouseDown <<Press

clicks:
  Pressed ->List (Svg.Attribute Msg)
clicks part=
  [ drags part
  , Svg.Events.onMouseOver (MouseOn part)
  , Svg.Events.onMouseOut (MouseOn NotPressed)
  ]

type alias SizesInBranchOff a=
  {a
  | fontSize: Float
  , triangleWidth: Float
  , halfBoxHeight: Float
  , pad: Float
  }
viewBranchOffShape:
  BranchingOffIndex
  ->String ->Use.Svg.Color
  ->Float ->SizesInBranchOff a
  ->Svg Msg
viewBranchOffShape
  index name color
  textWidth
  { fontSize
  , triangleWidth, halfBoxHeight, pad
  }
  =
  Use.Svg.group
    (clicks (BranchingOffPressed index))
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


branchOffColor: Rgba
branchOffColor=
  { red= 0.0, green= 0.3, blue= 0.134 }
  |>withAlpha 0.6

definitionColor: Rgba
definitionColor=
  { red= 0.5, green= 0.1, blue= 0.5 }
  |>withAlpha 0.6

missingBranchOffColor: Rgba
missingBranchOffColor=
  { red= 0.8, green= 0.28, blue= 0.16 }
  |>withAlpha 0.34

connectionColorFromBranchOff: Rgba ->Use.Svg.Color
connectionColorFromBranchOff=
  .rgb
  >>mapRed ((+) 0.35)
  >>mapGreen ((+) 0.21)
  >>mapBlue ((+) -0.11)
    >>toRgb Use.Svg.rgb

moveInsideTriangle: Float ->Float
moveInsideTriangle lineWidth=
  (/) lineWidth (sqrt 2)

branchOffInputLineStart:
  {a
  | triangleWidth: Float
  , lineWidth: Float
  }
  ->Translate
branchOffInputLineStart { triangleWidth, lineWidth }=
  { x=
      (-) triangleWidth
        (moveInsideTriangle lineWidth)
  , y= 0
  }

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
         color= connectionColorFromBranchOff baseColor
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
  

type alias SizesInMissingBranchOff a=
  {a
  | textHeight: Float
  , lineWidth: Float
  , pad: Float
  }
viewMissingBranchOff:
  BranchingOffIndex ->DefinitionId
  ->SizesInMissingBranchOff a
  ->Svg Msg
viewMissingBranchOff
  index definitionId
  { textHeight, pad, lineWidth }
  =
  let plusArmLength= (/) textHeight 2
      radius= (+) pad plusArmLength
  in
  Use.Svg.group
    (clicks
    <|MissingBranchingOffPressed index definitionId
    )
    [ Use.Svg.circle (Use.Svg.px radius)
        [ Use.Svg.translate { x= radius, y= 0 }
        , Use.Svg.fillColor
            (missingBranchOffColor |>toRgba Use.Svg.rgba)
        ]
    , Use.Svg.line
        (plusPoints plusArmLength)
        [ Use.Svg.translate { x= radius, y= 0 }
        , Use.Svg.lineWidth
            (Use.Svg.px lineWidth)
        , Use.Svg.linecap Use.Svg.roundCap
        , Use.Svg.linejoin Use.Svg.roundJoin
        , Use.Svg.lineColor
            (connectionColorFromBranchOff missingBranchOffColor)
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

defaultFont: String
defaultFont=
  "Noto Sans"

viewLabel: Use.Svg.Size ->String ->Svg msg
viewLabel fontSize name=
  Use.Svg.text name
    [ Use.Svg.fillColor (Use.Svg.rgb 1 1 1)
    , Use.Svg.fontFamily defaultFont
    , Use.Svg.fontSize fontSize
    , noTextSelect
    ]


viewTabRow:
  List (Ui.Element msg) ->Ui.Element msg
viewTabRow=
  viewInput []
  <<Ui.row
      [ Ui.spacing 8
      , Ui.height Ui.fill
      , Ui.width Ui.fill
      ]

viewBranchOffTabs:
  Dict BranchingOffIndex BranchingOff
  ->Maybe BranchingOffIndex
  ->Ui.Element Msg
viewBranchOffTabs branchingOffs maybeSelectedIndex=
  let tab=
        maybeSelectedIndex
        |>Maybe.map
            (\selectedIndex->
              (\index->
                (case (==) index selectedIndex of
                  True->
                    viewSelectedTab

                  False->
                    viewUnselectedTab
                )
                <|SelectBranchingOff index
              )
            )
        |>Maybe.withDefault
            (viewUnselectedTab <<SelectBranchingOff)
  in
  viewTabRow
    (branchingOffs
    |>Dict.toList
    |>List.map
        (\( index, branchingOff )->
          expectDefinition
            (val .definitionIndex branchingOff)
            branchingOffs
          |>Maybe.map
              (\definition->
                case branchingOff of
                  Definition def->
                    tab index definition.name
                      (branchOffColor|>toRgba Ui.rgba)
                    
                  Defined defined->
                    tab index definition.name
                      (branchOffColor|>toRgba Ui.rgba)
              )
          |>Maybe.withDefault
              definitionNotFoundUi
        )
    )

viewTab:
  Ui.Color ->msg ->String ->Ui.Color
  ->Ui.Element msg
viewTab fontColor onPress name bgColor=
  Ui.row []
    [ UiInput.button 
        [ UiBackground.color (Ui.rgba 0 0 0 0)
        , UiFont.color fontColor
        , Ui.paddingXY 3 5
        , Ui.height Ui.fill
        ]
        { onPress= Just onPress,
          label=
            Ui.el
              [ Ui.centerY, Ui.centerX ]
              (Ui.text name)
        }
    , Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fill)
        , Ui.paddingXY 6 0
        ]
        (Ui.html
          (Svg.svg
            [ Use.Svg.height (Use.Svg.px 2)
            , Use.Svg.width (Use.Svg.relative 1)
            ]
            [ Use.Svg.circle (Use.Svg.relative 1)
                [ Use.Svg.fillColor
                    (Use.Svg.rgba 1 0.36 0 0.38)
                ]
            ]
          )
        )
    ]

viewUnselectedTab:
  msg ->String ->Ui.Color ->Ui.Element msg
viewUnselectedTab=
  viewTab (Ui.rgb 0.55 0.55 0.55)

viewSelectedTab:
  msg ->String ->Ui.Color ->Ui.Element msg
viewSelectedTab=
  viewTab (Ui.rgb 1 1 1)

viewInput:
  List (Ui.Attribute msg)
  ->Ui.Element msg ->Ui.Element msg
viewInput attrs inputElement=
  Ui.column
    ([ Ui.spacing 3 ]
    ++attrs
    )
    [ Ui.el
        [ Ui.height Ui.fill ]
        (Ui.el
          [ UiFont.color (Ui.rgb 1 1 1)
          ]
          inputElement
        )
    , Ui.el
        [ Ui.width Ui.fill
        , Ui.height (Ui.fill)
        , Ui.paddingXY 6 0
        ]
        (Ui.html
          (Svg.svg
            [ Use.Svg.height (Use.Svg.px 2)
            , Use.Svg.width (Use.Svg.relative 1)
            ]
            [ Svg.rect
                [ Use.Svg.height (Use.Svg.relative 1)
                , Use.Svg.width (Use.Svg.relative 1)
                , Use.Svg.fillColor (Use.Svg.rgba 1 0.36 0 0.38)
                ] []
            ]
          )
        )
    ]


viewBranchOffProperties: String ->Ui.Element Msg
viewBranchOffProperties name=
  Ui.row
    [ Ui.width Ui.fill
    ]
    [ viewBranchOffRename name
    ]
viewBranchOffRename: String ->Ui.Element Msg
viewBranchOffRename name=
  viewInput [ Ui.width Ui.fill ]
    (UiInput.search
      [ UiBackground.color (Ui.rgba 0 0 0 0)
      , UiBorder.color (Ui.rgba 0 0 0 0)
      , Ui.width Ui.fill
      , Ui.height Ui.fill
      , UiFont.family [ UiFont.typeface defaultFont ]
      ]
      { onChange= RenameBranchingOff
      , text= name
      , placeholder= Nothing
      , label= UiInput.labelHidden "name"
      }
    )


viewEditorProperties:
  Dict BranchingOffIndex BranchingOff
  ->Maybe BranchingOffIndex
  ->Ui.Element Msg
viewEditorProperties branchOffs selected=
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
    , viewBranchOffTabs branchOffs selected
    ]

viewScaleButton: String ->msg ->Ui.Element msg
viewScaleButton text msg=
  viewInput
    [ UiFont.family [ UiFont.monospace ]
    ]
    (UiInput.button [ Ui.padding 7 ]
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
    , viewProjectRename
        (.name (ZipList.current projects))
    , viewProjectTabs projects
    ]
    |>List.map (Ui.el [ Ui.alignBottom ])
    )

viewSaveButton: Ui.Element Msg
viewSaveButton=
  viewInput []
    (UiInput.button [ Ui.padding 7 ]
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
    (UiInput.button [ Ui.padding 7 ]
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
    (UiInput.button [ Ui.padding 7 ]
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

viewProjectRename: String ->Ui.Element Msg
viewProjectRename name=
  viewInput [ Ui.width Ui.fill ]
    (UiInput.search
      [ UiBackground.color (Ui.rgba 0 0 0 0)
      , UiBorder.color (Ui.rgba 0 0 0 0)
      , Ui.width Ui.fill
      , Ui.height Ui.fill
      , UiFont.family [ UiFont.typeface defaultFont ]
      ]
      { onChange= RenameProject
      , text= name
      , placeholder= Nothing
      , label= UiInput.labelHidden "name"
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

