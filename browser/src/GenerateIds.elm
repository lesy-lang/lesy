module GenerateIds exposing (..)

{-| ui for generated ids in code
-}

import Id
import Element as Ui
import Element.Input as UiInput
import Element.Background as UiBackground
import Element.Font as UiFont
import Browser
import Html exposing (Html)
import Array exposing (Array)
import Use.Misc exposing (noTextSelect)

{- now get your ids **please use cmd + x**

𝆄𪊎𮇲򍮘񩨋󂦮󫷏򩳒
ḵ񾟑񰩷񶌱򻅤񬇖𾺚╫
񵧡񨟇󳯏쐌䪛񨏰𢻤񆿡
󛄁끧򏵧㶍򠇺򬝺󻹵𼷁
𑻁򰌠񀘈򛴉�󄫀򮒎񆎲
𩩨򤠍񒵮󜯷񤊏𺩁𴈱󐸤
򨖺􎨯󳢖򸮛򃬖򯃘񂥆􉷷
󼨴򛲴󶋧󲵐𙧣󇤌󓿎򒆤
򕏊󱀜󊽷񟶎򪊪񇁉񷪹
󉓿򾂈􈨒󐄆󖿂􉤟򕱑񶸘
𧹝𐡷񞫾􄤂𭃰𦛬򑤻𣡎
뜟𝸍􅮜񙄕򥢢󜲠󳂹󠅵
󗂾񎦬􊢌𙑢񮹋򳂛󢡉󲶿
񪶀𑳭􊫌𴹝𻫢󙏎򷦵󗨪
󌖼񩔊𥬿􎝲󸢴섏橶󖆶
󴣡𵴹񾜻𻃫􅗻Q񎎒󼐬
􁧁󊑚󍎅񊥻ᨐ񣠤򫤻𶶅
󜶕򂶷🗂򮕢󙭆𨼫쨌􄸩
󿊝򾌂􌥧񨱞𨔕𯆧󇲶򡹠
𦌢񺕭𻂆󛋖􏘍񺗘󕼵򭕬
񮦚󓘭㨓񈅳𠟱𽫦񧕸
�񤳭⓾󅇌󚢪񣸍𙚩񟗎
򞐲𪃚􇇛򚧶񑶙􍻵󤍿
􈄈󾺫󗰵򶫰𾙭򯿢𐀎󿕒
𹛘򡾔󼝪񏐋𙲋󣵳󉺺񨍆
󊐕𩀂﹤󮵛󣤈􃗞𳞾󍤖
򅔈󩩪񒹴񒮅򿒺𕊽򱬺򖧑
󸕅񚴜񙨼򞐾򂕴񊆕𡹖󛲋
񓜆򝝷񒱰񸽵򑾦񰓔󰬸󲲎

-}

main: Program () Model Msg
main=
  Browser.element
    { init=
        \_->
          ( Array.repeat 10 ("none-yet"|>String.toList)
          , generateIds
          )
    , update= update
    , view= view
    , subscriptions= \_-> Sub.none
    }

type Msg=
  GenerateId
  | IdGenerated Int Id.Id

type alias Model=
  Array Id.Id

update: Msg ->Model ->( Model, Cmd Msg )
update msg model=
  case msg of
    GenerateId->
      ( model, generateIds )
    
    IdGenerated index id->
      ( Array.set index id model, Cmd.none )

generateIds: Cmd Msg
generateIds=
  List.range 0 9
  |>List.map (\index-> Id.generate (IdGenerated index))
  |>Cmd.batch

view: Model ->Html Msg
view model=
  Ui.layout []
    (Ui.column
      [ Ui.height Ui.fill
      , Ui.width Ui.fill
      , UiBackground.color (Ui.rgb 0 0 0)
      , Ui.padding 20
      , Ui.spacing 30
      ]
      [ UiInput.button
          [ UiBackground.color (Ui.rgb 0 0.5 0.8)
          , UiFont.color (Ui.rgb 1 1 1)
          , UiFont.size 50
          , Ui.paddingXY 20 20
          ]
          { label= Ui.text "generate new ids"
          , onPress= Just GenerateId
          }
      , Ui.column
          [ UiBackground.color (Ui.rgb 0 0.1 0.1)
          ]
          (model
          |>Array.map
              (\id->
                Ui.el
                  [ UiFont.size 45
                  , UiFont.color (Ui.rgb 1 1 1)
                  , UiBackground.color (Ui.rgba 0 0 0 0)
                  ]
                  (Ui.text (String.fromList id))
              )
          |>Array.toList
          )
      , Ui.el
          [ UiFont.color (Ui.rgb 1 1 1)
          , Ui.htmlAttribute noTextSelect
          ]
          (Ui.text "(use Cmd + a, Cmd + c)")
      ]
    )
