{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Bootstrap where

{-
Collection of bootstrap related pagelets.
-}

import           Control.Monad (when)

import           Data.Data
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Blaze as B
import           Text.Blaze.Html5 hiding (map, link)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes hiding (id)
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.View.Fay.JSON.ServerSide

-- | Represents the possible sizes of columns
newtype ColumnSize = ColumnSize Int
  deriving Eq

columnSize f (ColumnSize s) = f s

colSize1  = ColumnSize 1
colSize2  = ColumnSize 2
colSize3  = ColumnSize 3
colSize4  = ColumnSize 4
colSize5  = ColumnSize 5
colSize6  = ColumnSize 6
colSize7  = ColumnSize 7
colSize8  = ColumnSize 8
colSize9  = ColumnSize 9
colSize10 = ColumnSize 10
colSize11 = ColumnSize 11
colSize12 = ColumnSize 12

-- Returns the HTML class attribute value for the given column size
columnSizeClass = columnSize $ \size -> "col-md-" ++ show size

-- | Represents the possible offsets of columns
newtype ColumnOffset = ColumnOffset Int
  deriving Eq

columnOffset f (ColumnOffset s) = f s

colOffset1  = ColumnOffset 1
colOffset2  = ColumnOffset 2
colOffset3  = ColumnOffset 3
colOffset4  = ColumnOffset 4
colOffset5  = ColumnOffset 5
colOffset6  = ColumnOffset 6
colOffset7  = ColumnOffset 7
colOffset8  = ColumnOffset 8
colOffset9  = ColumnOffset 9
colOffset10 = ColumnOffset 10
colOffset11 = ColumnOffset 11
colOffset12 = ColumnOffset 12

-- Returns the HTML class attribute value for the given column offset
columnOffsetClass = columnOffset $ \offset -> "col-md-offset-" ++ show offset

container = H.div ! class_ "container"

containerFullWidth = H.div ! class_ "container-fluid"

footer = H.div ! A.id "bead-footer" ! class_ "navbar navbar-default navbar-fixed-bottom"

-- | Fades out the footer after the given seconds
fadeOutFooter secs = do
  H.script $ toMarkup $ concat ["$('#bead-footer').delay(", show (secs * 1000), ").fadeOut('slow')"]

fadeOutFooterButton :: Text -> Text -> Text -> Html
fadeOutFooterButton custom ttl text = do
  a ! class_ (B.toValue ("btn " <> custom))
    ! role "button"
    ! A.title (B.toValue ttl)
    ! href "#"
    ! disabled ""
    $ (B.toMarkup text)

-- | Creates a warning style button, if the user clicks on the button the footer fades away.
fadeOutFooterWarningButton = fadeOutFooterButton "btn-warning"

-- | Creates a danger style button, if the user clicks on the button the footer fades away
fadeOutFooterDangerButton = fadeOutFooterButton "btn-danger"

formGroup = H.div ! class_ "form-group"

inputGroup = H.div ! class_ "input-group"

-- | Creates a list group div, which can contain a various list group items
listGroup = H.div ! class_ "list-group"

-- | Creates a list group div, which can contain a various list group items
--   The number of fully visible elements is bounded by the first parameter. It is given in pixels.
listGroupHeightLimit :: Int -> H.Html -> H.Html
listGroupHeightLimit n = H.div ! class_ "list-group" ! (A.style $ toValue $ concat ["max-height: ", show (n * 42), "px; overflow: auto;"])

-- | Creates and unordered list as a list group
unorderedListGroup = H.ul ! class_ "list-group"

-- | Creates a linked list group item with a route to point at, and a text to
-- display
listGroupLinkItem :: Text -> Html -> Html
listGroupLinkItem route text = H.a ! href (toValue route) ! class_ "list-group-item" $ text

-- | Creates an active linked list group item with a route to point at, and a text to display.
listGroupActiveLinkItem :: Text -> H.Html -> H.Html
listGroupActiveLinkItem route text = H.a ! href (toValue route) ! class_ "list-group-item active" $ text

-- | Creates a linked list group item with a route to point at, and a text to
-- display, rendered with the given color.
listGroupAlertLinkItem alert route text = H.a ! href (toValue route) ! class_ (toValue itemColor) $ text
  where
    itemColor = "list-group-item list-group-item-" ++ (alertAlgebra "success" "info" "warning" "danger" alert)

-- | Creates a texted list group item
listGroupTextItem text = H.li ! class_ "list-group-item" $ toMarkup text

tab :: H.Html -> H.Html
tab = H.ul ! class_ "nav nav-tabs"

tabItem :: Text -> Text -> H.Html
tabItem route text = H.li ! role "presentation"
                          $ H.a ! href (toValue route)
                                $ toMarkup text

tabItemActive :: Text -> Text -> H.Html
tabItemActive route text = tabItem route text ! class_ "active"

-- | Creates a badge that can be displayed in list group
badge :: ToMarkup a => a -> H.Html
badge text = H.span ! class_ "badge" $ toMarkup text

alert color = H.div ! class_ (toValue $ "alert alert-" ++ alertToColor color)

-- | Creates a caret sign
caret = H.span ! class_ "caret" $ mempty

-- | Creates a justified button group
buttonGroupJustified = H.div ! class_ "btn-group btn-group-justified"

-- | Creates a button group
buttonGroup = H.div ! class_ "btn-group"

buttonOnClick :: ToMarkup a => String -> a -> String -> Html
buttonOnClick ttl text onclick = customButton ["btn-default"] ttl text ! A.onclick (toValue onclick)

customButton :: (ToMarkup a, ToValue b) => [String] -> b -> a -> Html
customButton custom ttl text =
  a ! class_ (toValue ("btn " <> (unwords custom)))
    ! customAttribute "role" "button"
    ! A.title (toValue ttl)
    $ toMarkup text

-- | Creates a button link with custom button attribute, a route to point
-- a title and a text to show
customButtonLink :: (ToMarkup a, ToValue b, ToValue c) => [String] -> c -> b -> a -> Html
customButtonLink custom ref ttl text = customButton custom ttl text ! href (toValue ref)

-- | Creates a button styled link
buttonLink :: (ToValue b, ToMarkup a) => b -> a -> Html
buttonLink ref text = customButtonLink ["btn-default"] ref T.empty text

disabledButton :: (ToMarkup a, ToValue b) => a -> b -> Html
disabledButton text reason =
  H.div ! class_ (toValue ("btn btn-default" :: Text))
        ! customAttribute "role" "button"
        ! A.disabled ""
        ! A.title (toValue reason)
        $ toMarkup text

-- | Creates a block button styled link
blockButtonLink ref text = customButtonLink ["btn-default", "btn-block"] ref T.empty text

-- | Creates a primary block button styled link
primaryBlockButtonLink ref text = customButtonLink ["btn-primary", "btn-block"] ref T.empty text

-- | Warning button with a given text
warningButtonLink ref text = customButtonLink ["btn-warning"] ref T.empty text

-- | Danger button with a given text
dangerButtonLink ref text = customButtonLink ["btn-danger"] ref T.empty text

-- | Creates a date time picker using a third party library and turns on if the on switch
-- is set to True
datetimePicker paramName date on =
  H.div ! class_ "input-group date"
        ! A.id (toValue paramName) $ do
    input ! formControl
          ! name (toValue paramName)
          ! type_ "text"
          ! readonly ""
          ! required ""
          ! value (toValue date)
    H.span ! class_ "input-group-addon" $ H.span ! class_ "glyphicon glyphicon-calendar" $ mempty
    when on $ dateTimePickerScript paramName

dateTimePickerScript pickerId = script . toMarkup $ T.concat
  [ "$(function () {"
  ,   "$('#", pickerId, "').datetimepicker({"
  ,     "format: 'YYYY-MM-DD HH:mm:ss',"
  ,     "pick12HourFormat: false,"
  ,     "pickSeconds: true"
  ,   "});"
  , "});"
  ]

link :: (ToValue b, ToMarkup a) => b -> a -> H.Html
link ref text =
  a ! href (toValue ref)
    $ toMarkup text

linkNewTab :: (ToValue b, ToMarkup a) => b -> a -> H.Html
linkNewTab ref text = link ref text ! A.target "_blank"

-- | Creates a dropdown button
customDropdownButton custom text =
  button ! type_ "button"
         ! class_ (toValue $ unwords $ "btn" : "dropdown-toggle" : custom)
         ! dataAttribute "toggle" "dropdown"
         $ toMarkup text <> caret

dropdownButton text = customDropdownButton ["btn-default"] text

-- | Creates a list of dropdown menu items
dropdownMenu :: [MenuItem] -> Html
dropdownMenu items = H.ul ! class_ "dropdown-menu" ! customAttribute "role" "menu" $ mapM_ listItem items
  where
    listItem :: MenuItem -> Html
    listItem (Enabled ref text) = li (link ref text)
    listItem (Disabled text reason) = li ! class_ "disabled" $ a ! A.title (toValue reason) $ (toMarkup text)
    listItem Separator = li ! role "separator" ! class_ "divider" $ mempty

data MenuItem = Enabled Text Text
              | Disabled Text Text
              | Separator

-- | Creates a dropdown from the items with the given text on the button
dropdown :: ToMarkup a => a -> [MenuItem] -> Html
dropdown text items = buttonGroup $ do
  dropdownButton text
  dropdownMenu items

customSplitButton custom ref ttl text items = buttonGroup ! A.style "display:flex" $ do
  customButtonLink custom ref ttl text
  customDropdownButton custom ("" :: Text)
  dropdownMenu items

splitButton ref text items = customSplitButton ["btn-default"] ref T.empty text items

customButtonWithDropdown custom ref ttl text items = buttonGroup ! showOnMouseEnter ! hideOnMouseOut $ do
  customButtonLink custom ref ttl text 
  dropdownButton ("" :: Text) ! A.style "display:none"
  dropdownMenu items
  where
    showOnMouseEnter :: B.Attribute
    showOnMouseEnter = B.customAttribute "onmouseenter" "childNodes[1].click()"

    hideOnMouseOut :: B.Attribute
    hideOnMouseOut = A.onmouseout "childNodes[1].click()"

-- | Creates a paragrapth that represents a help block from a given text
helpBlock text = p ! class_ "help-block" $ toMarkup text

-- | Creates a form control selection with the given parameter name, a selector
-- function which determines the selected value, and possible values
selection :: (Show a, Data a) => Text -> (a -> Bool) -> [(a, Text)] -> Html
selection paramName selector values =
  formGroup $ selectionPart
    paramName
    [class_ "form-control", A.required ""]
    selector
    values

-- | Creates a form control selection with the given parameter name, a label, a selector
-- function which determines the selected value, and possible values
selectionWithLabel :: (Show a, Data a) => Text -> Text -> (a -> Bool) -> [(a, Text)] -> Html
selectionWithLabel paramName labelText selector values = formGroup $ do
  labelFor paramName labelText
  selectionPart
    paramName
    [class_ "form-control", A.required ""]
    selector
    values

-- | Creates a form control selection with the given parameter name, a selector
-- function which determines the selected value, and possible values
selectionWithPlaceholder :: (Show a, Data a) => Text -> Text -> [(a, Text)] -> Html
selectionWithPlaceholder paramName placeholder values =
  formGroup $ selectionPartWithPlaceholder
    paramName
    [class_ "form-control", A.required ""]
    placeholder
    values

-- | Creates a form control optional selection with the given parameter name, a label, a selector
-- function which determines the selected value, and possible values
selectionOptionalWithLabel paramName labelText selector values = formGroup $ do
  labelFor paramName labelText
  selectionOptionalPart
    paramName
    [class_ "form-control"]
    selector
    values


-- | Creates a submit block button with a given name and the given text
submitButton nameValue text =
  button ! type_ "submit"
         ! (name $ toValue nameValue)
         ! class_ "btn btn-block btn-default"
         $ toMarkup text

submitButtonColorful nameValue text =
  button ! type_ "submit"
         ! (name $ toValue nameValue)
         ! class_ "btn btn-block btn-primary"
         $ toMarkup text

submitButtonWithAttr attr text =
  button ! type_ "submit"
         ! class_ "btn btn-block btn-default"
         ! attr
         $ toMarkup text

submitButtonWithAttrColorful attr text =
  button ! type_ "submit"
         ! class_ "btn btn-block btn-primary"
         ! attr
         $ toMarkup text


-- | Creates a submit small button with a given name and the given text
smallSubmitButton nameValue text =
  button ! type_ "submit"
         ! (name $ toValue nameValue)
         ! class_ "btn btn-primary"
         $ toMarkup text

-- | Creates a password input with the given name as id, a given label within a form-group control
passwordInput paramName labelText =
  formGroup $ do
    labelFor paramName labelText
    H.input ! formControl
            ! type_ "password"
            ! required ""
            ! name (toValue paramName)
            ! A.id (toValue paramName)

inputForFormControl = H.input ! formControl

-- | Creates a text input field only with a default value
textInputFieldWithDefault paramName value =
    optionalTextInputFieldWithDefault paramName value ! A.required ""

-- | Creates an optional text input field onyl with default value
optionalTextInputFieldWithDefault :: Text -> Text -> Html
optionalTextInputFieldWithDefault paramName value =
    H.input ! formControl
            ! type_ "text"
            ! A.name (toValue paramName)
            ! A.id (toValue paramName)
            ! A.value (toValue value)

-- | Creates a text input with the given name as id, a given label and a placeholder text
textInputWithAttr :: ToValue a => a -> Text -> Text -> Attribute -> Html
textInputWithAttr paramName labelText placeholderText attr =
  formGroup $ do
    labelFor paramName labelText
    H.input ! formControl
            ! type_ "text"
            ! A.required ""
            ! A.name (toValue paramName)
            ! A.id (toValue paramName)
            ! A.placeholder (toValue placeholderText)
            ! attr

textInput paramName labelText placeholderText =
  textInputWithAttr paramName labelText placeholderText mempty

-- | Creates an optional text input with the given name as id, a given label and a placeholder text
optionalTextInput paramName labelText placeholderText =
  formGroup $ do
    labelFor paramName labelText
    H.input ! formControl
            ! type_ "text"
            ! A.name (toValue paramName)
            ! A.id (toValue paramName)
            ! A.placeholder (toValue placeholderText)

-- | Creates an optional text input with the given name as id, a given label and a default value
optionalTextInputWithDefault paramName labelText value =
    formGroup $ do
      labelFor paramName labelText
      optionalTextInputFieldWithDefault paramName value


-- | Creates a text input with the given name as id, a given label and a default value
textInputWithDefault paramName labelText value =
  formGroup $ do
    labelFor paramName labelText
    textInputFieldWithDefault paramName value

readOnlyTextInputWithDefault paramName labelText value =
  formGroup $ do
    labelFor paramName labelText
    (textInputFieldWithDefault paramName value) ! A.readonly ""

-- | Creates a label for the given id and given text
labelFor name text =
  H.label ! for (toValue name) $ (toMarkup text)

-- | Creates a labeled text as a form group element
labeledText name value =
  formGroup $ do
    H.label $ toMarkup $ name
    H.span ! formControl $ value



grayLabel :: Text -> Html
grayLabel t  = H.span ! class_ "label label-default" $ text t

greenLabel :: Text -> Html
greenLabel t = H.span ! class_ "label label-success" $ text t

redLabel :: Text -> Html
redLabel t = H.span ! class_ "label label-danger"  $ text t

blueLabel :: Text -> Html
blueLabel t = H.span ! class_ "label label-primary" $ text t

-- | Creates a text area input field with the given name as id, a given id
textAreaField paramName height =
    H.textarea ! formControl
               ! A.required ""
               ! A.rows (toValue . show $ textAreaRows height)
               ! A.id (toValue paramName)
               ! A.name (toValue paramName)
               ! textAreaStyle

-- | Creates an optional text area input field with the given name as id, a given id
textAreaOptionalField paramName height =
    H.textarea ! formControl
               ! A.rows (toValue . show $ textAreaRows height)
               ! A.id (toValue paramName)
               ! A.name (toValue paramName)
               ! textAreaStyle

data Size
  = Small
  | Medium
  | Large

sizeCata :: a -> a -> a -> Size -> a
sizeCata small medium large size =
  case size of
    Small -> small
    Medium -> medium
    Large -> large

textAreaRows :: Size -> Int
textAreaRows = sizeCata 4 8 12

textAreaStyle :: H.Attribute
textAreaStyle = A.style "font-family: monospace;"

-- | Creates a text area input with the given name as id, a given label
textArea paramName labelText height html =
  formGroup $ do
    labelFor paramName labelText
    textAreaField paramName height html

-- | Creates an optional text area input with the given name as id, a given label
optionalTextArea paramName labelText height html =
  formGroup $ do
    labelFor paramName labelText
    textAreaOptionalField paramName height html

-- | Creates a radio button group, with a given values and labels, the parameter name
-- as numbered ids. The first value is the primary active
radioButtonGroup :: Text -> [(Bool, String, Text)] -> Html
radioButtonGroup paramName valuesAndLabel =
  H.div ! class_ "btn-group" $
    mapM_ button ([1..] `zip` valuesAndLabel)
  where
    button (n,(c,v,l)) =
      H.label ! class_ "btn btn-default" $ do
        checked c $
          H.input ! type_ "radio"
                  ! name (toValue paramName)
                  ! A.id (toValue (paramName <> T.pack (show n)))
                  ! A.value (toValue v)
        toMarkup l
    checked c tag = if c then (tag ! A.checked "") else tag

-- | Creates a bootstrap row
row = H.div ! class_ "row"

-- | Creates a bootstrap column with the given offset
colMd size offset =
  H.div ! class_ (toValue $ concat [columnSizeClass size, " ", columnOffsetClass offset])

-- | Creates a bootstrap 12 column
colMd12 = H.div ! class_ "col-md-12"

-- | Creates a bootstrap 3 width column
colMd3 = H.div ! class_ "col-md-3"

-- | Creates a bootstrap 4 width column
colMd4 = H.div ! class_ "col-md-4"

-- | Creates a bootstrap 6 width column
colMd6 = H.div ! class_ "col-md-6"

-- | Creates a bootstrap 9 width column
colMd9 = H.div ! class_ "col-md-9"

-- | Creates a bootstrap row with only one colMd12 column
rowColMd12 = row . colMd12

-- | Creates a boostrap row with a 4 sized column in the middle of the page
rowCol4Offset4 = row . colMd colSize4 colOffset4

rowCol9Offset3 = row . colMd colSize9 colOffset3

-- | Creates a bootstrap page header

pageHeader :: Text -> Maybe Text -> Html
pageHeader title subtitle = H.div ! class_ "page-header" $ h2 $
  B.text title <> sub

  where
    sub :: Html
    sub = maybe mempty (\st -> br <> H.small (B.text st)) subtitle

-- | Creates a bootstrap table
table = H.table ! class_ "table table-bordered table-condensed table-hover table-striped"

-- Creates a table line first element is a bold text and the second is a HTML snippet
infixl 7 .|.
(.|.) :: (ToMarkup n, ToMarkup v) => n -> v -> Html
name .|. value = H.tr $ do
  H.td $ b $ toMarkup name
  H.td $ toMarkup value

-- Alerts

data Alert = Success | Info | Warning | Danger
  deriving (Eq, Show)

alertAlgebra
  success
  info
  warning
  danger
  = \case
    Success -> success
    Info    -> info
    Warning -> warning
    Danger  -> danger

alertToColor :: Alert -> String
alertToColor = alertAlgebra "success" "info" "warning" "danger"

-- HTML helpers

optionTag :: Text -> Text -> Bool -> Html
optionTag value text selected = H.option ! A.value (toValue value) !? (selected, A.selected "") $ toMarkup text

selectTag :: Text -> Html -> Html
selectTag name =
    H.select ! A.id (toValue name)
             ! A.name (toValue name)
             ! A.required ""

selectOptionalTag :: Text -> Html -> Html
selectOptionalTag name =
    H.select ! A.id (toValue name)
             ! A.name (toValue name)

-- Encodes the value to Fay JSON representation or throw an error for the given name
encode :: (Data a, Show a) => Text -> a -> Text
encode name value = maybe (name <> ": error encoding value") T.pack (encodeToFay value)

selectionPartWithPlaceholder :: (Show a, Data a) =>
  Text -> [Attribute] -> Text -> [(a, Text)] -> Html
selectionPartWithPlaceholder name attrs placeholder options = foldl (!) (selectTag name) attrs $ optionTag "" placeholder False <> mapM_ option options
  where
    option :: (Show a, Data a) => (a, Text) -> Html
    option (v,t) = optionTag (encode "selection" v) t False

selectionPart :: (Show a, Data a) =>
  Text -> [Attribute] -> (a -> Bool) -> [(a, Text)] -> Html
selectionPart name attrs def = foldl (!) (selectTag name) attrs . mapM_ option
  where
    option (v,t) = optionTag (encode "selection" v) t (def v)

selectionOptionalPart :: (Show a, Data a) =>
  Text -> [Attribute] -> (a -> Bool) -> [(a, Text)] -> Html
selectionOptionalPart name attrs def = foldl (!) (selectOptionalTag name) attrs . mapM_ option
  where
    option (v,t) = optionTag (encode "selection" v) t (def v)

panel :: Maybe Alert -> Maybe Html -> Html -> Html
panel color heading body =
  H.div
    ! class_ (toValue ("panel panel-" ++ maybe "default" alertToColor color))
    $ do maybe mempty (H.div ! class_ "panel-heading") heading
         H.div ! class_ "panel-body" $ body

-- Collapsible

-- | Creates a panel group
panelGroup =
  H.div ! A.class_ "panel-group" ! role "tablist"

-- Attributes

ariaExpanded = customAttribute "aria-expanded"

ariaControls = customAttribute "aria-controls"

ariaLabelledBy = customAttribute "aria-labelledby"

textCenter = A.class_ "text-center"
textRight = A.class_ "text-right"

dataToggle = customAttribute "data-toggle"

dataPlacement = customAttribute "data-placement"

dataParend = customAttribute "data-parent"

formControl = class_ "form-control"

areaMultiselectable = customAttribute "aria-multiselectable"

-- | Adds a tooltip to a given HTML tag
tooltip :: Attribute
tooltip = dataToggle "tooltip"

-- | Place the tooltip on the top
tooltipAtTop = dataPlacement "top"

-- | Constants

closed    = False
collapsed = True

-- | Icons

infoIcon :: Html
infoIcon = H.span ! class_ "glyphicon glyphicon-info-sign" ! A.style "font-size: xx-large" $ mempty
