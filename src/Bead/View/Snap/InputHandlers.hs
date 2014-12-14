module Bead.View.Snap.InputHandlers where

import           Control.Applicative ((<$>),(<*>))
import           Data.Time (UTCTime(..))

import           Text.Blaze.Html5 (Html)

import           Bead.Domain.Entities
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Evaluation
import           Bead.Domain.Relationships
import           Bead.View.Snap.Application (App(..))
import           Bead.View.Snap.DataBridge
import           Bead.View.Snap.Fay.Hooks
import           Bead.View.Snap.Fay.HookIds
import           Bead.View.Snap.HandlerUtils
import           Bead.View.Snap.I18N (IHtml, getI18N)
import           Bead.View.Snap.Pagelets
import           Bead.View.Snap.TemplateAndComponentNames
import           Bead.View.Snap.Translation


-- * Input pagelet and handler

class GetValueHandler i where
  getValue :: HandlerError App App i

class InputPagelet i where
  inputPagelet :: Maybe i -> IHtml

-- * Instances

instance GetValueHandler GroupKey where
  getValue = getParameter groupKeyPrm

emptyGroup :: Maybe Group
emptyGroup = Nothing

instance InputPagelet Group where
  inputPagelet g = do
    msg <- getI18N
    return $ do
      table "create-group" "create-group-table" $ do
        tableLine (msg $ Msg_Input_Group_Name "Title") $ required $ textInput (fieldName groupNameField) 10 (fmap groupName g)
        tableLine (msg $ Msg_Input_Group_Description "Description") $ textInput (fieldName groupDescField) 10 (fmap groupDesc g)

instance GetValueHandler Group where
  getValue = Group
      <$> getParameter (stringParameter (fieldName groupNameField) "Csoport neve")
      <*> getParameter (stringParameter (fieldName groupDescField) "Csoport leírása")

instance GetValueHandler CourseKey where
  getValue = getParameter courseKeyPrm

instance GetValueHandler Course where
  getValue = Course
    <$> getParameter (stringParameter (fieldName courseNameField) "Tárgy neve")
    <*> getParameter (stringParameter (fieldName courseDescField) "Tárgy leírása")
    <*> getParameter (jsonParameter (fieldName testScriptTypeField) "Script típusa")

instance GetValueHandler Role where
  getValue = getParameter rolePrm

instance GetValueHandler Username where
  getValue = getParameter usernamePrm

emptyAssignment :: Maybe Assignment
emptyAssignment = Nothing

instance GetValueHandler AssignmentKey where
  getValue = getParameter assignmentKeyPrm

-- * Combined input fields

emptyEvaluationConfig :: Maybe (EvaluationData Binary Percentage)
emptyEvaluationConfig = Nothing

evaluationConfig :: String -> IHtml
evaluationConfig n = do
  msg <- getI18N
  return $ selection n $ map (valueAndName msg) evaluationTypes
  where
    valueAndName msg e = (encodeEvalType e, msg $ name e)

    name (BinEval ()) = Msg_InputHandlers_BinEval "Binary"
    name (PctEval ()) = Msg_InputHandlers_PctEval "Percentage"


-- TODO
dateInput :: String -> Maybe UTCTime -> Html
dateInput n v = required . setHookClass datePickerClass $ textInput n 10 (show <$> v)

hourInput :: String -> Maybe Int -> Html
hourInput n v = required . setHookClass hourSpinnerClass $ textInput n 2 (show <$> v)

minInput :: String -> Maybe Int -> Html
minInput n v = required . setHookClass minuteSpinnerClass $ textInput n 2 (show <$> v)
