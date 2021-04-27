{-# LANGUAGE OverloadedStrings #-}
module Bead.View.RequestParams where

import Control.Monad (join)
import Data.Text (Text)
import qualified Data.Text as T

import Bead.Controller.Pages (CourseManagementContents)
import Bead.Domain.Entities (Username(Username), Uid(Uid))
import Bead.Domain.Relationships
import Bead.View.Dictionary (Language, languageCata)
import Bead.View.Fay.HookIds
import Bead.View.TemplateAndComponentNames

-- Request Parameter Name Constants

assignmentKeyParamName :: Text
assignmentKeyParamName = fieldName assignmentKeyField

assessmentKeyParamName :: Text
assessmentKeyParamName = fieldName assessmentKeyField

submissionKeyParamName :: Text
submissionKeyParamName = fieldName submissionKeyField

scoreKeyParamName :: Text
scoreKeyParamName = fieldName scoreKeyField

evaluationKeyParamName :: Text
evaluationKeyParamName = fieldName evaluationKeyField

languageParamName :: Text
languageParamName = fieldName changeLanguageField

courseKeyParamName :: Text
courseKeyParamName = fieldName courseKeyField

groupKeyParamName :: Text
groupKeyParamName = fieldName groupKeyField

testScriptKeyParamName :: Text
testScriptKeyParamName = fieldName testScriptKeyField

courseManagementContentsParamName :: Text
courseManagementContentsParamName = "course-management-contents"

mossScriptInvocationKeyParamName :: Text
mossScriptInvocationKeyParamName = "moss-script-invocation-key"

-- Request Param is a Pair of Strings, which
-- are key and value representing a parameter in
-- the GET or POST http request
newtype ReqParam = ReqParam (Text, Text)

-- Produces a string representing the key value pair
-- E.g: ReqParam ("name", "rika") = "name=rika"
queryStringParam :: ReqParam -> Text
queryStringParam (ReqParam (k,v)) = T.concat [k, "=", v]

-- Values that can be converted into a request param,
-- only the value of the param is calculated
class ReqParamValue p where
  paramValue :: p -> Text

-- Values that can be converted into request param,
-- the name and the value is also calculated
class (ReqParamValue r) => RequestParam r where
  requestParam :: r -> ReqParam

instance ReqParamValue CourseManagementContents where
  paramValue = T.pack . show

instance RequestParam CourseManagementContents where
  requestParam t = ReqParam (courseManagementContentsParamName, paramValue t)

instance ReqParamValue MossScriptInvocationKey where
  paramValue (MossScriptInvocationKey k) = T.pack k

instance RequestParam MossScriptInvocationKey where
  requestParam k = ReqParam (mossScriptInvocationKeyParamName, paramValue k)

instance ReqParamValue AssignmentKey where
  paramValue (AssignmentKey a) = T.pack a

instance RequestParam AssignmentKey where
  requestParam a = ReqParam (assignmentKeyParamName, paramValue a)

instance ReqParamValue AssessmentKey where
  paramValue (AssessmentKey a) = T.pack a

instance RequestParam AssessmentKey where
  requestParam a = ReqParam (assessmentKeyParamName, paramValue a)

instance ReqParamValue SubmissionKey where
  paramValue (SubmissionKey s) = T.pack s

instance RequestParam SubmissionKey where
  requestParam s = ReqParam (submissionKeyParamName, paramValue s)

instance ReqParamValue ScoreKey where
  paramValue (ScoreKey s) = T.pack s

instance RequestParam ScoreKey where
  requestParam s = ReqParam (scoreKeyParamName, paramValue s)

instance ReqParamValue GroupKey where
  paramValue (GroupKey g) = T.pack g

instance RequestParam GroupKey where
  requestParam g = ReqParam (groupKeyParamName, paramValue g)

instance ReqParamValue TestScriptKey where
  paramValue (TestScriptKey t) = T.pack t

instance RequestParam TestScriptKey where
  requestParam t = ReqParam (testScriptKeyParamName, paramValue t)

instance ReqParamValue CourseKey where
  paramValue (CourseKey c) = T.pack c

instance RequestParam CourseKey where
  requestParam g = ReqParam (courseKeyParamName, paramValue g)

instance ReqParamValue EvaluationKey where
  paramValue (EvaluationKey e) = T.pack e

instance RequestParam EvaluationKey where
  requestParam e = ReqParam (evaluationKeyParamName, paramValue e)

instance ReqParamValue Username where
  paramValue (Username u) = T.pack u

instance RequestParam Username where
  requestParam u = ReqParam (fieldName usernameField, paramValue u)

instance ReqParamValue Uid where
  paramValue (Uid u) = T.pack u

instance RequestParam Uid where
  requestParam u = ReqParam (fieldName userUidField, paramValue u)

instance ReqParamValue Language where
  paramValue = languageCata T.pack

instance RequestParam Language where
  requestParam l = ReqParam (languageParamName, paramValue l)
