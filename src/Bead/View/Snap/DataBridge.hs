module Bead.View.Snap.DataBridge where

import Control.Monad (join)
import Data.Time (UTCTime(..))

import Bead.Domain.Types (readMaybe)
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaulation
import Bead.View.Snap.TemplateAndComponentNames
import Bead.View.Snap.Fay.Hooks
import Bead.View.Snap.Fay.HookIds

{-
Parameters are the data bridge between the Server side and the Client side.
Values are encoded and decoded in the request parameters
-}

-- | Represents an encoding/decoding method for the request parameters
data Parameter a = Parameter {
    -- Produces a String representation of the given value
    encode :: a -> String
    -- Produces 'Just a' if the given string is represents a value
    -- otherwise 'Nothing'
  , decode :: String -> Maybe a
    -- The field name in the request
  , name  :: String
    -- The error message when decoding fails
  , decodeError :: String -> String
    -- The error message when the parameter is not present
  , notFound    :: String
  }

stringParameter :: String -> String -> Parameter String
stringParameter fieldName paramName = Parameter {
    encode = id
  , decode = Just . id
  , name = fieldName
  , decodeError = \m -> join [paramName, " is not decoded: ", m]
  , notFound    = join [paramName, " is not found."]
  }

evaluationValuePrm :: Parameter String
evaluationValuePrm = stringParameter (fieldName evaulationValueField) "Evaluation Value"

customGroupKeyPrm :: String -> Parameter GroupKey
customGroupKeyPrm field = Parameter {
    encode = groupKeyMap id
  , decode = Just . GroupKey
  , name   = field
  , decodeError = ("Invalid Group Key is given: "++)
  , notFound    = "Group Key is not found"
  }

-- Represents the GroupKey parameter
groupKeyPrm :: Parameter GroupKey
groupKeyPrm = customGroupKeyPrm (fieldName groupKeyName)

customCourseKeyPrm :: String -> Parameter CourseKey
customCourseKeyPrm field = Parameter {
    encode = courseKeyMap id
  , decode = Just . CourseKey
  , name   = field
  , decodeError = ("Invalid Course Key is given: "++)
  , notFound    = "Course Key is not found"
  }

-- Represents the CourseKey parameter
courseKeyPrm :: Parameter CourseKey
courseKeyPrm = customCourseKeyPrm (fieldName courseKeyInfo)

-- Represents the AssignmentKey parameter
assignmentKeyPrm :: Parameter AssignmentKey
assignmentKeyPrm = Parameter {
    encode = assignmentKeyMap id
  , decode = Just . AssignmentKey
  , name   = fieldName assignmentKeyField
  , decodeError = ("Invalid Assignment Key is given: "++)
  , notFound    = "Assignment Key is not found"
  }

-- Represents the SubmissionKey parameter
submissionKeyPrm :: Parameter SubmissionKey
submissionKeyPrm = Parameter {
    encode = submissionKeyMap id
  , decode = Just . SubmissionKey
  , name   = fieldName submissionKeyField
  , decodeError = ("Invalid Submission Key is given: "++)
  , notFound    = "Submission Key is not found"
  }

-- Represents the SubmissionKey parameter
evaluationKeyPrm :: Parameter EvaulationKey
evaluationKeyPrm = Parameter {
    encode = evaluationKeyMap id
  , decode = Just . EvaulationKey
  , name   = fieldName evaulationKeyField
  , decodeError = ("Invalid Evaluation Key is given: "++)
  , notFound    = "Evaluation Key is not found"
  }

evalConfigPrm :: EvaulationHook -> Parameter EvaulationConfig
evalConfigPrm hook = Parameter {
    encode = show
  , decode = readEvalConfig
  , name   = evHiddenValueId hook
  , decodeError = ("Invalid evaulation config is given: "++)
  , notFound = "Evaluation config is not found"
  }
  where
    readEvalConfig :: String -> Maybe (EvaulationData () PctConfig)
    readEvalConfig = fmap convert . readMaybe
      where
        convert :: EvaulationData () Double -> EvaulationData () PctConfig
        convert = evaluationDataMap (BinEval . id) (PctEval . PctConfig)

rolePrm :: Parameter Role
rolePrm = Parameter {
    encode = show
  , decode = parseRole
  , name   = fieldName userRoleField
  , decodeError = ("Invalid role is given: "++)
  , notFound    = "Role is not found"
  }

customUsernamePrm :: String -> Parameter Username
customUsernamePrm field = Parameter {
    encode = usernameMap id
  , decode = Just . Username
  , name = field
  , decodeError = ("Invalid username is given: "++)
  , notFound    = "Username is not found"
  }

usernamePrm :: Parameter Username
usernamePrm = customUsernamePrm (fieldName usernameField)

userEmailPrm :: Parameter Email
userEmailPrm = Parameter {
    encode = emailMap id
  , decode = parseEmail
  , name = fieldName userEmailField
  , decodeError = ("Invalid email is given: "++)
  , notFound = "Email is not found"
  }

-- Produces a Parameter for a read instance for the
-- given parameter and a name. The name is shown
-- when decoding error or absence occurs.
readablePrm :: (Show a, Read a) => String -> String -> Parameter a
readablePrm field name = Parameter {
    encode = show
  , decode = readMaybe
  , name = field
  , decodeError = (\v -> "Invalid value is given for, " ++ name ++ " " ++ v)
  , notFound = name ++ " is not found"
  }

assignmentTypePrm :: Parameter AssignmentType
assignmentTypePrm = readablePrm (fieldName assignmentTypeField) "Assignment Type"

assignmentStartPrm :: Parameter UTCTime
assignmentStartPrm = readablePrm (fieldName assignmentStartField) "Assignment Start"

assignmentEndPrm :: Parameter UTCTime
assignmentEndPrm = readablePrm (fieldName assignmentEndField) "Assignment End"