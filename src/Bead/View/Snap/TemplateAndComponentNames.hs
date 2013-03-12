{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
module Bead.View.Snap.TemplateAndComponentNames where

-- This module contains information about templates and
-- fields in the type safe manner.

-- Haskell imports
import Data.String
import qualified Bead.Controller.Pages as P

-- Test imports
import Bead.Invariants (UnitTests(..))
import Data.Set (Set)
import qualified Data.Set as Set

-- * Type safe declarations

class SnapFieldName f where
  fieldName :: (IsString s) => f -> s

-- * Component names

data LoginComp
  = UsernameField { lcFieldName :: String }
  | PasswordField { lcFieldName :: String }

instance SnapFieldName LoginComp where
  fieldName = fromString . lcFieldName

loginUsername = UsernameField "login"
loginPassword = PasswordField "password"

data RegistrationComp
  = RegFamilyName   { rFieldName :: String }
  | RegEmailAddress { rFieldName :: String }

instance SnapFieldName RegistrationComp where
  fieldName = fromString . rFieldName

registrationFamilyName   = RegFamilyName   "reg_family_name"
registrationEmailAddress = RegEmailAddress "reg_email_address"

data ExerciseForm
  = ExerciseForm     { eFieldName :: String }
  | ExerciseKeyField { eFieldName :: String }

instance SnapFieldName ExerciseForm where
  fieldName = fromString . eFieldName

exerciseForm = ExerciseForm "exercise"
exerciseKey  = ExerciseKeyField "exercise-key"

data CoursesForm
  = CoursesKey  { csFieldName :: String }
  | CoursesForm { csFieldName :: String }

instance SnapFieldName CoursesForm where
  fieldName = fromString . csFieldName

coursesForm = CoursesForm "courses"
coursesKey  = CoursesKey "courses-key"

data CourseFormInfo
  = CourseKeyInfo   { cFieldName :: String }
  | CourseFormInfo  { cFieldName :: String }
  | CourseNameField { cFieldName :: String }
  | CourseCodeField { cFieldName :: String }
  | CourseDescField { cFieldName :: String }

instance SnapFieldName CourseFormInfo where
  fieldName = fromString . cFieldName

courseKeyInfo  = CourseKeyInfo  "course-key"
courseFormInfo = CourseFormInfo "course"
courseCodeField = CourseCodeField "course-code"
courseNameField = CourseNameField "course-name"
courseDescField = CourseDescField "course-desc"

newtype GroupKeyName
  = GroupKeyName { gkFieldName :: String }

instance SnapFieldName GroupKeyName where
  fieldName = fromString . gkFieldName

groupKeyName = GroupKeyName "group-key"

data GroupField
  = GroupCodeField { gFieldName :: String }
  | GroupDescField { gFieldName :: String }
  | GroupNameField { gFieldName :: String }

instance SnapFieldName GroupField where
  fieldName = fromString . gFieldName

groupCodeField = GroupCodeField "group-code"
groupNameField = GroupNameField "group-name"
groupDescField = GroupDescField "group-desc"

data UsernameField
  = UsernameFiel { uFieldName :: String }

instance SnapFieldName UsernameField where
  fieldName = fromString . uFieldName

usernameField = UsernameField "username"

-- * Template names

newtype LoginTemp = LoginTemp String
  deriving (Eq)

loginTemp = LoginTemp "login"

-- * Unit tests

data SFN = forall n . SnapFieldName n => SFN n

instance SnapFieldName SFN where
  fieldName (SFN n) = fieldName n

fieldList :: [String]
fieldList = map fieldName $
  [ SFN loginUsername,  SFN loginPassword,   SFN registrationFamilyName, SFN registrationEmailAddress
  , SFN exerciseForm,   SFN exerciseKey,     SFN coursesForm,            SFN coursesKey
  , SFN courseFormInfo, SFN courseCodeField, SFN courseNameField,        SFN courseDescField
  , SFN groupKeyName,   SFN groupCodeField,  SFN groupNameField,         SFN groupDescField
  , SFN usernameField,  SFN courseKeyInfo
  ]

unitTests = UnitTests [
    ( "Field names must be unique"
      , ((Set.size . Set.fromList $ fieldList) == (length fieldList)) )
  ]
