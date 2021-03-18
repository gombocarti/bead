{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.CourseManagement.TestScript (
    createTestScript
  , newTestScriptPage
  , modifyTestScript
  , modifyTestScriptPage
  ) where

import           Control.Arrow ((***))
import           Data.String (fromString)
import           Data.String.Utils (replace)

import           Text.Blaze (toMarkup)
import           Text.Blaze.Html5 as H hiding (map)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RequestParams (courseKeyParamName)
import qualified Bead.View.UserActions as UA

-- * Content Handlers

data PageData
  = Create CourseKey
  -- ^ Create a new test script
  | Modify CourseKey TestScriptKey TestScript
  -- ^ Modify an existing test script with CourseName Key Script

pageDataCata :: (CourseKey -> a)
             -> (CourseKey -> TestScriptKey -> TestScript -> a)
             -> PageData
             -> a
pageDataCata
  create
  modify
  p = case p of
    Create ck       -> create ck
    Modify ck tk ts -> modify ck tk ts

createTestScript :: ModifyHandler
createTestScript = ModifyHandler postNewTestScript

modifyTestScript :: ModifyHandler
modifyTestScript = ModifyHandler postModifyTestScript

newTestScriptPage :: ContentHandler IHtml
newTestScriptPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  return $ pageContent (Create ck)

postNewTestScript :: POSTContentHandler
postNewTestScript = do
  script' <- TestScript
    <$> (getParameter (stringParameter (fieldName testScriptNameField) "Test Script Name"))
    <*> (getParameter (stringParameter (fieldName testScriptDescField) "Test Script Description"))
    <*> (getParameter (stringParameter (fieldName testScriptNotesField) "Test Script Notes"))
    <*> (replaceCrlf <$> getParameter (stringParameter (fieldName testScriptScriptField) "Test Script"))
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  script <- userStory $ do
    Story.isAdministratedCourse ck
    (course, _groupkeys) <- Story.loadCourse ck
    return (script' $ courseTestScriptType course)
  return $ Action $ do
    Story.saveTestScript ck script
    return $ redirection $ Pages.courseManagement ck Pages.TestScriptsContents ()

-- silently ignores the course key parameter in the query string
modifyTestScriptPage :: TestScriptKey -> ContentHandler IHtml
modifyTestScriptPage tsk = do
  (ck, script) <- userStory $ do
    Story.isAdministratedTestScript tsk
    (script, ck)  <- Story.loadTestScript tsk
    return (ck, script)
  return $ pageContent (Modify ck tsk script)

postModifyTestScript :: POSTContentHandler
postModifyTestScript = do
  script' <- TestScript
    <$> (getParameter (stringParameter (fieldName testScriptNameField) "Test Script Name"))
    <*> (getParameter (stringParameter (fieldName testScriptDescField) "Test Script Description"))
    <*> (getParameter (stringParameter (fieldName testScriptNotesField) "Test Script Notes"))
    <*> (replaceCrlf <$> getParameter (stringParameter (fieldName testScriptScriptField) "Test Script"))
  tsk <- getParameter testScriptKeyPrm
  (script, ck) <- userStory $ do
    (testscript, courseKey) <- Story.loadTestScript tsk
    return (script' $ tsType testscript, courseKey)
  return $ Action $ do
    Story.modifyTestScript tsk script
    return $ redirection $ Pages.courseManagement ck Pages.TestScriptsContents ()

pageContent :: PageData -> IHtml
pageContent pd = do
  msg <- getI18N

  let (defaultName, defaultDesc, defaultHelp, defaultScript) = pageDataCata
        (const ("", "", "", ""))
        (\_ck _key script -> (tsName script, tsDescription script, tsNotes script, tsScript script))
        pd

  let name        = Bootstrap.textInputWithDefault (fieldName testScriptNameField) (msg $ msg_NewTestScript_Name "Name") defaultName
  let description = Bootstrap.optionalTextInputWithDefault (fieldName testScriptDescField) (msg $ msg_NewTestScript_Description "Description") defaultDesc
  let help        = Bootstrap.optionalTextArea (fieldName testScriptNotesField) (msg $ msg_NewTestScript_Notes "Help for writing test cases") Bootstrap.Medium (toMarkup defaultHelp)
  let script      = Bootstrap.textArea (fieldName testScriptScriptField) (msg $ msg_NewTestScript_Script "Test script") Bootstrap.Large (toMarkup defaultScript)

  return $ do
    Bootstrap.rowColMd12 $ do
      let header = pageDataCata (const $ msg_NewTestScript_New "New test script for the course") (\_ _ _ -> msg_NewTestScript_Modify "Modify test script") pd
      H.h3 . toMarkup . msg $ header
      postForm (routeOf $ testScriptPage pd) $ do
        name
        description
        help
        script
        Bootstrap.submitButton (fieldName testScriptSaveButton) (fromString . msg $ msg_NewTestScript_Save "Commit")

  where
    testScriptPage = pageDataCata (\ck -> Pages.createTestScript ck ()) (\ck key _script -> Pages.modifyTestScript ck key ())

-- Helper

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"
