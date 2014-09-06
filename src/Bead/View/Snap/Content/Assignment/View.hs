{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content.Assignment.View where

import           Control.Arrow ((&&&))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BsLazy
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString(..), fromString)
import qualified Data.Time as Time

import           Fay.Convert
import qualified Text.Blaze.Html5.Attributes as A (id)
import           Text.Blaze.Html5.Attributes as A hiding (id)
import           Text.Blaze.Html5 as H hiding (map)
import           Text.Printf (printf)

import           Bead.Controller.Pages (PageDesc)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.Shared.Evaluation (binaryConfig, evConfigCata)
import           Bead.View.Snap.Fay.HookIds
import           Bead.View.Snap.Fay.Hooks
import           Bead.View.Snap.Content hiding (name, option, required)
import           Bead.View.Snap.Markdown

import           Bead.View.Snap.Content.Assignment.Data

newAssignmentContent :: PageData -> IHtml
newAssignmentContent pd = do
  msg <- getI18N
  let hook = assignmentEvTypeHook
  evalConfig <- evaluationConfig (evSelectionId hook)
  return $ do
            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ H.div ! class_ "page-header" $ do
                hr
                h1 . fromString . msg $ pageDataCata
                   (const5 $ Msg_LinkText_NewCourseAssignment "New Course Assignment")
                   (const5 $ Msg_LinkText_NewGroupAssignment "New Group Assignment")
                   (const6 $ Msg_LinkText_ModifyAssignment "Modify Assignment")
                   (const5 $ Msg_LinkText_ViewAssignment "View Assignment")
                   (const7 $ Msg_LinkText_NewCourseAssignmentPreview "New Course Assignment")
                   (const7 $ Msg_LinkText_NewGroupAssignmentPreview "New Group Assignment")
                   (const7 $ Msg_LinkText_ModifyAssignmentPreview "Modify Assignment")
                   pd

            H.div ! class_ "row" $ H.div ! class_ "col-md-12"
              $ H.form ! A.method "post"
              $ H.div ! A.id (fromString $ hookId assignmentForm) $ do

                H.div ! class_ "form-group" $ do
                    let assignmentTitleField = fromString $ fieldName assignmentNameField

                        assignmentTitlePlaceholder = fromString $
                           fromAssignment
                                (const "")
                                (fromString . msg $ Msg_NewAssignment_Title_Default "Unnamed Assignment")
                                pd

                        assignmentTitle = fromAssignment (fromString . Assignment.name) mempty pd

                    H.label ! for assignmentTitleField $
                      fromString $ msg $ Msg_NewAssignment_Title "Title"

                    editOrReadOnly pd $
                      input ! class_ "form-control" ! A.id assignmentTitleField
                            ! name assignmentTitleField
                            ! placeholder assignmentTitlePlaceholder ! type_ "text"
                            ! required ""
                            ! value assignmentTitle

                    H.p ! class_ "help-block"$ fromString . msg $ Msg_NewAssignment_Info_Normal $ concat
                      [ "Solutions may be submitted from the time of opening until the time of closing. "
                      , "The assignment will not be visible until it is opened. "
                      , "The assignments open and close automatically."
                      ]

                -- Visibility information of the assignment
                H.h4 $ fromString $ msg $ Msg_NewAssignment_SubmissionDeadline "Visibility"

                let dateTimePickerScript pickerId = script . fromString $ concat
                      [ "$(function () {"
                      ,   "$('#", pickerId, "').datetimepicker({"
                      ,     "format: 'YYYY-MM-DD HH:mm:ss',"
                      ,     "pick12HourFormat: false,"
                      ,     "pickSeconds: true"
                      ,   "});"
                      , "});"
                      ]

                    date t =
                      let localTime = timeZoneConverter t
                          timeOfDay = Time.localTimeOfDay localTime
                      in ( show $ Time.localDay         localTime
                         , printf "%02d" $ Time.todHour timeOfDay
                         , printf "%02d" $ Time.todMin  timeOfDay
                         )

                    showDate (dt, hour, min) = concat [dt, " ", hour, ":", min, ":00"]

                    startDateStringValue = showDate $ date $ pageDataCata
                      (\_tz t _c _ts _fs -> t)
                      (\_tz t _g _ts _fs -> t)
                      (\_tz _k a _ts _fs _tc -> Assignment.start a)
                      (\_tz _k a _ts _tc -> Assignment.start a)
                      (\_tz _t _c _ts _fs a _tc  -> Assignment.start a)
                      (\_tz _t _g _ts _fs a _tc  -> Assignment.start a)
                      (\_tz _k a _ts _fs _tc _tm -> Assignment.start a)
                      pd

                    endDateStringValue = maybe "" (showDate . date) $ pageDataCata
                      (\_tz _t _c _ts _fs -> Nothing)
                      (\_tz _t _g _ts _fs -> Nothing)
                      (\_tz _k a _ts _fs _tc -> Just $ Assignment.end a)
                      (\_tz _k a _ts _tc -> Just $ Assignment.end a)
                      (\_tz _t _c _ts _fs a _tc  -> Just $ Assignment.end a)
                      (\_tz _t _g _ts _fs a _tc  -> Just $ Assignment.end a)
                      (\_tz _k a _ts _fs _tc _tm -> Just $ Assignment.end a)
                      pd

                -- Opening and closing dates of the assignment
                H.div ! class_ "form-group" $ do
                  H.div ! class_ "row" $ do

                    -- Opening date of the assignment
                    H.div ! class_ "col-md-6" $ do
                      let assignmentStart' = fieldName assignmentStartField
                          assignmentStart  = fromString assignmentStart'

                      H.label ! for assignmentStart $ fromString $ msg $ Msg_NewAssignment_StartDate "Opens"
                      H.div ! class_ "input-group date" ! A.id assignmentStart $ do
                        input ! class_ "form-control" ! A.name assignmentStart
                              ! type_ "text" ! readonly "" ! required ""
                              ! value (fromString startDateStringValue)
                        H.span ! class_ "input-group-addon" $ H.span ! class_ "glyphicon glyphicon-calendar" $ mempty
                      onlyOnEdit pd $ dateTimePickerScript assignmentStart'

                    -- Closing date of the assignment
                    H.div ! class_ "col-md-6" $ do
                      let assignmentEnd' = fieldName assignmentEndField
                          assignmentEnd  = fromString assignmentEnd'

                      H.label ! for assignmentEnd $ fromString $ msg $ Msg_NewAssignment_EndDate "Closes"
                      H.div ! class_ "input-group date" ! A.id assignmentEnd $ do
                        input ! class_ "form-control" ! A.name assignmentEnd
                              ! type_ "text" ! readonly "" ! required ""
                              ! value (fromString endDateStringValue)
                        H.span ! class_ "input-group-addon" $ H.span ! class_ "glyphicon glyphicon-calendar" $ mempty
                      onlyOnEdit pd $ dateTimePickerScript assignmentEnd'

                -- Properties of the assignment
                H.h4 $ fromString $ msg $ Msg_NewAssignment_Properties "Properties"

                let editable = True
                    readOnly = False

                    assignmentPropertiesSection ed = do
                      let aas = fromAssignment Assignment.aspects Assignment.emptyAspects pd
                          pwd = if Assignment.isPasswordProtected aas
                                   then Just (Assignment.getPassword aas)
                                   else Nothing
                          editable x = if ed then x else (x ! A.readonly "")
                          assignmentAspect = fromString $ fieldName assignmentAspectField
                          assignmentPwd    = fromString $ fieldName assignmentPwdField

                      H.div ! class_ "form-group" $ do
                          H.div ! class_ "checkbox" $ H.label $ do
                              editable $ checkBox' (fieldName assignmentAspectField)
                                (Assignment.isBallotBox aas)
                                Assignment.BallotBox (msg $ Msg_NewAssignment_BallotBox "Ballot Box")

                          p ! class_ "help-block" $  fromString . msg $ Msg_NewAssignment_Info_BallotBox $ concat
                                [ "(Recommended for tests.) Students will not be able to access submissions and "
                                , "their evaluations until the assignment is closed."
                                ]

                      H.div ! class_ "form-group" $ do
                          H.div ! class_ "checkbox" $ H.label $ do
                            editable $ checkBox' (fieldName assignmentAspectField)
                              (Assignment.isPasswordProtected aas)
                              (Assignment.Password "")
                              (msg $ Msg_NewAssignment_PasswordProtected "Password-protected")

                          p ! class_ "help-block" $ fromString . msg $ Msg_NewAssignment_Info_Password $ concat
                                [ "(Recommended for tests.) Submissions may be only submitted by providing the password. "
                                , "The teacher shall use the password during the test in order to authenticate the "
                                , "submission for the student."
                                ]

                      H.div ! class_ "form-group" $ do
                          H.label $ fromString $ msg $ Msg_NewAssignment_Password "Password"
                          editable $ input ! class_ "form-control"
                                     ! name assignmentPwd ! type_ "text"
                                     ! value (fromString $ fromMaybe "" pwd)

                -- Assignment Properties
                pageDataCata
                  (const5 $ assignmentPropertiesSection editable)
                  (const5 $ assignmentPropertiesSection editable)
                  (const6 $ assignmentPropertiesSection editable)
                  (const5 $ assignmentPropertiesSection readOnly)
                  (const7 $ assignmentPropertiesSection editable)
                  (const7 $ assignmentPropertiesSection editable)
                  (const7 $ assignmentPropertiesSection editable)
                  pd

                -- Assignment Description
                H.div ! class_ "form-group" $ do
                    let assignmentDesc = fromString $ fieldName assignmentDescField
                    H.label ! for assignmentDesc $ fromString . msg $ Msg_NewAssignment_Description "Description"
                    editOrReadOnly pd $ textarea ! class_ "form-control" ! A.id assignmentDesc ! rows "20"
                                                 ! name assignmentDesc ! A.required "" $ do
                      fromString $ fromAssignment Assignment.desc (fromString . msg $
                        Msg_NewAssignment_Description_Default $ unlines
                          [ concat
                             [ "This text shall be in markdown format.  Here are some quick "
                             , "examples:"
                             ]
                          , ""
                          , "  - This is a bullet list item with *emphasis* (italic)."
                          , "  - And this is another item in the list with "
                          , "    **strong** (bold). Note that the rest of the item"
                          , "    shall be aligned."
                          , ""
                          , concat
                              [ "Sometimes one may want to write verbatim text, this how it can "
                              , "be done.  However, `verbatim` words may be inlined any time by "
                              , "using the backtick (`` ` ``) symbol."
                              ]
                          , ""
                          , "~~~~~"
                          , "verbatim text"
                          , "~~~~~"
                          , ""
                          , concat
                              [ "Note that links may be also [inserted](http://haskell.org/). And "
                              , "when everything else fails, <a>pure</a> <b>HTML code</b> "
                              , "<i>may be embedded</i>."
                              ]
                          ]) pd

                -- Preview of the assignment
                let assignmentPreview a = do
                      H.div ! class_ "form-group" $ do
                        H.label $ fromString $ msg $ Msg_NewAssignment_AssignmentPreview "Assignment Preview"
                        H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc a

                pageDataCata
                  (const5 empty)
                  (const5 empty)
                  (const6 empty)
                  (const5 empty)
                  (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
                  (\_tz _t _key _tsType _fs a _tc -> assignmentPreview a)
                  (\_tz _k a _t _fs _tst _tm -> assignmentPreview a)
                  pd

                -- Assignment Test Script Selection
                H.div ! class_ "form-group" $ do
                      testScriptSelection msg pd

                -- Test Case area
                H.div ! class_ "form-group" $ do
                    testCaseArea msg pd

                -- Evaluation config
                H.div ! class_ "form-group" $ do
                  let previewAndCommitForm cfg = do
                        evalSelectionDiv hook
                        hiddenInputWithId (evHiddenValueId hook) (toFayJSON cfg)
                        evalConfig

                  pageDataCata
                    (const5 (previewAndCommitForm binaryConfig))
                    (const5 (previewAndCommitForm binaryConfig))
                    (\_timezone _key asg _tsType _files _testcase -> previewAndCommitForm (Assignment.evType asg))
                    (\_timezone _key asg _tsInfo _testcase -> showEvaluationType msg $ Assignment.evType asg)
                    (\_timezone _time _courses _tsType _files assignment _tccreatio -> previewAndCommitForm (Assignment.evType assignment))
                    (\_timezone _time _groups _tsType _files assignment _tccreation -> previewAndCommitForm (Assignment.evType assignment))
                    (\_timezone _key asg _tsType _files _testcase _tcmod -> previewAndCommitForm (Assignment.evType asg))
                    pd

                -- Hidden course or group keys for the assignment creation
                pageDataCata
                  (\_tz _t (key,_course) _tsType _fs -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
                  (\_tz _t (key,_group)  _tsType _fs -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
                  (const6 (return ()))
                  (const5 (return ()))
                  (\_tz _t (key,_course) _tsType _fs _a _tc -> hiddenInput (fieldName selectedCourse) (courseKeyMap id key))
                  (\_tz _t (key,_group)  _tsType _fs _a _tc -> hiddenInput (fieldName selectedGroup) (groupKeyMap id key))
                  (const7 (return ()))
                  pd

                -- Submit buttons
                onlyOnEdit pd $ H.div ! class_ "row" $ do
                   H.div ! class_ "col-md-6" $
                      button ! type_ "submit"
                             ! class_ "btn btn-block btn-default"
                             ! onclick (fromString $ concat ["javascript: form.action='", routeOf $ pagePreview pd, "';"])
                             $ fromString . msg $ Msg_NewAssignment_PreviewButton "Preview"
                   H.div ! class_ "col-md-6"
                     $ button ! type_ "submit"
                              ! class_ "btn btn-block btn-default"
                              ! onclick (fromString $ concat ["javascript: form.action='", routeOf $ page pd, "';"])
                              $ fromString . msg $ Msg_NewAssignment_SaveButton "Commit"

            H.div ! class_ "row" $ H.div ! class_ "col-md-12" $ hr
            H.script ! type_ "text/javascript" $ "//\n        $(document).ready(function(){\n          $('.combobox').combobox()\n        });\n      //"

    where

      editOrReadOnly = pageDataCata
        (const5 id)
        (const5 id)
        (const6 id)
        (const5 (! A.readonly ""))
        (const7 id)
        (const7 id)
        (const7 id)

      onlyOnEdit pd t = pageDataCata
        (const5 t)
        (const5 t)
        (const6 t)
        (const5 mempty)
        (const7 t)
        (const7 t)
        (const7 t)
        pd

      timeZoneConverter = pageDataCata
        (\tz _t _c _ts _fs -> tz)
        (\tz _t _g _ts _fs -> tz)
        (\tz _k _a _ts _fs _tc -> tz)
        (\tz _k _a _ts _tc -> tz)
        (\tz _t _c _ts _fs _a _tc  -> tz)
        (\tz _t _g _ts _fs _a _tc  -> tz)
        (\tz _k _a _ts _fs _tc _tm -> tz)
        pd

      fromAssignment :: (Assignment -> a) -> a -> PageData -> a
      fromAssignment f d pd = maybe d f (get pd) where
        get (PD_Assignment _ _ a _ _ _)           = Just a
        get (PD_Assignment_Preview _ _ a _ _ _ _) = Just a
        get (PD_ViewAssignment _ _ a _ _)     = Just a
        get (PD_Course_Preview _ _ _ _ _ a _) = Just a
        get (PD_Group_Preview  _ _ _ _ _ a _) = Just a
        get _ = Nothing

      testScriptSelection :: (Translation String -> String) -> PageData -> H.Html
      testScriptSelection msg = pageDataCata
        (\_tz _t _c tsType _fs -> scriptSelection tsType)
        (\_tz _t _g tsType _fs -> scriptSelection tsType)
        (\_tz _k _a tsType _fs mts -> modificationScriptSelection tsType mts)
        (const5 (return ()))
        (\_tz _t _c tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _t _g tsType _fs _a tc  -> scriptSelectionPreview tsType tc)
        (\_tz _k _a tsType _fs mts tm -> modificationScriptSelectionPreview tsType mts tm)
        where
          testScriptField :: (IsString s) => s
          testScriptField = {-fromString $-} fieldName assignmentTestScriptField

          scriptSelection ts = maybe
            (return ())
            tsSelection
            ts

          tsSelection ts = do
            H.label ! for testScriptField $ fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            selectionWithDefAndAttr
              testScriptField
              [class_ "combobox form-control", A.style "display:none"]
              (const False)
              (map keyValue (Nothing:map Just ts))

          scriptSelectionPreview ts tcp = case tcp of
            (Just Nothing    , _, _) -> scriptSelection ts
            (Just (Just tsk) , _, _) -> preview ts tsk
            _ -> return ()
            where
              preview ts tsk = maybe (return ()) (tsSelectionPreview tsk) ts

          tsSelectionPreview tsk ts = do
            H.label ! for testScriptField $ fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            selectionWithDefAndAttr
              testScriptField
              [class_ "combobox form-control", A.style "display:none"]
              ((Just tsk)==)
              (map keyValue (Nothing:map Just ts))

          modificationScriptSelection ts mts = maybe
            (return ())
            (mtsSelection mts)
            ts

          mtsSelection mts ts = do
            H.label ! for testScriptField $ fromString . msg $ Msg_NewAssignment_TestScripts "Tester"
            selectionWithDefAndAttr
              testScriptField
              [class_ "combobox form-control", A.style "display:none"]
              (def mts)
              (map keyValue (Nothing:map Just ts))
            where
              def Nothing Nothing = True
              def Nothing _       = False
              def (Just (_,_,tsk)) (Just tsk') = tsk == tsk'
              def _                _           = False

          modificationScriptSelectionPreview ts _mts tm =
            case (tcmpTestScriptKey tm, ts) of
              (Just Nothing   , Just ts') -> mtsSelection' Nothing ts'
              (Just (Just tsk), Just ts') -> mtsSelection' (Just tsk) ts'
              _                           -> return ()
            where
              mtsSelection' tsk ts = do
                H.label ! for testScriptField $ fromString . msg $ Msg_NewAssignment_TestScripts "Test scripts"
                selectionWithDefAndAttr
                  testScriptField
                  [class_ "combobox form-control", A.style "display:none"]
                  (def tsk)
                  (map keyValue (Nothing:map Just ts))
                where
                  def Nothing Nothing = True
                  def Nothing _       = False
                  def (Just tsk) (Just tsk') = tsk == tsk'
                  def _                _     = False


          keyValue :: Maybe (TestScriptKey, TestScriptInfo) -> (Maybe TestScriptKey, String)
          keyValue Nothing = (Nothing, msg $ Msg_NewAssignment_NoTesting "Assignment without testing")
          keyValue (Just (testScriptKey, tsInfo)) = ((Just testScriptKey), tsiName tsInfo)

          nothing = Nothing :: Maybe TestScriptKey

      -- Test Case Ares

      testCaseArea msg = pageDataCata
        (\_tz _t _c tsType fs -> createTestCaseArea fs tsType)
        (\_tz _t _g tsType fs -> createTestCaseArea fs tsType)
        (\_tz _k _a tsType fs tc -> overwriteTestCaseArea fs tsType tc)
        (\_tz _k _a ts tc -> viewTestCaseArea ts tc)
        (\_tz _t _c tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _t _g tsType fs _a tc -> createTestCaseAreaPreview fs tsType tc)
        (\_tz _k _a tsType fs tc tm -> overwriteTestCaseAreaPreview fs tsType tc tm)
        where
          textArea val = do
            H.label ! for (fromString $ fieldName assignmentTestCaseField)
                    $ fromString $ msg $ Msg_NewAssignment_TestCase "Test cases"
            editOrReadOnly pd $ textAreaInput (fieldName assignmentTestCaseField) val
                                  ! class_ "form-control" ! rows "20"

          createTestCaseAreaPreview fs ts tcp = case tcp of
            (Just Nothing , Nothing, Nothing) -> createTestCaseArea fs ts
            (Just _       , Just uf, Nothing) -> userFileSelection uf
            (Just _       , Nothing,  Just f) -> textAreaPreview f
            _ -> return ()
            where
              userFileSelection uf = do
                H.label ! for (fromString $ fieldName assignmentUsersFileField)
                        $ fromString $ msg $ Msg_NewAssignment_TestFile "Test File"
                selectionWithDefault (fieldName assignmentUsersFileField) uf (map keyValue fs)
                H.p ! class_ "help-block" $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFileCata id))

              textAreaPreview f = textArea (Just f)

          createTestCaseArea fs ts = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where
              selectionOrTextArea = testScriptTypeCata
                (textArea Nothing)
                usersFileSelection

              usersFileSelection = do
                H.label ! for (fromString $ fieldName assignmentUsersFileField)
                        $ fromString $ msg $ Msg_NewAssignment_TestFile "Test File"
                selection (fieldName assignmentUsersFileField) (map keyValue fs)
                H.p ! class_ "help-block" $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue = (id &&& (usersFileCata id))

          testCaseText Nothing = Nothing
          testCaseText (Just (_,tc,_)) = withTestCaseValue (tcValue tc) Just (const Nothing)

          testCaseFileName Nothing = return ()
          testCaseFileName (Just (_,tc',_)) = fromString $ tcInfo tc'

          viewTestCaseArea ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType'' ts)
            where
              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc))
                (usersFile)

              usersFile = do
                H.b $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc

          overwriteTestCaseAreaPreview fs ts tc tm = maybe
            (return ())
            (selectionOrTextAreaPreview)
            (testScriptType' ts)
            where

              selectionOrTextAreaPreview = testScriptTypeCata
                (textArea (tcmpTextTestCase tm)) -- simple
                (maybe (return ()) userFileSelectionPreview (tcmpFileTestCase tm)) -- zipped

              userFileSelectionPreview uf = do
                H.label ! for (fromString $ fieldName assignmentUsersFileField) $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc
                selectionWithDefault (fieldName assignmentUsersFileField) uf (map keyValue ((Left ()):map Right fs))
                H.p ! class_ "help-block" $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFileCata id uf)


          overwriteTestCaseArea fs ts tc = maybe
            (return ())
            (selectionOrTextArea)
            (testScriptType' ts)
            where

              selectionOrTextArea = testScriptTypeCata
                (textArea (testCaseText tc)) -- simple
                usersFileSelection           -- zipped

              usersFileSelection = do
                H.label ! for (fromString (fieldName assignmentUsersFileField)) $ fromString . msg $ Msg_NewAssignment_TestFile "Test File"
                H.pre $ testCaseFileName tc
                selection (fieldName assignmentUsersFileField) (map keyValue ((Left ()):map Right fs))
                H.p ! class_ "help-block" $ fromString $ printf (msg $ Msg_NewAssignment_TestFile_Info
                  "A file passed to the tester (containing the test data) may be set here.  Files may be added on the \"%s\" subpage.")
                  (msg $ Msg_LinkText_UploadFile "Upload File")
                H.div ! A.id "menu" $ H.ul $ i18n msg $ linkToPageBlank uploadFile
                where
                  keyValue l@(Left ()) = (l, msg $ Msg_NewAssignment_DoNotOverwrite "No changes")
                  keyValue r@(Right uf) = (r, usersFileCata id uf)

      testScriptType' Nothing   = Nothing
      testScriptType' (Just []) = Nothing
      testScriptType' (Just ((_tk,tsi):_)) = Just $ tsiType tsi

      testScriptType'' = fmap tsiType

      pagePreview :: PageData -> PageDesc
      pagePreview = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase -> modifyAssignmentPreview key)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignmentPreview key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignmentPreview key)
        (\_timezone key _asg _tsType _files _testcase _tc -> modifyAssignmentPreview key)

      page :: PageData -> PageDesc
      page = pageDataCata
        (\_tz _t (key,_course) _tsType _fs -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs -> newGroupAssignment key)
        (\_tz ak _asg _tsType _files _testcase -> modifyAssignment ak)
        (\_tz k _a _ts _tc -> viewAssignment k)
        (\_tz _t (key,_course) _tsType _fs _a _tc -> newCourseAssignment key)
        (\_tz _t (key,_group)  _tsType _fs _a _tc -> newGroupAssignment key)
        (\_tz k _a _fs _ts _tc _tm -> modifyAssignment k)


      -- Converts a given value to a string that represents a JSON acceptable string
      -- for the Fay client side
      toFayJSON = BsLazy.unpack . Aeson.encode . showToFay

      showEvaluationType msg = H.div . evConfigCata
        (fromString . msg $ Msg_NewAssignment_BinaryEvaluation "Binary Evaluation")
        (\p -> do fromString . msg $ Msg_NewAssignment_PercentageEvaluation "Pass Limit: "
                  fromString ((show $ pct p) ++ " %"))
          where
            pct :: Double -> Int
            pct = floor . (100 *)

      -- Pages constants

      newCourseAssignment k = Pages.newCourseAssignment k ()
      newGroupAssignment k  = Pages.newGroupAssignment k ()
      modifyAssignment k    = Pages.modifyAssignment k ()
      newCourseAssignmentPreview k = Pages.newCourseAssignmentPreview k ()
      newGroupAssignmentPreview k  = Pages.newGroupAssignmentPreview k ()
      modifyAssignmentPreview k    = Pages.modifyAssignmentPreview k ()
      viewAssignment k = Pages.viewAssignment k ()
      uploadFile = Pages.uploadFile ()
