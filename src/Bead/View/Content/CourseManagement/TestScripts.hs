{-# LANGUAGE OverloadedStrings #-}

module Bead.View.Content.CourseManagement.TestScripts (
    testScriptsPage
  ) where

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Relationships (CourseKey, TestScriptKey, TestScriptInfo, tsiName)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content (ViewHandler(ViewHandler), setPageContents, GETContentHandler, getParameter)
import           Bead.View.ContentHandler (userStory, ContentHandler)
import           Bead.View.DataBridge (customCourseKeyPrm)
import           Bead.View.I18N (getI18N, i18n, IHtml)
import           Bead.View.Pagelets (linkButtonToPageBS)
import           Bead.View.RequestParams (courseKeyParamName)
import           Bead.View.RouteOf (routeOf)
import qualified Bead.View.Translation as Trans

import           Control.Monad (forM_)
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H

data PageData = PageData {
    pdCourse :: CourseKey
  , pdTestScripts :: [(TestScriptKey, TestScriptInfo)]
  }

testScriptsPage :: ContentHandler IHtml
testScriptsPage = do
  ck <- getParameter (customCourseKeyPrm courseKeyParamName)
  testScripts <- userStory $ S.testScriptInfosOfCourse ck
  return $ testScriptsContents $ PageData {
      pdCourse = ck
    , pdTestScripts = testScripts
    }

testScriptsContents :: PageData -> IHtml
testScriptsContents pd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      i18n msg $ linkButtonToPageBS (Pages.newTestScriptWithText (pdCourse pd))
      case (pdTestScripts pd) of
        []  -> H.p $ B.text $ msg $ Trans.msg_TestScripts_NoTestScriptsWereDefined "There are no testers for the course."
        tScripts -> Bootstrap.unorderedListGroup $ forM_ tScripts $ \(tsk, tsi) ->
          Bootstrap.listGroupLinkItem
            (routeOf (Pages.courseManagement (pdCourse pd) (Pages.ModifyTestScriptContents tsk) ()))
            (B.toMarkup $ tsiName tsi)
