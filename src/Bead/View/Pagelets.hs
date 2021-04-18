{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.Pagelets where

import           Prelude hiding (span)

import           Control.Monad (when, forM_)
import           Data.Char (isAlphaNum)
import           Data.Data
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Data.String (IsString(..), fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock

import qualified Text.Blaze as B
import           Text.Blaze.Html5 hiding (link, option, map)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html5.Attributes hiding (id, span)
import           Text.Pandoc.Highlighting (styleToCss, pygments)


import qualified Bead.Controller.UserStories as S
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext as SC (UserState, getStatus, clearStatus, uid, fullNameInState)
import           Bead.Domain.Entities as Entity (statusMessage, uid, PageSettings(needsLatex, needsSyntaxHighlight), Course, Group, shortCourseName, shortGroupName, isAdmin)
import           Bead.Domain.Relationships (CourseKey, GroupKey)
import           Bead.View.BeadContext (BeadHandler')
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.ContentHandler (ContentHandler, i18nH, i18nE, changeUserState, userStory, pageSettings, userState, HtmlPage, pageTitle, pageBody)
import           Bead.View.Fay.Hooks
import           Bead.View.Fay.JSON.ServerSide
import qualified Bead.View.I18N as I18N
import           Bead.View.I18N (IHtml, translate, getI18N, i18n)
import           Bead.View.RouteOf
import           Bead.View.TemplateAndComponentNames
import           Bead.View.Translation

-- * Definitions

css :: String -> Html
css c = H.link ! A.type_ "text/css" ! A.href (fromString c) ! A.rel "stylesheet"

js :: String -> Html
js j = H.script ! A.src (fromString j) $ mempty

bootStrapDocument :: Entity.PageSettings -> IHtml -> IHtml
bootStrapDocument settings body' = do
  body <- body'
  return $ do
    docType
    H.html $ do
      H.head $ do
        H.meta ! A.charset "utf-8"
        H.title "BE-AD Assignment Management System"
        H.link ! A.rel "shortcut icon" ! A.href "icon.ico"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        H.meta ! A.name "description" ! A.content ""
        H.meta ! A.name "author" ! A.content ""
        js "/jquery.js"
        css "/jquery-ui.css"
        js "/jquery-ui.js"
        js "/moment.js"
        css "/bead.css"
        css "/bootstrap.min.css"
        css "/bootstrap.custombutton.css"
        js "/bootstrap.min.js"
        css "/bootstrap-datetimepicker.min.css"
        js "/bootstrap-datetimepicker.min.js"
        when (needsSyntaxHighlight settings) $
          css ('/' : snd syntaxHighlightCss)
        when (needsLatex settings) $ do
          css "/katex/katex.min.css"
          js  "/katex/katex.min.js"
          css "/katex/contrib/copy-tex.min.css"
          js  "/katex/contrib/copy-tex.min.js"
          H.script $ fromString 
            "document.addEventListener(\"DOMContentLoaded\", function () {\n  var mathElements = document.getElementsByClassName(\"math\");\n  for (var i = 0; i < mathElements.length; i++) {\n    var texText = mathElements[i].firstChild;\n    if (mathElements[i].tagName == \"SPAN\") { katex.render(texText.data, mathElements[i], { displayMode: mathElements[i].classList.contains(\"display\"), throwOnError: false } );\n  }}});"
      H.body $ body

-- | Renders a Page with the given IHtml contents.
--   Implicit 'UserState' and 'PageSettings' influences the result.
bootstrapPage :: HtmlPage -> ContentHandler Html
bootstrapPage contents = do
  state <- userState
  settings <- pageSettings
  notifs <- userStory S.noOfUnseenNotifications
  nav <- userStory $ do
    subscribedGroups <- S.subscribedGroups
    grps <- S.administratedGroups
    courses <- S.administratedCourses
    return $ navigation state subscribedGroups grps courses
  changeUserState SC.clearStatus
  i18nE >>= (return . runBootstrapPage settings (bootstrapUserFrame state contents nav notifs))

  where
    navigation :: UserState -> [(CourseKey, Course, GroupKey, Group)] -> [(CourseKey, Course, [(GroupKey, Group)])] -> [(CourseKey, Course)] -> IHtml
    navigation state subscribedCourses adminedGroups adminedCourses
      | isAdmin state = do
          msg <- getI18N
          return $ nav [i18n msg administration]
      | otherwise = do
          msg <- getI18N
          return $ do
            when (not . null $ subscribedCourses) $
              Bootstrap.panel (Just $ B.toMarkup $ msg $ msg_Navigation_RegisteredCourses "Registered courses") $
                forM_ subscribedCourses $ \(_courseKey, course, groupKey, group_) -> do
                  H.p $ B.toMarkup $ shortCourseName course
                  nav [studentView groupKey group_]
            nav [i18n msg groupRegistration]
            when (not . null $ adminedGroups) $
              Bootstrap.panel (Just $ B.toMarkup $ msg $ msg_Navigation_AdminedGroups "Groups") $
                forM_ adminedGroups $ \(courseKey, course, groups) -> do
                  H.p $ courseAssignments courseKey course
                  nav $ map groupNavigationLink groups
            when (not . null $ adminedCourses) $
              Bootstrap.panel (Just $ B.toMarkup $ msg $ msg_Navigation_AdminedCourses "Courses") $
                nav $ map courseNavigationLink adminedCourses

      where
        nav :: [Html] -> Html
        nav items = H.ul ! A.class_ "nav nav-pills nav-stacked"
                    $ mapM_ H.li $ items

        groupRegistration :: IHtml
        groupRegistration = linkToPage P.groupRegistrationWithText

        studentView :: GroupKey -> Group -> Html
        studentView gk g = Bootstrap.link (routeOf $ P.studentView gk ()) (shortGroupName g)

        groupNavigationLink :: (GroupKey, Group) -> Html
        groupNavigationLink (gk, g) = Bootstrap.link (routeOf $ P.groupOverview gk ()) (shortGroupName g)

        courseNavigationLink :: (CourseKey, Course) -> Html
        courseNavigationLink (ck, c) = Bootstrap.link (routeOf $ P.courseManagement ck P.GroupManagementContents ()) (shortCourseName c)

        courseAssignments :: CourseKey -> Course -> Html
        courseAssignments ck c = Bootstrap.link (routeOf $ P.courseManagement ck P.AssignmentsContents ()) (shortCourseName c)

        administration :: IHtml
        administration = linkToPage P.administrationWithText

-- | Translates a public page selecting the I18N translation based on
--   the language stored in the cookie, if there is no such value, the
--   accept-language field is used, if there is no such value then
--   default translator function is used.
bootstrapPublicPage :: PageSettings -> HtmlPage -> BeadHandler' v Html
bootstrapPublicPage settings p = do
  translator <- i18nH
  return $ runBootstrapPage settings (publicFrame p) translator

runBootstrapPage :: Entity.PageSettings -> IHtml -> I18N -> Html
runBootstrapPage settings p i = translate i $ bootStrapDocument settings p

bootstrapUserFrame :: UserState -> HtmlPage -> IHtml -> Int -> IHtml
bootstrapUserFrame s contents sidebar newNotifs = do
  header <- bootstrapHeader s newNotifs
  bar <- sidebar
  title <- pageTitle contents
  body <- pageBody contents
  status <- bootstrapStatus s
  msg <- getI18N
  return $ do
    header
    Bootstrap.containerFullWidth $ do
      Bootstrap.rowColMd12 $ hr
      Bootstrap.rowCol9Offset3 title
      Bootstrap.row $ do
        Bootstrap.colMd3 bar
        Bootstrap.colMd9 body
      Bootstrap.rowColMd12 $ hr
      status

-- | Places a given content in a public frame
publicFrame :: HtmlPage -> IHtml
publicFrame content = do
  header <- publicHeader
  title <- pageTitle content
  body <- pageBody content
  return $ do
    header
    Bootstrap.containerFullWidth $ title <> body

syntaxHighlightCss :: (String, FilePath)
syntaxHighlightCss = (styleToCss pygments, "syntax-highlight.css")

-- * Basic building blocks

conditional :: Bool -> Html -> Html -> Html
conditional True _ visible = visible
conditional False text _   = text

nonEmpty :: [o] -> Html -> Html -> Html
nonEmpty os = conditional (not . null $ os)

-- * Input fields

hiddenInput :: (ToValue a, ToValue b) => a -> b -> Html
hiddenInput name value =
  H.input ! A.type_ "hidden"
          ! A.id (B.toValue name)
          ! A.name (B.toValue name)
          ! A.value (B.toValue value)

optionalFileInput :: Text -> Html
optionalFileInput name =
  H.input ! A.type_ "file"
          ! A.id (B.toValue name)
          ! A.name (B.toValue name)

fileInput :: Text -> Html
fileInput name = optionalFileInput name ! A.required ""

copyToClipboardButton :: I18N -> String -> Html
copyToClipboardButton msg ident = Bootstrap.buttonOnClick "" (msg $ msg_CopyToClipboard "Copy to Clipboard") action
  where
    action :: String
    action = "navigator.clipboard.writeText(document.getElementById('" ++ ident ++ "').textContent);"

-- Creates a number input with the given minimum and maximum, if the value is given
-- set as the default value
numberInput :: Text -> Maybe Int -> Maybe Int -> Maybe Int -> Html
numberInput name min_ max_ val_ = do
  let val x = maybe (! (A.value x)) (\y -> (! (A.value . fromString $ show y))) actValue
  let mn = maybe id (\m -> (! (A.min $ fromString $ show m))) min_
  let mx = maybe id (\m -> (! (A.max $ fromString $ show m))) max_
  val "" $ mn $ mx $ H.input ! A.type_ "number" ! A.name (B.toValue name)
  where
    actValue = case (min_, max_, val_) of
      (Just min, Just max, Just val) -> Just $ if and [min <= val, val <= max] then val else min
      (Just min, Nothing, Just val)  -> Just $ if min < val then val else min
      (Nothing, Just max, Just val)  -> Just $ if val < max then val else max
      (Just min, Nothing, Nothing)   -> Just $ min
      (Nothing, Just max, Nothing)   -> Just $ max
      _                              -> Nothing

submitButton :: String -> String -> Html
submitButton i t = H.input ! A.id (fromString i) ! A.type_ "submit" ! A.value (fromString t)

submitButtonDanger :: (ToValue a, ToValue b) => a -> b -> Html
submitButtonDanger i t =
  H.input ! A.id (B.toValue i)
          ! A.type_ "submit"
          ! A.class_ "btn btn-danger"
          ! A.value (B.toValue t)

checkBox :: Text -> String -> Bool -> Html
checkBox n v c =
  H.input ! A.name (B.toValue n)
          ! A.type_ "checkbox"
          ! A.value (B.toValue v)
          !? (c, A.checked "")

checkBox' :: (Show a, Data a) => Text -> Bool -> a -> String -> Html
checkBox' name checked value text = do
  checkBox name (encodeToFay' "checkBox" value) checked
  fromString text

checkBoxRO :: (Show a, Data a) => Text -> Bool -> Bool -> a -> Text -> Html
checkBoxRO name checked readonly value text = do
  checkBox name (encodeToFay' "checkBox" value) checked
    !? (readonly, A.disabled "")
  (B.toMarkup text)

withId :: (Html -> Html) -> String -> (Html -> Html)
withId f i = (f ! A.id (fromString i))

required h = h ! A.required ""

-- * Form

postForm :: Text -> Html -> Html
postForm action = H.form ! A.method "post" ! A.action (B.toValue action) ! A.acceptCharset "UTF-8"

getForm :: Text -> Html -> Html
getForm action = H.form ! A.method "get" ! A.action (B.toValue action) ! A.acceptCharset "UTF-8"

-- Returns a string which contains only alphanum caracters
jsFunctionName :: Text -> Text
jsFunctionName = T.filter isAlphaNum

-- Creates an HTML div which represents a countdown timer, showing
-- the time left in days hours:min:secs format from the given
-- now time beetwen the given until time.
startEndCountdownDiv :: Text -> Text -> Text -> UTCTime -> UTCTime -> Html
startEndCountdownDiv divId daystr overstr now until =
  countdownDiv divId daystr overstr True (floor $ diffUTCTime until now)

-- Creates an HTML div which represents a countdown timer, showing
-- the ETA time in days hours:min:secs format from the given
-- now time in seconds.
countdownDiv :: Text -> Text -> Text -> Bool -> Int -> Html
countdownDiv divId daystr overstr showDays seconds = do
  H.a ! A.id (B.toValue divId) $ B.toMarkup $
    if showDays
         then T.concat ["-", daystr, " --:--:--"]
         else "--:--"
  H.script $ B.toMarkup countdown
  where
    fname = jsFunctionName (divId <> "countdown")

    countdown = T.concat
      [ fname, "();"
      , "function ", fname, "() {"
      ,    "var minsecs = 60;"
      ,    "var hoursecs = minsecs * 60;"
      ,    "var daysecs = hoursecs* 24;"
      ,    "var mstime = ", T.pack $ show seconds, " * 1000;"
      ,    "var timestamp = new Date;"
      ,    "var interval = setInterval(function() {"
      ,       "var el = document.getElementById(\"", divId, "\");"
      ,       "var now = new Date;"
      ,       "var dt = now - timestamp;"
      ,       "timestamp = now;"
      ,       "mstime = mstime - dt;"
      ,       "var time = Math.round( mstime / 1000 );"
      ,       "if(time < 0) {"
      ,           "el.innerHTML = \"", overstr, "\";"
      ,           "clearInterval(interval);"
      ,           "return;"
      ,       "}"
      ,       "var edays   = Math.floor( time / daysecs );"
      ,       "var ehours1 = time % daysecs;"
      ,       "var ehours  = Math.floor( ehours1 / hoursecs );"
      ,       "if (ehours < 10) ehours = \"0\" + ehours;"
      ,       "var emins1  = ehours1 % hoursecs;"
      ,       "var emins   = Math.floor( emins1 / minsecs );"
      ,       "if (emins < 10) emins = \"0\" + emins;"
      ,       "var esecs   = emins1 % minsecs;"
      ,       "if (esecs < 10) esecs = \"0\" + esecs;"
      ,       if showDays
                  then T.concat
                     [ "var text=\"\";"
                     , "if(edays == 0) {"
                     ,    "if(ehours == 0) {"
                     ,       "text = emins + ':' + esecs;"
                     ,    "} else {"
                     ,       "text = ehours + ':' + emins + ':' + esecs;"
                     ,    "}"
                     , "} else {"
                     ,    "text = edays + \" ", daystr, " \" + ehours + ':' + emins + ':' + esecs;"
                     , "}"
                     ]
                  else "var text = emins + ':' + esecs;"
      ,       "el.innerHTML = text;"
      ,       "}, 1000);"
      , "}"
      ]

linkToPage :: P.Page' Translation -> IHtml
linkToPage p = do
  msg <- getI18N
  return $ Bootstrap.link (routeOf p) (msg $ P.pageValue p) ! A.id (B.toValue $ fieldName p)

linkToPageWithPostfix :: P.Page' Translation -> Text -> IHtml
linkToPageWithPostfix p s = do
  msg <- getI18N
  return $ Bootstrap.link (routeOf p) (msg (P.pageValue p) <> s) ! A.id (B.toValue $ fieldName p)

linkButtonToPageBS :: P.Page' Translation -> IHtml
linkButtonToPageBS p = do
  msg <- getI18N
  return $ Bootstrap.buttonLink (routeOf p) (msg $ P.pageValue p)

linkToPageBlank :: P.Page' Translation -> IHtml
linkToPageBlank p = do
  msg <- getI18N
  return $ Bootstrap.link (routeOf p) (msg $ P.pageValue p) ! A.target "_blank" ! A.id (B.toValue $ fieldName p)

-- Produces a HTML-link with the given route text and title
linkWithTitle :: String -> String -> String -> Html
linkWithTitle route title text =
  Bootstrap.link route text
    ! A.title (fromString title)

-- Html text in span tag with title attribute
spanWithTitle :: String -> String -> Html
spanWithTitle title text = H.span ! A.title (fromString title) $ fromString text

publicHeader :: IHtml
publicHeader = do
  msg <- getI18N
  return $ do
    H.div ! class_ "navbar navbar-default" $ do
      H.style ".body{padding-top:70px}"
      H.div ! class_ "container" $ do
        H.div ! class_ "navbar-header" $ do
         span ! class_ "navbar-brand" $ "BE-AD"

bootstrapHeader :: UserState -> Int -> IHtml
bootstrapHeader s newNotifs = do
  msg <- getI18N
  return $ do
        H.div ! class_ "navbar navbar-default navbar-fixed-top" $ do
            H.style ".body{padding-top:70px}"
            Bootstrap.containerFullWidth $ do
                H.div ! class_ "navbar-header" $ do
                    Bootstrap.link (routeOf home) ("BE-AD" :: Html) ! class_ "navbar-brand"
                    button ! type_ "button" ! class_ "navbar-toggle" ! dataAttribute "toggle" "collapse" ! dataAttribute "target" ".navbar-collapse" $ do
                        H.span ! class_ "sr-only" $ "Toggle navigation"
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                        H.span ! class_ "icon-bar" $ mempty
                H.div ! class_ "collapse navbar-collapse navbar-ex1-collapse" $ do
                    ul ! class_ "nav navbar-nav navbar-right" $ do
                        li $ H.a userId
                        li $ do (I18N.i18n msg $
                                    if newNotifs > 0
                                        then linkToPageWithPostfix notifications (T.concat [" (", T.pack $ show newNotifs, ")"])
                                        else linkToPage notifications)
                        li $ (I18N.i18n msg $ linkToPage profile)
                        li $ (I18N.i18n msg $ linkToPage logout)
  where
    logout = P.logoutWithText
    profile = P.profileWithText
    home = P.welcomeWithText
    userId = fromString $ concat [SC.fullNameInState s, " / ", Entity.uid id . SC.uid $ s]
    notifications = P.notificationsWithText

bootstrapStatus :: UserState -> IHtml
bootstrapStatus = maybe noMessage message . getStatus
  where
    noMessage = return $ return ()
    message m = do
      msg <- getI18N
      let labelMessage = statusMessage
            (Bootstrap.fadeOutFooterWarningButton "title" . msg)
            (Bootstrap.fadeOutFooterDangerButton "title" . msg)
            m
      return $ do
        Bootstrap.footer
          $ Bootstrap.container $ Bootstrap.rowColMd12 $ Bootstrap.buttonGroupJustified $ labelMessage
        Bootstrap.fadeOutFooter 30

-- * Picklist

option :: String -> String -> Bool -> Html
option value text False = H.option ! A.value (fromString value)                 $ fromString text
option value text True  = H.option ! A.value (fromString value) ! A.selected "" $ fromString text

selection' :: String -> Html -> Html
selection' name =
    H.select ! A.id (fromString name)
             ! A.name (fromString name)
             ! A.required ""

-- Encodes the value to Fay JSON representation or throw an error for the given name
encodeToFay' :: (Data a, Show a, IsString s) => String -> a -> s
encodeToFay' name value = fromString $ fromMaybe (name ++ ": error encoding value") (encodeToFay value)

selection :: (Show a, Data a) => String -> [(a,String)] -> Html
selection name = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t False where

selectionWithDefault :: (Eq a, Show a, Data a) => String -> a -> [(a,String)] -> Html
selectionWithDefault name def = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (v == def) where

selectionWithDefault' :: (Show a, Data a) => String -> (a -> Bool) -> [(a, String)] -> Html
selectionWithDefault' name def = selection' name . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (def v) where

selectionWithDefAndAttr :: (Show a, Data a) =>
  String -> [Attribute] -> (a -> Bool) -> [(a, String)] -> Html
selectionWithDefAndAttr name attrs def = foldl (!) (selection' name) attrs . mapM_ option'
  where
    option' (v,t) = option (encodeToFay' "selection" v) t (def v) where

evalSelectionDiv :: EvaluationHook -> Html
evalSelectionDiv h = ((H.div `withId` (evSelectionDivId h)) $ mempty)

-- * Radio buttons

-- One radiobutton for the json encodeable value with the given parameter name
radioButton :: (Show a, Data a) => String -> (a,String) -> Html
radioButton name (value,text) = do
  H.input ! A.type_ "radio" ! A.name (fromString name) ! A.value (encodeToFay' "radioButton" value)
  fromString text

-- Radio buttons placed horizontally for the given parameter name and values
horizontalRadioButtons :: (Show a, Data a) => String -> [(a, String)] -> Html
horizontalRadioButtons name values = mapM_ (radioButton name) values

-- Radio buttons placed horizontally for the given parameter name and values, checked the default nth
horizontalRadioButtonsDef :: (Show a, Data a) => String -> Int -> [(a, String)] -> Html
horizontalRadioButtonsDef name nth values = mapM_ (radioButton' name) (values `zip` [0..])
  where
    radioButton' n (v,i) =
      (if i == nth then (! A.checked "") else id) $ radioButton n v

-- Radio buttons placed vertically for the given parameter name and values
verticalRadioButtons :: (Show a, Data a) => String -> [(a, String)] -> Html
verticalRadioButtons name values = mapM_ button values
  where
    button v = do { radioButton name v; H.br }
