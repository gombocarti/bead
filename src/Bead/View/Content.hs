{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content (
    beadHandler
  , blaze
  , File
  , getParameter
  , getParameterWithDefault
  , getParameterValues
  , getOptionalParameter
  , getOptionalOrNonEmptyParameter
  , getJSONParam
  , getDictionaryInfos
  , redirectTo
  , routeOf
  , routeWithParams
  , routeWithOptionalParams
  , routeWithAnchor
  , downloadLazy
  , downloadStrict
  , downloadText
  , setPageContents
  , userStory
  , userState
  , i18nE
  , userTimeZone
  , userTimeZoneToLocalTimeConverter
  , userTimeZoneToUTCTimeConverter
  , foundTimeZones
  , withUserState
  , PageContents
  , GETContentHandler
  , POSTContentHandler
  , ViewPOSTContentHandler
  , ContentHandler
  , UserState(..)
  , userStateCata
  , usernameInState
  , RedirectionTarget
  , ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , UserTimeConverter

  , ViewHandler(..)
  , UserViewHandler(..)
  , ViewModifyHandler(..)
  , ModifyHandler(..)
  , DataHandler(..)
  , RestViewHandler(..)

  , PageHandler
  , viewHandlerCata
  , userViewHandlerCata
  , viewModifyHandlerCata
  , modifyHandlerCata
  , dataHandlerCata
  , restViewHandlerCata

  , module Snap
  , module Control.Applicative
  , module Data.ByteString.Char8
  , module Data.Monoid

  , module Bead.Domain.Entities
  , module Bead.Domain.Entity.Comment
  , module Bead.Domain.Func
  , module Bead.Domain.Relationships
  , module Bead.View.Anchor
  , module Bead.View.UserActions
  , module Bead.View.I18N
  , module Bead.View.Translation
  , module Bead.View.BeadContext
  , module Bead.View.Pagelets
  , module Bead.View.DataBridge
  , module Bead.View.TemplateAndComponentNames
  , module Bead.View.Fay.HookIds
  , module Bead.View.Fay.JSON.ServerSide
  ) where

import Control.Applicative hiding (empty)
import qualified Data.Aeson as Aeson
import Data.Monoid
import Data.Text (Text)

import Data.ByteString.Char8 (ByteString, unpack)
import qualified Data.ByteString.Lazy as L (ByteString)
import Snap hiding (Config(..), empty, get, route, (<$>), getCookie)
import Snap.Blaze (blaze)

import Bead.Controller.Pages
import qualified Bead.Controller.Pages as P
import Bead.Controller.ServiceContext hiding (uid)
import Bead.Domain.Entities hiding (name)
import Bead.Domain.Entity.Comment
import Bead.Domain.Func
import Bead.Domain.Relationships
import Bead.View.Anchor
import Bead.View.UserActions
import Bead.View.BeadContext hiding (getDictionaryInfos)
import Bead.View.ContentHandler
import Bead.View.DataBridge hiding (name)
import Bead.View.Fay.HookIds
import Bead.View.Fay.JSON.ServerSide
import Bead.View.I18N
import Bead.View.Translation
import Bead.View.TemplateAndComponentNames hiding (tcName)
import Bead.View.RouteOf
#ifdef TEST
import Bead.View.Pagelets hiding (invariants)
#else
import Bead.View.Pagelets
#endif

type RedirectionTarget = P.PageDesc

-- | Contents of a page is either contents of some other page
--   or some html.
type PageContents html = Either RedirectionTarget html

-- | A file to be downloaded consists of a filename, a mime info and
--   the means to write its contents into a response.
type File = (String, Mime, BeadHandler ())

downloadStrict :: String -> Mime -> ByteString -> ContentHandler File
downloadStrict filename mime contents = return (filename, mime, writeBS contents)

downloadLazy :: String -> Mime -> L.ByteString -> ContentHandler File
downloadLazy filename mime contents = return (filename, mime, writeLBS contents)

downloadText :: String -> Text -> ContentHandler File
downloadText filename contents = return (filename, MimePlainText, writeText contents)

-- Pages have the following structure. A header, a context-sensitive menu,
-- a footer, and the content area. Every content area has its GET and POST handlers, its
-- page type. The common Html templates can be found in the Pagelet module

type GETContentHandler  = ContentHandler (PageContents IHtml)
type POSTContentHandler = ContentHandler UserAction
type ViewPOSTContentHandler = ContentHandler (PageContents IHtml)
type RestViewContentHandler = ContentHandler Aeson.Encoding

setPageContents :: html -> ContentHandler (PageContents html)
setPageContents = return . Right

redirectTo :: RedirectionTarget -> ContentHandler (PageContents a)
redirectTo = return . Left

-- View Handler runs on a view page, that is generated by a GET request,
-- only the rendering of the page is the purpose of the handler.
newtype ViewHandler = ViewHandler { unViewHandler :: GETContentHandler }

viewHandlerCata f (ViewHandler x) = f x

-- User View Handler runs on a User View page, that is generated by a POST request,
-- only the rendering of the page is the purpose of the handler, but the
-- page content depends on the previous user input.
newtype UserViewHandler = UserViewHandler { unUserViewHandler :: GETContentHandler }

userViewHandlerCata f (UserViewHandler x) = f x

-- View Modify Handler runs on a GET request, which generates a page, and runs on the
-- corresponding POST request, which generates a UserAction, which will be interpreted
-- as an UserAction with a given UserStory.
data ViewModifyHandler = ViewModifyHandler GETContentHandler POSTContentHandler

viewModifyHandlerCata f (ViewModifyHandler x y) = f x y

-- Modify Handler runs only POST request that lously coupled to a GET generated page, which
-- should have multiple UserAction related, data modification pages.
newtype ModifyHandler = ModifyHandler { unModifyHandler :: POSTContentHandler }

modifyHandlerCata f (ModifyHandler x) = f x

-- Data Handler runs on GET requests and renders data from the actual user state and
-- from the persistence layer
newtype DataHandler = DataHandler { unDataHandler :: ContentHandler File }

dataHandlerCata f (DataHandler x) = f x

newtype RestViewHandler = RestViewHandler { unRestViewHandler :: RestViewContentHandler }

restViewHandlerCata f (RestViewHandler x) = f x

type PageHandler = Page ViewHandler UserViewHandler ViewModifyHandler ModifyHandler DataHandler RestViewHandler
