{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Snap.Content (
    Content(..)
  , I18N
  , (<$>)
  , emptyContent
  , getContentHandler
  , getPostContentHandler
  , postContentHandler
  , mkContent
  , blaze
  , getParamE
  , setInSessionE
  , setReqParamInSession
  , routeOf
  , routeWithParams
  , runStory
  , runStoryE
  , i18nE
  , blazeI18n
  , withUserState
  , withUserStateE
  , withUserStateAndFrame
  , GETContentHandler
  , POSTContentHandler
  , HandlerError
  , UserState(..)
  , ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , Html
  , module Snap
  , module Data.ByteString.Char8

  , module Bead.Domain.Entities
  , module Bead.Domain.Relationships
  , module Bead.View.UserActions
  , module Bead.View.Snap.Application
  , module Bead.View.Snap.Pagelets
  , module Bead.View.Snap.InputHandlers
  , module Bead.View.Snap.TemplateAndComponentNames
  ) where

import Snap hiding (empty, get, route, (<$>))
import Snap.Blaze (blaze)
import Data.ByteString.Char8 (ByteString, unpack)

import Bead.Controller.Pages as P
import Bead.Controller.ServiceContext (UserState(..))
import Bead.Domain.Entities
import Bead.Domain.Relationships
import Bead.View.UserActions
import Bead.View.Snap.Application (App)
import Bead.View.Snap.Dictionary (I18N)
import Bead.View.Snap.Pagelets hiding (invariants)
import Bead.View.Snap.RouteOf
import Bead.View.Snap.HandlerUtils
import Bead.View.Snap.RequestParams
import Bead.View.Snap.InputHandlers
import Bead.View.Snap.TemplateAndComponentNames
import Text.Blaze.Html5 (Html)

import Control.Applicative ((<$>))
import Control.Monad.Error

-- Pages have the following structure. A header, a context-sensitive menu,
-- a footer, and the content area. Every content area has its GET and POST handlers, its
-- page type. The common Html templates can be found in the Pagelet module

type GETContentHandler  = HandlerError App App ()
type POSTContentHandler = HandlerError App App UserAction

-- | Content Pages are rendered in content area.
data Content = Content {
    get   :: Maybe GETContentHandler
  , post  :: Maybe POSTContentHandler
  }

emptyContent :: Content
emptyContent = Content {
    get   = Nothing
  , post  = Nothing
  }

getContentHandler     g   = emptyContent { get = Just g }
postContentHandler    p   = emptyContent { post = Just p }
getPostContentHandler g p = emptyContent { get = Just g, post = Just p }

mkContent
  :: Maybe GETContentHandler
  -> Maybe POSTContentHandler
  -> Content
mkContent g p = Content { get = g, post = p }

withUserStateAndFrame :: (UserState -> Html) -> HandlerError App App ()
withUserStateAndFrame f = withUserStateE $ \state ->
  lift . blaze $ withUserFrame state (f state)

