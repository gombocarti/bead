module Bead.View.Content.Rest.Group (
    usersInGroup
  ) where

import qualified Bead.Domain.Entities as E
import           Bead.View.Content (getParameter, RestViewHandler(RestViewHandler), userStory, customGroupKeyPrm)
import           Bead.View.RequestParams (groupKeyParamName)
import qualified Bead.Controller.UserStories as S

import qualified Data.Aeson as Aeson

usersInGroup :: RestViewHandler
usersInGroup = RestViewHandler $ do
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  users <- userStory $ do
    us <- S.subscribedToGroup gk
    mapM (fmap E.u_uid . S.loadUser) us
  return $ Aeson.toEncoding $ map (E.uid id) users
