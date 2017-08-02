{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetSubmission (
    getSubmission
  ) where

import           Control.Monad.Trans (lift)
import           Data.String (fromString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)

import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content

getSubmission :: DataHandler
getSubmission = DataHandler $ do
  sk <- getParameter submissionKeyPrm
  (s, description) <- userStory (Story.getSubmission sk)
  let submission = solution s
  let basename = concat [uid id $ eUid description, "_", submissionKeyMap id sk]
  let fname = submissionValue (const (++ ".txt")) (const (++ ".zip")) submission basename
  lift . modifyResponse $
    setHeader "Content-Disposition" (fromString $ concat ["attachment; filename=\"",fname,"\""])
  lift $ submissionValue downloadPlain downloadZipped submission
  where
    downloadPlain :: MonadSnap m => String -> m ()
    downloadPlain text = do
      modifyResponse
        $ setHeader "Content-Type" "text/plain; charset=\"UTF-8\""
      writeBS (BsUTF8.fromString text)

    downloadZipped :: MonadSnap m => B.ByteString -> m ()
    downloadZipped zip = do
      modifyResponse
        $ setHeader "Content-Type" "application/zip, application/octet-stream"
      writeBS zip
