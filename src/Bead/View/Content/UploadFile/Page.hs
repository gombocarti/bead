{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.UploadFile.Page (
    uploadFile
  ) where

import           Control.Monad (forM)
import           Control.Monad.Trans (lift)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BUTF8

import           Snap.Util.FileUploads
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A (enctype)

import           Bead.Config (maxUploadSizeInKb)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.BeadContext
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap

data PageData = PageData [(UsersFile FilePath, FileInfo)] Int

pageDataCata f (PageData x s) = f x s

uploadFile = ViewModifyHandler getUploadFile postUploadFile

getUploadFile :: GETContentHandler
getUploadFile = do
  fs <- userStory Story.listUsersFiles
  size <- fmap maxUploadSizeInKb $ lift getConfiguration
  return $ uploadFileContent (PageData fs size)

data Success a
  = PolicyFailure String
  | UnnamedFile
  | StoryError String
  | Success a

successCata
  policyFailure
  unnamedFile
  storyError
  success
  s = case s of
    PolicyFailure msg -> policyFailure msg
    UnnamedFile       -> unnamedFile
    StoryError    msg -> storyError    msg
    Success       x   -> success x

isSuccess :: Success a -> Bool
isSuccess (Success _) = True
isSuccess _           = False

isFailure :: Success a -> Bool
isFailure = not . isSuccess

-- NOTE: This content handler a bit special case. As in out model, we use
-- the POST content handlers basically to compute the UserAction which will be interpreted
-- as UserStory, but the handleFileUploads works differently, as it deletes the uploaded files
-- after the given 'parts' handlers are run. In our model, this is not an option. In this special
-- case we run the saveUsersFile story manually.
postUploadFile :: POSTContentHandler
postUploadFile = do
  uploadResults <- lift $ do
    cfg <- getConfiguration
    let sizeLimit = (fromIntegral $ maxUploadSizeInKb cfg) * 1024
    let perPartUploadPolicy = const $ allowWithMaximumSize sizeLimit
    let uploadPolicy = setMaximumFormInputSize sizeLimit defaultUploadPolicy
    tmpDir <- getTempDirectory
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy handlePart
  results <- lift $ forM
                      uploadResults
                      (successCata (return . PolicyFailure)
                                   (return UnnamedFile)
                                   (return . StoryError)
                                   (uncurry saveFile)
                      )
  return . StatusMessage $
    if (null $ filter isFailure results)
      then (msg_UploadFile_Successful "File upload was successful.")
      else (msg_UploadFile_ErrorInManyUploads "An error occured uploading one or more files.")
  where
    handlePart :: PartInfo -> Either PolicyViolationException FilePath -> IO (Success (FilePath, B.ByteString))
    handlePart _partInfo (Left exception) = return . PolicyFailure . T.unpack $ policyViolationExceptionReason exception
    handlePart partInfo (Right filePath) =
      case (partFileName partInfo) of
        Just fp | not (B.null fp) -> do
            contents <- B.readFile filePath
            return (Success (BUTF8.toString fp, contents))
        _                         -> return UnnamedFile

                                     
    saveFile :: FilePath -> B.ByteString -> Handler BeadContext BeadContext (Success ())
    saveFile name contents = do
      i18n <- i18nH
      r <- runStory $ Story.saveUsersFile name (UsersPublicFile contents)
      case r of
        Left e  -> return . StoryError $ Story.translateUserError i18n e
        Right _ -> return (Success ())

uploadFileContent :: PageData -> IHtml
uploadFileContent pd = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ do
      h3 $ fromString $ msg $ msg_UploadFile_FileSelection "File Selection"
      p $ do
        fromString . msg $ msg_UploadFile_Info "Please choose a file to upload.  Note that the maximum file size in kilobytes: "
        fromString (pageDataCata (const show) pd)

    postForm (routeOf uploadFile) ! A.enctype "multipart/form-data" $ do
      fileInput (fieldName fileUploadField)
      Bootstrap.submitButton (fieldName fileUploadSubmit) (msg $ msg_UploadFile_UploadButton "Upload")

    Bootstrap.rowColMd12 $ Bootstrap.table $ do
      thead $ headerLine msg
      tbody usersFileLines

  where
    uploadFile = Pages.uploadFile ()

    headerLine i18n = H.tr $ do
      td . fromString . i18n $ msg_UploadFile_FileName "File Name"
      td . fromString . i18n $ msg_UploadFile_FileSize "File Size (bytes)"
      td . fromString . i18n $ msg_UploadFile_FileDate "File Date"

    numFiles       = pageDataCata (\fs _ -> length fs) pd

    usersFileLines = pageDataCata (\fs _ -> (mapM_ usersFileLine fs)) pd
      where
        usersFileLine (usersfile, fileInfo) = H.tr $ do
          td . fromString $ usersFile id id usersfile
          withFileInfo fileInfo $ \size date -> do
            td . fromString $ show size
            td . fromString $ show date
