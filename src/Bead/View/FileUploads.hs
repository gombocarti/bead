module Bead.View.FileUploads (
   FormField(..)
 , uploadFiles
 , UploadResult(..)
 ) where

import           Bead.Config.Configuration (maxUploadSizeInKb)
import qualified Bead.View.Content as C
import           Bead.View.ContentHandler (ContentHandler, beadHandler)

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Control.Monad.Trans (lift, liftIO)
import qualified Snap as S
import           Snap.Util.FileUploads (UploadPolicy)
import qualified Snap.Util.FileUploads as U
import           System.Directory (doesFileExist)
import           System.FilePath (FilePath)

data FormField = FormField {
    fieldName  :: Text
  , fieldValue :: UploadResult
  }

data UploadResult
  = File FilePath !ByteString
  | PolicyFailure
  | InvalidFile
  | UnnamedFile

uploadFiles :: ContentHandler [FormField]
uploadFiles = beadHandler $ do
  tmpDir <- C.getTempDirectory
  size <- maxUploadSizeInKb <$> C.getConfiguration
  let policy = U.defaultUploadPolicy
      maxSize = fromIntegral (size * 1000)
      perPartUploadPolicy = U.allowWithMaximumSize maxSize
  U.handleFileUploads tmpDir policy (const perPartUploadPolicy) handlePart
  where
    handlePart :: U.PartInfo -> Either U.PolicyViolationException FilePath -> IO FormField
    handlePart partInfo (Left _exception) = return $ FormField (fieldName partInfo) PolicyFailure
    handlePart partInfo (Right filePath) =
      FormField (fieldName partInfo) <$>
        case (U.partFileName partInfo) of
          Just fp | not (B.null fp) -> do
            contents <- liftIO $ do
              exists <- doesFileExist filePath
              if exists
                then Just <$> B.readFile filePath
                else return Nothing
            return $ case contents of
                       Just body -> File (BC.unpack fp) body
                       _         -> InvalidFile
          _ -> return UnnamedFile

    fieldName :: U.PartInfo -> Text
    fieldName = T.decodeUtf8 . U.partFieldName

    isFile :: UploadResult -> Bool
    isFile (File _ _) = True
    isFile _          = False

