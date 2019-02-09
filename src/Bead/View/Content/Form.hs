{-# LANGUAGE OverloadedStrings #-}

module Bead.View.Content.Form
  ( dateTime
  , errorsOf
  , file
  , Form
  , getForm
  , postForm
  , selection
  , textArea
  , textInput
  , toggle
  , View
  , UploadPolicy(..)
  ) where

import           Control.Monad.Identity (Identity(Identity), runIdentity)
import           Control.Applicative ((<$>))
import           Control.Monad (void)
import           Control.Monad.Trans (lift)
import           Data.Maybe (listToMaybe)
import           Data.Monoid ((<>))
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Digestive as DF

import           Bead.View.Content (getRequest, rqParam, fileInput, I18N, Translation)
import           Bead.View.Content.Bootstrap (Input, Help)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.ContentHandler (ContentHandler)
import qualified Bead.View.FileUploads as Uploads

import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as BC

type Form a = DF.Form T.Text Identity a
type View   = DF.View T.Text

getEnvironment :: [(T.Text, T.Text)] -> ContentHandler (DF.Env Identity)
getEnvironment extraParams = do
  req <- lift $ getRequest
  let env path = return $ maybe [] (map (DF.TextInput . T.decodeUtf8)) $ rqParam (T.encodeUtf8 $ DF.fromPath path) req
  return env

getForm :: T.Text -> Form a -> View
getForm name = runIdentity . DF.getForm name

postForm :: T.Text -> DF.Form v Identity a -> UploadPolicy -> ContentHandler (DF.View v, Maybe a)
postForm name form policy = do
  case policy of
    InsertFilesIntoView -> void $ Uploads.uploadFiles
    IgnoreFiles -> void $ Uploads.uploadFiles
    NoFilesInForm -> return ()
  env <- getEnvironment []
  return $ runIdentity $ DF.postForm name form (const (return env))

data UploadPolicy
  = InsertFilesIntoView
  | IgnoreFiles
  | NoFilesInForm

errorsOf :: T.Text -> Input -> View -> [Help]
errorsOf name input view = map Bootstrap.ErrorExplanation errors

  where
    errors :: [Text]
    errors = Bootstrap.inputTypeCata
      (\_ _ -> DF.errors (DF.absoluteRef name view) view) -- TextInput
      (\_ _ -> DF.errors (DF.absoluteRef name view) view)   -- TextArea
      (\_ -> DF.errors (DF.absoluteRef name view) view)     -- FixedChoiceInput
      (DF.errors (DF.absoluteRef name view) view)   -- File
      (\_ _ -> DF.errors (DF.absoluteRef name view) view)   -- Toggle
      (\_ _ -> let v = DF.subView name v                    -- DateTime
               in DF.errors "date" v ++ DF.errors "time" v)
      (Bootstrap.inputType input)

textInput :: T.Text -> View -> Input
textInput name view
  | not (T.null value) = textInput (Just value)
  | otherwise = textInput Nothing

    where
      textInput :: Maybe Text -> Input
      textInput = Bootstrap.textInput (DF.absoluteRef name view) Bootstrap.PlainText

      value :: T.Text
      value = DF.fieldInputText name view

dateTime :: T.Text -> View -> Input
dateTime name view = let v = DF.subView name view
                     in Bootstrap.dateTime
                          (DF.absoluteRef name view)
                          (DF.fieldInputText "date" v)
                          (DF.fieldInputText "time" v)

textArea :: T.Text -> Bootstrap.Size -> View -> Input
textArea name size view = Bootstrap.textArea (DF.absoluteRef name view) size (Just $ DF.fieldInputText name view)

toggle :: T.Text -> T.Text -> View -> Input
toggle label name view =
  let ident = DF.absoluteRef name view
      state = if DF.fieldInputBool name view then Bootstrap.On else Bootstrap.Off
  in Bootstrap.toggle ident label state

selection :: T.Text -> View -> Input
selection name view = Bootstrap.selection (DF.absoluteRef name view) (DF.fieldInputChoice name view)

file :: T.Text -> View -> Input
file name view = Bootstrap.file (DF.absoluteRef name view)
