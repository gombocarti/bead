{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Rest.SubmissionTable.Page (
    submissionTable
  ) where

import           Bead.View.Content (RestViewHandler(RestViewHandler), groupKeyPrm)
import           Bead.View.ContentHandler (ContentHandler)
import qualified Bead.View.ContentHandler as CH
import qualified Bead.View.DataBridge as Bridge
import qualified Bead.View.RequestParams as Params
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Relationships as Rel
import qualified Bead.Controller.UserStories as S

import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Data.Aeson.Encoding as Enc
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>), mempty)
import           Data.Semigroup (Endo(Endo, appEndo))

submissionTable :: RestViewHandler
submissionTable = RestViewHandler submissionTablePage

submissionTablePage :: ContentHandler Aeson.Encoding
submissionTablePage = do
  gk <- CH.getParameter $ Bridge.customGroupKeyPrm Params.groupKeyParamName
  table <- CH.userStory $ S.groupSubmissionTable gk
  return $ Rel.submissionTableInfoCata
    (\_ _ _ _ _ _ -> Enc.emptyObject_)
    (\_ _ _ userLines _ _ -> Enc.dict (Enc.text . rawAsgKey) (Enc.list submissionToObject) encodeMap (collectSubmissions userLines))
    table

  where
    encodeMap :: (Rel.AssignmentKey -> [(Rel.SubmissionKey, Rel.SubmissionState)] -> a -> a)
              -> a
              -> Map Rel.AssignmentKey [(Rel.SubmissionKey, Rel.SubmissionState)]
              -> a
    encodeMap step x m = Map.foldrWithKey step x m

    submissionToObject :: (Rel.SubmissionKey, Rel.SubmissionState) -> Aeson.Encoding
    submissionToObject s = Enc.dict Enc.text id encodeBoth s
      where
        encodeBoth :: (T.Text -> Aeson.Encoding -> a -> a) -> a -> (Rel.SubmissionKey, Rel.SubmissionState) -> a
        encodeBoth insert x (sk, st) = insert "key" (Enc.text $ rawSubmKey sk) (insert "state" (Aeson.toEncoding st) x)

    rawAsgKey :: Rel.AssignmentKey -> T.Text
    rawAsgKey = Rel.assignmentKeyMap T.pack

    rawSubmKey :: Rel.SubmissionKey -> T.Text
    rawSubmKey = Rel.submissionKeyMap T.pack

    collectSubmissions :: [(E.UserDesc, Map Rel.AssignmentKey (Rel.SubmissionKey, Rel.SubmissionState))]
                       -> Map Rel.AssignmentKey [(Rel.SubmissionKey, Rel.SubmissionState)]
    collectSubmissions userLines =
      Map.map (\skSts -> appEndo skSts []) . Map.unionsWith (<>) . map (Map.map (\skSt -> Endo (skSt : )) . snd) $ userLines
