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
import           Data.List (transpose)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Monoid ((<>), mempty)
import           Data.Semigroup (Endo(Endo, appEndo))
import qualified Data.Text as T
import           Data.Tuple.Utils (fst3)

submissionTable :: RestViewHandler
submissionTable = RestViewHandler submissionTablePage

submissionTablePage :: ContentHandler Aeson.Encoding
submissionTablePage = do
  gk <- CH.getParameter $ Bridge.customGroupKeyPrm Params.groupKeyParamName
  table <- CH.userStory $ S.groupSubmissionTable gk
  return $ Rel.submissionTableInfoCata
    (\_ _ _ _ _ -> Enc.emptyObject_)
    (\_ cgAssignments userLines _ _ -> Enc.dict (Enc.text . rawAsgKey) (Enc.list submissionToObject) (encodeTable (map (E.cgInfoCata fst3 fst3) cgAssignments)) (map snd userLines))
    table

  where
    encodeTable :: [Rel.AssignmentKey]
                -> (Rel.AssignmentKey -> [(Rel.SubmissionKey, Rel.SubmissionState)] -> a -> a)
                -> a
                -> [[Maybe (Rel.SubmissionKey, Rel.SubmissionState)]]
                -> a
    encodeTable aks step x table = foldr (\(ak, column) acc -> step ak (catMaybes column) acc) x (zip aks (transpose table))

    submissionToObject :: (Rel.SubmissionKey, Rel.SubmissionState) -> Aeson.Encoding
    submissionToObject s = Enc.dict Enc.text id encodeBoth s
      where
        encodeBoth :: (T.Text -> Aeson.Encoding -> a -> a) -> a -> (Rel.SubmissionKey, Rel.SubmissionState) -> a
        encodeBoth insert x (sk, st) = insert "key" (Enc.text $ rawSubmKey sk) (insert "state" (Aeson.toEncoding st) x)

    rawAsgKey :: Rel.AssignmentKey -> T.Text
    rawAsgKey = Rel.assignmentKeyMap T.pack

    rawSubmKey :: Rel.SubmissionKey -> T.Text
    rawSubmKey = Rel.submissionKeyMap T.pack
