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
  return $ Aeson.pairs $ Rel.submissionTableInfoCata
    (\_ _ _ _ _ -> mempty)
    (\_ _ _ userLines _ _ -> Map.foldrWithKey (\ak sks acc -> rawAsgKey ak .= map rawSubmKey sks <> acc) mempty (collectSubmissions userLines))
    table

  where
    rawAsgKey :: Rel.AssignmentKey -> T.Text
    rawAsgKey = Rel.assignmentKeyMap T.pack

    rawSubmKey :: Rel.SubmissionKey -> String
    rawSubmKey = Rel.submissionKeyMap id

    collectSubmissions :: [(E.UserDesc, Map Rel.AssignmentKey (Rel.SubmissionKey, Rel.SubmissionState))]
                       -> Map Rel.AssignmentKey [Rel.SubmissionKey]
    collectSubmissions userLines =
      Map.map (\sks -> appEndo sks []) . Map.unionsWith (<>) . map (Map.map (\(sk, _) -> Endo (sk : )) . snd) $ userLines
