{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.ScoreInfo (
    scoreInfoToText
  , scoreInfoToRawText
  , scoreInfoToIcon
  , scoreInfoToIconLink
  ) where

import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.String (fromString)

import           Bead.View.Content (I18N, Translation, ScoreInfo)
import qualified Bead.View.Content as C
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.VisualConstants
import           Bead.Domain.Evaluation

scoreInfoToText :: String -> I18N -> ScoreInfo -> String
scoreInfoToText notFound msg = C.scoreInfoAlgebra
                               notFound
                               (\_ek result -> 
                                  evResultCata
                                  (binaryCata (resultCata (msg accepted) (msg rejected)))
                                  (\p -> let Percentage (Scores [d]) = p in percent d)
                                  (freeForm id)
                                  result)

scoreInfoToRawText :: String -> I18N -> ScoreInfo -> String
scoreInfoToRawText notFound msg = C.scoreInfoAlgebra
                                  notFound
                                  (\_ek result -> 
                                   evResultCata
                                   (binaryCata (resultCata (msg accepted) (msg rejected)))
                                   (\p -> let Percentage (Scores [d]) = p in show . round $ (d * 100))
                                   (freeForm id)
                                   result)

evResultToIcon :: I18N -> EvResult -> Html
evResultToIcon msg = evResultCata (binaryCata (resultCata passed' failed')) percentage free
  where 
    passed' = passed msg
    failed' = failed msg

scoreInfoToIcon :: I18N -> ScoreInfo -> Html
scoreInfoToIcon msg = C.scoreInfoAlgebra notFound' $ \_ek -> evResultToIcon msg
  where
    notFound' = notFound msg


scoreInfoToIconLink :: I18N -> String -> String -> ScoreInfo -> Html
scoreInfoToIconLink msg notFoundLink foundLink =
  C.scoreInfoAlgebra (Bootstrap.link notFoundLink notFound') $ \_ek -> (Bootstrap.link foundLink . evResultToIcon msg)
  where 
    notFound' = notFound msg

tooltip :: I18N -> Translation String -> H.Attribute
tooltip msg = A.title . fromString . msg

notFound :: I18N -> Html
notFound msg = H.i ! A.class_ "glyphicon glyphicon-stop" ! A.style "color:#AAAAAA; font-size: xx-large"
                   ! tooltip msg (C.msg_SubmissionState_NonEvaluated "Non evaluated") $ mempty

accepted, rejected :: Translation String
accepted = C.msg_SubmissionState_Accepted "Accepted"
rejected = C.msg_SubmissionState_Rejected "Rejected"

passed :: I18N -> Html
passed msg = H.i ! A.class_ "glyphicon glyphicon-thumbs-up" ! A.style "color:#00FF00; font-size: xx-large"
                 ! tooltip msg accepted $ mempty -- accepted

failed :: I18N -> Html
failed msg = H.i ! A.class_ "glyphicon glyphicon-thumbs-down" ! A.style "color:#FF0000; font-size: xx-large"
                 ! tooltip msg rejected $ mempty -- rejected

percentage :: Percentage -> Html
percentage (Percentage (Scores [p])) = H.span ! A.class_ "label label-primary" $ fromString $ percent p
percentage _ = error "SubmissionTable.coloredSubmissionCell percentage is not defined"

free :: FreeForm -> Html
free = freeForm $ \msg ->
  let cell = if length msg < displayableFreeFormResultLength then msg else "..." in
  H.span ! A.class_ "label label-primary"
         ! A.title (fromString msg) $ (fromString cell)


percent :: RealFrac a => a -> String
percent x = concat [show . round $ (100 * x), "%"]
