{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Bead.View.Content.SubmissionState
  ( formatSubmissionState
  , Style(..)
  , toLabel
  , toIcon
  , toMediumIcon
  , toLargeIcon
  , toBadge
  , toColoredBadge
  )
where

import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           Bead.Domain.Shared.Evaluation (EvResult)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content
import           Bead.View.Content.VisualConstants (displayableFreeFormResultLength)
import qualified Bead.Domain.Shared.Evaluation as Eval

data Style = Style {
    nonEvaluated :: I18N -> H.Html
  , testsPassed :: I18N -> H.Html
  , testsFailed :: I18N -> H.Html
  , accepted :: I18N -> H.Html
  , rejected :: I18N -> H.Html
  , percentageTag :: String -> H.Html
  , freeFormTag :: String -> H.Html
  , freeFormPlaceholder :: I18N -> String
}

-- |Convert a 'SubmissionState' into a colored label.
-- This 'Style' is used on the student section of the Home page.
toLabel :: Style
toLabel = Style {
    nonEvaluated = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , testsPassed = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> Bootstrap.grayLabel $ msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> Bootstrap.greenLabel $ msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> Bootstrap.redLabel $ msg $ msg_SubmissionState_Rejected "Rejected"
  , percentageTag = Bootstrap.blueLabel
  , freeFormTag = Bootstrap.blueLabel
  , freeFormPlaceholder = const "..."
  }

toMediumIcon :: Style
toMediumIcon = toIcon Bootstrap.Medium

toLargeIcon :: Style
toLargeIcon = toIcon Bootstrap.Large

-- |Convert a 'SubmissionState' into a colored icon.
-- This 'Style' is used on the admin section of the Home page,
-- at the top of the Evaluation page, and on the EvaluationTable page.
toIcon :: Bootstrap.Size -> Style
toIcon size = Style {
    nonEvaluated = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-stop"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_NonEvaluated "Non evaluated")
          $ mempty
  , testsPassed = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-ok-circle"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Tests_Passed "Tests are passed")
          $ mempty
  , testsFailed = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-remove-circle"
          ! A.style (H.toValue $ unwords ["color:#AAAAAA;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Tests_Failed "Tests are failed")
          $ mempty
  , accepted = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-thumbs-up"
          ! A.style (H.toValue $ unwords ["color:#00FF00;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Accepted "Accepted")
          $ mempty
  , rejected = \msg ->
      H.i ! A.class_ "glyphicon glyphicon-thumbs-down"
          ! A.style (H.toValue $ unwords ["color:#FF0000;", iconSize])
          ! tooltip (msg $ msg_SubmissionState_Rejected "Rejected")
          $ mempty
  , percentageTag =
      Bootstrap.blueLabel
  , freeFormTag =
      Bootstrap.blueLabel
  , freeFormPlaceholder =
      const "..."
  }
  where
    tooltip :: String -> H.Attribute
    tooltip s = A.title (H.toValue s)

    iconSize :: String
    iconSize = unwords ["font-size:", Bootstrap.sizeCata "medium" "large" "xx-large" size]

toBadge :: Style
toBadge = Style {
    nonEvaluated = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_NonEvaluated "Non-evaluated"
  , testsPassed = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Tests_Passed "Tests are passed"
  , testsFailed = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Tests_Failed "Tests are failed"
  , accepted = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Accepted "Accepted"
  , rejected = \msg -> Bootstrap.badge $ msg $ msg_SubmissionState_Rejected "Rejected"
  , percentageTag = Bootstrap.badge
  , freeFormTag = Bootstrap.badge
  , freeFormPlaceholder = \msg -> msg $ msg_SubmissionState_FreeFormEvaluated "Evaluated"
  }

-- |Convert a 'SubmissionState' into a colored icon.
-- Uses the same color as labels. This is forward-compatible with Bootstrap 4,
-- which uses same colors for pill badges and labels.
-- When Bootstrap 4 rolls in, colored badges can use 'Bootstrap.Alert'.
toColoredBadge :: Style
toColoredBadge = toBadge {
    accepted = \msg -> (accepted toBadge msg) ! acceptedColor
  , rejected = \msg -> (rejected toBadge msg) ! rejectedColor
  , percentageTag = \text -> (percentageTag toBadge text) ! evaluatedColor
  , freeFormTag = \text -> (freeFormTag toBadge text) ! evaluatedColor
  }

acceptedColor :: H.Attribute
acceptedColor = A.style "background-color: #5cb85c;"

rejectedColor :: H.Attribute
rejectedColor = A.style "background-color: #d9534f;"

evaluatedColor :: H.Attribute
evaluatedColor = A.style "background-color: #337ab7;"

formatSubmissionState :: Style -> I18N -> SubmissionState -> H.Html
formatSubmissionState style msg =
  submissionStateCata
    (nonEvaluated style msg)
    tested
    (\_key result -> val result) -- evaluated

  where
    tested :: Bool -> H.Html
    tested = bool (testsPassed style msg) (testsFailed style msg)

    val :: Eval.EvResult -> H.Html
    val = Eval.evResultCata
            (Eval.binaryCata (Eval.resultCata (accepted style msg) (rejected style msg)))
            percentage
            (Eval.freeForm $ \text ->
              let cell = if length text < displayableFreeFormResultLength then text else freeFormPlaceholder style msg in
              freeFormTag style cell ! A.title (H.toValue text))

      where
        percentage :: Eval.Percentage -> H.Html
        percentage (Eval.Percentage (Eval.Scores [p])) = percentageTag style $ concat [show . round $ (100 * p), "%"]
        percentage _ = percentageTag style "???%"

evResult :: forall a. a -> a -> (String -> a) -> (String -> a) -> Eval.EvResult -> a
evResult passed failed percentage freeFormMsg =
 Eval.evResultCata
  (Eval.binaryCata (Eval.resultCata passed failed))
  score
  (Eval.freeForm freeFormMsg)
  where
    percent :: Double -> String
    percent x = concat [show . round $ (100 * x), "%"]

    score :: Eval.Percentage -> a
    score (Eval.Percentage (Eval.Scores [p])) = percentage $ percent p
    score _                                   = percentage "?%"
