module Bead.View.Translation.I18N where

import Bead.View.Translation.Base

import Data.Text (Text, pack, unpack)
import Text.Printf (printf)

-- The I18N is a mapping from a given translation key
-- to the actual translation of the message
type I18N = Translation -> Text

-- | The Translation Message represents a message that
-- can be rendered out the the UI, the message could
-- be a normal message or a parametrized one
data TransMsg
  = TransMsg Translation
  | TransPrmMsg Translation Text
  | TransPrm2Msg Translation Text Text
  | TransPrm3Msg Translation Text Text Text
  | TransPrm4Msg Translation Text Text Text Text
  deriving (Show)

-- Template method for TransMsg function
transMsgCata
  transMsg     f
  transPrmMsg  g
  transPrm2Msg h
  transPrm3Msg i
  transPrm4Msg k
  tm = case tm of
    TransMsg     t          -> transMsg     (f t)
    TransPrmMsg  t p1       -> transPrmMsg  (g t) p1
    TransPrm2Msg t p1 p2    -> transPrm2Msg (h t) p1 p2
    TransPrm3Msg t p1 p2 p3 -> transPrm3Msg (i t) p1 p2 p3
    TransPrm4Msg t p1 p2 p3 p4 -> transPrm4Msg (k t) p1 p2 p3 p4

-- Translate the parametrized message with the given localization
translateMessage :: I18N -> TransMsg -> Text
translateMessage i18n = transMsgCata
  id     i18n
  (\template p1 -> pack $ printf template p1) (unpack . i18n)
  (\template p1 p2 -> pack $ printf template p1 p2) (unpack . i18n)
  (\template p1 p2 p3 -> pack $ printf template p1 p2 p3) (unpack . i18n)
  (\template p1 p2 p3 p4 -> pack $ printf template p1 p2 p3 p4) (unpack . i18n)
