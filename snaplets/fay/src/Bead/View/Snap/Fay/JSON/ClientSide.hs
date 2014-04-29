module Bead.View.Snap.Fay.JSON.ClientSide where

import FFI
import Prelude
import Data.Data
import Bead.Domain.Shared.Evaluation

evResultJson :: EvResult -> String
evResultJson = ffi "JSON.stringify(%1)"

evConfigJson :: EvConfig -> String
evConfigJson = ffi "JSON.stringify(%1)"

resultJson :: Result -> String
resultJson = ffi "JSON.stringify(%1)"

evaluationDataJson :: EvaluationData b p -> String
evaluationDataJson = ffi "JSON.stringify(%1)"
