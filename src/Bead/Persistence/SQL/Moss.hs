module Bead.Persistence.SQL.Moss (
    newMossScriptInvocationKey
  , saveMossScriptInvocation
  , loadMossScriptInvocation
  ) where

import qualified Data.Text as T (empty)
import           Database.Esqueleto (insert, update, set, val, just, (==.), (=.), where_, (^.), limit, from, select, entityVal, InnerJoin(InnerJoin), on, unValue)
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import           Bead.Persistence.SQL.Class (fromDomainKey, toDomainKey)
import           Bead.Persistence.SQL.Entities

newMossScriptInvocationKey :: Domain.AssignmentKey -> Persist Domain.MossScriptInvocationKey
newMossScriptInvocationKey ak = do
  key <- insert emptyInvocation
  insert (AssignmentOfMossInvocation key (fromDomainKey ak))
  return (toDomainKey key)
  where
    emptyInvocation = MossScriptInvocation Nothing Nothing Nothing

saveMossScriptInvocation :: Domain.MossScriptInvocationKey -> Domain.MossScriptInvocation -> Persist ()
saveMossScriptInvocation k v = update $ \invocation -> do
  set invocation attributes
  where_ (invocation ^. MossScriptInvocationId ==. val (fromDomainKey k))

  where
    exitCodeToInt :: ExitCode -> Int
    exitCodeToInt ExitSuccess = 0
    exitCodeToInt (ExitFailure n) = n

    attributes =
      let (exitCode, output, reportUrl) =
            Domain.mossScriptInvocationCata
              (\output reportUrl -> (0, Just output, Just reportUrl)) -- MossScriptInvocationSuccess
              (\output exitCode -> (exitCodeToInt exitCode, Just output, Nothing))  -- MossScriptInvocationFailure
              (\output -> (exitCodeToInt ExitSuccess, Just output, Nothing)) -- MossScriptInvocationNotInterpretableOutput
              v
      in [ MossScriptInvocationExitCode =. just (val exitCode)
         , MossScriptInvocationOutput =. val output
         , MossScriptInvocationReportUrl =. val reportUrl
         ]

-- Loads result of invocation of Moss script. Just means the script
-- terminated either successfully or erroneously. Nothing indicates
-- the script is still running and not terminated.
loadMossScriptInvocation :: Domain.MossScriptInvocationKey -> Persist (Maybe Domain.MossScriptInvocation, Domain.AssignmentKey)
loadMossScriptInvocation key = do
  invocations <- select $ from $ \(invocation `InnerJoin` ai) -> do
    on (invocation ^. MossScriptInvocationId ==. ai ^. AssignmentOfMossInvocationMossInvocation)
    where_ (invocation ^. MossScriptInvocationId ==. val (fromDomainKey key))
    limit 1
    return (invocation, ai ^. AssignmentOfMossInvocationAssignment)
  case invocations of
    (invocation, ak) : _ -> do
      let ak' = toDomainKey (unValue ak)
          invocation' = entityVal invocation
      case mossScriptInvocationExitCode invocation' of
        Just 0 ->
          case (mossScriptInvocationOutput invocation', mossScriptInvocationReportUrl invocation') of
            (Just output, Just reportUrl) -> return (Just $ Domain.MossScriptInvocationSuccess output reportUrl, ak')
            (Just output, Nothing) -> return (Just $ Domain.MossScriptInvocationNotInterpretableOutput output, ak')
            (Nothing, Just reportUrl) -> return (Just $ Domain.MossScriptInvocationSuccess T.empty reportUrl, ak')
            (Nothing, Nothing) -> persistError "loadMossScriptInvocation" $ "Successful invocation without output and url: " ++ show key
        Just exitCode ->
          case mossScriptInvocationOutput invocation' of
            Just output -> return (Just $ Domain.MossScriptInvocationFailure output (ExitFailure exitCode), ak')
            Nothing -> persistError "loadMossScriptInvocation" $ "Unsuccessful invocation without output: " ++ show key
        Nothing -> return (Nothing, ak')
    [] -> persistError "loadMossScriptInvocation" $ "Moss script invocation is not found: " ++ show key
