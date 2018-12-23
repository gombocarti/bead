module Bead.Daemon.LDAP.Query (
    QuerySettings(..)
  , QueryResult(..)
  , queryResult
  , query
  ) where

import           Control.Applicative
import           Control.Arrow ((&&&), (***))
import           Control.Monad.Reader
import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.ByteString.Base64
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.String
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.Process
import           System.Timeout
import           Text.Printf

data QueryResult
  = QueryOK [(String,String)]
  | QueryInvalid
  | QueryError String
  deriving (Eq,Show)

queryResult
  queryOk
  queryInvalid
  queryError
  q = case q of
    QueryOK attrs  -> queryOk attrs
    QueryInvalid   -> queryInvalid
    QueryError msg -> queryError msg

data QuerySettings = QuerySettings
  { queryTimeout     :: Int
  , queryCommand     :: String
  , queryUsernameKey :: String
  }

exitCode :: (Int -> a) -> a -> ExitCode -> a
exitCode _ x ExitSuccess     = x
exitCode f _ (ExitFailure y) = f y

exitCode' f x = exitCode (const f) x

type Query a = ExceptT String (ReaderT QuerySettings IO) a

ldapsearch :: String -> [String] -> Query [(String,String)]
ldapsearch username attrs = do
  (t,(cmd:args,usernameKey)) <- asks $ queryTimeout &&& (words . queryCommand) &&& queryUsernameKey
  (ec,out,err) <- liftIO (timeout (t * 1000000) $
    readProcessWithExitCode
      cmd (args ++ concat [usernameKey, "=", username] : attrs)
      "")
    >>= maybe (throwError "timed out") return
  let out1 = DT.replace (fromString "\n ") (fromString "") $ fromString out
  let out2 = filter (not . DT.null) $ DT.lines out1
  let r    = map ((DT.unpack *** translate) . DT.break (== ':')) out2
  exitCode' (throwError err) (return r) ec
 where
   textToBS :: DT.Text -> BS.ByteString
   textToBS = BS.pack . DT.unpack

   translate cs
     | colon `DT.isPrefixOf` cs      = DT.unpack cs1
     | colonColon `DT.isPrefixOf` cs =
       either (const $ DT.unpack cs2) (DT.unpack . DTE.decodeUtf8) (decode $ textToBS cs2)
     | otherwise = DT.unpack $ cs
     where
       colon      = fromString ": "
       colonColon = fromString ":: "
       Just cs1   = DT.stripPrefix colon cs
       Just cs2   = DT.stripPrefix colonColon cs

query :: QuerySettings -> String -> [String] -> IO QueryResult
query cfg username attrs =
 fmap (either QueryError id) $ flip runReaderT cfg . runExceptT $ do
   QueryOK <$> ldapsearch username attrs
