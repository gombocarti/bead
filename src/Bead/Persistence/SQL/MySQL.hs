{-# LANGUAGE OverloadedStrings, Rank2Types, TypeFamilies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE CPP #-}

module Bead.Persistence.SQL.MySQL where

import           Control.Exception
import           Control.Monad.Logger
import qualified Data.ByteString.Char8 as BC
import           Data.String (fromString)
import           Data.Text (Text)
import           Data.Word (Word8)
import           Database.MySQL.Connection (utf8mb4_unicode_ci)
import           Database.Persist.Sql
import           Database.Persist.MySQL

import qualified Bead.Config as Config
import           Bead.Domain.Types (Erroneous(..))
import           Bead.Persistence.Initialization
import           Bead.Persistence.SQL.Entities
import qualified Bead.Persistence.SQL.FileSystem as FS

import Control.Monad.Trans.Resource

data Config = Config {
    dbName :: String
  , host :: String
  , port :: Int
  , user :: String
  , pass :: String
  } deriving (Eq, Read, Show)

defaultConfig = Config {
    dbName = "bead"
#ifndef TEST
  , host = "localhost"
#else
  , host = "mysql"
#endif
  , port = 3306
  , user = "root"
  , pass = "password"
  }

configToPersistConfig :: Config.Config -> Config
configToPersistConfig = mysqlConfigToPersistConfig . Config.persistConfig
  where
    mysqlConfigToPersistConfig p = Config {
        dbName = Config.mySQLDbName p
      , host = Config.mySQLHost p
      , port = Config.mySQLPort p
      , user = Config.mySQLUser p
      , pass = Config.mySQLPass p
      }

parseConfig :: String -> Config
parseConfig = read

configToConnectInfo :: Config -> MySQLConnectInfo
configToConnectInfo c = setMySQLConnectInfoCharset utf8mb4_unicode_ci .
                          setMySQLConnectInfoPort (fromIntegral $ port c) $ connectInfo

  where
    connectInfo :: MySQLConnectInfo
    connectInfo = mkMySQLConnectInfo
                    (host c)
                    (BC.pack (user c))
                    (BC.pack (pass c))
                    (BC.pack (dbName c))

runMySql pool query = runResourceT . runNoLoggingT $ runSqlPool query pool

runMySqlConn :: MySQLConnectInfo -> Persist a -> IO a
runMySqlConn conn query = runResourceT . runNoLoggingT . withMySQLConn conn $ runSqlConn query

createPersistInit :: Config -> IO PersistInit
createPersistInit config = do
  let conn = configToConnectInfo config
  let dbname = dbName config
  let useDatabase = rawExecute (fromString $ ("USE " ++ dbname)) []
  let select = runMySqlConn conn $ do
        useDatabase
        createTables <- fmap (filter (not . isExceptionalMigration))
                             (getMigration migrateAll)
        fsSetUp <- FS.isSetUpFS
        return (and [null createTables, fsSetUp])
  let initDatabase = do
        runMySqlConn conn . void $ do
          useDatabase
          runMigrationSilent migrateAll
        FS.initFS
  let dropDatabase = do
        runMySqlConn conn $ do
          rawExecute (fromString $ ("DROP DATABASE " ++ dbname)) []
          rawExecute (fromString $ ("CREATE DATABASE " ++ dbname)) []
        FS.removeFS
  return $! PersistInit {
      isSetUp = select
    , initPersist = initDatabase
    , tearDown = dropDatabase
    }

newtype Interpreter
  = Interpreter { unInt :: forall a . Persist a -> IO (Either [Char] a) }

createPersistInterpreter :: Config -> IO Interpreter
createPersistInterpreter config = do
  let connectInfo = configToConnectInfo config
  pool <- runResourceT . runNoLoggingT $ createMySQLPool connectInfo 100
  return $! Interpreter (\query -> do
    result <- trySomeEx $ runMySql pool query
    return $! either (Left . show) Right result)
  where
    trySomeEx :: IO a -> IO (Either SomeException a)
    trySomeEx = try

runInterpreter :: Interpreter -> Persist a -> IO (Erroneous a)
runInterpreter (Interpreter run) = run

-- * Migration fix

isExceptionalMigration :: Text -> Bool
isExceptionalMigration cmd = elem cmd migrationCommandExceptions

-- If a field is created with sqltype modifier, the persist migrates
-- with alter table. It is probably an issue of the persistent package.
migrationCommandExceptions :: [Text]
migrationCommandExceptions =
  [ "ALTER TABLE `assessment` CHANGE `created` `created` datetime(6) NOT NULL"
  , "ALTER TABLE `assessment` CHANGE `description` `description` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assessment` CHANGE `eval_config` `eval_config` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assessment` CHANGE `title` `title` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `created` `created` datetime(6) NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `end` `end` datetime(6) NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `description` `description` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `start` `start` datetime(6) NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `type` `type` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `assignment` CHANGE `eval_config` `eval_config` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `comment` CHANGE `date` `date` datetime(6) NOT NULL"
  , "ALTER TABLE `comment` CHANGE `text` `text` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `comment` CHANGE `author` `author` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `comment` CHANGE `type` `type` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `course` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `course` CHANGE `description` `description` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `course` CHANGE `test_script_type` `test_script_type` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `evaluation` CHANGE `result` `result` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `evaluation` CHANGE `written` `written` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `feedback` CHANGE `date` `date` datetime(6) NOT NULL"
  , "ALTER TABLE `feedback` CHANGE `info` `info` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `group` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `group` CHANGE `description` `description` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `notification` CHANGE `message` `message` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `notification` CHANGE `date` `date` datetime(6) NOT NULL"
  , "ALTER TABLE `notification` CHANGE `event` `event` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `notification` CHANGE `type` `type` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `score` CHANGE `score` `score` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `submission` CHANGE `post_date` `post_date` datetime(6) NOT NULL"
  , "ALTER TABLE `submission` CHANGE `simple` `simple` longtext character set utf8mb4 collate utf8mb4_unicode_ci NULL"
  , "ALTER TABLE `submission` CHANGE `zipped` `zipped` longblob NULL"
  , "ALTER TABLE `test_case` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_case` CHANGE `description` `description` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_case` CHANGE `simple_value` `simple_value` longtext character set utf8mb4 collate utf8mb4_unicode_ci NULL"
  , "ALTER TABLE `test_case` CHANGE `zipped_value` `zipped_value` longblob NULL"
  , "ALTER TABLE `test_case` CHANGE `info` `info` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `description` `description` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `notes` `notes` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `script` `script` longtext character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `test_script` CHANGE `test_script_type` `test_script_type` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `role` `role` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `email` `email` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `time_zone` `time_zone` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `language` `language` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user` CHANGE `email_notifications` `email_notifications` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user_notification` CHANGE `created` `created` datetime(6) NOT NULL"
  , "ALTER TABLE `user_registration` CHANGE `timeout` `timeout` datetime(6) NOT NULL"
  , "ALTER TABLE `user_registration` CHANGE `username` `username` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user_registration` CHANGE `email` `email` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user_registration` CHANGE `name` `name` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  , "ALTER TABLE `user_registration` CHANGE `token` `token` text character set utf8mb4 collate utf8mb4_unicode_ci NOT NULL"
  ]
