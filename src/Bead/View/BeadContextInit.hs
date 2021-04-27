{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Bead.View.BeadContextInit (
    beadContextInit
  , beadConfigFileName
  , Daemons(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor (first)
import           Data.Char (toUpper)
import qualified Data.HashMap.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as TE

import           Snap hiding (Config(..))
import           System.FilePath ((</>))
import           System.Directory

import           Bead.Config
import           Bead.Controller.ServiceContext as S hiding (serviceContext)
import           Bead.Daemon.Email (EmailDaemon)
#ifdef SSO
import           Bead.Daemon.LDAP
#endif
import           Bead.Domain.Entities (UserRegInfo, Username(..))

import           Bead.Domain.TimeZone
import qualified Bead.View.AuthToken as Auth
import           Bead.View.BeadContext hiding (ldapDaemon)
import           Bead.View.DataDir
import           Bead.View.Dictionary (dictionaries, Language(..))
import           Bead.View.Pagelets (syntaxHighlightCss)
import           Bead.View.Routing

iconFileName :: String
iconFileName = "icon.ico"

-- The collection of the daemons that are necessary to create the
-- application
-- TODO: Use lenses for optional fields.
data Daemons = Daemons {
    emailDaemon  :: EmailDaemon
#ifdef SSO
  , ldapDaemon   :: LDAPDaemon
#endif
  }

beadContextInit :: Config -> ServiceContext -> Daemons -> FilePath -> SnapletInit BeadContext BeadContext
beadContextInit config s daemons tempDir = makeSnaplet "bead" description dataDir $ do
  copyDataContext

  generateSyntaxHighlightCss

  authTokenManager <- liftIO $ Auth.createAuthTokenManager
  auth <- nestSnaplet "authentication" authContext $ createAuthTokenContext authTokenManager

  ss <- nestSnaplet "context" serviceContext $ contextSnaplet s

  liftIO $ putStrLn $ "Available languages: " ++ (show $ Map.keys dictionaries)
  ds <- nestSnaplet "dictionary" dictionaryContext $
    dictionarySnaplet dictionaries (Language $ defaultLoginLanguage config)

#ifdef EmailEnabled
  se <- nestSnaplet "sendemail" sendEmailContext (emailSenderSnaplet config (emailDaemon daemons))
#endif

  rp <- nestSnaplet "randompassword" randomPasswordContext passwordGeneratorSnaplet

  ts <- nestSnaplet "tempdir" tempDirContext $ tempDirectorySnaplet tempDir

  cs <- nestSnaplet "config" configContext $ configurationServiceContext config

#ifndef SSO
  un <- nestSnaplet "usernamechecker" checkUsernameContext $ regexpUsernameChecker config
#endif

  timeZoneConverter <- liftIO $ createTimeZoneConverter (timeZoneInfoDirectory config)

  tz <- nestSnaplet "timezoneconverter" timeZoneContext $ createTimeZoneContext timeZoneConverter

#ifdef SSO
  ldap <- nestSnaplet "ldap-config" ldapContext $
            createLDAPContext (LDAP $ ldapDaemon daemons)
#endif

  addRoutes (map (first TE.encodeUtf8) $ routes config)

  wrapSite (<|> pages)

  return $
#ifdef SSO
#ifdef EmailEnabled
    BeadContext auth ss ds se rp ts cs tz ldap
#else
    BeadContext auth ss ds rp ts cs tz ldap
#endif
#else
#ifdef EmailEnabled
    BeadContext auth ss ds se rp ts cs un tz
#else
    BeadContext auth ss ds rp ts cs un tz
#endif
#endif
  where
    description = "The BEAD website"

    dataDir = Just referenceDataDir

-- | Creating data context in the current directory,
--   copying reference files.
copyDataContext :: Initializer b v ()
copyDataContext = do
  reference <- liftIO referenceDataDir
  dataDir   <- getSnapletFilePath
  let skips = [beadConfigFileName, iconFileName]
  liftIO $ copyFiles skips reference dataDir
  return ()

-- | Copy and update files if missing or outdated, skip the ones
-- from the outdate check that names are the same as in the skipped list
copyFiles :: [FilePath] -> FilePath -> FilePath -> IO ()
copyFiles skips src dst = do
  dstExist <- doesDirectoryExist dst
  unless dstExist $ createDirectory dst
  contents <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) contents
  forM_ xs $ \name -> do
    let srcPath = src </> name
        dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyFiles skips srcPath dstPath
      else do
        dstFileExist <- doesFileExist dstPath
        when (not dstFileExist || name `notElem` skips) $ do
          doCopy <- if dstFileExist
            then do
              srcDate <- getModificationTime srcPath
              dstDate <- getModificationTime dstPath
              return $ dstDate < srcDate
            else return True
          when doCopy $ copyFile srcPath dstPath

generateSyntaxHighlightCss :: Initializer b v ()
generateSyntaxHighlightCss = do
  staticDir <- getStaticDirectory
  let (css, path) = syntaxHighlightCss
  liftIO $ do
    createDirectoryIfMissing False staticDir
    writeFile (staticDir </> path) css
