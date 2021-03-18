{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Bead.View.AuthToken (
    AuthTokenManager(..)
  , Cookie(..)
  , cookieCata
  , isLoggedIn
  , createAuthTokenManager
  ) where

import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Relationships as R
import           Bead.View.Translation (Translation)

import qualified Control.Exception as Exception
import           Control.Monad.Except (ExceptT(ExceptT), runExceptT, throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as AesonTH
import           Data.Bifunctor (bimap)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Either (either)
import           Data.Functor ((<$>))
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.UUID (UUID)
import           GHC.Generics (Generic)
import qualified Jose.Jwa as Jwa
import qualified Jose.Jwk as Jwk
import qualified Jose.Jwt as Jwt
import qualified System.Directory as Dir
import           System.FilePath ((</>))

data AuthTokenManager = AuthTokenManager {
    encryptCookie :: Cookie -> IO (Either T.Text B.ByteString)
  , decryptCookie :: B.ByteString -> IO (Either T.Text Cookie)
  }

data Cookie
  = NotLoggedInCookie { -- ^ Cookie information for a user who is not logged in
      cookieLanguage :: E.Language     -- User's preferred language
    }
  | LoggedInCookie { -- ^ Cookie information for a user who is logged in
      cookieUsername :: E.Username     -- Username
    , cookieUid  :: E.Uid
    , cookieName :: String           -- User's full name
    , cookieLanguage :: E.Language     -- User's preferred language
    , cookieRole :: E.Role             -- User's role
    , cookieUuid :: UUID             -- Token for the active user session
    , cookieTimezone :: E.TimeZoneName -- Timezone of the user
    , cookieStatus :: Maybe (E.StatusMessage (Translation String)) -- The last status message
    , cookieHomePage :: R.HomePageContents
    }
#ifdef TEST
  deriving (Eq, Show)
#endif

cookieCata :: (E.Language -> a)
           -> (E.Username -> E.Uid -> String -> E.Language -> E.Role -> UUID -> E.TimeZoneName -> Maybe (E.StatusMessage (Translation String)) -> R.HomePageContents -> a)
           -> Cookie
           -> a
cookieCata notLoggedIn loggedIn cookie =
  case cookie of
    NotLoggedInCookie lang -> notLoggedIn lang
    LoggedInCookie username uid name lang role uuid timezone status homePage -> loggedIn username uid name lang role uuid timezone status homePage

$(AesonTH.deriveJSON Aeson.defaultOptions ''Translation)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.StatusMessage)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.Username)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.Uid)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.Language)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.Role)

$(AesonTH.deriveJSON Aeson.defaultOptions ''E.TimeZoneName)

$(AesonTH.deriveJSON Aeson.defaultOptions ''Cookie)

$(AesonTH.deriveJSON Aeson.defaultOptions ''R.GroupKey)

$(AesonTH.deriveJSON Aeson.defaultOptions ''R.CourseKey)

$(AesonTH.deriveJSON Aeson.defaultOptions ''R.HomePageContents)

isLoggedIn :: Cookie -> Bool
isLoggedIn (LoggedInCookie {}) = True
isLoggedIn _                   = False

newtype RSAPublicKey = RSAPublicKey { getPublicKey :: Jwk.Jwk }

newtype RSAPrivateKey = RSAPrivateKey { getPrivateKey :: Jwk.Jwk }

newtype AESKey = AESKey { getSymmetricKey :: Jwk.Jwk }

$(AesonTH.deriveJSON Aeson.defaultOptions ''RSAPublicKey)

$(AesonTH.deriveJSON Aeson.defaultOptions ''RSAPrivateKey)

$(AesonTH.deriveJSON Aeson.defaultOptions ''AESKey)

-- Files for key store

keysDir :: FilePath
keysDir = "keys"

rsaPublicKeyFile :: FilePath
rsaPublicKeyFile = keysDir </> "rsa.pub"

rsaPrivateKeyFile :: FilePath
rsaPrivateKeyFile = keysDir </> "rsa"

aesKeyFile :: FilePath
aesKeyFile = keysDir </> "aes"

-- Algorithms

signAlg :: Jwt.JwtEncoding
signAlg = Jwt.JwsEncoding Jwa.RS512

encryptAlg :: Jwt.JwtEncoding
encryptAlg = Jwt.JweEncoding Jwa.A256KW Jwa.A256GCM

-- Reading, saving, generating keys

readRsaKeys :: IO (Either String (RSAPublicKey, RSAPrivateKey))
readRsaKeys = do
  priv <- Aeson.eitherDecodeStrict' <$> B.readFile rsaPrivateKeyFile
  pub <- Aeson.eitherDecodeStrict' <$> B.readFile rsaPublicKeyFile
  return $ do
    pubKey <- pub
    privKey <- priv
    return (RSAPublicKey pubKey, RSAPrivateKey privKey)

saveRsaKeys :: RSAPublicKey -> RSAPrivateKey -> IO ()
saveRsaKeys (RSAPublicKey pub) (RSAPrivateKey priv) = do
  Dir.createDirectoryIfMissing False keysDir
  BL.writeFile rsaPublicKeyFile (Aeson.encode pub)
  BL.writeFile rsaPrivateKeyFile (Aeson.encode priv)

genRsaKeys :: IO (RSAPublicKey, RSAPrivateKey)
genRsaKeys = do
  now <- getCurrentTime
  keys @ (pub, priv) <- bimap RSAPublicKey RSAPrivateKey <$> Jwk.generateRsaKeyPair 512 (Jwt.KeyId T.empty) Jwk.Sig Nothing -- 4096 bits
  saveRsaKeys pub priv
  return keys

-- | Reads the RSA key pair from file. If that fails (for instance because the files do not exist),
--   it generates a new key pair.
rsaKeys :: IO (RSAPublicKey, RSAPrivateKey)
rsaKeys =
  readRsaKeys `Exception.catch` handleException >>= either handleError return
  where
    handleException :: Exception.IOException -> IO (Either String (RSAPublicKey, RSAPrivateKey))
    handleException _ = Right <$> genRsaKeys

    handleError :: String -> IO (RSAPublicKey, RSAPrivateKey)
    handleError e = do
      putStrLn $ unlines [ "Failed to read RSA keys because " ++ e
                         , "Generating new RSA key pair..."
                         ]
      genRsaKeys

readAesKey :: IO (Either String AESKey)
readAesKey = Aeson.eitherDecodeStrict' <$> B.readFile aesKeyFile

saveAesKey :: AESKey -> IO ()
saveAesKey key = do
  Dir.createDirectoryIfMissing False keysDir
  BL.writeFile aesKeyFile $ Aeson.encode key

genAesKey :: IO AESKey
genAesKey = do
  now <- getCurrentTime
  key <- AESKey <$> Jwk.generateSymmetricKey 32 (Jwt.KeyId T.empty) Jwk.Enc Nothing -- 256 bits
  saveAesKey key
  return key

aesKey :: IO AESKey
aesKey = readAesKey `Exception.catch` handleException >>= either handleError return
  where
    handleException :: Exception.SomeException -> IO (Either String AESKey)
    handleException _ = Right <$> genAesKey

    handleError :: String -> IO AESKey
    handleError e = do
      putStrLn $ unlines [ "Failed to read AES key because " ++ e
                         , "Generating new AES key..."
                         ]
      genAesKey

-- | The meat and potatoes: encryption and decryption
createAuthTokenManager :: IO AuthTokenManager
createAuthTokenManager = do
  (pub, priv) <- rsaKeys
  sym <- aesKey
  let encrypt cookie = runExceptT $ do
        signed <- jwt $ Jwt.encode [getPrivateKey priv] signAlg (Jwt.Claims . BL.toStrict $ Aeson.encode cookie)
        Jwt.unJwt <$> (jwt $ Jwt.encode [getSymmetricKey sym] encryptAlg (Jwt.Nested signed))

      decrypt cookie = do
        verified <- runExceptT $ do
          decrypted <- jwt $ Jwt.decode [getSymmetricKey sym] Nothing cookie
          case decrypted of
            Jwt.Jwe (_header, payload) -> do
              verified <- jwt $ Jwt.decode [getPublicKey pub] (Just signAlg) payload
              case verified of
                Jwt.Jws (_header, contents) ->
                  return contents
                _                           ->
                  throwError $ T.pack "Could not verify the signature of a cookie. Ignoring."
            _                          ->
              throwError $ T.pack $ "Could not decrypt a cookie. Ignoring."
        return $ do
          contents <- verified
          decodeCookie contents
  return $ AuthTokenManager {
      encryptCookie = encrypt
    , decryptCookie = decrypt
    }

  where
    decodeCookie :: B.ByteString -> Either T.Text Cookie
    decodeCookie = either (Left . T.pack) Right . Aeson.eitherDecodeStrict'

jwt :: IO (Either Jwt.JwtError a) -> ExceptT T.Text IO a
jwt m = ExceptT (either (Left . errorToText) (Right . id) <$> m)
  where
    errorToText :: Jwt.JwtError -> T.Text
    errorToText = T.pack . show
    
