{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server (main) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Except (ExceptT, MonadError, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Data.Maybe (fromMaybe, listToMaybe)
import Network.HTTP.Simple hiding (Proxy)
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Aeson.Types (FromJSON)
import Data.Text (Text, splitOn, unpack, pack, replace)
import GHC.Generics (Generic)
import Servant
import Servant.API (ReqBody)
import Servant.Server (err400, err404, err409)
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Data.Vector
import Data.Pool
import Data.ByteString (ByteString)
import Data.Int (Int64)
--import qualified GHC.IsList as Vector
import Network.URL
import Crypto.Hash.MD5 (hash)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BS
import Network.Wai.Middleware.Cors
import System.Environment (getEnv)

import Models
import HuggingFaceDetection
import App

type ImagesAPI =
    "images"  :>
       (       QueryParam "objects" Text :> Get '[JSON] [Image]
          :<|> Capture "imageId" Int :> Get '[JSON] Image
          :<|> ReqBody '[JSON] ImageCreateRequest :> Post '[JSON] Image
       )

imageAPIProxy :: Proxy ImagesAPI
imageAPIProxy = Proxy

mkServer :: Env -> Server ImagesAPI
mkServer env = hoistServer imageAPIProxy nt imageCatalogServer
  where
    nt :: AppT IO a -> Handler a
    nt app = Handler $ runReaderT (runAppT @IO app) env

mkApplication :: Env -> Application
mkApplication env = serve imageAPIProxy (mkServer env)

initConnectionPool :: ConnectInfo -> IO (Pool Connection)
initConnectionPool info =
  createPool (connect info)
             close
             2 -- stripes
             60 -- unused connections are kept open for a minute
             10 -- max. 10 connections open per stripe

corsPolicyToSupportSwaggerUI :: CorsResourcePolicy
corsPolicyToSupportSwaggerUI = simpleCorsResourcePolicy
  { corsOrigins = Just (["http://localhost:3000"], True)
  , corsRequestHeaders = ["Content-Type"]
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsExposedHeaders = Just ["Access-Control-Allow-Origin", "Access-Control-Allow-Credentials"]
  , corsMaxAge = Just 3600
  , corsVaryOrigin = False
  , corsRequireOrigin = False
  , corsIgnoreFailures = False
  }

corsMiddleware = cors (const $ Just corsPolicyToSupportSwaggerUI)

main :: IO ()
main = do
  -- Load Environment variables
  let hostVar = "PG_HOST"
      dbVar   = "PG_DATABASE"
      userVar = "PG_USER"
      hfTokVar = "HF_TOKEN"

  pgHost <- getEnv hostVar
  dbName  <- getEnv dbVar
  user   <- getEnv userVar
  hfTok  <- getEnv hfTokVar

  let connInfo = defaultConnectInfo
        { connectHost = pgHost 
        , connectUser = user
        , connectDatabase = dbName
        , connectPassword = "" -- TODO Make the password a value read getEnv
        }


  pool <- initConnectionPool connInfo 
  run 8080 $ corsMiddleware $ mkApplication (Env pool hfTok Info)

imageCatalogServer :: MonadIO m => ServerT ImagesAPI (AppT m)
imageCatalogServer = getAllHandler :<|> getSingleHandler  :<|> postImageHandler

getAllHandler :: MonadIO m => Maybe Text -> AppT m [Image]
getAllHandler objectsParameter = do
    pool <- asks configConnection
    liftIO $ withResource pool $ \conn ->
            runAllQuery conn objectsParameter

runAllQuery :: Connection -> Maybe Text -> IO [Image]
runAllQuery conn Nothing        = query_ conn "SELECT id, url, label, objects FROM images"
 -- NOTE we use && in this query, meaning we return the image if there is an intersection between the images objects and the query objects
 -- but if we want logic that requires all the query objects to be found in the images objects, then we will use contains 
 -- operator:  ... where objects @> ?
runAllQuery conn (Just os) = query conn  "SELECT id, url, label, objects FROM images where objects && ?" [pgArray]
    where pgArray :: PGArray Text
          pgArray = PGArray {fromPGArray = splitOn "," os}

getSingleRecord :: Int -> Connection -> IO (Maybe Image)
getSingleRecord imageId conn = do
    (is :: [Image]) <- query conn "SELECT id, url, label, objects FROM images where id = ?" [imageId]
    return (listToMaybe is)

getSingleRecordByImageUrl :: Text -> Connection -> IO (Maybe Image)
getSingleRecordByImageUrl imageUrl conn = do
    (is :: [Image]) <- query conn "SELECT id, url, label, objects FROM images where url = ?" [imageUrl]
    return (listToMaybe is)

getSingleHandler :: MonadIO m => Int -> AppT m Image
getSingleHandler imageId = do
    pool <- asks configConnection
    mi <- liftIO $ withResource pool $ getSingleRecord imageId
    logInfo $ "Found this in the database for ID = " <> (pack $ show imageId) <> ": " <> (pack $ show mi)
    case mi of
        Just i -> pure i
        Nothing -> throwError $ err404 { errBody = "Image doesnt exist"}

isAbsoluteUrl :: Text -> Bool
isAbsoluteUrl t =
    let maybeUrl = importURL (unpack $ t)
    in case maybeUrl of
        Just (URL (Absolute h) _ _) -> True
        _ -> False

postImageHandler :: MonadIO m => ImageCreateRequest -> AppT m Image
postImageHandler icr = do
    if isAbsoluteUrl (imageUrl icr) then
        do
            pool <- asks configConnection
            hfToken <- asks huggingfaceToken
            (maybeExisting :: Maybe Image) <- liftIO $ withResource pool $ getSingleRecordByImageUrl (imageUrl icr)
            case maybeExisting of
                Just _ -> throwError $ err409 { errBody = "Image already exists for that url"}
                Nothing -> liftIO $ withResource pool $ \conn ->
                            do
                               let labelToUse :: Text
                                   -- Use the provided imageLabel as the label, otherwise if not provided, use hex-encoded md5 hash of imageUrl as a default
                                   labelToUse = fromMaybe (replace "\"" "" $ pack $ show $ B16.encode (Data.ByteArray.convert $ hash (encodeUtf8 $ imageUrl icr))) (imageLabel icr)
                               os <- if (enableObjectDetection icr) then getDetectedObjects (unpack $ imageUrl icr) hfToken else pure []
                               let tup = (imageUrl icr, labelToUse, (PGArray os) :: PGArray Text)
                               (ids :: [Only Int])  <- returning conn "insert into images (url, label, objects) values (?, ?, ?) returning id" [tup]
                               pure (Image (fromOnly $ Prelude.head ids) (imageUrl icr) labelToUse (fromList os))
    else
        throwError $ err400 { errBody = "imageUrl is not a valid Url"}
