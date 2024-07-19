{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module HuggingFaceDetection (getDetectedObjects) where

import GHC.Generics (Generic)
import Data.Text

import Network.HTTP.Simple hiding (Proxy)
import Data.Aeson
import qualified Data.ByteString.Char8 as BS

data Box = Box {
  xmin  :: Int,
  ymin  :: Int,
  xmax  :: Int,
  ymax  :: Int
} deriving (Show, Generic)

instance FromJSON Box 

data Detection = Detection {
  score :: Float,
  detectionLabel :: Text,
  box   :: Box
} deriving (Show, Generic)

instance FromJSON Detection where
    parseJSON = withObject "Detection" $ \v -> Detection
        <$> v .: "score"
        <*> v .: "label"
        <*> v .: "box"

--  $ curl https://api-inference.huggingface.co/models/facebook/detr-resnet-50 \
-- -X POST \
-- -H "Authorization: Bearer " \
-- -H "Content-Type: application/json" \
-- -d '{"inputs": "https://ava-pictures.s3.amazonaws.com/chloe_1_post.jpg"}'
-- 
-- [{"score":0.8257198929786682,"label":"teddy bear","box":{"xmin":279,"ymin":108,"xmax":391,"ymax":277}},{"score":0.9540954828262329,"label":"dining table","box":{"xmin":0,"ymin":1,"xmax":711,"ymax":523}},{"score":0.9854941368103027,"label":"cup","box":{"xmin":425,"ymin":79,"xmax":608,"ymax":259}}]%  

runDetectionRequest :: String -> String -> IO (Either String [Detection])
runDetectionRequest imageUrl bearerToken = do
    -- Create the initial request
    initialRequest <- parseRequest "https://api-inference.huggingface.co/models/facebook/detr-resnet-50"
    
    -- Modify the request
    let request = setRequestMethod "POST"
                $ setRequestBodyLBS (encode $ object [ "inputs" .= imageUrl ])
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestHeader "Authorization" [BS.pack $ "Bearer " Prelude.++ bearerToken]
                $ initialRequest
    
    -- Send the request and get the response
    response <- httpLBS request
    
    -- Handle the response
    let responseBody = getResponseBody response
    return $ eitherDecode responseBody

getDetectedObjects :: String -> String -> IO [Text]
getDetectedObjects imageUrl bearerToken = do
    e <- runDetectionRequest imageUrl bearerToken
    case e of
        Right ts -> return (fmap detectionLabel ts)
        Left  s -> do
            putStrLn $ "Error parsing response from huggingface" Prelude.++ s
            return []
