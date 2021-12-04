{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, LambdaCase #-}
module Config where

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.Monoid
import Data.Typeable
import Data.Yaml
import System.IO
import System.IO.Error
import Data.Text (Text)
import Data.ByteString (append, ByteString)
import Data.ByteString.Char8 (pack)
import qualified Data.ByteString as B
import qualified Data.Text as T
import System.FilePath ((</>))
import System.Directory (copyFile)

fileName :: String
fileName = "config.yaml"

data PongGameConfig = PongGameConfig
  {  pgcScorePlayer1 :: Int
  ,  pgcScorePlayer2 :: Int
  } deriving (Eq, Show)

data Configuration = Configuration
  {  configurationPath :: FilePath
  ,  configurationVar  :: MVar PongGameConfig
  }

newConfiguration :: FilePath -> IO Configuration
newConfiguration path = do
  configValue <- readConfiguration path
  configVar <- newMVar configValue
  return Configuration
    {  configurationPath = path
    ,  configurationVar = configVar
    }

scorePlayer1 :: Text
scorePlayer1 = "score-player-1"

scorePlayer2 :: Text
scorePlayer2 = "score-player-2"

programVersion :: String
programVersion = "1.0.0"

instance FromJSON PongGameConfig where
  parseJSON (Object v) = PongGameConfig
    <$> v .:  scorePlayer1
    <*> v .:  scorePlayer2
  parseJSON _ = mzero

instance ToJSON PongGameConfig where
  toJSON x = object
    [ scorePlayer1 .= pgcScorePlayer1 x
    , scorePlayer2 .= pgcScorePlayer2 x
    ]

data ConfigurationFileError
  = ConfigurationFileTooLarge
  | ConfigurationFileOther SomeException
  deriving (Show, Typeable)

instance Exception ConfigurationFileError

configurationFileSizeLimit :: Int
configurationFileSizeLimit = 32 * 1024 -- 32 kilobytes

-- This function only reads a configuration file and decodes data.
-- If the file does not exist it throws the DoesNotExist IOError.
readConfiguration :: (FromJSON a) => FilePath -> IO a
readConfiguration path = do
  try (withFile path ReadMode readHandle) >>= \case
    Left e -> do
      throwIO $ ConfigurationFileOther e
    Right x ->
      return x

readConfigurationGeneric :: (FromJSON a, ToJSON a) => FilePath -> IO a
readConfigurationGeneric path = do
  r <- tryJust (guard . isDoesNotExistError) $ withFile path ReadMode readHandle
  case r of
    Left _ -> do
      copyDefault
      r' <- try $ withFile path ReadMode readHandle
      case r' of
        Left e -> do
          throw $ ConfigurationFileOther e
        Right x ->
          writeConfigurationSilent path x
          >> return x
    Right x ->
      writeConfigurationSilent path x
      >> return x
  where
    copyDefault = do
      let defaultName = path <> ".default"
      copyFile defaultName path

readHandle :: (FromJSON a) => Handle -> IO a
readHandle h = do
  sz <- hFileSize h
  if sz > fromIntegral configurationFileSizeLimit
    then throwIO ConfigurationFileTooLarge
    else B.hGetContents h >>= \bs -> case decodeEither' bs of
      Left e -> throwIO . ConfigurationFileOther $ SomeException e
      Right x -> return x

writeConfiguration :: ToJSON a => FilePath -> a -> IO ()
writeConfiguration path val = do
  writeConfigurationSilent path val
  print("Wrote configuration to: " <> T.pack path)

writeConfigurationSilent :: ToJSON a => FilePath -> a -> IO ()
writeConfigurationSilent path = B.writeFile path . addVersion . encode where
  addVersion :: ByteString -> ByteString
  addVersion = append . pack $ "# Version " ++ programVersion ++ "\n"
