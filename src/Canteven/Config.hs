{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{- |
  Utilities to help automate yaml file based program configuration.
-}
module Canteven.Config (
  canteven
) where


import Control.Applicative ((<$>), liftA2)
import Data.Aeson (Value(String, Object), (.:?), (.!=), (.:))
import Data.Maybe (fromMaybe)
import Data.Yaml (FromJSON(parseJSON), decodeFileEither)
import System.Console.GetOpt(ArgOrder(Permute), ArgDescr(ReqArg, NoArg),
  OptDescr(Option), getOpt, usageInfo)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.FilePath (dropFileName)
import System.IO (stdout)
import System.Log (Priority(INFO, DEBUG))
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Logger (updateGlobalLogger, setHandlers, setLevel)
import System.Posix.Process (exitImmediately)
import qualified Data.Text as T


canteven :: (FromJSON config) => IO config
canteven = do
  configPath <- parseOptions
  (userConfig, systemConfig) <- loadConfig configPath
  setupLogging systemConfig
  return userConfig


{- |
  Defines all the "system" config, where "system" means everything that
  Moonshine knows about.
-}
data SystemConfig =
  SystemConfig {
    logging :: Maybe LoggingConfig
  }

instance FromJSON SystemConfig where
  parseJSON (Object topLevel) = do
    logging <- topLevel .:? "logging"
    return SystemConfig {logging}
  parseJSON value =
    fail $ "Couldn't parse system config from value " ++ show value


{- |
  Defines command line options.
-}
data Opt = Help
         | Config String
         deriving Show


data LoggingConfig =
  LoggingConfig {
    level :: LogPriority,
    logfile :: Maybe FilePath,
    loggers :: [LoggerDetails]
  }

instance FromJSON LoggingConfig where
  parseJSON (Object logging) = do
    level <- logging .:? "level" .!= LP INFO
    logfile <- logging .:? "logfile"
    loggers <- logging .:? "loggers" .!= []
    return LoggingConfig {level, logfile, loggers}
  parseJSON value =
    fail $ "Couldn't parse logging config from value " ++ show value


setupLogging :: SystemConfig -> IO ()
setupLogging SystemConfig {logging} =
  installLoggingConfig (fromMaybe defaultLoggingConfig logging)


defaultLoggingConfig :: LoggingConfig
defaultLoggingConfig = LoggingConfig {
  level = LP INFO,
  logfile = Nothing,
  loggers = []
  }


{- |
  Do all of the things that it takes to get logging set up the way we
  want it.
-}
installLoggingConfig :: LoggingConfig -> IO ()
installLoggingConfig LoggingConfig {logfile, level = LP level, loggers} = do
  fileHandlers <-
    case logfile of
      Nothing -> return []
      Just filename -> do
        createDirectoryIfMissing True (dropFileName filename)
        file <- tweak <$> fileHandler filename DEBUG
        return [file]

  console <- tweak <$> streamHandler stdout DEBUG
  let handlers = console:fileHandlers
  updateGlobalLogger "" (setLevel level . setHandlers handlers)
  sequence_ [
      updateGlobalLogger loggerName (setLevel loggerLevel) |
      LoggerDetails {loggerName, loggerLevel = LP loggerLevel} <- loggers
    ]
  where
    tweak h = setFormatter h (simpleLogFormatter logFormat)
    logFormat = "$prio [$tid] [$time] $loggername - $msg"


{- |
  Load the configuration from YAML.
-}
loadConfig :: FromJSON a => FilePath -> IO (a, SystemConfig)
loadConfig path = do
  eUserConfig <- decodeFileEither path
  eSystemConfig <- decodeFileEither path
  case liftA2 (,) eUserConfig eSystemConfig of
    Left errorMsg -> error $
      "Couldn't decode YAML config from file "
      ++ path ++ ": " ++ show errorMsg
    Right configs -> return configs


{- |
  Parse command line options.
-}
parseOptions :: IO String
parseOptions = do
  args <- getArgs
  case getOpt Permute options args of
    ([], [], []) ->
                 return "config.yml"
    ([Config config], [], []) ->
                 return config
    ([Help], [], []) -> do
                 usage
                 exitImmediately ExitSuccess
                 error "can't reach this statement"
    (_, _, []) -> do
                 usage
                 die
    (_, _, errors) -> do
                 mapM_ putStr errors
                 usage
                 die
  where
    options =
      [ Option "c" ["config"] (ReqArg Config "FILE") "specify configuration file"
      , Option "h" ["help"]   (NoArg Help)           "display help and exit"
      ]
    usage = do
      progName <- getProgName
      putStr $ usageInfo ("Usage: " ++ progName ++ " -c FILE") options
    die = do
      exitImmediately (ExitFailure 1)
      error "can't reach this statement"


{- |
  A wrapper for Priority, so we can avoid orphan instances
-}
newtype LogPriority = LP Priority

instance FromJSON LogPriority where
  parseJSON (String s) = case reads (T.unpack s) of
    [(priority, "")] -> return (LP priority)
    _ -> fail $ "couldn't parse Priority from string " ++ show s
  parseJSON value = fail $ "Couldn't parse Priority from value " ++ show value


{- |
  A way to set more fined-grained configuration for specific loggers.
-}
data LoggerDetails =
  LoggerDetails {
    loggerName :: String,
    loggerLevel :: LogPriority
  }

instance FromJSON LoggerDetails where
  parseJSON (Object details) = do
    loggerName <- details .: "logger"
    loggerLevel <- details .: "level"
    return LoggerDetails {loggerName, loggerLevel}
  parseJSON value =
    fail $ "Couldn't parse logger details from value " ++ show value


