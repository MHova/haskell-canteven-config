{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{- |
  Utilities to help automate yaml file based program configuration.
-}
module Canteven.Config (
  canteven,
  loadConfig
) where


import Data.Yaml (FromJSON, decodeFileEither)
import System.Console.GetOpt(ArgOrder(Permute), ArgDescr(ReqArg, NoArg),
  OptDescr(Option), getOpt, usageInfo)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Posix.Process (exitImmediately)


canteven :: (FromJSON config) => IO config
canteven = loadConfig =<< parseOptions


{- |
  Defines command line options.
-}
data Opt = Help
         | Config String
         deriving Show


{- |
  Load the configuration from YAML.
-}
loadConfig :: FromJSON a => FilePath -> IO a
loadConfig path = do
  eUserConfig <- decodeFileEither path
  case eUserConfig of
    Left errorMsg -> error $
      "Couldn't decode YAML config from file "
      ++ path ++ ": " ++ show errorMsg
    Right config -> return config


{- |
  Parse command line options.
-}
parseOptions :: IO String
parseOptions = do
  args <- getArgs
  case getOpt Permute options args of
    ([], [], []) ->
                 return "config.yml"
    ([Config config], _, []) ->
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
