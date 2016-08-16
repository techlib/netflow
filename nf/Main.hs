{-|
Module      :  Main
Description :  The NetFlow Tool
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main)
where
  import BasePrelude hiding (getArgs, yield)

  import Database.PostgreSQL.Simple
  import Data.Time.Clock.POSIX
  import System.Console.GetOpt
  import System.Environment
  import Data.Aeson
  import Pipes

  import qualified Data.ByteString.Lazy as BL

  import Network.Flow.Receive
  import Network.Flow.V9


  data Options
    = Options
      { func :: Options -> IO ()
      , pgdb :: Maybe String
      , host :: String
      , port :: String }


  defaults :: Options
  defaults = Options
              { func = mainCapture
              , pgdb = Nothing
              , host = "::"
              , port = "9800" }


  options :: [OptDescr (Options -> Options)]
  options =
    [ Option "h" ["help"]
             (NoArg (\opts -> opts { func = mainHelp }))
             "Show this help"

    , Option "V" ["version"]
             (NoArg (\opts -> opts { func = mainVersion }))
             "Show version information"

    , Option "H" ["host"]
             (ReqArg (\host' opts -> opts { host = host' }) "::")
             "Local address to receive flow datagrams"

    , Option "P" ["port"]
             (ReqArg (\port' opts -> opts { port = port' }) "9800")
             "Local port to receive flow datagrams"

    , Option "d" ["database"]
             (ReqArg (\pgdb' opts -> opts { pgdb = Just pgdb' }) "dsn")
             "PostgreSQL connection string" ]


  main :: IO ()
  main = do
    args <- getArgs
    opts <- parseOptions args

    (func opts) opts


  parseOptions :: [String] -> IO Options
  parseOptions argv = case getOpt Permute options argv of
    (actions, [], [])   -> return $ foldl (.) id actions $ defaults
    (_, _, [])          -> die "Too many arguments."
    (_, _, (e:_))       -> die e


  mainHelp :: Options -> IO ()
  mainHelp _opts = do
    prog <- getProgName

    let header = "Usage: " <> prog <> " [--host ::] [--port 9800]\n\
                 \Capture NetFlow data and dump them as JSON objects.\n\
                 \\n\
                 \Use `--database` to specify a PostgreSQL connection if you wish\n\
                 \to store flows there instead. See the documentation for details.\n\n\
                 \OPTIONS:"

    putStrLn $ usageInfo header options
    putStrLn "Report bugs to <helpdeskict@techlib.cz>."


  mainVersion :: Options -> IO ()
  mainVersion _opts = do
    prog <- getProgName
    putStrLn $ prog <> " (NTK) 1"


  mainCapture :: Options -> IO ()
  mainCapture opts = case (pgdb opts) of
                       Nothing  -> mainCaptureJSON opts
                       Just dsn -> mainCapturePG opts dsn


  mainCaptureJSON :: Options -> IO ()
  mainCaptureJSON opts = do
    withReceiver (host opts) (port opts) $ \sock ->
      runEffect $ datagrams sock
              >-> decodeRecords
              >-> decodeFlows
              >-> jsonEncode
              >-> putLines


  mainCapturePG :: Options -> String -> IO ()
  mainCapturePG opts dsn = do
    conn <- connectPostgreSQL (fromString dsn)

    withReceiver (host opts) (port opts) $ \sock ->
      runEffect $ datagrams sock
              >-> decodeRecords
              >-> decodeFlows
              >-> pgStore conn


  pgStore :: Connection -> Consumer Flow IO ()
  pgStore conn = forever $ do
    (Flow time uptime _ _ scope fields) <- await

    let utcTime = posixSecondsToUTCTime $ fromIntegral time
    let q = "insert into flow (time, uptime, scope, fields) values (?, ?, ?, ?)"

    liftIO $ execute conn q (utcTime, uptime, scope, toJSON fields)


  jsonEncode :: (ToJSON a) => Pipe a BL.ByteString IO ()
  jsonEncode = forever $ await >>= (yield . encode)


  putLines :: Consumer BL.ByteString IO ()
  putLines = forever $ do
    item <- await
    lift $ BL.putStr item
    lift $ BL.putStr "\n"


-- vim:set ft=haskell sw=2 ts=2 et:
