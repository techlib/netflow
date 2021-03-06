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
{-# LANGUAGE QuasiQuotes #-}

module Main (main)
where
  import BasePrelude hiding (getArgs, yield)

  import Database.PostgreSQL.Simple
  import Database.PostgreSQL.Simple.SqlQQ

  import Data.Time.Clock.POSIX
  import System.Console.GetOpt
  import System.Environment
  import Data.ByteString.Lazy (ByteString)
  import Data.Aeson
  import Data.HashSet (HashSet)
  import Data.Text (Text)
  import Pipes
  import Pipes.Concurrent

  import qualified Data.ByteString.Lazy as ByteString
  import qualified Data.HashMap.Lazy as HashMap
  import qualified Data.HashSet as HashSet

  import Network.Flow.Receive
  import Network.Flow.V9


  data Options
    = Options
      { func :: Options -> IO ()
      , pgdb :: Maybe String
      , host :: String
      , port :: String
      , brief :: Bool
      }


  defaults :: Options
  defaults = Options
              { func = mainCapture
              , pgdb = Nothing
              , host = "::"
              , port = "9800"
              , brief = False
              }


  fieldsInBriefOutput :: HashSet Text
  fieldsInBriefOutput =
    HashSet.fromList
      [ "FlowStartSysUpTime"
      , "FlowEndSysUpTime"
      , "SourceIPv6Address"
      , "DestinationIPv6Address"
      , "SourceIPv4Address"
      , "DestinationIPv4Address"
      , "PostNATSourceIPv4Address"
      , "PostNATDestinationIPv4Address"
      , "SourceTransportPort"
      , "DestinationTransportPort"
      , "PostNAPTSourceTransportPort"
      , "PostNAPTDestinationTransportPort"
      ]


  options :: [OptDescr (Options -> Options)]
  options =
    [ Option "h" ["help"]
             (NoArg (\opts -> opts { func = mainHelp }))
             "Show this help"

    , Option "V" ["version"]
             (NoArg (\opts -> opts { func = mainVersion }))
             "Show version information"

    , Option "b" ["brief"]
             (NoArg (\opts -> opts { brief = True }))
             "Log only the very basic metadata"

    , Option "H" ["host"]
             (ReqArg (\host' opts -> opts { host = host' }) "::")
             "Local address to receive flow datagrams"

    , Option "P" ["port"]
             (ReqArg (\port' opts -> opts { port = port' }) "9800")
             "Local port to receive flow datagrams"

    , Option "d" ["database"]
             (ReqArg (\pgdb' opts -> opts { pgdb = Just pgdb' }) "dsn")
             "PostgreSQL connection string"
    ]


  main :: IO ()
  main = do
    args <- getArgs
    opts <- parseOptions args

    (func opts) opts


  parseOptions :: [String] -> IO Options
  parseOptions argv = case getOpt Permute options argv of
    (actions, [], [])   -> return $ foldl' (.) id actions $ defaults
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
              >-> (if (brief opts) then pruneFields else cat)
              >-> jsonEncode
              >-> putLines


  mainCapturePG :: Options -> String -> IO ()
  mainCapturePG opts dsn = do
    conn <- connectPostgreSQL (fromString dsn)

    withReceiver (host opts) (port opts) $ \sock -> do
      (output, input) <- spawn (bounded 1000)

      _ <- forkIO $
        runEffect $ datagrams sock
                >-> decodeRecords
                >-> decodeFlows
                >-> (if (brief opts) then pruneFields else cat)
                >-> wrapInJust
                >-> toOutput output

      _ <- forkIO $
        runEffect $ tick 1000000 >-> toOutput output

      runEffect $ fromInput input
              >-> pgStore conn


  tick :: Int -> Producer (Maybe a) IO ()
  tick t = forever $ do
    lift $ threadDelay t
    yield Nothing


  wrapInJust :: Pipe a (Maybe a) IO ()
  wrapInJust = forever $ do
    x <- await
    yield $ Just x


  pgStore :: Connection -> Consumer (Maybe Flow) IO ()
  pgStore conn = do
    -- Begin the transaction.
    _ <- liftIO $ execute_ conn "begin"

    forever $ do
      -- Wait for either a flow or a cork signal.
      mf <- await

      case mf of
        Just (Flow time uptime _ _ scope fields) -> do
          let utcTime = posixSecondsToUTCTime $ fromIntegral time
          _ <- liftIO $ execute conn
                           [sql| insert into flow (time, uptime, scope, fields)
                                 values (?, ?, ?, ?) |]
                           (utcTime, uptime, scope, toJSON fields)
          return ()

        Nothing -> do
          _ <- liftIO $ execute_ conn "commit"
          _ <- liftIO $ execute_ conn "begin"
          return ()


  pruneFields :: (Monad m) => Pipe Flow Flow m ()
  pruneFields = forever $ await >>= (yield . pruneFields')


  pruneFields' :: Flow -> Flow
  pruneFields' flow = flow {flowFields = filtered (flowFields flow)}
    where filtered = HashMap.filterWithKey isWanted
          isWanted k _ = HashSet.member k fieldsInBriefOutput


  jsonEncode :: (ToJSON a) => Pipe a ByteString IO ()
  jsonEncode = forever $ await >>= (yield . encode)


  putLines :: Consumer ByteString IO ()
  putLines = forever $ do
    item <- await
    lift $ ByteString.putStr item
    lift $ ByteString.putStr "\n"


-- vim:set ft=haskell sw=2 ts=2 et:
