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

module Main (main)
where
  import BasePrelude hiding (getArgs)

  {-
  import Data.ByteString (ByteString)
  import Network.Socket
  -}

  import System.Console.GetOpt
  import System.Environment
  import Pipes

  import GHC.IO.Exception as Exn

  import Network.Flow.Receive
  import Network.Flow.V9


  data Options
    = Options
      { func :: Options -> IO ()
      , path :: Maybe String
      , host :: String
      , port :: String }


  defaults :: Options
  defaults = Options
              { func = mainCapture
              , path = Nothing
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
             "Address of a local interface to receive datagrams on."

    , Option "P" ["port"]
             (ReqArg (\port' opts -> opts { port = port' }) "9800")
             "Local port to receive datagrams on."

    , Option "p" ["prefix"]
             (ReqArg (\path' opts -> opts { path = Just path' }) "path")
             "Prefix of output file names." ]


  parseOptions :: [String] -> IO Options
  parseOptions argv = case getOpt Permute options argv of
    (actions, [], [])   -> return $ foldl (.) id actions $ defaults
    (_, _, [])          -> die "Too many arguments."
    (_, _, (e:_))       -> die e


  mainHelp :: Options -> IO ()
  mainHelp _opts = do
    prog <- getProgName

    let header = "Usage: " <> prog <> " [--prefix path] [--host ::] [--port 9800]\n\
                 \Capture NetFlow data and store them as plain text.\n\nOPTIONS:"

    putStrLn $ usageInfo header options
    putStrLn "Report bugs to <helpdeskict@techlib.cz>."


  mainVersion :: Options -> IO ()
  mainVersion _opts = do
    prog <- getProgName
    putStrLn $ prog <> " (NTK) 1"


  mainCapture :: Options -> IO ()
  mainCapture opt = withReceiver (host opt) (port opt) receive
    where receive sock = runEffect $ datagrams sock >-> decodeRecords >-> decodeFlows >-> showItems


  showItems :: (Show a) => Consumer a IO ()
  showItems = do
    -- Consume a single item.
    item <- await

    -- Try to output it.
    x <- lift $ try $ putStrLn (show item)

    case x of
      -- Gracefully terminate if we got a broken pipe error.
      Left e@(Exn.IOError {Exn.ioe_type = t}) ->
        lift $ unless (t == Exn.ResourceVanished) $ throwIO e

      -- Otherwise loop...
      Right () -> showItems


  main :: IO ()
  main = do
    args <- getArgs
    opts <- parseOptions args

    (func opts) opts


-- vim:set ft=haskell sw=2 ts=2 et:
