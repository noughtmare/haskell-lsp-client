{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import System.IO
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import qualified Language.Haskell.LSP.TH.ClientCapabilities as LSP
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.Process
import Control.Exception (bracket, handle, IOException)
import Control.Monad
import Control.Concurrent
import Compat (getPID)
import qualified Data.IntMap as M
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Proxy
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Control.Applicative
import Data.Maybe (fromJust)
import System.Exit (exitFailure)

-- Using concrete types to communicate via MVars, because ghc doesn't support impredicative polymorphism
-- https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism
-- e.g. `MVar (forall a. a)` is impossible.

data ClientCommunication where
  ClientRequest :: forall params resp. (ToJSON params, ToJSON resp, FromJSON resp)
                => LSP.ClientMethod
                -> params
                -> (MVar (Either LSP.ResponseError resp))
                -> ClientCommunication
  ClientNotification :: forall params . (ToJSON params)
                     => LSP.ClientMethod
                     -> params
                     -> ClientCommunication


data Response where
  Response :: forall resp. (ToJSON resp, FromJSON resp)
           => Either LSP.ResponseError resp
           -> Response

sendClientRequest :: forall params resp . (ToJSON params, ToJSON resp, FromJSON resp)
                  => Proxy (LSP.RequestMessage LSP.ClientMethod params resp)
                  -> MVar ClientCommunication
                  -> LSP.ClientMethod
                  -> params
                  -> IO (Either LSP.ResponseError resp)
sendClientRequest Proxy reqVar method params = do
  respVar <- newEmptyMVar :: IO (MVar (Either LSP.ResponseError resp))
  putMVar reqVar (ClientRequest method params respVar)
  takeMVar respVar

sendClientNotification :: forall params . (ToJSON params)
                        => MVar ClientCommunication
                        -> LSP.ClientMethod
                        -> params
                        -> IO ()
sendClientNotification reqVar method params =
  putMVar reqVar (ClientNotification method params)

main :: IO ()
main = do
  pid <- getPID

  let caps = LSP.ClientCapabilities (Just workspaceCaps) (Just textDocumentCaps) Nothing
      workspaceCaps = LSP.WorkspaceClientCapabilities
        (Just False)
        (Just (LSP.WorkspaceEditClientCapabilities (Just False)))
        (Just (LSP.DidChangeConfigurationClientCapabilities (Just False)))
        (Just (LSP.DidChangeWatchedFilesClientCapabilities (Just False)))
        (Just (LSP.SymbolClientCapabilities (Just False)))
        (Just (LSP.ExecuteClientCapabilities (Just False)))
      textDocumentCaps = LSP.TextDocumentClientCapabilities
        (Just (LSP.SynchronizationTextDocumentClientCapabilities
                 (Just False)
                 (Just False)
                 (Just False)
                 (Just False)))
        (Just (LSP.CompletionClientCapabilities
                 (Just False)
                 (Just (LSP.CompletionItemClientCapabilities (Just False)))))
        (Just (LSP.HoverClientCapabilities (Just False)))
        (Just (LSP.SignatureHelpClientCapabilities (Just False)))
        (Just (LSP.ReferencesClientCapabilities (Just False)))
        (Just (LSP.DocumentHighlightClientCapabilities (Just False)))
        (Just (LSP.DocumentSymbolClientCapabilities (Just False)))
        (Just (LSP.FormattingClientCapabilities (Just False)))
        (Just (LSP.RangeFormattingClientCapabilities (Just False)))
        (Just (LSP.OnTypeFormattingClientCapabilities (Just False)))
        (Just (LSP.DefinitionClientCapabilities (Just False)))
        (Just (LSP.CodeActionClientCapabilities (Just False)))
        (Just (LSP.CodeLensClientCapabilities (Just False)))
        (Just (LSP.DocumentLinkClientCapabilities (Just False)))
        (Just (LSP.RenameClientCapabilities (Just False)))

      initializeParams :: LSP.InitializeParams
      initializeParams = (LSP.InitializeParams (Just pid) Nothing Nothing Nothing caps Nothing)

  reqVar <- start

  sendClientRequest (Proxy :: Proxy LSP.InitializeRequest) reqVar LSP.Initialize initializeParams
    >>= print
  sendClientNotification reqVar LSP.Initialized (Just LSP.InitializedParams)
  sendClientRequest (Proxy :: Proxy LSP.ShutdownRequest) reqVar LSP.Shutdown Nothing
    >>= print
  sendClientNotification reqVar LSP.Exit (Just LSP.ExitParams)

data ResponseVar where
  ResponseVar :: forall resp . FromJSON resp => MVar (Either LSP.ResponseError resp) -> ResponseVar

start :: IO (MVar ClientCommunication)
start = handle (\(e :: IOException) -> hPutStrLn stderr (show e) >> exitFailure >> undefined) $ do
  (Just inp, Just out, Just err, pid) <- createProcess (proc "hie" ["--lsp"])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering

  req <- newEmptyMVar :: IO (MVar ClientCommunication)

  requestMap <- newMVar mempty :: IO (MVar (M.IntMap ResponseVar))

  -- the receiving thread
  forkIO $ forever $ do
    headers <- getHeaders out
    case lookup "Content-Length" headers >>= readMaybe of
      -- FIXME: Think of some way to recover from malformed headers.
      -- It might just be best to restart the server and try again.
      Nothing -> error "Couldn't read Content-Length header"
      Just size -> do
        message <- B.hGet out size

        let choice :: Alternative f => [f a] -> f a
            choice (a:as) = a <|> choice as

        choice [ case decode message :: Maybe (LSP.ResponseMessage Value) of
                   Just resMsg@(LSP.ResponseMessage _ (LSP.IdRspInt lspId) mayResult mayError) -> do
                     mayResVar <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
                     case mayResVar of
                       Nothing -> error $ "Server send us an unknown id of type Int" ++ "\n" ++ B.unpack message
                       Just (ResponseVar (resVar :: FromJSON resp => MVar (Either LSP.ResponseError resp))) ->
                         case mayResult of
                           Nothing -> putMVar resVar $ Left $ fromJust mayError
                           Just result -> putMVar resVar $ Right (fromSuccess $ fromJSON result :: resp)
                   Just (LSP.ResponseMessage _ (LSP.IdRspString _) _ _) -> error "Server send us an unknown id of type String"
                   Just (LSP.ResponseMessage _ LSP.IdRspNull _ _) -> error "Server couldn't read our id"
                   _ -> undefined
               -- , case decode message of
               --     Just (LSP.RequestMessage _ _ _ _) -> do
               --       -- TODO: Handle server requests
               --       return ()
               --     _ -> undefined
               -- , case decode message of
               --     Just (LSP.NotificationMessage _ _ _) -> do
               --       -- TODO: Handle server notifications
               --       return ()
               --     _ -> undefined
               , error "Malformed LSP response message"
               ]

  -- the sending thread
  forkIO $ forever $ do
    clientMessage <- takeMVar req
    case clientMessage of
       (ClientRequest method (req :: req) (respVar :: (ToJSON resp, FromJSON resp) => MVar (Either LSP.ResponseError resp))) -> do
          lspId <- head . (\m -> filter (`M.notMember` m) [minBound..]) <$> readMVar requestMap
          putStrLn $ "We send request with lspId: " ++ show lspId
          B.hPutStr inp (addHeader (encode (LSP.RequestMessage "2.0" (LSP.IdInt lspId) method req :: LSP.RequestMessage LSP.ClientMethod req resp)))
          modifyMVar_ requestMap $ return . M.insert lspId (ResponseVar respVar)
       (ClientNotification method req) -> B.hPutStr inp (addHeader (encode (LSP.NotificationMessage "2.0" method req)))

  return req
  where
    getHeaders :: Handle -> IO [(String,String)]
    getHeaders h = do
      l <- hGetLine h
      let (name,val) = span (/= ':') l
      if null val
        then return []
        else ((name,drop 2 val) :) <$> getHeaders h

    addHeader :: B.ByteString -> B.ByteString
    addHeader content = B.concat
      [ "Content-Length: ", B.pack $ show $ B.length content, "\r\n"
      , "\r\n"
      , content
      ]

    fromSuccess (Success a) = a
