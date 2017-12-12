{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : LSP.Client
-- Description : A client implementation for the Language Server Protocol.
-- Copyright   : (c) Jaro Reinders, 2017
-- License     : GPL-2
-- Maintainer  : jaro.reinders@gmail.com
-- Stability   : experimental
-- 
-- This module contains an implementation of a client for the 
-- <https://github.com/Microsoft/language-server-protocol Language Server Protocol>. 
-- It uses the same data types as the 
-- <https://hackage.haskell.org/package/haskell-lsp haskell-lsp> library.
-- 
-- This client is intended to be used by text editors written in haskell 
-- to provide the user with IDE-like features.
-- 
-- TODO:
--   
--   * Implement proper exception handling.
-- 
--   * More documentation.
-- 
--   * Handle request messages from the server.
-- 
--   * Implement proper communication with the LSP-server process.
-- 
--   * Make all functions total (except the forever loops).

module LSP.Client 
  ( sendClientRequest
  , sendClientNotification
  , start
  , NotificationMessageHandler (..)
  ) where

import Control.Lens
import System.IO
import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import qualified Language.Haskell.LSP.TH.ClientCapabilities as LSP
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import System.Process
import Control.Exception (handle, IOException)
import Control.Monad (forever)
import Control.Concurrent
import qualified Data.IntMap as M
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Text.Read (readMaybe)
import Control.Arrow ((&&&))
import Control.Applicative (Alternative (..))
import Data.Maybe (fromJust)
import System.Exit (exitFailure)
import qualified Data.Text.IO as T
import Control.Exception (SomeException)

--------------------------------------------------------------------------------
-- These should be added to Language.Haskell.LSP.TH.DataTypesJSON

instance Traversable LSP.List where
  traverse f (LSP.List l) = LSP.List <$> traverse f l

instance Foldable LSP.List where
  foldMap f (LSP.List l) = foldMap f l

--------------------------------------------------------------------------------
-- The types

data ClientMessage 
  = forall params resp. (ToJSON params, ToJSON resp, FromJSON resp) 
    => ClientRequest LSP.ClientMethod params (MVar (Either LSP.ResponseError resp))
  | forall params. (ToJSON params) 
    => ClientNotification LSP.ClientMethod params

data ResponseVar = forall resp . FromJSON resp => 
  ResponseVar (MVar (Either LSP.ResponseError resp))

--------------------------------------------------------------------------------
-- Sending a client request

sendClientRequest 
  :: forall params resp . (ToJSON params, ToJSON resp, FromJSON resp)
  => Proxy (LSP.RequestMessage LSP.ClientMethod params resp)
  -> MVar ClientMessage
  -> LSP.ClientMethod
  -> params
  -> IO (Either LSP.ResponseError resp)
sendClientRequest Proxy reqVar method params = do
  respVar <- newEmptyMVar :: IO (MVar (Either LSP.ResponseError resp))
  putMVar reqVar (ClientRequest method params respVar)
  takeMVar respVar

--------------------------------------------------------------------------------
-- Sending a client notification

sendClientNotification 
  :: forall params . (ToJSON params)
  => MVar ClientMessage
  -> LSP.ClientMethod
  -> params
  -> IO ()
sendClientNotification reqVar method params =
  putMVar reqVar (ClientNotification method params)

--------------------------------------------------------------------------------
-- Starting the language server

start :: IO (MVar ClientMessage)
start = handle (\(e :: IOException) -> hPrint stderr e >> exitFailure >> undefined) $ do
  (Just inp, Just out, Just err, pid) <- createProcess (proc "hie" ["--lsp"])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering

  req <- newEmptyMVar :: IO (MVar ClientMessage)

  requestMap <- newMVar mempty :: IO (MVar (M.IntMap ResponseVar))

  forkIO (receiving inp out requestMap)
  forkIO (sending inp req requestMap)

  return req

receiving inp out requestMap = forever $ handle (\(e :: SomeException) -> hPrint stderr e) $ do
    headers <- getHeaders out
    case lookup "Content-Length" headers >>= readMaybe of
      -- TODO: Think of some way to recover from malformed headers.
      -- It might just be best to restart the server and try again.
      Nothing -> fail "Couldn't read Content-Length header"
      Just size -> do
        message <- B.hGet out size

        -- Requestmessages require id and method fields 
        --   so it should be the first in this list
        -- Notificationmessages require only the method field
        -- ResponseMessages require only the id field
        --
        -- The decode function is very permissive, so 
        -- it will drop fields that it doesn't recognize.
        -- If handleNotificationMessage would be before
        -- handleRequestMessage, then all requestMessages
        -- would be converted automatically to 
        -- notificationmessages.
        case decode message of
          Just m -> handleRequestMessage inp testRequestMessageHandler m
          Nothing -> case decode message of
            Just m -> handleNotificationMessage testNotificationMessageHandler m
            Nothing -> case decode message of
              Just m -> handleResponseMessage requestMap m
              Nothing -> fail "malformed message"
  where
    getHeaders :: Handle -> IO [(String,String)]
    getHeaders h = do
      l <- hGetLine h
      let (name,val) = span (/= ':') l
      if null val
        then return []
        else ((name,drop 2 val) :) <$> getHeaders h

sending inp req requestMap = forever $ handle (\(e :: SomeException) -> hPrint stderr e) $ do
    clientMessage <- takeMVar req
    case clientMessage of
      (ClientRequest method (req :: req) (respVar :: MVar (Either LSP.ResponseError resp))) -> do
         lspId <- head . (\m -> filter (`M.notMember` m) [minBound..]) <$> readMVar requestMap
         B.hPutStr inp $ addHeader $ encode
           (LSP.RequestMessage "2.0" (LSP.IdInt lspId) method req
             :: LSP.RequestMessage LSP.ClientMethod req resp)
         modifyMVar_ requestMap $ return . M.insert lspId (ResponseVar respVar)

      (ClientNotification method req) -> 
        B.hPutStr inp (addHeader (encode (LSP.NotificationMessage "2.0" method req)))

addHeader :: B.ByteString -> B.ByteString
addHeader content = B.concat
  [ "Content-Length: ", B.pack $ show $ B.length content, "\r\n"
  , "\r\n"
  , content
  ]

--------------------------------------------------------------------------------
-- Handle response messages

handleResponseMessage :: MVar (M.IntMap ResponseVar) -> LSP.ResponseMessage Value -> IO ()
handleResponseMessage requestMap = \case
  LSP.ResponseMessage _ (LSP.IdRspInt lspId) (Just result) Nothing -> do
    mayResVar <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
    case mayResVar of
      Nothing -> fail "Server sent us an unknown id of type Int"
      Just (ResponseVar resVar) -> 
        putMVar resVar $ Right (fromSuccess $ fromJSON result)
  LSP.ResponseMessage _ (LSP.IdRspInt lspId) Nothing (Just rspError) -> do
    mayResVar <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
    case mayResVar of
      Nothing -> fail "Server sent us an unknown id of type Int"
      Just (ResponseVar resVar) ->
        putMVar resVar $ Left rspError
  LSP.ResponseMessage _ (LSP.IdRspString _) (Just _) Nothing ->
    fail "Server sent us an unknown id of type String"
  LSP.ResponseMessage _ (LSP.IdRspString _) Nothing (Just _) ->
    fail "Server sent us an unknown id of type String"
  LSP.ResponseMessage _ LSP.IdRspNull _ _ ->
    fail "Server couldn't read our id"
  _ -> fail "Malformed message"
  where
    fromSuccess :: Result a -> a
    fromSuccess (Success a) = a

--------------------------------------------------------------------------------
-- Handle request messages

data RequestMessageHandler = RequestMessageHandler
  { handleWindowShowMessageRequest   
      :: LSP.ShowMessageRequest          -> IO LSP.ShowMessageResponse
  , handleClientRegisterCapability   
      :: LSP.RegisterCapabilityRequest   -> IO LSP.ErrorResponse
  , handleClientUnregisterCapability 
      :: LSP.UnregisterCapabilityRequest -> IO LSP.ErrorResponse
  , handleWorkspaceApplyEdit         
      :: LSP.ApplyWorkspaceEditRequest   -> IO LSP.ApplyWorkspaceEditResponse
  }

testRequestMessageHandler :: RequestMessageHandler
testRequestMessageHandler = RequestMessageHandler
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  (\m -> emptyResponse m <$ print m)
  where
    toRspId (LSP.IdInt i) = LSP.IdRspInt i
    toRspId (LSP.IdString t) = LSP.IdRspString t
    
    emptyResponse :: LSP.RequestMessage m req resp -> LSP.ResponseMessage a
    emptyResponse m = LSP.ResponseMessage (m ^. LSP.jsonrpc) (toRspId (m ^. LSP.id)) Nothing Nothing

handleRequestMessage 
  :: Handle
  -> RequestMessageHandler
  -> LSP.RequestMessage LSP.ServerMethod Value Value 
  -> IO ()
handleRequestMessage inp RequestMessageHandler {..} m = do
  resp <- case m ^. LSP.method of
    method@LSP.WindowShowMessageRequest ->
      case fromJSON (m ^. LSP.params) :: Result LSP.ShowMessageRequestParams of
        Success params -> encode <$> handleWindowShowMessageRequest
          (LSP.RequestMessage (m ^. LSP.jsonrpc) (m ^. LSP.id) method params)

    method@LSP.ClientRegisterCapability ->
      case fromJSON (m ^. LSP.params) :: Result LSP.RegistrationParams of
        Success params -> encode <$> handleClientRegisterCapability
          (LSP.RequestMessage (m ^. LSP.jsonrpc) (m ^. LSP.id) method params)

    method@LSP.ClientUnregisterCapability ->
      case fromJSON (m ^. LSP.params) :: Result LSP.UnregistrationParams of
        Success params -> encode <$> handleClientUnregisterCapability
          (LSP.RequestMessage (m ^. LSP.jsonrpc) (m ^. LSP.id) method params)
    
    method@LSP.WorkspaceApplyEdit ->
      case fromJSON (m ^. LSP.params) :: Result LSP.ApplyWorkspaceEditParams of
        Success params -> encode <$> handleWorkspaceApplyEdit
          (LSP.RequestMessage (m ^. LSP.jsonrpc) (m ^. LSP.id) method params)
  
  B.hPutStr inp $ addHeader resp

--------------------------------------------------------------------------------
-- Handle notification messages

data NotificationMessageHandler = NotificationMessageHandler
  { handleWindowShowMessage              :: LSP.ShowMessageNotification        -> IO ()
  , handleWindowLogMessage               :: LSP.LogMessageNotification         -> IO ()
  , handleTelemetryEvent                 :: LSP.TelemetryNotification          -> IO ()
  , handleTextDocumentPublishDiagnostics :: LSP.PublishDiagnosticsNotification -> IO ()
  }

testNotificationMessageHandler :: NotificationMessageHandler
testNotificationMessageHandler = NotificationMessageHandler
  (T.putStrLn . view (LSP.params . LSP.message))
  (T.putStrLn . view (LSP.params . LSP.message))
  (print . view LSP.params)
  (mapM_ T.putStrLn . (^.. LSP.params . LSP.diagnostics . traverse . LSP.message))

handleNotificationMessage 
  :: NotificationMessageHandler 
  -> LSP.NotificationMessage LSP.ServerMethod Value 
  -> IO ()
handleNotificationMessage NotificationMessageHandler {..} m =
  case m ^. LSP.method of
    method@LSP.WindowShowMessage ->
      case fromJSON (m ^. LSP.params) :: Result LSP.ShowMessageParams of
        Success params -> handleWindowShowMessage 
          (LSP.NotificationMessage (m ^. LSP.jsonrpc) method params)
        _ -> fail "Malformed parameters of window/showMessage notification"

    method@LSP.WindowLogMessage -> 
      case fromJSON (m ^. LSP.params) :: Result LSP.LogMessageParams of
        Success params -> handleWindowLogMessage
          (LSP.NotificationMessage (m ^. LSP.jsonrpc) method params)
        _ -> fail "Malformed parameters of window/logMessage notification"

    LSP.TelemetryEvent -> handleTelemetryEvent m

    method@LSP.TextDocumentPublishDiagnostics ->
      case fromJSON (m ^. LSP.params) :: Result LSP.PublishDiagnosticsParams of
        Success params -> handleTextDocumentPublishDiagnostics
          (LSP.NotificationMessage (m ^. LSP.jsonrpc) method params)
        _ -> fail "Malformed parameters of textDocument/publishDiagnostics notification"

    _ -> fail $ "unknown method: " ++ show (m ^. LSP.method)

