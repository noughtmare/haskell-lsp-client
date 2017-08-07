{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
module Client (sendClientRequest, sendClientNotification, start) where

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

data ClientMessage 
  = forall params resp. (ToJSON params, ToJSON resp, FromJSON resp) 
    => ClientRequest LSP.ClientMethod params (MVar (Either LSP.ResponseError resp))
  | forall params. (ToJSON params) 
    => ClientNotification LSP.ClientMethod params

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

sendClientNotification 
  :: forall params . (ToJSON params)
  => MVar ClientMessage
  -> LSP.ClientMethod
  -> params
  -> IO ()
sendClientNotification reqVar method params =
  putMVar reqVar (ClientNotification method params)

data ResponseVar = forall resp . FromJSON resp => 
  ResponseVar (MVar (Either LSP.ResponseError resp))

start :: IO (MVar ClientMessage)
start = handle (\(e :: IOException) -> hPutStrLn stderr (show e) >> exitFailure >> undefined) $ do
  (Just inp, Just out, Just err, pid) <- createProcess (proc "hie" ["--lsp"])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering

  req <- newEmptyMVar :: IO (MVar ClientMessage)

  requestMap <- newMVar mempty :: IO (MVar (M.IntMap ResponseVar))

  -- the receiving thread
  forkIO $ forever $ do
    headers <- getHeaders out
    case lookup "Content-Length" headers >>= readMaybe of
      -- FIXME: Think of some way to recover from malformed headers.
      -- It might just be best to restart the server and try again.
      Nothing -> fail "Couldn't read Content-Length header"
      Just size -> do
        message <- B.hGet out size

        -- `choice` catches the `empty`s and `fails` from the IO actions
        let choice :: Alternative f => [f a] -> f a
            choice (a:as) = a <|> choice as

        -- Requestmessages require id and method fields 
        --   so it should be the first in this list
        -- Notificationmessages require only the method field
        -- ResponseMessages require only the id field
        choice [ handleRequestMessage message
               , handleNotificationMessage message
               , handleResponseMessage requestMap message
               , fail "Malformed message"
               ]

  -- the sending thread
  forkIO $ forever $ do
    clientMessage <- takeMVar req
    case clientMessage of
      (ClientRequest method (req :: req) 
        (respVar :: (ToJSON resp, FromJSON resp) => MVar (Either LSP.ResponseError resp))
        ) -> do
         lspId <- head . (\m -> filter (`M.notMember` m) [minBound..]) <$> readMVar requestMap
         B.hPutStr inp $ addHeader $ encode
           (LSP.RequestMessage "2.0" (LSP.IdInt lspId) method req
             :: LSP.RequestMessage LSP.ClientMethod req resp)
         modifyMVar_ requestMap $ return . M.insert lspId (ResponseVar respVar)

      (ClientNotification method req) -> 
        B.hPutStr inp (addHeader (encode (LSP.NotificationMessage "2.0" method req)))

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

handleResponseMessage :: MVar (M.IntMap ResponseVar) -> B.ByteString -> IO ()
handleResponseMessage requestMap message = 
  case decode message :: Maybe (LSP.ResponseMessage Value) of
    Just (LSP.ResponseMessage _ (LSP.IdRspInt lspId) (Just result) Nothing) -> do
      mayResVar <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
      case mayResVar of
        Nothing -> fail $ "Server send us an unknown id of type Int"
        Just (ResponseVar (resVar :: FromJSON resp => MVar (Either LSP.ResponseError resp))) ->
          putMVar resVar $ Right (fromSuccess $ fromJSON result :: resp)

    Just (LSP.ResponseMessage _ (LSP.IdRspInt lspId) Nothing (Just rspError)) -> do
      mayResVar <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
      case mayResVar of
        Nothing -> fail $ "Server send us an unknown id of type Int"
        Just (ResponseVar (resVar :: FromJSON resp => MVar (Either LSP.ResponseError resp))) ->
          putMVar resVar $ Left rspError

    Just (LSP.ResponseMessage _ (LSP.IdRspString _) (Just _) Nothing) ->
      error "Server send us an unknown id of type String"
    Just (LSP.ResponseMessage _ (LSP.IdRspString _) Nothing (Just _)) ->
      error "Server send us an unknown id of type String"
    Just (LSP.ResponseMessage _ LSP.IdRspNull _ _) ->
      error "Server couldn't read our id"
    _ -> empty
  where
    fromSuccess (Success a) = a

handleRequestMessage :: B.ByteString -> IO ()
handleRequestMessage message = 
  case decode message :: Maybe (LSP.RequestMessage LSP.ServerMethod Value Value) of
    Just (LSP.RequestMessage _ _ _ _) -> do
      -- TODO: Handle server requests
      B.putStrLn message
      return ()
    _ -> empty

handleNotificationMessage :: B.ByteString -> IO ()
handleNotificationMessage message =
  case decode message :: Maybe (LSP.NotificationMessage LSP.ServerMethod Value) of
    Just (LSP.NotificationMessage _ _ _) -> do
      -- TODO: Handle server notifications
      B.putStrLn message
      return ()
    _ -> empty
