{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Data.IntSet as S
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

data LSPReader = LSPReader
  { requestVar :: MVar (ClientMessage, MVar (LSP.ResponseMessage Value))
  , jsonrpc :: Text
  }

data LSPState = LSPState
  { currentLspIds :: S.IntSet
  }

smallestNonMember :: S.IntSet -> Int
smallestNonMember set = head $ filter (flip S.notMember set) [minBound..]

type LSPM a = ReaderT LSPReader (StateT LSPState IO) a

runLSPM :: LSPM a -> IO a
runLSPM action = do
  reqVar <- start
  (flip evalStateT) (LSPState S.empty) $ runReaderT action $ LSPReader reqVar "2.0"

-- Using concrete types to communicate via MVars, because ghc doesn't support impredicative polymorphism
-- https://ghc.haskell.org/trac/ghc/wiki/ImpredicativePolymorphism
-- e.g. `MVar (forall a. a)` is impossible.

type ClientMessage = LSP.RequestMessage LSP.ClientMethod Value Value
type ClientNotification = LSP.NotificationMessage LSP.ClientMethod Value
type ServerMessage = LSP.RequestMessage LSP.ServerMethod Value Value
type ServerNotification = LSP.NotificationMessage LSP.ServerMethod Value

sendRequest :: (ToJSON req, ToJSON resp, FromJSON resp)
            => Proxy (LSP.RequestMessage LSP.ClientMethod req resp) 
            -> LSP.ClientMethod 
            -> req 
            -> LSPM (Either LSP.ResponseError resp)
sendRequest Proxy method params = do
  resp <- liftIO newEmptyMVar
  lspId <- do 
    LSPReader reqVar jsonrpc <- ask
    lspIdSet <- lift $ gets currentLspIds
    let lspId = smallestNonMember lspIdSet
    lift $ put $ LSPState $ S.insert lspId lspIdSet 
    liftIO $ putMVar reqVar (LSP.RequestMessage jsonrpc (LSP.IdInt lspId) method (toJSON params), resp)
    return lspId
  LSP.ResponseMessage _ lspRspId result error <- liftIO $ takeMVar resp
  when (lspRspId /= LSP.IdRspInt lspId) $ Prelude.error "Mismatched ids"
  lift $ modify (\(LSPState s) -> LSPState $ S.delete lspId s)
  return $ case result of 
    Nothing -> Left $ fromJust $ error
    Just result -> Right $ fromSuccess $ fromJSON result
  where
    fromSuccess (Success a) = a

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

      initializedNotification :: LSP.InitializedNotification
      initializedNotification = LSP.NotificationMessage "2.0" LSP.Initialized 
        (Just LSP.InitializedParams)

      shutdownRequest :: LSP.ShutdownRequest
      shutdownRequest = LSP.RequestMessage "2.0" (LSP.IdInt 1) LSP.Shutdown
        Nothing 

      exitNotification :: LSP.ExitNotification
      exitNotification = LSP.NotificationMessage "2.0" LSP.Exit (Just LSP.ExitNotificationParams)
  runLSPM $ do
    sendRequest (Proxy :: Proxy LSP.InitializeRequest) LSP.Initialize initializeParams 
      >>= liftIO . print
    sendRequest (Proxy :: Proxy LSP.ShutdownRequest) LSP.Shutdown Nothing 
      >>= liftIO . print

start :: IO (MVar (ClientMessage, MVar (LSP.ResponseMessage Value)))
start = handle (\(e :: IOException) -> hPutStrLn stderr (show e) >> undefined) $ do
  (Just inp, Just out, Just err, pid) <- createProcess (proc "hie" ["--lsp"])
    {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering inp NoBuffering
  hSetBuffering out NoBuffering
  hSetBuffering err NoBuffering

  req <- newEmptyMVar :: IO (MVar (ClientMessage, MVar (LSP.ResponseMessage Value)))

  requestMap <- newMVar mempty :: IO (MVar (M.IntMap (MVar (LSP.ResponseMessage Value))))

  -- the receiving thread
  forkIO $ forever $ do
    headers <- getHeaders out
    case lookup "Content-Length" headers >>= readMaybe of
      -- FIXME: Think of some way to recover from malformed headers.
      -- It might just be best to restart the server and try again.
      Nothing -> error "Malformed LSP headers"
      Just size -> do
        message <- B.hGet out size

        let choice :: Alternative f => [f a] -> f a
            choice (a:as) = a <|> choice as
        
        choice [ case decode message :: Maybe (LSP.ResponseMessage Value) of
                   Just resMsg@(LSP.ResponseMessage _ (LSP.IdRspInt lspId) result error) -> do
                     (Just resVar) <- modifyMVar requestMap $ return . (M.delete lspId &&& M.lookup lspId)
                     putMVar resVar resMsg
                   _ -> undefined
               , case decode message :: Maybe ServerMessage of
                   Just reqMsg@(LSP.RequestMessage _ lspId method params) -> do
                     -- TODO: Handle server requests
                     return ()
                   _ -> undefined
               , case decode message :: Maybe ServerNotification of
                   Just notification@(LSP.NotificationMessage _ method params) -> do
                     -- TODO: Handle server notifications
                     return ()
                   _ -> undefined
               , error "Malformed LSP response message"
               ]

  -- the sending thread
  forkIO $ forever $ do
    (reqMsg@(LSP.RequestMessage _ (LSP.IdInt lspId) _ _), resVar) <- takeMVar req
    B.hPutStr inp (addHeader (encode reqMsg))
    modifyMVar_ requestMap $ return . M.insert lspId resVar

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
