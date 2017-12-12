{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Language.Haskell.LSP.TH.DataTypesJSON as LSP
import qualified Language.Haskell.LSP.TH.ClientCapabilities as LSP
import Data.Proxy
import qualified Data.Text.IO as T
import Control.Concurrent

import qualified LSP.Client as Client
import qualified Compat

main :: IO ()
main = do
  pid <- Compat.getPID

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
      initializeParams = LSP.InitializeParams (Just pid) Nothing Nothing Nothing caps Nothing

  reqVar <- Client.start

  Client.sendClientRequest (Proxy :: Proxy LSP.InitializeRequest) reqVar LSP.Initialize initializeParams
--    >>= print
  Client.sendClientNotification reqVar LSP.Initialized (Just LSP.InitializedParams)

  let path = "/home/jaro/haskell/haskell-lsp-client/src/LSP/Client.hs"

  txt <- T.readFile path

  Client.sendClientNotification reqVar LSP.TextDocumentDidOpen (Just (LSP.DidOpenTextDocumentParams (LSP.TextDocumentItem (LSP.filePathToUri path) "haskell" 1 txt)))

  threadDelay 100000

  Client.sendClientRequest (Proxy :: Proxy LSP.ShutdownRequest) reqVar LSP.Shutdown Nothing
--    >>= print
  Client.sendClientNotification reqVar LSP.Exit (Just LSP.ExitParams)


