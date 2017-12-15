# haskell-lsp-client
[![Hackage](https://img.shields.io/hackage/v/haskell-lsp-client.svg)](https://hackage.haskell.org/package/haskell-lsp-client)

This package is intended for developers of text editors who want to make their text editor
compatible with the [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md).

I have developed this package with plans to integrate it in the [Yi editor](https://github.com/yi-editor/yi).

## The example client

Contained in this repository is an example client. This example client just runs the [Haskell IDE Engine](https://github.com/alanz/haskell-ide-engine/)
and opens the file specified on the command line. Then it asks the language server for all symbols in the document and prints the result it gets.

To run the example:

1. Download and install the Haskell IDE Engine. (make sure `hie` is on your PATH)
2. `git clone https://github.com/noughtmare/haskell-lsp-client`
3. `cd haskell-lsp-client`
4. `stack build`
5. `stack exec example-client -- example/Main.hs`

On my system it shows something about a capability registration request and that a certain hoogle
database is used. You can replace example/Main.hs with any haskell file you would like to check.
It reports ghc errors and hlint suggestions and it shows the symbol list.
