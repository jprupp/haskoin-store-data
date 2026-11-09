# haskoin-store-data

Data types and API client of [Haskoin Store](https://github.com/jprupp/haskoin-store),
an indexer for Bitcoin and Bitcoin Cash with a REST API.

Use this package to talk to a Haskoin Store server from Haskell. It has no
database or network node of its own, so it is small to depend on.

## Modules

- `Haskoin.Store.Data`: the types that the server answers with (blocks,
  transactions, balances, unspent outputs, extended public keys, health
  checks, events, and the Blockchain.info compatible formats), with their
  JSON and binary encodings.
- `Haskoin.Store.WebCommon`: the requests of the API, one type per endpoint,
  with their paths and parameters.
- `Haskoin.Store.WebClient`: `apiCall`, which sends a request and decodes
  the answer.

## Example

```haskell
import Data.Default (def)
import Haskoin (btc, textToAddr, withContext)
import Haskoin.Store.WebClient

main :: IO ()
main = withContext $ \ctx -> do
  let cfg = def {net = btc} -- https://api.haskoin.com/btc/...
      Just addr = textToAddr btc "bc1q…"
  r <- apiCall ctx cfg (GetAddrBalance addr)
  print r
```

The client appends the name of the network to the host, as in the layout of
api.haskoin.com. The binary encoding is used on the wire, as it is smaller and
faster to decode than JSON.

## Building

The package builds with [Stack](https://haskellstack.org) and needs
libsecp256k1 (`libsecp256k1-dev` on Debian and Ubuntu), or Nix with
`stack --nix`:

```sh
stack build
stack test
```

The tests check every type by law: binary encodings round-trip and agree
between Serial, cereal and binary, JSON round-trips, the fast encoders write
the same JSON as `toJSON`, and the wire format of the basic types is pinned.

## Versions

Up to 1.8.1, this package was released from the
[haskoin-store](https://github.com/jprupp/haskoin-store) repository with the
same version number as the server. It now has its own releases; a server
speaks the wire format of the haskoin-store-data version it depends on.

## License

[MIT](LICENSE)
