# haskoin-store-data

Data types and API client of [Haskoin Store](https://github.com/jprupp/haskoin-store),
an indexer for Bitcoin and Bitcoin Cash with a REST API.

Use this package to talk to a Haskoin Store server from Haskell. It has no
database or network node of its own, so it is small to depend on.

## Modules

- `Haskoin.Store.Data`: the types that the server answers with (blocks,
  transactions, balances, unspent outputs, extended public keys, health
  checks, events, and the Blockchain.info compatible formats), with their
  JSON. The types that the server keeps in its database also have a binary
  encoding, for the server's own use.
- `Haskoin.Store.WebCommon`: the requests of the API, one type per endpoint,
  with their paths and parameters.
- `Haskoin.Store.WebClient`: `apiCall`, which sends a request and reads
  the answer, and `apiEvents`, which listens to the events of a server.

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
api.haskoin.com. The API speaks JSON, and so does the client.

Errors come back as `Left` an `Except`.

## Events

`apiEvents` calls a function with each new block and each transaction that
enters or leaves the mempool, for as long as the function answers `True`.
After a lost connection, `apiEventsAfter` takes up from the last event
heard, so nothing is missed; if the server no longer has that event it
answers `ThingNotFound`, and the client reads what it follows again.

Over the WebSocket at `/events` a client can also choose the transactions
it hears about. `EventCommand` is what it sends: addresses to watch,
extended public keys (one command for each type of address), or a Bloom
filter as in BIP 37, each a JSON object sent in a text frame. This package
has no WebSocket client of its own; send the command with the one you use:

```haskell
import Haskoin (marshalJSON)
import Haskoin.Store.Data (EventCommand (..))

-- a text frame: {"type":"watch","addresses":["bc1q…"]}
frame = marshalJSON (net, ctx) (EventWatch [addr])
```

Blocks are always told. A connection that has sent no command hears of
every transaction, unless its URL says what to watch, or `quiet=true` for
blocks alone.

The JSON of every type, for someone who writes a client in another
language, is in
[`swagger.yaml`](https://github.com/jprupp/haskoin-store/blob/master/swagger.yaml)
in the haskoin-store repository.

## Building

The package builds with [Stack](https://haskellstack.org) and needs
libsecp256k1 (`libsecp256k1-dev` on Debian and Ubuntu), or Nix with
`stack --nix`:

```sh
stack build
stack test
```

The tests check every type by law: JSON round-trips, the fast encoders
write the same JSON as `toJSON`, the client reads what the server writes,
the binary encodings of the database round-trip and agree between Serial,
cereal and binary, and the format of the basic types is pinned.

## Versions

Up to 1.8.1, this package was released from the
[haskoin-store](https://github.com/jprupp/haskoin-store) repository with the
same version number as the server. It now has its own releases; a server
speaks the format of the haskoin-store-data version it depends on.

## License

[MIT](LICENSE)
