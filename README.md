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
import Haskoin (btc, textToAddr)
import Haskoin.Store.WebClient

main :: IO ()
main = do
  let cfg = def {net = btc} -- https://api.haskoin.com/btc/...
      Just addr = textToAddr btc "bc1q…"
  r <- apiCall cfg (GetAddrBalance addr)
  print r
```

The client appends the name of the network to the host, as in the layout of
api.haskoin.com. The API speaks JSON, and so does the client.

A store on the same machine may listen on a Unix socket (`--socket`). Give
its path in `socket`, and every request goes to it, whatever host the URL
names:

```haskell
let cfg = def {net = btc, host = "http://localhost/", socket = Just "/run/haskoin-store/web.sock"}
```

## Tokens of Bitcoin Cash

On Bitcoin Cash an output can hold tokens (CashTokens), written at the
front of its script field. `scriptTokens net script` reads them from the
script of a `StoreOutput`, a `StoreInput` or an `Unspent`, which have no
field of their own for them. Their JSON has a `token` field on the Bitcoin
Cash networks: the category, the fungible amount as a string, and the
non-fungible token, or `null`. On Bitcoin the field is not there.

The requests of the token endpoints of haskoin-store 2.0.0:

| Request | Endpoint | Answer |
| --- | --- | --- |
| `GetAddrTokens address` | `/address/:address/tokens` | a `TokenBalance` per category that the address holds |
| `GetTokenHolders category limit offset` | `/token/:category/holders` | a `TokenBalance` per address that holds the category |
| `GetTokenUnspent category limits` | `/token/:category/unspent` | the `Unspent` outputs that hold it, newest first |
| `GetTokenTxs category limits` | `/token/:category/transactions` | the `TxRef`s that moved it, newest first |

A `TokenBalance` counts the mempool, as a `Balance` does:
`confirmedTokens` is what blocks hold and the mempool does not spend, and
`unconfirmedTokens` what the mempool adds, so their sum is what the address
holds now. `nfts` counts its non-fungible tokens, `tokenUtxo` the unspent
outputs that hold the category, `tokenTxs` the transactions that moved it
to or from the address, and `receivedTokens` every fungible token it ever
received. A category is the `TxHash` of the transaction whose output 0 the
first transaction of the tokens spent. On Bitcoin the server answers these
requests with empty lists.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default (def)
import Haskoin (bch, textToAddr)
import Haskoin.Store.Data (SerialList (..), TokenBalance (..), Unspent (..), scriptTokens)
import Haskoin.Store.WebClient

main :: IO ()
main = do
  let cfg = def {net = bch}
      Just addr = textToAddr bch "bitcoincash:qz…"
  Right (SerialList balances) <- apiCall cfg (GetAddrTokens addr)
  mapM_ (\b -> print (b.category, b.confirmedTokens + b.unconfirmedTokens)) balances
  -- the outputs of the first category, with the tokens of each
  case balances of
    b : _ -> do
      let limits = LimitsParam Nothing (OffsetParam 0) Nothing
      Right (SerialList outs) <- apiCall cfg (GetTokenUnspent b.category limits)
      mapM_ (\u -> print (u.outpoint, scriptTokens bch u.script)) outs
    [] -> return ()
```

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
frame = marshalJSON net (EventWatch [addr])
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

These commands build the package as it is released, with optimisation. For
the time between two commits there is a faster loop: `haskell-dev`, from
[jprupp/tools](https://github.com/jprupp/tools), on your `PATH`.

```sh
haskell-dev build          # build without optimisation
haskell-dev test           # run the tests in several processes at once
haskell-dev docs           # build the documentation and check its comments
```

`haskell-dev.toml` tells it which spec modules are slow, so that it shares
them between the test processes. Do not mix the two kinds of build in one
folder: the flags differ, so each switch compiles everything again. Never
time code with `haskell-dev`.

The tests check every type by law: JSON round-trips, the fast encoders
write the same JSON as `toJSON`, the client reads what the server writes,
the binary encodings of the database round-trip and agree between Serial,
cereal and binary, on good bytes and on bad ones, and the format of the basic
types is pinned.

## Versions

Up to 1.8.1, this package was released from the
[haskoin-store](https://github.com/jprupp/haskoin-store) repository with the
same version number as the server. It now has its own releases; a server
speaks the format of the haskoin-store-data version it depends on.

## License

[MIT](LICENSE)
