# stm-notify

A structure for updating and watching values.

Watching a value can ONLY occur in a single thread and you0 0are NOT GARUNTEED TO GET EVERY VALUE. At all. If the program runs faster than the watching thread can be woken up then you won't get the next updates if they occur while it's updating.

## Installation

Clone the source and then inside of a cabal project that is sandboxed run

```
cabal sandbox add-source ~/path/to/stm-notify/
cabal clean
cabal configure
cabal install
```

## Usage

```haskell

import Control.Concurrent.STM.Notify
import Control.Concurrent


main = do
  (env, addr) <- spawnIO "Hello" -- Spawn an Envelope and address for strings
  forkOnChange env putStr        -- with the envelope, watch for changes in a seperate thread. When there is a change, print the new string
  sendIO addr " World!\n"        -- Send another string to the address
  threadDelay 10000              -- Wait some time for the repl
  return ()

```
## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

Pull requests and tests
