cabal sandbox delete 
cabal sandbox init

cabal sandbox add-source non-hackage-dependencies/ghcjs-base
cabal sandbox add-source non-hackage-dependencies/ghcjs-canvas
cabal sandbox add-source non-hackage-dependencies/ghcjs-ffiqq
cabal sandbox add-source non-hackage-dependencies/ghcjs-vdom
cabal sandbox add-source non-hackage-dependencies/stm-notify

cabal install --ghcjs
cabal configure --ghcjs
