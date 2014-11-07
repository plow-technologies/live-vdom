cabal sandbox delete 
cabal sandbox init ghcjs 
cabal install --ghcjs
cabal sandbox add-source ~/programs/ghcjs-ffiqq/
cabal sandbox add-source ~/programs/ghcjs-vdom/
cabal install --ghcjs

cabal configure -f build-example
