cabal sandbox delete 
cabal sandbox init ghcjs 
cabal install --ghcjs
cabal sandbox add-source /home/scott/programs/src/ghcjs-ffiqq/
cabal sandbox add-source /home/scott/programs/src/ghcjs-vdom/
cabal install --ghcjs

cabal configure -f build-example
