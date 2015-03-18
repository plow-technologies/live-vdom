{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module Main where

import Data.Aeson
-- Base
import           Control.Applicative
import           Data.Maybe
-- import           Pipes.Concurrent
import           Prelude                                    hiding (div)

import           Control.Concurrent.STM.Notify
import           Control.Concurrent.STM.Message

-- GHCJS/VDom/Ophelia
import           GHCJS.Foreign
import           GHCJS.Types
import           GHCJS.Foreign.QQ
import           GHCJS.VDOM
import           Shakespeare.Dynamic.Render
import           Shakespeare.Ophelia.Parser.VDOM
import           Shakespeare.Ophelia.Parser.VDOM.Event
import qualified VDOM.Adapter                               as VDA

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad                              (forever)
import           Control.Monad.STM
import           Shakespeare.Ophelia

import           Shakespeare.Ophelia.Parser.VDOM.Components
import Text.Read

main :: IO ()
main = do
  addCss "../../../../css/bootstrap.min.css"
  addCss "../../../../css/bootstrap-responsive.css"
  container <- createContainer
  mb1@(env1,addr1) <- spawnIO $ False
  tgConfig@(tgEnv, tgAddr) <- spawnIO $ TankGaugeWidgetConfig Unfired (Left "Please enter a tank height")
  sbmtBttn@(smbtEnv, sbmtAddr) <- spawnIO $ Unfired
  tgs@(tgsEnv, tgsAddr) <- spawnIO []
  sendIO addr1 $ False
  forkIO $ onChange smbtEnv (\x -> do
    mtg <- recvIO tgEnv
    attemptInsertTank mtg tgs
    )
  runDomI container (tankGaugeConfig tgConfig sbmtAddr <$> tgsEnv)


addCss :: String -> IO ()
addCss str = [js_|
var ss = document.createElement("link");
ss.type = "text/css";
ss.rel = "stylesheet";
ss.href = `str;
document.getElementsByTagName("head")[0].appendChild(ss);
|]

modify :: (STMEnvelope Int, Address Int) -> IO ()
modify (env, addr) = do
  atomically $ do
    x <- recv env
    send addr $ x + 1
  return ()

  -- forever $ runEffect $ (fromInput $ (\a b -> (a,b)) <$> inp2 <*> inp) >-> printC

data TankGaugeWidgetConfig = TankGaugeWidgetConfig {
  tankGaugeWidgetName :: Event String
, tankGaugeWidgetHeight :: Either String Int
} deriving (Eq, Show)

data TankGaugeConfig = TankGaugeConfig {
  tankGaugeName :: String
, tankGaugeHeight :: Int
} deriving (Eq, Show)


attemptInsertTank :: TankGaugeWidgetConfig -> STMMailbox ([TankGaugeConfig]) -> IO ()
attemptInsertTank (TankGaugeWidgetConfig eName eHeight) (env, addr) = case (orEmpty eName) of
                                                                        Unfired -> [js_|alert("Unable to add tank because there is no name")|]
                                                                        Fired name -> case eHeight of
                                                                                        (Left err) -> [js_|alert(`err)|]
                                                                                        (Right h) -> do
                                                                                                        xs <- recvIO env
                                                                                                        sendIO addr $ (TankGaugeConfig name h):xs
                                                                                                        print $ (TankGaugeConfig name h):xs
                                                                                                        return ()
  where orEmpty Unfired = Unfired
        orEmpty (Fired "") = Unfired
        orEmpty (Fired x) = Fired x 


setName :: TankGaugeWidgetConfig -> String -> TankGaugeWidgetConfig
setName tg name = tg {tankGaugeWidgetName = Fired name}

setTankHeight :: TankGaugeWidgetConfig -> String -> TankGaugeWidgetConfig
setTankHeight tg heightStr = case readMaybe heightStr of
                                (Just h) -> tg {tankGaugeWidgetHeight = Right h}
                                Nothing -> tg {tankGaugeWidgetHeight = Left "Unable to parse height"}


modifyTankHeight :: STMMailbox TankGaugeWidgetConfig -> String -> Message Bool
modifyTankHeight mb = modifyMailbox mb setTankHeight

modifyTankName :: STMMailbox TankGaugeWidgetConfig -> String -> Message Bool
modifyTankName mb = modifyMailbox mb setName

displayOption :: String -> LiveVDom VDA.JSEvent
displayOption str = [gertrude|
<option>
  #{str}
|]

tankGaugeOptions :: [String]
tankGaugeOptions = ["Oil", "Water"]

printval :: JSRef a -> IO ()
printval v = [js_|console.log(`v)|]

addTankForm tankMb sbmtBttnAddr = addEvent (VDA.JSLoad  printval) [gertrude|
<form class="form-horizontal">
  <div class="control-group">
    <label class="control-label" for="tankName">
      Tank Name:
    !{return $ textBoxWith (modifyTankName tankMb) [] Nothing}
  <div class="control-group">
    <label class="control-label" for="tankHeight">
      Tank Height:
    !{return $ textBoxWith (modifyTankHeight tankMb) [VDA.Property "type" $ VDA.JSPText "number"] Nothing}
  <div class="control-group">
    !{return $ button sbmtBttnAddr [] "Add"}
|]

tankGaugeConfig tankMb sbmtBttnAddr tanks = [gertrude|
<div>
  <div class="text-center">
    <h2 class="unselectable" style="cursor:default;" unselectable="on">
      Location - Conrady 1-28
  <div class="text-center">
    !{return $ addTankForm tankMb sbmtBttnAddr}
  <table class="table table-condensed table-striped table-bordered bootstrap-datatable datatable">
    <thead>
      <tr>
        <th>
        <th class="unselectable" style="cursor:default;" unselectable="on">
          Tank Description
        <th class="unselectable" style="cursor:default;" unselectable="on">
          Tank Color
        <th class="unselectable" style="cursor:default;" unselectable="on">
          Tank Height
        <th class="unselectable" style="cursor:default;" unselectable="on">
          Lines
    <tbody>
      &{return $ map displayTank tanks}

|]

displayTank :: TankGaugeConfig -> LiveVDom VDA.JSEvent
displayTank (TankGaugeConfig name height) = [gertrude|
<tr>
  <th>
  <th class="unselectable" style="cursor:default;" unselectable="on">
    #{name}
  <th class="unselectable" style="cursor:default;" unselectable="on">
    Tank Color
  <th class="unselectable" style="cursor:default;" unselectable="on">
    #{show height}
  <th class="unselectable" style="cursor:default;" unselectable="on">
    Lines
|]



-- Possible solution to canvas ready events in dom
-- <!DOCTYPE html>
-- <html>
-- <body>

-- <canvas name="canvas" id="myCanvas" width="200" height="100" style="border:1px solid #000000;">
-- Your browser does not support the HTML5 canvas tag.
-- </canvas>
-- 
-- 
-- <script>
--   console.log("Running");
--   document.getElementById("myCanvas").addEventListener("canvasReady", function(event) {
--     console.log("DOM fully loaded and parsed");
--   });
-- 
--   var xs = document.getElementsByName("canvas");
--   console.log(xs[0]);
--   
--   for(var i=0;i<xs.length;i++){
--      var event = new CustomEvent('canvasReady',{'canvas': xs[i]});
--      xs[i].dispatchEvent(event);
--   }
-- 
-- </script>
-- </body>
-- </html>
