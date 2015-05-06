{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-
  Straight stolt on from virtual-dom
  virtual-dom bindings demo, rendering a large pixel grid with a bouncing red
  square. the step and patch are calculated asynchronously, the update is
  batched in an animation frame
 -}


module Main where

import           Data.Aeson
import           TankGauge
-- Base
import           Control.Applicative
import           Data.Maybe
import           Data.Traversable
-- import           Pipes.Concurrent
import           Prelude                        hiding (div, sequence)

import           Control.Concurrent.STM.Message
import           Control.Concurrent.STM.Notify
import           Control.Concurrent.STM.TMVar

-- GHCJS/VDom/Ophelia
import           GHCJS.Foreign
import           GHCJS.Foreign.QQ
import           GHCJS.Marshal
import           GHCJS.Types
import           GHCJS.VDOM
import qualified JavaScript.Canvas              as Canvas
import           Shakespeare.Dynamic.Components
import           Shakespeare.Dynamic.Event
import           Shakespeare.Dynamic.Render
import qualified VDOM.Adapter                   as VDA

import           Control.Concurrent
import           Control.Monad                  (void, when, join)
import           Control.Monad.STM
import           Data.Text                      (Text)
import           Shakespeare.Ophelia
import           Text.Read

main :: IO ()
main = do
  putStrLn "Haskell loaded"
  addCustomEvent "canvasLoad"
  exportRunTankGauge


exportRunTankGauge :: IO ()
exportRunTankGauge = do
  addCustomEvent "canvasLoad"
  cb <- asyncCallback2 NeverRetain $ \jsDomRef tankGaugeConfRef -> do
    mJsDom <- fromJSRef jsDomRef
    mTankGaugeConf <- fromJSRef tankGaugeConfRef
    res <- sequence $ liftA2 runTankGaugeWidget mJsDom mTankGaugeConf
    when (isNothing res) $ error $ "Incorrect arguments given to runTankGaugeWidget \n" ++ (show mTankGaugeConf)
  [js_| runTankGaugeWidget = `cb; |]



instance FromJSON TankGaugeWidgetConfig where
  parseJSON (Object v) = TankGaugeWidgetConfig <$> v .: "name" <*> v .: "height"
  parseJSON _ = empty

instance FromJSRef TankGaugeWidgetConfig where
  fromJSRef r = do
    name <- join <$> (traverse fromJSRef =<< getPropMaybe ("name" :: Text) r) :: IO (Maybe String)
    height <- join <$> (traverse fromJSRef =<< getPropMaybe ("height" :: Text) r) :: IO (Maybe Int)
    return $ (\n h -> TankGaugeWidgetConfig (Fired n) (Right h)) <$> name <*> height


runTankGaugeWidget :: DOMNode -> TankGaugeWidgetConfig -> IO ()
runTankGaugeWidget container config = do
  addCustomEvent "canvasLoad"
  tgConfig@(tgEnv, _) <- spawnIO $ config
  (smbtEnv, sbmtAddr) <- spawnIO $ Unfired
  tgs@(tgsEnv, _) <- spawnIO []
  tmContainer <- newEmptyTMVarIO
  forkTankGauge tmContainer
  _ <- forkIO $ onChange smbtEnv (\_ -> do
    mtg <- recvIO tgEnv
    attemptInsertTank mtg tgs
    )
  runDomI container notifyAll (tankGaugeConfig tmContainer tgConfig sbmtAddr <$> tgsEnv)

addCss :: String -> IO ()
addCss str = [js_|
var ss = document.createElement("link");
ss.type = "text/css";
ss.rel = "stylesheet";
ss.href = `str;
document.getElementsByTagName("head")[0].appendChild(ss);
|]


data TankGaugeWidgetConfig = TankGaugeWidgetConfig {
  tankGaugeWidgetName   :: Event String
, tankGaugeWidgetHeight :: Either String Int
} deriving (Eq, Show)

data TankGaugeConfig = TankGaugeConfig {
  tankGaugeName   :: String
, tankGaugeHeight :: Int
} deriving (Eq, Show)


attemptInsertTank :: TankGaugeWidgetConfig -> STMMailbox ([TankGaugeConfig]) -> IO ()
attemptInsertTank (TankGaugeWidgetConfig eName eHeight) (env, addr) = case (orEmpty eName) of
                                                                        Unfired -> [js_|alert("Unable to add tank because there is no name")|]
                                                                        Fired name -> case eHeight of
                                                                                        (Left err) -> [js_|alert(`err)|]
                                                                                        (Right h) -> do
                                                                                                        xs <- recvIO env
                                                                                                        void $ sendIO addr $ (TankGaugeConfig name h):xs
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

getTarget :: JSRef a -> IO (JSRef b)
getTarget jsr = [js|`jsr.currentTarget|]

notifyAll :: IO ()
notifyAll = [js_|
var d = h$vdom.getDelegator();
d.rawEventListeners.canvasLoad({target:document.querySelector("canvas")});
|]

-- Run the tank gauge in a DOMNode
forkTankGauge :: TMVar DOMNode -> IO ()
forkTankGauge dm = void . forkIO $ do
  container <- atomically $ readTMVar dm
  canvasContext <- Canvas.getContext =<< getTarget container
  drawTankGauge canvasContext

-- >> (void . forkIO $ drawTankGauge =<< Canvas.getContext =<< getTarget jsr)))
tankGaugeCanvas :: TMVar (JSRef a) -> LiveVDom VDA.JSEvent
tankGaugeCanvas tmContainer = addEvent (VDA.JSCanvasLoad (\jsr -> void . atomically $ tryPutTMVar tmContainer jsr)) [gertrude|
<canvas name="tankGaugeCanvas" id="myCanvas" width="630" height="600">
  Your browser does not support the HTML5 canvas tag.
|]

addTankForm :: STMMailbox TankGaugeWidgetConfig
                     -> Address (Event ()) -> LiveVDom VDA.JSEvent
addTankForm tankMb sbmtBttnAddr = [gertrude|
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

tankGaugeConfig :: TMVar DOMNode -> STMMailbox TankGaugeWidgetConfig
                         -> Address (Event ()) -> [TankGaugeConfig] -> LiveVDom VDA.JSEvent
tankGaugeConfig tmContainer tankMb sbmtBttnAddr tanks = [gertrude|
<div>
  !{return $ tankGaugeCanvas tmContainer}
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
