Shakespeare Dynamic
=================

The idea for shakespeare dynamic is to be able to write templates similar to hamlet and then transform them to vdom in order to use the Javascript [vdom library](https://github.com/Matt-Esch/vdom)

GHCJS Shakespeare dynamic
----

Be able to convert the "pure" templated html into ghcjs-vdom types

Shakespeare Dynamic Opheilia
----

Similar templating to hamlet that you can run without needing to use ghcjs

VDom adapter
----

Types to convert from opheilia templates to VDom



Possible discription of Widget (change)

Event map
===
data Event = Event {
    eventName :: EventId
,   eventData :: Value
} deriving (Eq, Ord, Show)

newtype EventId = EventId {unEventName :: String}


typeclass (ToJSON a, FromJSON a) => IsEvent a where
    toEvent :: a -> EventId -> Event
    fromEvent :: Event -> Maybe a -- Or Either a

type EventMap = Map Event ([Widget])

type Widget = IsEvent a => (a -> (VNode, [Event]))
alttype Widget = Widget (VNode, [Event]) a
data ClientHandlerT eventmap m a


11
data ButtonClick = ButtonClick { mouseLocation :: (Int,Int)}

instance IsEvent ButtonClick where 
  toEvent bc = (\eid -> 
             Event eid (toJSON bc ))

subscribeButton :: ButtonClick
subscribeButton = 

main = do
    evMap <- initEventMap
    runWebPage evMap webPage

runWebPage :: EventMap -> ClientHandlerT e m VDOM -> ClientHandlerT e m () 

webPage = do
    buildHeader
    menuPanel <- buildMenuPanel
    document <- buildDocument
    renderVDOM webPageTempate
     where 
      webPageTemplate = [opheilia|
<div> 
  ^{header}
<div> 
 <div> 
  ^{menuPanel}
 <div>
  ^{document}
      |]

Opheilia :: (ToVdom a) => [a] -> VDOM 

eventToWidget :: Event -> Widget
eventToWidget e -> (VNull, [e])

buildDocument :: ClientHandlerT e m Widget
buildDocument = do
    let e = toEvent subscribeButton
    buildDocument' e

buildDocument' e ev = do
    eventToWidget e
    panel <- addEventsGetVDOM buildPanel 
    vdomToWidget.renderVDom $ documentTemplate panel



registerListener :: (IsEvent ev) => ev -> Widget -> ClientHandlerT EventMap m ()

getContentWidgetAJAXCall :: ClientHandlerT e m [Widget]

documentTemplate = [hamlet| Hello  ^{panel}|] 