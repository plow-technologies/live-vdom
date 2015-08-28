{-# LANGUAGE JavaScriptFFI, ForeignFunctionInterface, QuasiQuotes, FlexibleInstances, EmptyDataDecls, TypeFamilies, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}

{-
  warning, unfinished!

  a proof of concept integration with the virtual-dom library.

  The diff function has been changed slightly to allow it to work with full
  functionality in asynchronous threads. It's possible to implement the bindings
  without the modifications at the cost of tail-call optimization and
  preemptive threading in the diff, by recursively forcing the thunks
  in synchronous threads.

 -}

module GHCJS.VDOM ( Properties(..), Children(..)
                  , VNode(..), Patch, DOMNode
                  , JSIdent
                  , diff, patch
                  , memo, memoKey
                  , noProps, props
                  , noChildren, children, single, mkChildren
                  , div, p, a
                  , js_vnode          
                  , emptyDiv
                  , text
                  , toProperties
                  , createElement
                  , click
                  , dblclick
                  , keypress
                  , oninput
                  , canvasLoad
                  , addCustomEvent
                  , removeEventHandler
                  ) where

import Prelude hiding (div)

import Control.Concurrent

import GHCJS.Types
import GHCJS.Foreign.QQ
import GHCJS.Prim
import GHCJS.Marshal
import GHCJS.Foreign

import GHCJS.VDOM.Internal
import GHCJS.VDOM.QQ

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Maybe

import Data.Traversable (sequenceA)

import Data.Text (Text, unpack)

import System.IO.Unsafe
import Unsafe.Coerce

import GHCJS.PureMarshal

class MemoNode a where memoNode :: (J, [JSIdent], a) -> a

instance MemoNode VNode
  where
    memoNode (_,[],a) = a
    memoNode (k,xs,v) =
      let vd     = unsafeExport v
          xs1    = unsafePerformIO $ toJSArray xs
      in VNode [j| new h$vdom.HSThunk(`vd, `xs1, `k) |]
    {-# INLINE memoNode #-}

instance MemoNode b => MemoNode (a -> b)
  where
    memoNode (k,xs,f) = \a -> memoNode (k, objectIdent a:xs, f a)
    {-# INLINE memoNode #-}

memoKey :: MemoNode a => JSString -> a -> a
memoKey k = memo' (castRef k)
{-# NOINLINE memoKey #-}

memo :: MemoNode a => a -> a
memo = memo' [j| $r = null; |]
{-# NOINLINE memo #-}

memo' :: MemoNode a => J -> a -> a
memo' k f = memoNode (k,[objectIdent f],f)
{-# INLINE memo' #-}

noProps :: Properties
noProps = Properties [js'| {} |]
{-# INLINE noProps #-}

singlePropI :: JSString -> Int -> Properties
singlePropI k v = Properties [j| $r={}; $r[`k] = `v; |] -- can we do more efficient literals here?

singlePropS :: JSString -> JSString -> Properties
singlePropS k v = Properties [j| $r={}; $r[`k] = `v; |]


toProperties :: JSRef () -> Properties
toProperties = Properties

noChildren :: Children
noChildren = Children [js'| [] |]
{-# INLINE noChildren #-}

single :: VNode -> Children
single (VNode x) = Children [js'| [`x] |]
{-# INLINE single #-}

class SomeChildren a where someChildren :: a -> Children
instance SomeChildren VNode where
  someChildren v = single v
  {-# INLINE someChildren #-}
instance SomeChildren (VNode,VNode) where
  someChildren (VNode a, VNode b) = Children (castRef $ ptoJSRef (a,b))
  {-# INLINE someChildren #-}
instance SomeChildren (VNode,VNode,VNode) where
  someChildren (VNode a, VNode b, VNode c) = Children (castRef $ ptoJSRef (a,b,c))
  {-# INLINE someChildren #-}
instance SomeChildren (VNode,VNode,VNode,VNode) where
  someChildren (VNode a, VNode b, VNode c, VNode d) = Children (castRef $ ptoJSRef (a,b,c,d))
  {-# INLINE someChildren #-}

mkChildren :: [VNode] -> Children
mkChildren [x]       = single x
mkChildren [x,y]     = someChildren (x,y)
mkChildren [x,y,z]   = someChildren (x,y,z)
mkChildren [x,y,z,v] = someChildren (x,y,z,v)
mkChildren xs        = Children $ unsafePerformIO (toJSArray $ unsafeCoerce xs)
                   {- the unsafeCoerce is safe since VNode is a newtype
                      for JSRef, the unsafePerformIO is safe because we
                      never pass the result to anyone that can mutate it -}
{-# INLINE mkChildren #-}

diff :: VNode -> VNode -> Patch
diff (VNode a) (VNode b) = Patch (unsafePerformIO $ diff' a b)

diff' :: J -> J -> IO J
diff' a b = do
  thunks <- [jsu| [] |]
  patch  <- [js| h$vdom.diff(`a, `b, `thunks) |]
  when [j| `thunks.length > 0|] (forceThunks thunks)
  forcePatch patch
  return patch

forceThunks :: J -> IO ()
forceThunks = fromJSArray >=> mapM_ forceNode
  where
    forceNode n = do
      forceThunkNode [j| `n.a |]
      forceThunkNode [j| `n.b |]
      patch <- diff' [j| `n.a.vnode |] [j| `n.b.vnode |]
      [jsu_| h$vdom.setThunkPatch(`n, `patch); |]

foreign import javascript unsafe "$1.hst" getThunk :: J -> IO Double

forceThunkNode :: J -> IO ()
forceThunkNode x
  | [j| `x && `x.hst |] = do
     (h::Double) <- getThunk x
     let (Just (t::VNode)) = unsafeCoerce h
     (VNode u) <- evaluate t
     [jsu| `x.hst = null; `x.vnode = `u; |]
  | otherwise = return ()

forcePatch :: J -> IO ()
forcePatch p = do
  thunks <- [jsu| h$vdom.forcePatch(`p) |]
  forceTree [thunks]

forceTree :: [J] -> IO ()
forceTree [] = return ()
forceTree (x:xs) = do
  x' <- fromJSArray x
  ys <- forM x' $ \t -> do
    forceThunkNode t
    newThunks <- [jsu| [] |]
    [jsu_| h$vdom.forceTree(`t.vnode, `newThunks) |]
    return newThunks
  forceTree (filter (\a -> [jsu'| `a.length !== 0 |]) ys ++ xs)

patch :: DOMNode -> Patch -> IO ()
patch n (Patch p) = [js| h$vdom.patch(`n, `p); |]

text :: JSString -> VNode
text xs = VNode [jsu'| new h$vdom.VText(`xs) |]
{-# INLINE text #-}

emptyDiv :: VNode
emptyDiv = div noProps noChildren
{-# INLINE emptyDiv #-}

div :: Properties -> Children -> VNode
div = js_vnode "div"
{-# INLINE div #-}

p :: Properties -> Children -> VNode
p = js_vnode "p"
{-# INLINE p #-}

a :: Properties -> Children -> VNode
a = js_vnode "a"
{-# INLINE a #-}

js_vnode :: JSString -> Properties -> Children -> VNode
js_vnode tag (Properties props) (Children children) =
  VNode [jsu'| new h$vdom.VNode(`tag, `props, `children) |]


createElement :: VNode -> IO DOMNode
createElement (VNode y) = [jsu| h$vdom.createElement(`y) |]

---- these things should be in ghcjs-prim

-- make a unique identifier
objectIdent :: a -> JSIdent
objectIdent x = js_unique (unsafeCoerce x)
{-# INLINE objectIdent #-}

js_unique :: Double -> JSIdent
js_unique o = [jsu'| h$mkUnique(`o) |]

unsafeExport :: forall a. a -> J
unsafeExport x =
  let (xd::Double) = unsafeCoerce (Just x)
  in  js_unsafeExport xd

foreign import javascript unsafe "$r = $1;" js_unsafeExport :: Double -> JSRef a

click = setEventHandler "click"
dblclick = setEventHandler "dblclick"

onEvent :: (FromJSRef a, PToJSRef a) => JSString -> (a -> IO b) -> Properties  -> IO Properties
onEvent event f pl = do
  cbfun <- mkCallback
  return $ setEventHandler1 event cbfun pl
  where cb ev = do
          str <- fromJSRef ev
          void . sequenceA $ f <$> str
        mkCallback = syncCallback1 NeverRetain True cb 

canvasLoad :: (JSRef a -> IO ()) -> Properties -> IO Properties
canvasLoad f props = onEvent "canvasLoad" f props


oninput :: (String -> IO b) -> Properties -> IO Properties
oninput f props = onEvent "input" func props
  where func jsEv = fJsRef =<< (GHCJS.Foreign.getProp ("value" :: String) =<< GHCJS.Foreign.getProp ("target" :: String) jsEv)
        fJsRef = f . GHCJS.Foreign.fromJSString



keypress :: (String -> IO b) -> Properties -> IO Properties
keypress f props = onEvent "keypress" func =<< onEvent "keyup" func props
  where func jsEv = fJsRef =<< (GHCJS.Foreign.getProp ("value" :: String) =<< GHCJS.Foreign.getProp ("target" :: String) jsEv)
        fJsRef = f . GHCJS.Foreign.fromJSString

addCustomEvent :: JSString -> IO ()
addCustomEvent = js_addCustomEvent

foreign import javascript safe "h$vdom.getDelegator().listenTo($1)"
  js_addCustomEvent :: JSString -> IO ()

-- we will defenitly want a cleaner API For this stuff. but it' s a start.
foreign import javascript unsafe "h$vdom.setEventHandler($3,$1,$2)"
  setEventHandler :: JSString -> JSFun (IO a) -> Properties -> Properties

foreign import javascript "h$vdom.setEventHandler($3,$1,$2)"
  setEventHandler1 :: JSString -> (JSFun (JSRef a -> IO ())) -> Properties -> Properties

foreign import javascript "h$vdom.removeEventHandler($2,$1)"
  removeEventHandler :: JSString -> Properties -> Properties