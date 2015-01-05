In order to create a web page with vdom there are two things that are needed:
    1. Being able to render dom
    2. Being able to listen for events on dom and change dom


Rendering dom is done im the [Render.hs file](src/Shakespeare/Dynamic/Render.hs)

Because this is done using pipes, the missing piece is writing the portion to generate the vdom.

Right now my idea is to use pipes concurrency to create a finalizer that is passed to the render function that adds button callbacks. This would look something like:

```haskell
addEvents :: Output () -> Output () -> IO ()
addEvents output1 output2 = do
  someButton <- select "#button-id"
  anotherButton <- select "#some-other-button-id"
  click (\_ -> (yield ()) >-> toOutput output1) def someButton
  click (\_ -> (yield ()) >-> toOutput output2) def anotherButton
```

This solves the problem of needing to re-register events after they have been re-rendered

The second problem (that hasn't been solved at all yet) is the need to be able to listen to dom. The current idea is:

```haskell
type LiveDom = Tree (Producer VDomAdapter)
```

Using the monad instance on Producer and the Traversable instance on Tree means that

```haskell
treeProducer :: LiveDom -> Producer (Tree VDomAdapter)
treeProducer = sequence
```

I think using this it would make sense to modify the type of VDom to be

```haskell
data VDom = VNode -- With children
          | VText -- Without children

```



If compiling fails due to transformers/transformers-compat, try to install them first and then run ```cabal install```