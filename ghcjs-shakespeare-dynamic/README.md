Shakespeare Dynamic
==

In order to create a web page with vdom there are two things that are needed:
    1. Being able to render dom
    2. Generating dom that's able to be rendered
    3. Being able to listen for events on dom and change dom


Rendering DOM
====
Rendering dom is done in [Render.hs](src/Shakespeare/Dynamic/Render.hs)

The external function for DOM rendering is [`runDomI`](src/Shakespeare/Dynamic/Render.hs). This takes a DOMNode to run inside of, an IO action to perform after the first render, and an STMEnvelope of LiveVDom.

Generating LiveVDom
====
This is done mostly through the gertrude quasiquoter where you can render a simple language that's similar to hamlet.

for static dom you can use
```haskell
foo = [gertrude|
<div>
    <some test="works">
|]
```

This doesn't require ending brackets and the best way to use this is to just have children of a node be indented below the parent.

If you want to compose dom inside of dom you can use
```haskell
-- Display a list of Maybe Int
foo :: [Maybe Int] -> LiveVDom JSEvent 
foo xs = [gertrude|
<div>
    <ul>
        !{return $ bar xs}
|]
        
-- Map baz across each Maybe Int
bar :: [Maybe Int] -> LiveVDom JSEvent
bar xs = [gertrude|
&{return $ map baz xs} 
|]


-- Display a single option
baz :: Maybe Int -> LiveVDom JSEvent
baz Nothing = [gertrude|
<li>
   Empty
|]
baz (Just i) = [gertrude|
<li>
    A value: #{show i}
|]

```

The things to not in the example are the use of interpolators are:

    * #{expr} expects a haskell expression for a string
    * &{expr} expects a haskell expression for STMEnvelope [LiveVDom JSEvent]. In the example we use the monad instance on STMEnvelope to use return
    * !{expr} expects a haskell expression for STMEnvelope (LiveVDom JSEvent). This uses the monad instance on STMEnvelope again

