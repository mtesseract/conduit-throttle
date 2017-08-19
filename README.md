### About

This is `conduit-throttle`, a small Haskell package providing
configurable throttling support for Conduit. It is built on top of the
`io-throttle` package and uses `unliftio`.

### Example

```haskell
simpleProducerThrottling :: IO ()
simpleProducerThrottling = do
  let conf = newThrottleConf
             & throttleConfThrottleProducer
             & throttleConfSetInterval 1000
             & throttleConfSetMaxThroughput 1
  runResourceT . runConduit $ throttleProducer conf (sourceList [1..5]) .| printC
```
