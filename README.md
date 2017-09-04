# throttle-conduit [![Hackage version](https://img.shields.io/hackage/v/throttle-conduit.svg?label=Hackage)](https://hackage.haskell.org/package/throttle-conduit) [![Stackage version](https://www.stackage.org/package/throttle-conduit/badge/lts?label=Stackage)](https://www.stackage.org/package/throttle-conduit) [![Build Status](https://travis-ci.org/mtesseract/throttle-conduit.svg?branch=master)](https://travis-ci.org/mtesseract/throttle-conduit)

### About

This is `conduit-throttle`, a small Haskell package providing
configurable throttling support for Conduit. It is built on top of the
packages `throttle-io-stream` and `unliftio`.

The API (`Data.Conduit.Throttle`) is designed for being imported
qualified, which is why the exported functions and values are not
prefixed by the package name or something similar.

The core function exported by this package is the following:

```haskell
throttleProducer :: (MonadUnliftIO m, MonadResource m)
                 => Conf a
                 -> Producer m a
                 -> Producer m a
```

That is, given a `ThrottleConf` and a Conduit `Producer`, create and
return a new Conduit `Producer`, which yields the same stream of
values like the provided one but throttled according to the provided
throttling configuration.

A throttling configuration is created using `newConf and then
configured using the functions `setMeasure`, `setMaxThroughput`,
`setBufferSize` and others.

Primarily, the throughput is defined by the provided measure function
and the configured maximum throughput. A measure function is a
function which defines how "big" a conduit element `a` is. In other
words, it is a function of type `a -> Double`. For instance, it could
simply calculate the length of some deserialized value or it could
simply be the constant function `const 1` when it is simply desired to
throttle the number of elements a Conduit producer produces. The
maximum throughput together with the measure function then defines the
maximum throughput to produce (of course it can be less, if the
original producer provides data with a smaller throughput). The
throughput is considered with respect to a base time interval, which
can be changed using `setInterval`, by default it is `1000` (ms).

The underlying package `throttle-io-stream` uses exponentially
weighted moving averages for smoothing the actual throughput changes.

### Example

```haskell
import qualified Data.Conduit.Throttle as ConduitThrottle

simpleProducerThrottling :: IO ()
simpleProducerThrottling = do
  let conf = ConduitThrottle.newConf
             & ConduitThrottle.setInterval 1000
             & ConduitThrottle.setMaxThroughput 1
  runResourceT . runConduit $
    ConduitThrottle.throttleProducer conf (sourceList [1..5]) .| printC
```
