{-# LANGUAGE FlexibleContexts #-}

import Streamly
import qualified Streamly.Prelude as S
import Streamly.Prelude ((.:), nil, (|:))
import Control.Concurrent
import System.IO
import Data.Foldable (fold)
import Control.Monad.IO.Class
import Control.Monad
import Data.Function ((&))
import System.Clock
import Test.Hspec

durationShouldBe :: (Double, Double) -> IO () -> Expectation
durationShouldBe d@(tMin, tMax) action = do
        t0 <- getTime Monotonic
        action
        t1 <- getTime Monotonic
        let t = (fromIntegral $ toNanoSecs (t1 - t0)) / 1e9
            -- tMax = fromNanoSecs (round $ d*10^9*1.2)
            -- tMin = fromNanoSecs (round $ d*10^9*0.8)
        putStrLn $ "Expected: " ++ show d ++ " Took: " ++ show t
        (t <= tMax && t >= tMin) `shouldBe` True

measureRate :: (IsStream t, Num a)
    => String
    -> (t IO a -> SerialT IO a)
    -> Double
    -> Int
    -> Int
    -> (Double, Double)
    -> Spec
measureRate desc t rate consumerDelay producerDelay dur = do
    it (desc ++ " rate: " ++ show rate
             ++ ", consumer latency: " ++ show consumerDelay
             ++ ", producer latency: " ++ show producerDelay)
    $ durationShouldBe dur $ do
        runStream
            $ (if consumerDelay > 0
              then S.mapM (\x -> threadDelay (consumerDelay * 10^6) >> return x)
              else id)
            $ t
            $ maxBuffer  (-1)
            $ maxThreads (-1)
            $ maxRate rate
            $ S.take  (round $ rate * 10)
            $ S.repeatM $ do
                if producerDelay > 0
                then threadDelay (producerDelay * 10^6)
                else return ()
                return 1

main :: IO ()
main = hspec $ do
    let range = (8,12)

    -- XXX very low rates (e.g. 0.1) fail because we introduce the delay
    -- after yield and not before.
    let rates = [1, 10, 100, 1000, 10000, 100000, 1000000]
     in describe "asyncly no consumer delay no producer delay" $ do
            forM_ rates (\r -> measureRate "asyncly" asyncly r 0 0 range)

    -- XXX try staggering the dispatches to achieve higher rates
    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "asyncly no consumer delay and 1 sec producer delay" $ do
            forM_ rates (\r -> measureRate "asyncly" asyncly r 0 1 range)

    let rates = [1, 10, 100, 1000, 10000, 100000, 1000000]
     in describe "wAsyncly no consumer delay no producer delay" $ do
            forM_ rates (\r -> measureRate "wAsyncly" wAsyncly r 0 0 range)

    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "wAsyncly no consumer delay and 1 sec producer delay" $ do
            forM_ rates (\r -> measureRate "wAsyncly" wAsyncly r 0 1 range)

    -- XXX million takes a long time, need to be fixed.
    let rates = [1, 10, 100, 1000, 10000, 100000]
     in describe "aheadly no consumer delay no producer delay" $ do
            forM_ rates (\r -> measureRate "aheadly" aheadly r 0 0 range)

    -- XXX does not work precisely for rate of 10 or more, need to be fixed
    let rates = [1, 10, 100, 1000, 10000, 25000]
     in describe "aheadly no consumer delay and 1 sec producer delay" $ do
            forM_ rates (\r -> measureRate "aheadly" aheadly r 0 1 range)

    describe "asyncly with 1 sec producer delay and varying consumer delay" $ do
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 1 1 (14, 16))
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 2 1 (21, 23))
        forM_ [1] (\r -> measureRate "asyncly" asyncly r 3 1 (31, 33))

    describe "aheadly with 1 sec producer delay and varying consumer delay" $ do
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 1 1 (11, 16))
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 2 1 (21, 23))
        forM_ [1] (\r -> measureRate "aheadly" aheadly r 3 1 (31, 33))
