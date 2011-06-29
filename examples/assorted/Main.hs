#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MSampleVar as MSampleVar
import Control.Monad
import Data.Global
import Text.Printf

-- | Calling thread sleeps (GHC only)
sleepSeconds :: Integer -> IO ()
sleepSeconds = threadDelay . fromIntegral . (* 1000000)

un "numChildProcessesAvailable" =:: ([| 3 :: QSemQuantity |], ut [t| () |] :: UT (Const QSemN))
-- TODO: others: SampleVar, FairRWLock, MSampleVar, MSem, MSemN, TVar, TMVar, TChan, TArray

-- | Alias to 'numChildProcessesAvailable'
numChildren :: QSemN
numChildren = numChildProcessesAvailable

un "sample" =:: (uninitialized, ut [t| () |] :: UT (UDEmpty SampleVar))
un "sample2" =:: ([| () |], ut [t| () |] :: UT SampleVar)

un "channel" =:: (uninitialized, ut [t| String |] :: UT (UDEmpty MSampleVar))

-- | Part from the currently joined channel.  If there is no channel, do nothing
dropChannel :: IO ()
dropChannel = do
    emptySV channel
    putStrLn $ printf "no longer in channel"

-- | Go to a channel; bring it to focus; backlog is downloaded if necessary.  If a channel is already in focus
gotoChannel :: String -> IO ()
gotoChannel name = do
    writeSV channel name
    putStrLn $ printf "joined '%s'" name

-- Normally, this would be split into multiple actions.
main :: IO ()
main = do
    -- Start two individual processes, independent of each other
    waitQSemN numChildren 1 >> putStrLn "A starts"
    waitQSemN numChildren 1 >> putStrLn "B starts"

    -- Write 
    writeSampleVar sample ()
    writeSampleVar sample ()
    writeSampleVar sample ()
    writeSampleVar sample ()
    -- ^ Since it's a SampleVar, when it's full, this thread won't block; the value will be overwriting instead

    -- Start two mutually dependant processes who communicate with each other
    void . forkIO $ do
        waitQSemN numChildren 2  -- C, D
        putStrLn $ printf "C and D started"
    sleepSeconds 1

    -- Take sample
    readSampleVar sample
    -- The sample variable is now empty; reading from it again will cause the thread to block since it is now empty

    signalQSemN numChildren 1 >> putStrLn "A finishes"
    signalQSemN numChildren 1 >> putStrLn "C finishes"
    signalQSemN numChildren 1 >> putStrLn "D finishes"
    writeSampleVar sample2 ()  -- (Doesn't block)
    signalQSemN numChildren 1 >> putStrLn "B finishes"

    gotoChannel "#haskell"
    gotoChannel "#lojban"
    dropChannel
    dropChannel
    dropChannel
    gotoChannel "##categorytheory"
    dropChannel
    gotoChannel "#agda"
    gotoChannel "#bitcoin"
    gotoChannel "#bitcoin-otc-foyer"

    sleepSeconds 3

    -- …

    -- Very unreliable means of waiting for other threads to finish
    sleepSeconds 5

    putStrLn "TODO: finish (include every instance that isn't in any other example)"
