#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

-- This example is quite similar to the "mvar" example, but uses a FIFO channel to store thread signals, enabling forked threads to terminate before their signal is consumed

module Main where

import Control.Concurrent
import Control.Monad
import Data.Function
import Data.Global
import Data.IORef

un "waitThreads" =:: (utl [ud| fix $ writeIORef waitThreads |], ut [t| IO () |] :: UT IORef)
un "waits" =:: ([| () |], ut [t| () |] :: UT (UDEmpty Chan))

-- | Fork a thread; waitForThreadsToFinish will not terminate until every thread created by this does.
forkIO' :: IO () -> IO ThreadId
forkIO' m = do
    atomicModifyIORef waitThreads $ \a -> (a >> readChan waits, ())
    forkIO $ m >> writeChan waits ()

forkIO'' :: IO () -> IO ()
forkIO'' = void . forkIO'

-- | Calling thread sleeps (GHC only)
sleepSeconds :: Integer -> IO ()
sleepSeconds = threadDelay . fromIntegral . (* 1000000)

thread1 :: IO ()
thread1 = do
    putStrLn $ "thread1 begin"
    sleepSeconds 1
    putStrLn $ "thread1 end"

remoi :: IO ()
remoi = do
    putStrLn $ "remoi begin"
    putStrLn $ "remoi end"

foobarquux :: IO ()
foobarquux = do
    putStrLn $ "foobarquux begin"
    putStrLn $ "foobarquux begins its sleeping for 3 seconds"
    sleepSeconds 3
    putStrLn $ "foobarquux end"

-- | Alias to reading action stored in 'waitThreads' and executing it.
waitForThreadsToFinish :: IO ()
waitForThreadsToFinish = join $ readIORef waitThreads

main :: IO ()
main = (>> waitForThreadsToFinish) $ do
    putStrLn "main begin"
    waitForThreadsToFinish  -- no threads to wait to finish
    putStrLn "Potentially barring output begin"
    forkIO'' thread1
    forkIO'' remoi
    forkIO'' foobarquux
    waitForThreadsToFinish

    putStrLn "All other threads have finished; potentially quuxy output end"
    forkIO'' foobarquux
    -- main will not terminate yet until all the other threads created by 'forkIO'' terminate (so we'll see "foobarquux end")
