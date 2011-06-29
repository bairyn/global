#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Concurrent
import Control.Monad
import Data.Function
import Data.Global
import Data.IORef
import Text.Printf

un "waitThreads" =:: (utl [ud| fix $ writeIORef waitThreads |], ut [t| IO () |] :: UT IORef)  -- This exemplifies self-referential recursion.  The action, when executed, will stop the current thread until all the threads created by 'forkIO'' have finished.
un "waits" =:: ([| () |], ut [t| () |] :: UT (UDEmpty MVar))

-- 'test' is used as an IORef would be in this program, but we declare it as an 'MVar'.
un "test" =:: ([| 'a' |], ut [t| Char |] :: UT MVar)

-- | Fork a thread; waitForThreadsToFinish will not terminate until every thread created by this does.
forkIO' :: IO () -> IO ThreadId
forkIO' m = do
    atomicModifyIORef waitThreads $ \a -> (a >> takeMVar waits, ())
    forkIO $ m >> putMVar waits ()  -- The thread won't terminate until 'waitThreads' consumes our signal.  Using "channels" solves this problem, since the thread won't wait until the 'MVar' is empty; as is seen in a similar example.

forkIO'' :: IO () -> IO ()
forkIO'' = void . forkIO'

-- | Calling thread sleeps (GHC only)
sleepSeconds :: Integer -> IO ()
sleepSeconds = threadDelay . fromIntegral . (* 1000000)

thread1 :: IO ()
thread1 = do
    putStrLn $ "thread1 begin"
    incTest
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

incTest :: IO ()
incTest = do
    modifyMVar_ test $ return . succ

main :: IO ()
main = (>> waitForThreadsToFinish) $ do
    putStrLn . printf "'%c' was taken from 'test'" =<< takeMVar test
    putMVar test 'c'
    putStrLn "Potentially quuxy output begin"
    forkIO'' thread1
    forkIO'' remoi
    forkIO'' foobarquux
    incTest
    waitForThreadsToFinish

    putStrLn "All other threads have finished; potentially quuxy output end"
    putStrLn . printf "'test' is now '%c'" =<< readMVar test
    incTest
    putStrLn . printf "'test' is now '%c'" =<< readMVar test
    forkIO'' foobarquux
    -- main will not terminate yet until all the other threads created by 'forkIO'' terminate (so we'll see "foobarquux end")
