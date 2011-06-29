#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Function
import Data.Global
import Data.IORef
import Data.Time.Clock.POSIX
import System.CPUTime
import Text.Printf

un "waitThreads" =:: (utl [ud| fix $ writeIORef waitThreads |], ut [t| IO () |] :: UT IORef)
un "waits" =:: ([| () |], ut [t| () |] :: UT (UDEmpty Chan))

-- | Fork a thread; waitForThreadsToFinish will not terminate until every thread created by this does.
forkIO' :: IO () -> IO ThreadId
forkIO' m = do
    atomicModifyIORef waitThreads $ \a -> (a >> readChan waits, ())
    forkIO $ m >> writeChan waits ()

forkIO'' :: IO () -> IO ()
forkIO'' = void . forkIO'

-- | Alias to reading action stored in 'waitThreads' and executing it.
waitForThreadsToFinish :: IO ()
waitForThreadsToFinish = join $ readIORef waitThreads


un "vacant_study_rooms" =:: ([| 10 :: QSemQuantity |], ut [t| () |] :: UT (Const QSem))

-- | Calling thread sleeps (GHC only)
sleepSeconds :: Integer -> IO ()
sleepSeconds = threadDelay . fromIntegral . (* 1000000)

un "epoch" =:: (uninitialized, ut [t| Integer |] :: UT IORef)

putStrCPUSecsLn :: String -> IO ()
--putStrMCPUSecsLn s = ((`div` 1000000000) <$> getCPUTime) >>= (\t -> putStrLn $ printf "[%dms] %s" t s)
--putStrCPUSecsLn s = ((`div` 1000000000000) <$> getCPUTime) >>= (\t -> putStrLn $ printf "[%ds] %s" t s)
putStrCPUSecsLn s = ((`div` 1000000000000) <$> getCPUTime) >>= (\t -> putStrLn $ printf "[%02ds] %s" t s)  -- Formatted for just this program

putStrSecsLn :: String -> IO ()
putStrSecsLn s = getSecondsSinceEpoch >>= (\t -> putStrLn $ printf "[%02ds] %s" t s)  -- Formatted for just this program

setEpoch :: IO ()
setEpoch = writeIORef epoch =<< getSecondsSince

getMicrosecondsSince :: IO Integer
getMicrosecondsSince = round . (* 1000000) <$> getPOSIXTime

getSecondsSince :: IO Integer
getSecondsSince = round <$> getPOSIXTime

getSecondsSinceEpoch :: IO Integer
getSecondsSinceEpoch = pure (-) <*> getSecondsSince <*> readIORef epoch

requestRoomSeconds :: String -> Integer -> IO ()
requestRoomSeconds name seconds = forkIO'' $ do
    putStrSecsLn $ printf "%-30s requests a study room for   %02d seconds" name seconds
    waitQSem vacant_study_rooms
    putStrSecsLn $ printf "%-30s enters   a study room for   %02d seconds" name seconds
    sleepSeconds seconds
    putStrSecsLn $ printf "%-30s leaves   a study room after %02d seconds" name seconds
    signalQSem vacant_study_rooms

data LibraryEvent =
    WaitSeconds Integer
  | RequestRoom String  Integer

newtype Library = Library {unLibrary :: [LibraryEvent]}

runLibraryEvent :: LibraryEvent -> IO ()
runLibraryEvent (WaitSeconds seconds)      = sleepSeconds seconds
runLibraryEvent (RequestRoom name seconds) = requestRoomSeconds name seconds

runLibrary :: Library -> IO ()
runLibrary = mapM_ runLibraryEvent . unLibrary

main :: IO ()
main = (>> waitForThreadsToFinish) $ do
    setEpoch
    runLibrary es
    putStrSecsLn $ printf "No more requests"
    where room = flip RequestRoom
          wait = WaitSeconds
          es   = Library $
              [ room 20 "Manuel Chakravarty"
              , wait 5
              , room 15 "Tim Chevalier"
              , room 20 "Duncan Coutts"
              , room 15 "Iavor S Diatchki"
              , room 15 "Andy Gill"
              , wait 5
              , room 05 "David Himmelstrup"
              , room 15 "Roman Leshchinskiy"
              , room 20 "Ben Lippmeier"
              , room 20 "Andres Loeh"
              , room 10 "Ian Lynagh"
              , room 20 "Simon Marlow"
              , room 10 "John Meacham"
              , room 15 "Ross Paterson"
              , room 15 "Sven Panne"
              , room 20 "Simon Peyton Jones"
              , room 10 "Norman Ramsey"
              , room 20 "Don Stewart"
              , room 15 "Josef Svenningsson"
              , wait 5
              , room 10 "Audrey Tang"
              , room 10 "David Terei"
              , room 05 "Wolfgang Thaller"
              , room 10 "David Waern"
              , wait 5
              , room 05 "Malcolm Wallace"
              , room 05 "Ashley Yakeley"
  
              , wait 10
  
              , room 01 "Krasimir Angelov"
              , room 05 "Lennart Augustsson"
              , room 03 "Jean-Philippe Bernardy"
              , room 02 "Jost Berthold"
              , room 04 "Bjorn Bringert"
              , room 01 "Sebastien Carlier"
              , room 05 "Andrew Cheadle"
              , room 01 "Sigbjorn Finne"
              , room 01 "Kevin Glynn"
              , room 04 "John Goerzen"
              , room 03 "Cordy Hall"
              , room 05 "Kevin Hammond"
              , room 01 "Tim Harris"
              , room 04 "José Iborra"
              , room 03 "Isaac Jones"
              , room 02 "Ralf Laemmel"
              , room 05 "Hans Wolfgang Loidl"
              , room 01 "John Launchbury"
              , room 03 "Ryan Lortie"
              , room 01 "Jim Mattson"
              , room 05 "Darren Moffat"
              , room 03 "Nick Nethercote"
              , room 04 "Thomas Nordin"
              , room 05 "Bryan O'Sullivan"
              , room 04 "Sungwoo Park"
              , room 04 "Will Partain"
              , room 01 "Juan Quintela"
              , room 05 "Alastair Reid"
              , room 01 "Ben Rudiak-Gould"
              , room 03 "Patrick Sansom"
              , room 03 "André Santos"
              , room 04 "Sean Seefried"
              , room 04 "Julian Seward"
              , room 04 "Dominic Steinitz"
              , room 04 "Volker Stolz"
              , room 05 "Dinko Tenev"
              , room 05 "Mike Thomas"
              , room 01 "Reuben Thomas"
              , room 03 "Christopher D. Thompson-Walsh"
              , room 01 "Dylan Thurston"
              , room 03 "Phil Trinder"
              , room 03 "Mark Tullsen"
              , room 01 "David N Turner"
              , room 03 "Philip Wadler"
              , room 01 "Michael Weber"
              , room 04 "N. Xu"
  
              , wait 15
  
              , room 01 "Sigbjorn Finne"
              , room 04 "Simon Marlow"
              , room 05 "Simon Peyton Jones"
              , room 03 "FreeBSD Haskell Team"
              , room 01 "Matthias Kilian"
              , room 01 "Sven Panne,Ralf Hinze"
              , room 02 "Gentoo Haskell team"
              , room 02 "Kari Pahula"
              , room 05 "Manuel Chakravarty"
              , room 05 "Fedora Haskell SIG"
              , room 05 "Audrey Tang"
              , room 05 "Ryan Lortie"
              , room 01 "Gentoo Haskell team"
              , room 03 "Kari Pahula"
              , room 01 "Wolfgang Thaller,Thorkil Naur"
              , room 02 "Fedora Haskell SIG"
              , room 03 "Ben Lippmeier"
              , room 03 "Gentoo Haskell team"
              , room 01 "Kari Pahula"
              , room 01 "Simon Marlow"
              , room 03 "Gentoo Haskell team"
              , room 04 "Kari Pahula"
              , room 02 "FreeBSD Haskell Team"
              , room 03 "Matthias Kilian"
              , room 04 "Fedora Haskell SIG"
              , room 04 "Don Stewart"
              , room 01 "Kari Pahula"
              , room 05 "Kari Pahula"
              , room 02 "Matt Chapman"
              , room 01 "Gentoo Haskell team"
              , room 01 "Kari Pahula"
              , room 03 "Ken Shan"
              , room 04 "Gentoo Haskell team"
              , room 04 "Kari Pahula"
              , room 01 "Gentoo Haskell team"
              , room 04 "Kari Pahula"
              , room 03 "Kari Pahula"
              , room 05 "Kari Pahula"
              ]
