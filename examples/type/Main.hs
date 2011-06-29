#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, ExplicitForAll #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.IORef
import Data.Global
import Text.Printf

un "etcetera" =:: ([| [] |], ut [t| [Integer] |] :: UT IORef)
--un "etcetera" =:: ([| [] |], ut [t| forall a. [a] |] :: UT IORef)  -- If this line were uncommented (and the above were commented) then this program should not compile.

main :: IO ()
main = do
    putStrLn . printf "Initial value of 'etcetera': %s" . show =<< readIORef etcetera
    writeIORef etcetera [3]
    putStrLn "'etcetera' updated"
    putStrLn . printf "Current value of 'etcetera': %s" . show =<< readIORef etcetera
