#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

-- NB: This file, like the others, is small enough for other code to be
-- included in the same module.  In larger scale applications, it may be
-- advantageous in terms of performance to isolate the effects of -fno-cse to
-- a single module containing the unique declarations.  (-fcse is an
-- optimization that enables GHC to "factor out" similar code and treat them
-- as the same expression.)  As of global-0.1.0, this needs to be disabled
-- with "-fno-cse" for uniquely declaring 'IORef's.

module Main where

import Data.IORef
import Data.Global
import Text.Printf

un "lives" =:: ([| 3 :: Integer |], ut [t| Integer |] :: UT IORef)

main :: IO ()
main = do
    putStrLn . printf "Initial value of 'lives': %d" =<< readIORef lives
    writeIORef lives 4
    putStrLn "'lives' updated"
    putStrLn . printf "Current value of 'lives': %d" =<< readIORef lives
