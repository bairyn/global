#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-cse #-}

-- This program can crashed by attempting to read an uninitialized 'IORef' with incorrect user input (when he chooses not to input a line after the confirmation)

module Main where

import Data.IORef
import Data.Global
import System.IO
import Text.Printf

un "input" =:: (uninitialized, ut [t| String |] :: UT IORef)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    putStr $ printf "This program prints its input (after the confirmation prompt).  Would you like to provide input? [y/N]: "
    getLine >>= \conf -> case conf of
        ('y':_) -> do
            putStr $ "Input: "
            getLine >>= writeIORef input
        _       -> return ()
    putStrLn . printf "The input: %s" =<< readIORef input
