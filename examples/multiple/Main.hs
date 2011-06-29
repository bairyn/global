#!/usr/bin/env runhaskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-cse #-}
--{-# OPTIONS_GHC -fcse #-}  -- If this line were uncommented (and the preceding line were uncommented), the program might behave differently, if 'the_log0' and 'the_log1' are "commoned-up".

{- This program shows a potential pitfall, two variables referring to the
 - same data, due to the documented caveat of unique, top-level 'IORef's; and
 - how it can be avoided: by ensuring that "cse" is disabled.  This can occur
 - if the defalut  and the  are the same.  Or This can occur if the (initial
 - values|types) are the same, even if the <other>
 - differ.
 -
 - NB: The implementation of appending strings here is inefficient but simple
 - enough to serve this example's purpose.
 -
 - NB: On my system, even with -fcse, 'the_log0' and 'the_log1' were still
 - bound to distinct 'IORef's.
 -}

module Main where

import Data.Global
import Data.IORef
import Text.Printf

un "the_log0" =:: ([| "" |], ut [t| String |] :: UT IORef)
un "the_log1" =:: ([| "" |], ut [t| String |] :: UT IORef)

-- | Log a line to 'the_log0'; a newline character is appended by the action.
log0 :: String -> IO ()
log0 l = atomicModifyIORef the_log0 $ \s -> (s ++ l ++ "\n", ())

-- | Log a line to 'the_log1'; a newline character is appended by the action.
log1 :: String -> IO ()
log1 l = atomicModifyIORef the_log1 $ \s -> (s ++ l ++ "\n", ())

main :: IO ()
main = do
    putStrLn . printf "the_log0 nows contains '%s'" =<< readIORef the_log0
    putStrLn . printf "the_log1 nows contains '%s'" =<< readIORef the_log1
    log0 "This should only appear in log0"
    log1 "This should only appear in log1"
    putStrLn . printf "the_log0 nows contains '%s'" =<< readIORef the_log0
    putStrLn . printf "the_log1 nows contains '%s'" =<< readIORef the_log1
