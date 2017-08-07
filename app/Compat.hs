{-# LANGUAGE CPP #-}
module Compat where

#ifdef mingw_HOST_OS

import System.Win32.Process (getCurrentProcessId)

getPID :: IO Int
getPID = fromIntegral <$> getCurrentProcessId

#else

#ifdef linux_HOST_OS

import System.Posix.Process (getProcessID)

getPID :: IO Int
getPID = fromIntegral <$> getProcessID

#else

getPID :: IO Int
getPID = error "This software is not compatible with your OS."

#endif

#endif