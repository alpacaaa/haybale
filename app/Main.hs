{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib
import qualified Control.Exception as Exception


main :: IO ()
-- main = Lib.fakeRequestUploadFile

main = Lib.startServer
