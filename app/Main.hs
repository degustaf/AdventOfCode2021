{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import System.Environment (getArgs)
import qualified RIO.Text as T

main :: IO ()
main = do
  options <- getArgs
  lo <- logOptionsHandle stderr False
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = fmap T.pack options
          }
     in runRIO app run
