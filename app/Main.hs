{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Colour.SRGB (sRGB24)
import System.Console.ANSI

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.State (evalStateT)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Lib.ConsoleManipulation
import Lib.Image
import Lib.StateLib
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering 
    hSetEcho stdin False 
    keyQueue <- newTQueueIO
    let listener = forever $ do
            k <- getChar
            atomically $ writeTQueue keyQueue k
    makeEmpty >>= \case
        Just state -> withAsync listener $ const $ evalStateT (mainLoop keyQueue) state
        Nothing -> return ()
