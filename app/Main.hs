module Main where

import Control.Monad (forM_, forever, replicateM_, unless, when)
import Data.Colour.SRGB (sRGB24)
import System.Console.ANSI

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Lib.ConsoleManipulation
import Lib.Image
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering -- 1. Don't wait for Enter
    hSetEcho stdin False -- 2. Don't show the key on screen
    keyQueue <- newTQueueIO
    -- Start the background key listener
    -- This won't block your image drawing
    let listener = forever $ do
            k <- getChar
            atomically $ writeTQueue keyQueue k
    withAsync listener $ \_ -> do
        renderLoop keyQueue
    return ()

renderLoop :: TQueue Char -> IO ()
renderLoop queue = do
    -- 1. Try to see if a key was pressed (Non-blocking check)
    maybeKey <- atomically $ readTQueue queue

    x <- case maybeKey of
        'q' -> putStrLn "Quitting..." >> return True
        _ -> return False
    unless x $ do
        -- 2. Draw your image
        -- drawImage myImage
        putStrLn "Printing Image"
        threadDelay 50000 -- 20 FPS
        renderLoop queue

-- Use StateT s m a in orde
