module Lib.ConsoleManipulation where

import System.Console.ANSI
import Control.Concurrent (threadDelay)
import System.IO

resetScreen :: IO ()
resetScreen = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

waitTimer :: Int -> IO ()
waitTimer s = do 
        hFlush stdout
        threadDelay $ s * (10 ^ 6)


