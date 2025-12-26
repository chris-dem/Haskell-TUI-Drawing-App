{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------------------------------------------------------

module Lib.StateLib where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Data.Maybe (isNothing)

import Lib.CoordAlg
import Lib.Image
import System.Console.ANSI (getTerminalSize)

------------------------------------------------------------------------------------------------------------------------------
data AppState = AppState
    { _cursorPosition :: !Point
    , _shouldExit :: !Bool
    , _dims :: !(Int, Int)
    , _currentImage :: !Image
    }
    deriving (Show)

makeLenses ''AppState

------------------------------------------------------------------------------------------------------------------------------

data Actions = Exit | RenderImage [Int]

keyboardManagement :: Char -> StateT AppState IO Actions
keyboardManagement 'q' = do
    liftIO (putStrLn "Quitting...")
    return Exit
keyboardManagement _ = undefined

renderImage :: [Int] -> IO ()
renderImage = undefined

handleAction :: Actions -> StateT AppState IO ()
handleAction Exit = return ()
handleAction (RenderImage im) = do
    liftIO $ renderImage im
    liftIO $ threadDelay 50000

mainLoop :: TQueue Char -> StateT AppState IO ()
mainLoop queue = whileM_ (gets (not . _shouldExit)) $ do
    key <- liftIO $ atomically $ readTQueue queue
    keyboardManagement key >>= handleAction

makeEmpty :: IO (Maybe AppState)
makeEmpty = fmap f <$> getTerminalSize
  where
    f (x, y) =
        AppState
            { _cursorPosition = Point 0 0
            , _shouldExit = False
            , _dims = (x, y)
            , _currentImage = createEmpty x y
            }
