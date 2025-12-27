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
import Debug.Trace

import Data.VectorSpace
import Lib.CoordAlg
import Lib.Image
import System.Console.ANSI (getTerminalSize)
import System.Console.ANSI qualified as TUI
import System.IO (hFlush, stdout)

------------------------------------------------------------------------------------------------------------------------------
data AppState = AppState
    { _cursorPosition :: !Point
    , _shouldExit :: !Bool
    , _terminalDims :: !(Int, Int)
    , _currentImage :: !Image
    }
    deriving (Show)

makeLenses ''AppState

------------------------------------------------------------------------------------------------------------------------------

data Actions = Exit | RenderImage | MousePosition

toposGen x y = Topos x y False

keyboardManagement :: Char -> StateT AppState IO Actions
keyboardManagement 'f' = do
    topos <- gets (uncurry toposGen . _terminalDims)
    gets (cursorPosition %~ (fieldSpace topos . (^+^ Point (-1) 0))) >>= put
    return MousePosition
keyboardManagement 's' = do
    topos <- gets (uncurry toposGen . _terminalDims)
    gets (cursorPosition %~ (fieldSpace topos . (^+^ Point 1 0))) >>= put
    return MousePosition
keyboardManagement 'r' = do
    topos <- gets (uncurry toposGen . _terminalDims)
    gets (cursorPosition %~ (fieldSpace topos . (^+^ Point 0 (-1)))) >>= put
    return MousePosition
keyboardManagement 't' = do
    topos <- gets (uncurry toposGen . _terminalDims)
    gets (cursorPosition %~ (fieldSpace topos . (^+^ Point 0 1))) >>= put
    return MousePosition
keyboardManagement 'q' = do
    liftIO (putStrLn "Quitting...")
    gets (set shouldExit True) >>= put
    return Exit
keyboardManagement _ = return RenderImage

handleAction :: Actions -> StateT AppState IO ()
handleAction Exit = return ()
handleAction MousePosition = do
    a@(Point x y) <- gets _cursorPosition
    liftIO $ TUI.setCursorPosition x y
handleAction RenderImage = do
    im <- gets _currentImage
    liftIO $ drawImage im

mainLoop :: TQueue Char -> StateT AppState IO ()
mainLoop queue = whileM_ (gets (not . _shouldExit)) $ do
    key <- liftIO $ atomically $ readTQueue queue
    keyboardManagement key >>= handleAction
    liftIO $ hFlush stdout

makeEmpty :: IO (Maybe AppState)
makeEmpty = fmap f <$> getTerminalSize
  where
    f (y, x) =
        AppState
            { _cursorPosition = Point 0 0
            , _shouldExit = False
            , _terminalDims = (x, y)
            , _currentImage = createEmpty x y
            }
