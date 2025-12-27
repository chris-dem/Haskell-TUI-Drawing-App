{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Lens.Micro.TH (makeLenses)

import Lib.StateLib

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Table

main :: IO ()
main = undefined

app :: App () () ()
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = undefined
        , appStartEvent  = undefined
        }

drawUI = undefined
