{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard, void)
import Data.Maybe (fromMaybe)

import Lens.Micro.TH (makeLenses)

import Lib.StateLib

import Brick
import Brick qualified as V
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Brick.Widgets.Center (center)
import Brick.Widgets.Table
import Data.Complex (Complex ((:+)), magnitude)
import Data.List (intersperse)
import Graphics.Vty (Color)
import Graphics.Vty qualified as V
import Lens.Micro
import Linear (V2 (V2))

main :: IO ()
main = do
    let app =
            App
                { appDraw = const [drawUI]
                , appChooseCursor = neverShowCursor
                , appHandleEvent = appHandleEventFo
                , appStartEvent = return ()
                , appAttrMap = const theMap
                }
    defaultMain app ()

appHandleEventFo :: BrickEvent n e -> EventM n s ()
appHandleEventFo (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appHandleEventFo (VtyEvent (V.EvKey (V.KChar 'Q') [])) = halt
appHandleEventFo _ = return ()

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (colouredSquare, V.yellow `V.on` V.yellow)
        ]

menu :: Widget ()
menu =
    vLimitPercent 20 $
        withBorderStyle unicode $
            borderWithLabel (str "Layout") $
                center $
                    hBox $
                        intersperse (fill ' ') $
                            map center [str "This", str "is", str "a", str "test"]

colouredStr :: Color -> String -> Widget n
colouredStr c s = raw $ V.string (V.defAttr `V.withForeColor` c `V.withBackColor` c) s

normaliseComplex :: Int -> Int -> Int -> Int -> Complex Double
normaliseComplex h w x y = (x' / w' * l - 3 * l / 4) :+ (y' / h' * l - l / 2)
  where
    h' = fromIntegral h
    w' = fromIntegral w
    x' = fromIntegral x
    y' = fromIntegral y
    l = 2.5

drawingArea :: Widget ()
drawingArea = Widget Greedy Greedy $ do
    ctx <- getContext
    let w = ctx ^. availWidthL -- available width
    let h = ctx ^. availHeightL -- available height
    let rows = [hBox $ cellsInRow r | r <- [h - 1, h - 2 .. 0]]
        cellsInRow y = [drawCoord (normaliseComplex h w x y) | x <- [0 .. w - 1]]
    render $
        withBorderStyle unicode $
            borderWithLabel (str "Drawing Area") $
                vBox rows <+> fill ' '

mandle :: Complex Double -> Int
mandle p = mandle' 0 p 0

mandle' :: Complex Double -> Complex Double -> Int -> Int
mandle' p c i
    | i >= 100 = 100
    | magnitude p >= 2 = i
mandle' p c i = mandle' (p * p + c) c (i + 1)

toRGB :: Double -> (Int, Int, Int)
toRGB x | x <= 0.25 = (0, 0, floor (255 * (x / 0.25)))
toRGB x | x <= 0.5 = (0, floor (255 * ((x - 0.25) / 0.25)), 255)
toRGB x
    | x <= 0.75 =
        ( floor $ 255 * ((x - 0.5) / 0.25)
        , floor $ 255 * (1 - (x - 0.5) / 0.25)
        , 255 - (floor $ 255 * ((x - 0.5) / 0.25))
        )
    | x <= 1 =
        ( 255
        , floor $ 255 * ((x - 0.75) / 0.25)
        , 0
        )

drawCoord :: Complex Double -> Widget ()
drawCoord p = colouredStr c " "
  where
    (r, g, b) = toRGB . (/ 100) . fromIntegral $ mandle p
    c = V.rgbColor r g b

colouredSquare :: AttrName
colouredSquare = attrName "colouredSquare"

drawUI :: Widget ()
drawUI = vBox [menu, drawingArea]
