{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

---
module Lib.Image where

----
import Control.Lens
import Control.Monad (forM_, when)
import Data.Vector qualified as Vec
import Data.Vector.Strict qualified as VS
import Data.Word8
import System.Console.ANSI

import Data.Colour.SRGB (sRGB24)
import Lib.ConsoleManipulation

----

data Image = Image
    { _pixels :: VS.Vector (Word8, Word8, Word8)
    , _dims :: (Int, Int)
    }

makeLenses ''Image

defaultColour = (0, 0, 0)

createEmpty :: Int -> Int -> Image
createEmpty a b = Image (VS.replicate (a * b) defaultColour) (a, b)

createFromTerminal :: IO Image
createFromTerminal = do
    size <- getTerminalSize
    let a = case size of
            Just (r, c) -> (c, r)
            Nothing -> (24, 80)
    return $ uncurry createEmpty a

drawImage :: Image -> IO ()
drawImage (Image pix (xLim, yLim)) = do
    resetScreen
    forM_ [0 .. yLim - 1] $ \r -> do
        forM_ [0 .. xLim - 1] $ \c -> do
            -- print (i `mod` xLim, i `div` xLim, i)
            let i = c + r * xLim
            (r, g, b) <- VS.indexM pix i
            setSGR [SetRGBColor Background $ sRGB24 r g b]
            putStr " "
        putStrLn ""
