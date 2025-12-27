{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib.CoordAlg where

import Control.Lens
import Data.Proxy
import Data.VectorSpace
import GHC.TypeLits

data Point = Point
    { _xDim :: Int
    , _yDim :: Int
    }
    deriving (Eq, Show)

makeLenses ''Point

data Topos = Topos
    { _xLim :: Int
    , _yLim :: Int
    , _modular :: Bool
    }
    deriving (Eq, Show)

instance AdditiveGroup Point where
    zeroV = Point 0 0
    (Point x1 y1) ^+^ (Point x2 y2) = Point (x1 + x2) (y1 + y2)
    negateV (Point x y) = Point (-x) (-y)

instance VectorSpace Point where
    type Scalar Point = Integer

    a *^ Point x y = Point (fromInteger a * x) (fromInteger a * y)

addP :: Topos -> Point -> Point -> Point
addP t a = fieldSpace t . (^+^ a)

fieldSpace :: Topos -> Point -> Point
fieldSpace (Topos x y b) p =
    p
        & xDim %~ g b x
        & yDim %~ g b y
  where
    g b z = if b then flip mod z else min z . max 0
