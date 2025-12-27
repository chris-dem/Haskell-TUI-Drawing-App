{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

------------------------------------------------------------------------------------------------------------------------------

module Lib.StateLib where

import Control.Monad
import Data.Maybe (isNothing)
import Debug.Trace

data AppState = AState ()
