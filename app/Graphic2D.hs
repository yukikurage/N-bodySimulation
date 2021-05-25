module Graphic2D where

import           Graphics.Gloss as Gloss

class Draw a where
  draw :: a -> Gloss.Picture
