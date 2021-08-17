{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax  #-}

module Graphic2D where

import qualified Algebra.Algebraic  as Algebraic
import qualified Algebra.Vector     as Vector
import qualified Data.Monoid        as Monoid
import qualified Data.Vector        as V
import qualified Debug.Trace        as Trace
import qualified Graphics.Gloss     as Gloss
import qualified GravitationalField
import qualified MassPoint
import qualified Number.SI          as SI
import qualified NumericExtend
import           NumericPrelude

class Draw a where
  draw :: a -> Gloss.Picture

-- JSONで書けるようにしたい
-- | 質点をプロットするときのサイズ
massPointSize :: Float
massPointSize = 2

instance Graphic2D.Draw MassPoint.T where
  draw m = Gloss.translate x y $ Gloss.circleSolid massPointSize
    where
    conv = V.map NumericExtend.siToFrac (MassPoint._x m)
    x = conv V.! 0
    y = conv V.! 1

instance Graphic2D.Draw GravitationalField.T where
  draw gf =  foldr (Monoid.<>) Gloss.blank
    [
      Gloss.line [(i, j), (i + gapX, j + gapY)] |
      i <- map ((* drawUnit) . fromIntegral) [- unitsWidth .. unitsWidth],
      j <- map ((* drawUnit) . fromIntegral) [- unitsHeight .. unitsHeight],
      let
        v = V.map NumericExtend.siToFrac . gf . V.fromList $ [realToFrac i * SI.meter, realToFrac j * SI.meter]
        fx = v V.! 0
        fy = v V.! 1
        f = sqrt (fx ^ 2 + fy ^ 2)
        [gapX, gapY] = (gap * gapFunc f / f) Vector.*> [fx, fy]
    ]
    where
    unitsWidth = ceiling $ fromIntegral (windowWidth `div` 2) / drawUnit :: Int
    unitsHeight = ceiling $ fromIntegral (windowHeight `div` 2) / drawUnit :: Int

gap :: Float
gap = 28

gapFunc :: Float -> Float
gapFunc x = 1 - Algebraic.power (- x) (1 + 1e-2)

windowWidth :: Int
windowWidth  = 640
windowHeight :: Int
windowHeight = 480

drawUnit :: Float
drawUnit = 30
