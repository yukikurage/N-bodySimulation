{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax  #-}

module GravitationalField where

import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Vector    as Vector
import qualified Data.Monoid       as Monoid
import qualified Debug.Trace       as Trace
import qualified Graphic2D
import qualified Graphics.Gloss    as Gloss
import qualified MassPoint
import qualified Number.SI         as SI
import qualified Number.SI.Unit    as Unit
import           NumericPrelude

type SIDouble = SI.T Double Double

type T = [SIDouble] -> [SIDouble]

gap :: Float
gap = 30

gapFunc :: Float -> Float
gapFunc x = 1 - Algebraic.power (- x) (1 + 1e-2)

windowWidth :: Int
windowWidth  = 640
windowHeight :: Int
windowHeight = 480

drawUnit :: Float
drawUnit = 40

instance Graphic2D.Draw ([SIDouble] -> [SIDouble]) where
  draw gf = foldr (Monoid.<>) Gloss.blank
    [Gloss.line [(i, j), (i + gapX, j + gapY)] |
      i <- map ((* drawUnit) . fromIntegral) [- unitsWidth .. unitsWidth],
      j <- map ((* drawUnit) . fromIntegral) [- unitsHeight .. unitsHeight],
      let [fx, fy] = map MassPoint.siToFrac $ gf [realToFrac i Vector.*> SI.meter, realToFrac j Vector.*> SI.meter],
      let f = sqrt (fx ^ 2 + fy ^ 2),
      let [gapX, gapY] = (gap * gapFunc f / f) Vector.*> [fx, fy]
    ]
    where
    unitsWidth = ceiling $ fromIntegral (windowWidth `div` 2) / drawUnit :: Int
    unitsHeight = ceiling $ fromIntegral (windowHeight `div` 2) / drawUnit :: Int


gConst :: SIDouble
gConst = 6.67430e-11 * SI.meter ^ 3 / (Unit.kilo * SI.gramm * SI.second ^ 2)

compute :: SIDouble -> [SIDouble] -> T
compute m r1 r2
  | r1 == r2 = replicate (length r1) $ 0 * SI.meter / SI.second ^ 2
  | r1 /= r2 = - (gConst * m / magnitude r ^ 3) Vector.*> r
  where
  r = r2 - r1

fromMassPoint :: MassPoint.T -> T
fromMassPoint  m = compute (MassPoint._m m) (MassPoint._x m)

forceMassPoint :: T -> MassPoint.T -> MassPoint.T
forceMassPoint g m = MassPoint.force
  (MassPoint._m m Vector.*> g (MassPoint._x m))
  m

magnitude :: Algebraic.C a => [a] -> a
magnitude = sqrt . sum1 . map (^ 2)

zero :: Int -> T
zero n = const $ replicate n (0 * SI.meter / SI.second ^ 2)
