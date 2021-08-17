{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax  #-}

module GravitationalField where

import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Vector    as Vector
import qualified Data.Monoid       as Monoid
import qualified Data.Vector       as V
import qualified Debug.Trace       as Trace
import qualified Graphics.Gloss    as Gloss
import qualified MassPoint
import qualified Number.SI         as SI
import qualified Number.SI.Unit    as Unit
import qualified NumericExtend
import           NumericPrelude

type SIDouble = SI.T Double Double

type T = V.Vector SIDouble -> V.Vector SIDouble

-- | 重力定数
gConst :: SIDouble
gConst = 6.67430e-11 * SI.meter ^ 3 / (Unit.kilo * SI.gramm * SI.second ^ 2)

-- | 質点の質量、位置から重力場を計算
compute :: SIDouble -> V.Vector SIDouble -> T
compute m r1 r2
  | r1 == r2 = V.replicate (V.length r1) $ 0 * SI.meter / SI.second ^ 2
  | r1 /= r2 = negate (gConst * m / NumericExtend.magnitude r ^ 3) Vector.*> r
  where
  r = r2 - r1

-- | 質点から重力場を計算
fromMassPoint :: MassPoint.T -> T
fromMassPoint m = compute (MassPoint._m m) (MassPoint._x m)

-- | すべての位置において0ベクトルである重力場(n次元)
zero :: Int -> T
zero n = const $ V.replicate n (0 * SI.meter / SI.second ^ 2)
