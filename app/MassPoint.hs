{-# LANGUAGE RebindableSyntax #-}

module MassPoint where

import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Vector    as Vector
import qualified Debug.Trace       as Trace
import qualified GHC.Real          as Real
import qualified Graphic2D
import qualified Graphics.Gloss    as Gloss
import qualified Number.Physical   as Physical
import qualified Number.SI         as SI
import qualified Number.SI.Unit    as Unit
import           NumericPrelude

type SIDouble = SI.T Double Double

data T = Cons {
  _x :: [SIDouble],
  _v :: [SIDouble],
  _a :: [SIDouble],
  _m :: SIDouble
} deriving (Eq, Show)

instance Graphic2D.Draw T where
  draw m = Gloss.translate x y $ Gloss.circleSolid massPointSize
    where
    (x, y) = case _x m of
      [x', y'] -> (siToFrac x', siToFrac  y')
      _        -> error "Invalid Dimension"

-- JSONで書けるようにしたい
-- | 質点をプロットするときのサイズ
massPointSize :: Float
massPointSize = 2

-- なんか嫌だけどしかたない
-- | SI単位系を無理やりFracクラスに属する型に変換
siToFrac :: (Real.Real a1, Real.Fractional b) => SI.T a2 a1 -> b
siToFrac (SI.Cons (Physical.Cons _ x)) = realToFrac x

-- | オイラー法
-- | 適当に更新
update :: SIDouble -> T -> T
update t m = m {
  _x = _x m + t Vector.*> _v m,
  _v = _v m + t Vector.*> _a m,
  _a = replicate (length $ _a m) (0 * SI.meter / SI.second ^ 2)
  }

-- | Leap-frog 法
-- | 前フレームからの経過時間と、今のフレームの加速度から時間発展を計算
updateLF :: SIDouble -> [SIDouble] -> T -> T
updateLF t a' m = m {
  _x = _x m + t Vector.*> _v m + (t ^ 2 / 2) Vector.*> _a m,
  _v = _v m + (t / 2) Vector.*> (_a m + a'),
  _a = a'
  }

-- | すべてのパラメータが0で初期化されたn次元上の質点
zeros :: Int -> T
zeros n = Cons {
  _x = replicate n (0 * SI.meter),
  _v = replicate n (0 * SI.meter / SI.second),
  _a = replicate n (0 * SI.meter / SI.second ^ 2),
  _m = 0 * Unit.kilo * SI.gramm
  }
