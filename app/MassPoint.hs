{-# LANGUAGE RebindableSyntax #-}

module MassPoint where

import qualified Algebra.Algebraic as Algebraic
import qualified Algebra.Vector    as Vector
import qualified Data.Vector       as V
import qualified Debug.Trace       as Trace
import qualified GHC.Real          as Real
import qualified Graphics.Gloss    as Gloss
import qualified Number.Physical   as Physical
import qualified Number.SI         as SI
import qualified Number.SI.Unit    as Unit
import qualified NumericExtend
import           NumericPrelude

data T = Cons {
  _x :: V.Vector NumericExtend.SIDouble,
  _v :: V.Vector NumericExtend.SIDouble,
  _a :: V.Vector NumericExtend.SIDouble,
  _m :: NumericExtend.SIDouble
} deriving (Eq, Show)


-- | オイラー法
-- | 適当に更新
update :: NumericExtend.SIDouble -> T -> T
update t m = m {
  _x = _x m + t Vector.*> _v m,
  _v = _v m + t Vector.*> _a m,
  _a = V.replicate (length $ _a m) (0 * SI.meter / SI.second ^ 2)
  }

-- | Leap-frog 法
-- | 前フレームからの経過時間と、今のフレームの加速度から時間発展を計算
updateLF :: NumericExtend.SIDouble -> V.Vector NumericExtend.SIDouble -> T -> T
updateLF t a' m = m {
  _x = _x m + t Vector.*> _v m + (t ^ 2 / 2) Vector.*> _a m,
  _v = _v m + (t / 2) Vector.*> (_a m + a'),
  _a = a'
  }

-- | すべてのパラメータが0で初期化されたn次元上の質点
zeros :: Int -> T
zeros n = Cons {
  _x = V.replicate n (0 * SI.meter),
  _v = V.replicate n (0 * SI.meter / SI.second),
  _a = V.replicate n (0 * SI.meter / SI.second ^ 2),
  _m = 0 * Unit.kilo * SI.gramm
  }
