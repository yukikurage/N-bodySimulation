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

massPointSize :: Float
massPointSize = 2

instance Graphic2D.Draw T where
  draw m = Gloss.translate x y $ Gloss.circleSolid massPointSize
    where
    (x, y) = case _x m of
      [x', y'] -> (siToFrac x', siToFrac  y')
      _        -> error "Invalid Dimension"

siToFrac :: (Real.Real a1, Real.Fractional b) => SI.T a2 a1 -> b
siToFrac (SI.Cons (Physical.Cons _ x)) = realToFrac x

update :: SIDouble -> T -> T
update t m = m {
  _x = _x m + t Vector.*> _v m,
  _v = _v m + t Vector.*> _a m,
  _a = replicate (length $ _a m) (0 * SI.meter / SI.second ^ 2)
  }

force :: [SIDouble] -> T -> T
force f m = m {
  _a = (1 / _m m) Vector.*> f
  }

zeros :: Int -> T
zeros n = Cons {
  _x = replicate n (0 * SI.meter),
  _v = replicate n (0 * SI.meter / SI.second),
  _a = replicate n (0 * SI.meter / SI.second ^ 2),
  _m = 0 * Unit.kilo * SI.gramm
  }
