{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Algebra.Vector                   as Vector
import qualified Data.Monoid                      as Monoid
import qualified Data.Vector                      as V
import qualified Debug.Trace                      as Trace
import qualified Graphic2D
import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Data.ViewPort     as ViewPort
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified GravitationalField
import qualified MassPoint
import qualified Number.SI                        as SI
import qualified Number.SI.Unit                   as Unit
import qualified NumericExtend
import           NumericPrelude


type MassPoints = [MassPoint.T]
-- ^ 質点群

data Model = Model {
  massPoints         :: MassPoints,
  gravitationalField :: GravitationalField.T,
  userGravity        :: (Bool, (Float, Float)),
  massTrace          :: [[(Float, Float)]]
  }
-- ^ モデル

-- | 質点のデフォルトの質量
defaultMass :: SI.T Double Double
defaultMass = 7e18 * SI.gramm

-- | 固定重力のデフォルトの質量
userMass :: SI.T Double Double
userMass = 5e19 * SI.gramm

-- | 上下に十分離れているか
outOfWindowHeight :: Float -> Bool
outOfWindowHeight y = y <= -10000 || 10000 <= y

-- | 左右に十分離れているか
outOfWindowWidth :: Float -> Bool
outOfWindowWidth x = x <= -10000 || 10000 <= x

-- | 近すぎる判定をする距離
enoughNear :: SI.T Double Double
enoughNear = 0.1 * SI.meter

-- | 質点群の初期化
initMps :: MassPoints
initMps = []

-- | 固定重力の初期化
initUserGravity :: (Bool, (Float, Float))
initUserGravity = (False, (0, 0))

-- | モデルの初期化
initModel :: Model
initModel = Model {
  massPoints = initMps,
  gravitationalField = GravitationalField.zero 2,
  userGravity = initUserGravity,
  massTrace = []
  }

drawMassTrace :: [[(Float, Float)]] -> Gloss.Picture
drawMassTrace = foldr ((Monoid.<>) .  Gloss.line) Gloss.blank

-- | モデルを描画
drawModel :: Model -> Gloss.Picture
drawModel model = Gloss.color (Gloss.greyN 0.4) (drawMassTrace mt) Monoid.<> Gloss.color (Gloss.greyN 0.6) (Graphic2D.draw gf) Monoid.<> drawMassPoints mp Monoid.<>
  if isUserGrav
    then Gloss.translate x y (Gloss.circle 10)
    else Gloss.blank
  where
  mp = massPoints model
  ug = userGravity model
  gf = gravitationalField model
  mt = massTrace model
  (isUserGrav, (x, y)) = ug

-- | モデルを更新
updateModel :: Float -> Model -> Model
updateModel t model = model {
  massPoints = mps'',
  gravitationalField = gf'',
  userGravity = ug,
  massTrace = mt'
  }
  where
  mt = massTrace model
  mps = massPoints model
  gf = gravitationalField model
  ug = userGravity model
  mps' = deleteOutWindowMps mps
  mps'' = map (\mp ->
      MassPoint.updateLF (realToFrac t * SI.second) (gf'' (MassPoint._x mp)) mp
    )
    mps'
  gf' = foldr ((+) . GravitationalField.fromMassPoint) (GravitationalField.zero 2) mps
  gf'' = gf' + userGF
  (isUserGrav, (x, y)) = ug
  userGF = if isUserGrav
    then GravitationalField.compute userMass $
      V.fromList [realToFrac x * SI.meter, realToFrac y * SI.meter]
    else GravitationalField.zero 2
  mt' = zipWith (\mp tr -> (NumericExtend.siToFrac $ MassPoint._x mp V.! 0, NumericExtend.siToFrac $ MassPoint._x mp V.! 1) : tr) mps'' mt

-- updateModel :: p1 -> p2 -> a -> a
-- updateModel _ _ = id

-- | ユーザーからの操作を監視
-- | 左クリックで質点追加, 右クリック押しっぱなしで固定重力(謎概念)を追加
userUpdate :: Gloss.Event -> Model -> Model
userUpdate (
  Gloss.EventKey
  (Gloss.MouseButton Gloss.LeftButton)
  Gloss.Up _ (x, y)
  )
  model = model {
    massPoints = (MassPoint.zeros 2) {
      MassPoint._x = V.fromList [realToFrac x * SI.meter, realToFrac y * SI.meter],
      MassPoint._m = defaultMass
      }
    : massPoints model,
    massTrace = [] : massTrace model
    }
userUpdate (
  Gloss.EventKey
  (Gloss.MouseButton Gloss.RightButton)
  Gloss.Down _ pos
  )
  model = model {
  userGravity = (True, pos)
  }
userUpdate (
  Gloss.EventKey
  (Gloss.MouseButton Gloss.RightButton)
  Gloss.Up _ pos
  )
  model = model {
  userGravity = initUserGravity
  }
userUpdate _ model = model

-- | 質点群をプロット
drawMassPoints :: MassPoints -> Gloss.Picture
drawMassPoints = foldr ((Monoid.<>) . Graphic2D.draw) Gloss.blank

-- | かなり離れた質点を削除
deleteOutWindowMps :: MassPoints -> MassPoints
deleteOutWindowMps = filter (not . f)
  where
  f mp = outOfWindowHeight y || outOfWindowWidth x
    where
    v = V.map NumericExtend.siToFrac . MassPoint._x $ mp
    x = v V.! 0
    y = v V.! 1

-- ここら辺の設定はJSONでできるようにしたい
windowWidth, windowHeight :: Int
windowWidth  = 640
windowHeight = 480

window :: Gloss.Display
window = Gloss.InWindow "N-body Simulation" (windowWidth, windowHeight) (100, 100)

fps :: Int
fps = 60

main :: IO ()
main = Gloss.play window Gloss.white fps initModel drawModel userUpdate updateModel
