{-# LANGUAGE RebindableSyntax #-}

module Main where

import qualified Data.Monoid                      as Monoid
import qualified Debug.Trace                      as Trace
import qualified Graphic2D
import qualified Graphics.Gloss                   as Gloss
import qualified Graphics.Gloss.Data.ViewPort     as ViewPort
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import qualified GravitationalField
import qualified MassPoint
import qualified Number.SI                        as SI
import qualified Number.SI.Unit                   as Unit
import           NumericPrelude

type MassPoints = [MassPoint.T]
-- ^ 質点群

data Model = Model {
  massPoints         :: MassPoints,
  gravitationalField :: GravitationalField.T,
  userGravity        :: (Bool, (Float, Float))
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
  userGravity = initUserGravity
  }

-- | モデルを描画
drawModel :: Model -> Gloss.Picture
drawModel model = Gloss.color (Gloss.greyN 0.5) (Graphic2D.draw gf) Monoid.<> drawMassPoints mp Monoid.<>
  if isUserGrav
    then Gloss.translate x y (Gloss.circle 10)
    else Gloss.blank
  where
  mp = massPoints model
  ug = userGravity model
  gf = gravitationalField model
  (isUserGrav, (x, y)) = ug

-- | モデルを更新
updateModel :: Float -> Model -> Model
updateModel t model = model {
  massPoints = mps'',
  gravitationalField = gf'',
  userGravity = ug
  }
  where
  mps = massPoints model
  mps' = deleteOutWindowMps mps
  gf = gravitationalField model
  ug = userGravity model
  mps'' = map (\mp ->
      MassPoint.updateLF (realToFrac t * SI.second) (gf'' (MassPoint._x mp)) mp
    )
    mps'
  gf' = sum . map GravitationalField.fromMassPoint $ mps
  gf'' = gf' + userGF
  (isUserGrav, (x, y)) = ug
  userGF = if isUserGrav
    then GravitationalField.compute userMass
      [realToFrac x * SI.meter, realToFrac y * SI.meter]
    else GravitationalField.zero 2

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
    MassPoint._x = [realToFrac x * SI.meter, realToFrac y * SI.meter],
    MassPoint._m = defaultMass
    }
    : massPoints model
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
    [x, y] = map MassPoint.siToFrac . MassPoint._x $ mp

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
