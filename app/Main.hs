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

defaultMass :: SI.T Double Double
defaultMass = 7e18 * SI.gramm

userMass :: SI.T Double Double
userMass = 5e19 * SI.gramm

outOfWindowHeight :: Float -> Bool
outOfWindowHeight y = y <= -10000 || 10000 <= y

outOfWindowWidth :: Float -> Bool
outOfWindowWidth x = x <= -10000 || 10000 <= x

enoughNear :: Float
enoughNear = 0.1

data Model = Model {
  massPoints         :: MassPoints,
  gravitationalField :: GravitationalField.T,
  userGravity        :: (Bool, (Float, Float))
  }

initMps :: MassPoints
initMps = []

initUserGravity = (False, (0, 0))

initModel :: Model
initModel = Model {
  massPoints = initMps,
  gravitationalField = GravitationalField.zero 2,
  userGravity = initUserGravity
  }

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

updateModel :: Float -> Model -> Model
updateModel t model = Model {
  massPoints = mps'',
  gravitationalField = gf'',
  userGravity = ug
  }
  where
  mps = massPoints model
  mps' = deleteOutWindowMps mps
  gf = gravitationalField model
  ug = userGravity model
  mps'' = map
    (MassPoint.update (realToFrac t * SI.second) . GravitationalField.forceMassPoint gf'')
    mps'
  gf' = sum . map GravitationalField.fromMassPoint $ mps
  gf'' = gf' + userGF
  (isUserGrav, (x, y)) = ug
  userGF = if isUserGrav
    then GravitationalField.compute userMass [realToFrac x * SI.meter, realToFrac y * SI.meter]
    else GravitationalField.zero 2

-- updateModel :: p1 -> p2 -> a -> a
-- updateModel _ _ = id

userUpdate :: Gloss.Event -> Model -> Model
userUpdate (Gloss.EventKey (Gloss.MouseButton Gloss.LeftButton) Gloss.Up _ (x, y)) model = model {
  massPoints = (MassPoint.zeros 2) {
    MassPoint._x = [realToFrac x * SI.meter, realToFrac y * SI.meter],
    MassPoint._m = defaultMass
    }
    : massPoints model
  }
userUpdate (Gloss.EventKey (Gloss.MouseButton Gloss.RightButton) Gloss.Down _ pos) model = model {
  userGravity = (True, pos)
  }
userUpdate (Gloss.EventKey (Gloss.MouseButton Gloss.RightButton) Gloss.Up _ pos) model = model {
  userGravity = initUserGravity
  }
userUpdate _ model = model

drawMassPoints :: MassPoints -> Gloss.Picture
drawMassPoints = foldr ((Monoid.<>) . Graphic2D.draw) Gloss.blank

deleteOutWindowMps :: MassPoints -> MassPoints
deleteOutWindowMps = filter (not . f)
  where
  f mp = outOfWindowHeight y || outOfWindowWidth x
    where
    [x, y] = map MassPoint.siToFrac . MassPoint._x $ mp

windowWidth, windowHeight :: Int
windowWidth  = 640
windowHeight = 480

window :: Gloss.Display
window = Gloss.InWindow "N-body Simulation" (windowWidth, windowHeight) (100, 100)

fps :: Int
fps = 60

main :: IO ()
main = Gloss.play window Gloss.white fps initModel drawModel userUpdate updateModel
