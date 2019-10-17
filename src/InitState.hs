module InitState where

import DataTypes
import Foreign.C.Types (CInt)
import System.Random (mkStdGen)
import SDL.Vect (V2(..))

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (1280, 1024)

ballRadius :: CInt
ballRadius = 15

initGameState :: GameState
initGameState = GameState
  { sBoard = V2 ((screenWidth - 180) `div` 2) (screenHeight - 60)
  , sBoard2 = V2 ((screenWidth - 180) `div` 2) (40)
  , sDirection = Nothing
  , sDirection2 = Nothing
  , sBallLife = Alive
  , sBallLife2 = Alive
  , sMagnet = Just $ V2 (200) (500)
  , sMagnetTimer = 300
  , sBonus = Just $ V2 (700) (700)
  , sBonusTimer = 600
  , sRandomGen = mkStdGen 1
  , sBall = Just $ V2 (screenWidth `div` 2) (screenHeight - 61 - ballRadius)
  , sBall2 = Just $ V2 (screenWidth `div` 2) (61 + ballRadius)
  , sBallStatus = Stay
  , sBallStatus2 = Stay
  , sSpeed = 1
  , sMoveChanging = V2 (0) (0)
  , sMoveChanging2 = V2 (0) (0)
  , sMagn = V2 (0) (0)
  , sBlocks = [(V2 0 0,0)]
  , sScore = 0
  , sScore2 = -1
  , rectLengh = 180
  , rectLengh2 = 180
  , sBlockXIndent = 0
  , sBlockYIndent = 390
  , sBlocksLines = 5
  , sBlocksColumns = 8
  , sMode = Solo
  , sCurPlayer = 0
  }
