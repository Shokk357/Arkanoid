module  DataTypes where

import SDL.Vect (V2(..))
import Foreign.C.Types (CInt)
import System.Random (StdGen)

data GameState
  = GameState
  { sBoard :: (V2 CInt)
  , sBoard2 :: (V2 CInt)
  , sDirection :: (Maybe Direction)
  , sDirection2 :: (Maybe Direction)
  , sBallLife :: BallLife
  , sBallLife2 :: BallLife
  , sMagnet :: Maybe (V2 CInt)
  , sMagnetTimer :: Int
  , sRandomGen :: StdGen
  , sBall :: Maybe (V2 CInt)
  , sBall2 :: Maybe (V2 CInt)
  , sBonus :: Maybe (V2 CInt)
  , sBonusTimer :: CInt
  , sBallStatus :: BallStatus
  , sBallStatus2 :: BallStatus
  , sSpeed :: CInt
  , sMoveChanging :: (V2 CInt)
  , sMoveChanging2 :: (V2 CInt)
  , sMagn :: (V2 CInt)
  , sBlocks :: [(V2 CInt, CInt)]
  , sScore :: Int
  , sScore2 :: Int
  , rectLengh :: CInt
  , rectLengh2 :: CInt
  , sBlockXIndent :: CInt
  , sBlockYIndent :: CInt
  , sBlocksLines :: CInt
  , sBlocksColumns :: CInt
  , sMode :: Mode
  , sCurPlayer :: Int
  }
  deriving (Read, Show)

data Mode
  = Solo
  | Dual
  deriving (Read, Show, Eq)

data Direction
  = DirLeft
  | DirRight
  deriving (Read, Show)

data BallStatus
  = Stay
  | Move
  deriving (Read, Show, Eq)

data BallLife
  = Alive
  | Dead
  deriving (Read, Show, Eq)

data MyEvents
  = MyEvents
  { eQuit :: Bool
  , eArrowLeft :: Bool
  , eArrowRight :: Bool
  , ePushLBMouse :: Bool
  , eMousePos :: V2 CInt
  }
  deriving (Read, Show)
