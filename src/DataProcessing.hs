{-# LANGUAGE BangPatterns #-}

module DataProcessing where

import           Control.Concurrent (threadDelay)
import           Control.Monad (unless)
import           Control.Concurrent.MVar
import           Data.Binary (encode)
import           Data.Maybe
import           DataTypes
import           Foreign.C.Types (CInt)
import           GraphicRender
import           InitState
import           Network.Socket.ByteString.Lazy (sendAll)
import           Network.Socket hiding (recv)
import           SDL.Input.Mouse(getAbsoluteMouseLocation)
import           System.Random (StdGen, randomR, split)
import           SDL.Vect (Point(P), V2(..))
import qualified SDL

loop :: SDL.Renderer -> MVar GameState -> GameState -> Socket -> IO ()
loop renderer mVState oldState sock = do
  state  <- fmap (fromMaybe oldState) $ tryTakeMVar mVState
  events <- fetchEvents state
  sendAll sock (encode $ show events)
  render renderer state
  threadDelay $ 20000
  unless (eQuit events) (loop renderer mVState state sock)
  close sock

initializeBlocks :: CInt -> CInt -> CInt -> CInt -> StdGen -> [(V2 CInt,CInt)]
initializeBlocks _ _ _ 0 _                 =
  []
initializeBlocks locX locY line column gen =
  let (a,b) = split gen
  in (initializeLineBlocks locX locY line a) ++ initializeBlocks locX (locY + 40) line (column - 1) b

initializeLineBlocks :: CInt -> CInt -> CInt -> StdGen -> [(V2 CInt,CInt)]
initializeLineBlocks _ _ 0 _            =
  []
initializeLineBlocks locX locY line gen =
  let (new, gen') = newVal gen
  in (V2 locX locY, new `mod` 3 + 1) : initializeLineBlocks (locX + 160) (locY) (line - 1) gen'

newVal :: StdGen -> (CInt, StdGen)
newVal gen = randomR (0, 100) gen

getX :: V2 CInt -> CInt
getX (V2 locX _) = locX

getY :: V2 CInt -> CInt
getY (V2 _ locY) = locY

getXJ :: Maybe (V2 CInt) -> CInt
getXJ (Just (V2 locX _)) = locX
getXJ _ = 0

getYJ :: Maybe (V2 CInt) -> CInt
getYJ (Just (V2 _ locY)) = locY
getYJ _ = 0

shiftSize :: CInt
shiftSize = 10

fetchEvents :: GameState -> IO MyEvents
fetchEvents state = do
  events           <- SDL.pollEvents
  isKeyPressed     <- SDL.getKeyboardState
  isLeftButtonHold <- SDL.getMouseButtons
  curPos           <- getAbsoluteMouseLocation
  let (P pos) = curPos
  pure $ MyEvents
    { eQuit = (elem SDL.QuitEvent $ map SDL.eventPayload events) || (sBallLife state == Dead)
    , eArrowLeft = isKeyPressed SDL.ScancodeLeft
    , eArrowRight = isKeyPressed SDL.ScancodeRight
    , ePushLBMouse = isLeftButtonHold SDL.ButtonLeft
    , eMousePos = pos
    }

update :: MyEvents -> GameState -> GameState
update events state
  | sBallLife state == Dead =
    state
  | otherwise =
    collide . updateMagnet . checkBonus . updateGameState . magnetto . updateBlocks . changeDir events $ state

updateBlocks :: GameState -> GameState
updateBlocks state =
  case (sBlocks state) of
    []            -> state { sBall = sBall initGameState
                           , sBall2 = sBall2 initGameState
                           , sBlocks = initializeBlocks (sBlockXIndent state) (sBlockYIndent state) (sBlocksColumns state) (sBlocksLines state) (sRandomGen state)
                           , sBallStatus = Stay
                           , sBallStatus2 = Stay
                           , sBallLife = sBallLife state
                           , sBallLife2 = sBallLife2 state
                           , sScore = sScore state
                           , sScore2 = sScore2 state
                           , sBoard = sBoard initGameState
                           , sBoard2 = sBoard2 initGameState
                           , sMoveChanging = sMoveChanging initGameState
                           , sMoveChanging2 = sMoveChanging2 initGameState
                           }

    [(V2 0 0, 0)] -> state { sBlocks = initializeBlocks (sBlockXIndent state) (sBlockYIndent state) (sBlocksColumns state) (sBlocksLines state) (sRandomGen state) }
    _             -> state

checkBonus :: GameState -> GameState
checkBonus state = collideBonus (sBonus state) (sBall state) state

collideBonus :: Maybe (V2 CInt) -> Maybe (V2 CInt) -> GameState -> GameState
collideBonus _ Nothing state                         =
  state
collideBonus Nothing (Just _) state                  =
  state
collideBonus (Just (V2 x y)) (Just (V2 xx yy)) state =
  if (abs (x - xx) <= 20 && abs (y - yy) <= 20 && (rectLengh state) == 180 && (sBonusTimer state /= 0))
    then state { rectLengh = 240
               , sBonusTimer = 0
               }
    else state { rectLengh = 180 }

magnetto :: GameState -> GameState
magnetto state
  | (sMoveChanging state /= (V2 0 0)) =
    let !(V2 x y) = ismagn (sBall state) (sMagnet state) in
    let (V2 xx yy) = sMoveChanging state in
    let ysq = y + yy
        xsq = x + xx
        sqr = fromInteger $ toInteger $ truncate $ (sqrt (fromInteger (toInteger (xsq * xsq + ysq * ysq))) / 10) in
    if (sqr /= 0) then state { sMoveChanging = V2 (xsq `div` sqr) (ysq `div` sqr) } else state
  | otherwise                         = state

changeDir :: MyEvents -> GameState -> GameState
changeDir events state
  | eArrowLeft  events    = state { sDirection = Just DirLeft }
  | eArrowRight events    = state { sDirection = Just DirRight }
  | ePushLBMouse events   = if (sMoveChanging state == (V2 (0) (0))) then
    state { sMoveChanging = calc (sBall state) (eMousePos events)
          , sBallStatus   = Move
          }
    else state
  | otherwise             = state { sDirection = Nothing }

calc :: Maybe (V2 CInt) -> V2 CInt -> V2 CInt
calc Nothing _                              = V2 0 0
calc (Just (V2 locX locY)) (V2 locXX locYY) = V2 fstK sndK where
  fstK = if ((xsq `div` sqr) == 0) then signum xsq else xsq `div` sqr
  sndK = if ((ysq `div` sqr) == 0) then signum ysq else (ysq `div` sqr)
  xsq = (locXX - locX)
  ysq = (locYY - locY)
  sqr = fromInteger $ toInteger $ truncate $ (sqrt (fromInteger (toInteger (xsq * xsq + ysq * ysq))) / 10)

updateGameState :: GameState -> GameState
updateGameState state
    | sBonusTimer state == 0  = updateBonus state
    | sMagnetTimer state == 0 = updateMagnet state
    | sMagnetTimer state /= 0 =
    let !sball = if (sBallStatus state == Stay)
                  then
                    moveStayBall state (sDirection state) (sBall state)
                  else
                    moveBall (sBall state) (sMoveChanging state) (sSpeed state) in
    let !smove = help (check2 (sBlocks state) sball state) state in
    let !(sblock, score) = check (sBlocks state, sScore state) sball in
    let !smagnet = (sMagnetTimer state) - 1 in
    let !sbonus = (sBonusTimer state) - 1 in
    state { sBoard = newBlock state (sDirection state) (sBoard state)
          , sBall = sball
          , sDirection = Nothing
          , sMagnetTimer = smagnet
          , sMoveChanging = smove
          , sBlocks = sblock
          , sScore = score
          , sBonusTimer = sbonus
          }
    | otherwise               = state

ismagn :: Maybe (V2 CInt) -> Maybe (V2 CInt) -> V2 CInt
ismagn (Just (V2 locX locY)) (Just (V2 locXX locYY)) =
  if (sqr <= 7 && sqr /= 0) then
   V2 (xsq `div` sqr) (ysq `div` sqr)
  else
   V2 (0) (0)
  where
    xsq = (locXX - locX)
    ysq = (locYY - locY)
    sqr = fromInteger $ toInteger $ truncate $ (sqrt (fromInteger (toInteger (xsq * xsq + ysq * ysq))) / 10)
ismagn _ Nothing                                     = V2 0 0
ismagn Nothing (Just _)                              = V2 0 0


moveBall :: Maybe (V2 CInt) -> (V2 CInt) -> CInt -> Maybe (V2 CInt)
moveBall Nothing _ _                                  = Just $ V2 0 0
moveBall (Just (V2 locX locY)) (V2 locXX locYY) speed = Just $ V2 (locX + locXX * speed) (locY + locYY * speed)

moveStayBall :: GameState -> Maybe Direction -> Maybe (V2 CInt) -> Maybe (V2 CInt)
moveStayBall _ _ Nothing                     = Just $ V2 0 0
moveStayBall state dir (Just (V2 locX locY)) =
  case dir of
    Just DirLeft  -> Just $ V2 (if (locX - shiftSize - (rectLengh state) `div` 2) >= 0 then locX - shiftSize else locX) locY
    Just DirRight -> Just $ V2 (if (locX + shiftSize + (rectLengh state) `div` 2) <= screenWidth then locX + shiftSize else locX) locY
    Nothing       -> Just $ V2 locX locY

newBlock :: GameState -> Maybe Direction -> V2 CInt -> V2 CInt
newBlock state dir (V2 locX locY) =
  case dir of
    Just DirLeft  -> V2 (if (locX - shiftSize) >= 0 then locX - shiftSize else locX) locY
    Just DirRight -> V2 (if (locX + shiftSize + (rectLengh state)) <= screenWidth then locX + shiftSize else locX) locY
    Nothing       -> V2 locX locY

updateMagnet :: GameState -> GameState
updateMagnet state
  | sMagnetTimer state == 0 =
    let
      (x, stdGen')  = randomR (0,  screenWidth - 70) (sRandomGen state)
      (y, stdGen'') = randomR (300, screenHeight - 300) stdGen'
    in state { sMagnetTimer = sMagnetTimer initGameState
             , sMagnet = maybe (Just $ V2 x y) (const Nothing) $ sMagnet state
             , sRandomGen = stdGen''
             }
  | otherwise               = state

updateBonus :: GameState -> GameState
updateBonus state
  | sBonusTimer state == 0 =
    let
      (x, stdGen')  = randomR (0,  screenWidth - 70) (sRandomGen state)
      (y, stdGen'') = randomR (300, screenHeight - 300) stdGen'
    in state { sBonusTimer = sBonusTimer initGameState
             , sBonus = maybe (Just $ V2 x y) (const Nothing) $ sBonus state
             , sRandomGen = stdGen''
             }
  | otherwise              = state

collide :: GameState -> GameState
collide state
  | (ballRadius + getXJ (sBall state) > screenWidth)
  , (getXJ (sBall state) < screenWidth)                                   =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (-(abs locX)) (locY)}
  | (- ballRadius + getXJ (sBall state) < 0) && (getXJ (sBall state) > 0) =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (abs locX) (locY)}
  | (- ballRadius + getYJ (sBall state) < 0) && (getYJ (sBall state) > 0)
  , (getY (sBoard state) == 964)                                          =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (locX) (abs locY)}
  | (ballRadius + getYJ (sBall state) > screenHeight)
  , (getYJ (sBall state) < screenHeight)
  , (getY (sBoard state) == 40)                                           =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (locX) (-(abs locY))}
  | ((ballRadius + getYJ (sBall state)) > (getY ( sBoard state)))
  , ((getYJ (sBall state)) < (getY ( sBoard state)))
  , ((getX ( sBoard state)) < getXJ (sBall state))
  , (((rectLengh state) + getX (sBoard state)) > getXJ (sBall state))
  , (getY (sBoard state) == 964)                                          =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (locX) (-(abs locY))}
  | ((-ballRadius + getYJ (sBall state)) < (getY ( sBoard state) + 20))
  , ((getYJ (sBall state)) > (getY ( sBoard state) + 20))
  , ((getX ( sBoard state)) < getXJ (sBall state))
  , (((rectLengh state) + getX (sBoard state)) > getXJ (sBall state))
  , (getY (sBoard state) == 40)                                           =
      let (V2 locX locY) = sMoveChanging state
      in state { sMoveChanging = V2 (locX) (abs locY)}
  | ((ballRadius + getYJ (sBall state)) > (getY ( sBoard state)))
  , (getY (sBoard state) == 964)                                          =
      state { sBallLife = Dead}
  | ((-ballRadius + getYJ (sBall state)) < (getY ( sBoard state)))
  , (getY (sBoard state) == 40)                                           =
      state { sBallLife = Dead}
  | otherwise                                                             = state

help :: [Maybe (V2 CInt)] -> GameState -> (V2 CInt)
help arr state =
  case arr of
    [Just a] -> a
    _        -> sMoveChanging state

check :: ([(V2 CInt, CInt)], Int) -> Maybe (V2 CInt) -> ([(V2 CInt, CInt)], Int)
check (x : xs, score) (Just ball) =
  let tmp = isCollide x ball
  in if (snd tmp > 0)
      then
        let (f,s) = check (xs, score) (Just ball)
        in (tmp : f, s)
      else
        let (f,s) = check (xs, score + 100) (Just ball)
        in (f, s)
check ([], score) (Just _)        = ([], score)
check ([], score) Nothing         = ([], score)
check _ Nothing                   = ([],0)

isCollide :: (V2 CInt, CInt) -> V2 CInt -> (V2 CInt, CInt)
isCollide (V2 locX locY, b) (V2 locXX locYY)
  | ((locY + 30 > locYY - ballRadius) && (locY + 30 < locYY) && (locX < locXX) && (locX + 160 > locXX))  = (V2 locX locY, b - 1)
  | ((locY < locYY + ballRadius) && (locY > locYY) && (locX < locXX) && (locX + 160 > locXX))            = (V2 locX locY, b - 1)
  | ((locX < locXX + ballRadius) && (locX > locXX) && (locY < locYY) && (locY + 30 > locYY))             = (V2 locX locY, b - 1)
  | ((locX + 160 > locXX - ballRadius) && (locX + 160 < locXX) && (locY < locYY) && (locY + 30 > locYY)) = (V2 locX locY, b - 1)
  | otherwise                                                                                            = (V2 locX locY, b)

check2 :: [(V2 CInt, CInt)] -> Maybe (V2 CInt) -> GameState -> [Maybe (V2 CInt)]
check2 _ Nothing _         = []
check2 x (Just ball) state = do
  cur <- x
  let element = isCollide2 cur ball state
  if (element /= Nothing) then
     return element
  else
    []

isCollide2 :: (V2 CInt, CInt) -> V2 CInt -> GameState -> Maybe (V2 CInt)
isCollide2 (V2 locX locY, _) (V2 locXX locYY) state
  | (locY + 30 > locYY - ballRadius) && (locY + 30 < locYY)
  , (locX < locXX) && (locX + 160 > locXX) =
      let (V2 locXXX locYYY) = sMoveChanging state
      in Just (V2 locXXX (abs locYYY))
  | (locY < locYY + ballRadius) && (locY > locYY)
  , (locX < locXX) && (locX + 160 > locXX) =
      let (V2 locXXX locYYY) = sMoveChanging state
      in Just (V2 locXXX (-(abs locYYY)))
  | (locX < locXX + ballRadius) && (locX > locXX)
  , (locY < locYY) && (locY + 30 > locYY)  =
      let (V2 locXXX locYYY) = sMoveChanging state
      in Just (V2 (-(abs locXXX)) locYYY)
  | (locX + 160 > locXX - ballRadius) && (locX + 160 < locXX)
  , (locY < locYY) && (locY + 30 > locYY)  =
      let (V2 locXXX locYYY) = sMoveChanging state
      in Just (V2 (abs locXXX) locYYY)
  | otherwise                               = Nothing
