module  GraphicRender where

import           DataTypes
import           Foreign.C.Types (CInt)
import           InitState
import           SDL.Vect (Point(P), V2(..), V4(..))
import           SDL.Primitive (fillCircle, circle)
import           SDL (($=))
import qualified SDL

rectWidth:: CInt
rectWidth = 20

blockSize :: V2 CInt
blockSize = V2 (160) (30)

render :: SDL.Renderer -> GameState -> IO ()
render renderer state = do
  SDL.rendererDrawColor renderer $= V4 39 160 194 1

  SDL.clear renderer

  let
    drawBonusBoard location =
      fillCircle renderer location 10 (V4 120 83 58 255)
  mapM_ drawBonusBoard $ sBonus state

  let
    drawMagnet location =
      fillCircle renderer location 10 (V4 maxBound 0 111 maxBound)
  mapM_ drawMagnet $ sMagnet state

  let
    drawMagnetBorder location =
      circle renderer location 70 (V4 maxBound 0 111 maxBound)
  mapM_ drawMagnetBorder $ sMagnet state

  let
    drawBlock (location,i) = do
      case i of
        1 -> SDL.rendererDrawColor renderer $= V4 184 24 24 1
        2 -> SDL.rendererDrawColor renderer $= V4 196 207 50 1
        _ -> SDL.rendererDrawColor renderer $= V4 73 222 4 1
      SDL.fillRect renderer $
        Just $ SDL.Rectangle (P location) blockSize
  mapM_  drawBlock (sBlocks state)

  let
    drawBlockBorder (location,_) =
      SDL.drawRect renderer $
        Just $ SDL.Rectangle (P location) blockSize
  SDL.rendererDrawColor renderer $= V4 176 108 19 1
  mapM_  drawBlockBorder (sBlocks state)
  if (sMode state == Dual)
    then do
      let
        drawBall location =
          fillCircle renderer location ballRadius (V4 125 62 214 maxBound)
      mapM_ drawBall $ sBall state
      let
        drawBall2 location =
          fillCircle renderer location ballRadius (V4 191 33 194 maxBound)
      mapM_ drawBall2 $ sBall2 state
      let
        drawBoard location =
          SDL.fillRect renderer $
            Just $ SDL.Rectangle (P location) (V2 (rectLengh state) rectWidth)
      SDL.rendererDrawColor renderer $= V4 120 83 58 1
      drawBoard $ sBoard state
      let
        drawBoard2 location =
          SDL.fillRect renderer $
            Just $ SDL.Rectangle (P location) (V2 (rectLengh2 state) rectWidth)
      SDL.rendererDrawColor renderer $= V4 120 83 58 1
      drawBoard2 $ sBoard2 state
    else do
      let
        drawBall location =
          fillCircle renderer location ballRadius (V4 125 62 214 maxBound)
      mapM_ drawBall $ sBall state
      let
        drawBoard location =
          SDL.fillRect renderer $
            Just $ SDL.Rectangle (P location) (V2 (rectLengh state) rectWidth)
      SDL.rendererDrawColor renderer $= V4 120 83 58 1
      drawBoard $ sBoard state
  SDL.present renderer
