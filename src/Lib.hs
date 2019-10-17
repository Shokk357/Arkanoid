{-# LANGUAGE ViewPatterns, OverloadedStrings #-}

module Lib (run) where

import           Control.Exception(displayException, catch )
import           Control.Concurrent.MVar
import           DataProcessing
import           DataTypes
import           InitState
import qualified SDL
import           Network.Socket
import           SDL.Vect (V2(..))

run :: GameState -> MVar GameState -> Socket -> IO ()
run state mVState sock = do
  SDL.initialize [SDL.InitVideo]
  window   <-
    SDL.createWindow
      "Arkanoid"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight}
  renderer <-
    SDL.createRenderer
      window
      (-1)
      SDL.RendererConfig
         { SDL.rendererType = SDL.AcceleratedRenderer
         , SDL.rendererTargetTexture = False
         }
  loop renderer mVState state sock `catch`
    (\e -> let s = (displayException (e :: IOError)) in
    if (s /= "")
      then
        putStrLn "Game over, see you soon. Bye - Bye!"
      else
        putStrLn "sd")
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
