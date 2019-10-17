module Main where

import           BD
import           Control.Concurrent (forkIO, killThread)
import           Control.Monad (forever, when)
import           Control.Exception(bracket)
import           Control.Concurrent.MVar
import           Control.Monad (unless)
import           Data.ByteString.Lazy (ByteString)
import           Data.Binary (encode, decode)
import qualified Data.ByteString.Lazy as S
import           DataTypes
import           Lib
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import           Network.Socket hiding (recv)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)


main :: IO ()
main = withSocketsDo $ do
  args                       <- getArgs
  (ip, port, userName, mode) <- case args of ip:port:userName:mode:[] -> return (ip, port, userName, mode)
                                             _                        -> do
                                                                          putStrLn "IncorrectArguments ip port userName mode expected"
                                                                          exitFailure
  addr                       <- resolve ip port
  bracket (open addr) close (runClient userName mode)

resolve :: String -> String -> IO AddrInfo
resolve host port = do
  let hints = defaultHints { addrSocketType = Stream }
  addrInfo:_ <- getAddrInfo (Just hints) (Just host) (Just port)
  return addrInfo

open :: AddrInfo -> IO Socket
open addr = do
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock (addrAddress addr)
  return sock

runClient :: String -> String -> Socket -> IO ()
runClient userName mode sock = do
  sendAll sock (encode mode)
  putStrLn $ "send mode=" ++ mode
  case mode of
    "3" -> do
          status    <- fmap (decode :: ByteString -> String) (recv sock 20000)
          putStrLn status
          sendAll sock (encode userName)
          putStrLn "send name"
          soloBD    <- (recv sock 20000)
          unless (S.null soloBD) $ do
            solo <- fmap ((read :: String -> [SoloDB]) . decode) (return soloBD)
            when (solo /= []) $ mapM_ print solo
          sendAll sock (encode ("OK" :: String))
          firstDuo  <- (recv sock 20000)
          unless (S.null firstDuo) $ do
            duoFirst <- fmap ((read :: String -> [DuoDB]) . decode) (return firstDuo)
            when (duoFirst /= []) $ mapM_ print duoFirst
          sendAll sock (encode ("OK" :: String))
          secondDuo <- (recv sock 20000)
          unless (S.null secondDuo) $ do
            duoSecond <- fmap ((read :: String -> [DuoDB]) . decode) (return secondDuo)
            when (duoSecond /= []) $ mapM_ print duoSecond
          close sock
    _ -> do
          status   <- fmap (decode :: ByteString -> String) (recv sock 20000)
          putStrLn status
          sendAll sock (encode userName)
          putStrLn "send name"
          state    <- fmap (read . decode) (recv sock 20000)
          putStrLn "recv State"
          recvMVar <- newMVar state
          threadId <- forkIO $ recvLoop recvMVar sock
          run state recvMVar sock
          killThread threadId

recvLoop :: MVar GameState -> Socket -> IO ()
recvLoop recvMVar sock = do
  forever $ do
    tmp <- (recv sock 20000)
    unless (S.null tmp) $ do
      received <- fmap (read . decode) (return tmp)
      case (sMode received) of
        Dual -> if (sBallLife received == Dead)
          then do
            putStrLn $ "You are losed. Your score is: " ++ (show $ sScore received) ++ " Your id: " ++ (show (sCurPlayer received `mod` 2 + 1))
          else
            if (sBallLife2 received == Dead)
              then do
                putStrLn $ "You win. Your score is: " ++ (show $ sScore received ) ++ " Your id: " ++ (show (sCurPlayer received `mod` 2 + 1))
              else do
                putMVar recvMVar received
        _    -> if (sBallLife received == Dead)
                  then
                    putStrLn $ "You are losed. Your score is: " ++ (show $ sScore received) ++ " Your id: " ++ (show (sCurPlayer received `mod` 2 + 1))
                  else do
                    if (sScore received > sScore2 received && sScore2 received /= -1)
                      then do
                        putStrLn $ "You are win. Your score is: " ++ (show $ sScore received) ++ " Your id: " ++ (show (sCurPlayer received `mod` 2 + 1))
                      else
                        putMVar recvMVar received
