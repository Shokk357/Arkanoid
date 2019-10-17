{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           BD
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (concurrently)
import           Control.Monad (forever)
import           Control.Exception (bracket)
import           Control.Concurrent.MVar
import           Data.Time.Clock (utctDay, getCurrentTime)
import           DataProcessing (update)
import           Data.ByteString.Lazy (ByteString)
import           Data.Time.Calendar (toGregorian)
import qualified Database.SQLite.Simple as SQ
import           Data.Binary (encode, decode)
import           Data.Time.LocalTime
import qualified Data.Text as T
import           DataTypes
import           InitState
import           Network.Socket.ByteString.Lazy (recv, sendAll)
import           Network.Socket hiding (recv)
import           System.Random (StdGen, getStdGen)
import           System.Environment (getArgs)
import           System.Exit (exitFailure)

data ServerData = ServerData
  { soloPlayerSocket      :: MVar Socket
  , duoFirstPlayerSocket  :: MVar Socket
  , duoSecondPlayerSocket :: MVar Socket
  , dataBaseSocket        :: MVar Socket
  }

main :: IO ()
main = do
  conSolo <- SQ.open "dataBaseSolo.db"
  conDuo  <- SQ.open "dataBaseDuo.db"
  SQ.execute_ conSolo "CREATE TABLE IF NOT EXISTS dataBaseSolo (id INTEGER PRIMARY KEY AUTOINCREMENT, mode INTEGER, firstPlayerName TEXT, firstPlayerScore INTEGER, gameData TEXT)"
  SQ.execute_ conDuo "CREATE TABLE IF NOT EXISTS dataBaseDuo (id INTEGER PRIMARY KEY AUTOINCREMENT, mode INTEGER, firstPlayerName TEXT, secondPlayerName TEXT, firstPlayerScore INTEGER, secondPlayerScore INTEGER, gameData TEXT)"
  args    <- getArgs
  port    <- case args of port:[] -> return port
                          _       -> do
                                      putStrLn "IncorrectArgument port value excpected"
                                      exitFailure
  addr    <- resolve port
  bracket (open addr) close (runApplications conSolo conDuo)

resolve :: String -> IO AddrInfo
resolve port = do
 let hints = defaultHints { addrFlags = [AI_PASSIVE]
                          , addrSocketType = Stream
                          }
 addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
 return addr

open :: AddrInfo -> IO Socket
open addr = do
 sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
 setSocketOption sock ReuseAddr 1
 bind sock (addrAddress addr)
 listen sock 1
 return sock

runApplications :: SQ.Connection -> SQ.Connection -> Socket -> IO ()
runApplications conSolo conDuo sock = do
  mVar1 <- (newEmptyMVar :: IO (MVar Socket))
  mVar2 <- (newEmptyMVar :: IO (MVar Socket))
  mVar3 <- (newEmptyMVar :: IO (MVar Socket))
  mVar4 <- (newEmptyMVar :: IO (MVar Socket))
  serverData <- return (ServerData mVar1 mVar2 mVar3 mVar4)
  _ <- concurrently (listener sock serverData) (concurrently (soloCreater serverData conSolo) (concurrently (dataBaseQuery serverData conSolo conDuo) (duoCreater serverData conDuo)))
  return ()

dataBaseQuery :: ServerData -> SQ.Connection -> SQ.Connection -> IO ()
dataBaseQuery serverData conSolo conDuo = forever $ do
  playerSock <- takeMVar (dataBaseSocket serverData)
  sendAll playerSock (encode ("OK" :: String))
  userName   <- fmap (decode :: ByteString -> String) $ recv playerSock 20000
  putStrLn userName
  solo       <- makeQuerySolo conSolo (T.pack userName)
  sendAll playerSock (encode $ show solo)
  status1     <- fmap (decode :: ByteString -> String) (recv playerSock 20000)
  putStrLn status1
  firstDuo   <- makeQueryDuoFirst conDuo (T.pack userName)
  sendAll playerSock (encode $ show firstDuo)
  status2     <- fmap (decode :: ByteString -> String) (recv playerSock 20000)
  putStrLn status2
  secondDuo  <- makeQueryDuoSecond conDuo (T.pack userName)
  sendAll playerSock (encode $ show secondDuo)

listener :: Socket -> ServerData -> IO ()
listener sock serverData = forever $ do
  putStrLn "listener start"
  (playerSock,_) <- accept sock
  putStrLn "accept socket"
  mode <- fmap (decode :: ByteString -> String) $ recv playerSock 20000
  putStrLn $ "mode = " ++ (show mode)
  case mode of
    "1" -> putMVar (soloPlayerSocket serverData) playerSock
    "2" -> tryPutMVar (duoFirstPlayerSocket serverData) playerSock >>=
      (\e -> if (e == True) then putStrLn "first use" else tryPutMVar (duoSecondPlayerSocket serverData) playerSock >>=
        (\f ->if (f == True) then putStrLn "second use" else putMVar (duoFirstPlayerSocket serverData) playerSock))
    _   -> putMVar (dataBaseSocket serverData) playerSock
  return ()

soloCreater :: ServerData -> SQ.Connection -> IO ()
soloCreater serverData conSolo = forever $ do
  putStrLn "soloCreater"
  playerSock <- takeMVar (soloPlayerSocket serverData)
  sendAll playerSock (encode ("OK" :: String))
  userName   <- fmap (decode :: ByteString -> String) $ recv playerSock 20000
  putStrLn $ "name " ++ (show userName)
  gen        <- getStdGen
  _          <- forkIO $ loopSolo playerSock (setBlocksPos $ pushGen initGameState gen) conSolo userName
  return ()

duoCreater :: ServerData -> SQ.Connection -> IO ()
duoCreater serverData conDuo = forever $ do
  putStrLn "duoCreater"
  secondPlayerSock <- takeMVar (duoSecondPlayerSocket serverData)
  sendAll secondPlayerSock (encode ("OK" :: String))
  secondUserName   <- fmap (decode :: ByteString -> String) $ recv secondPlayerSock 20000
  putStrLn $ "name " ++ (show secondUserName)
  firstPlayerSock  <- takeMVar (duoFirstPlayerSocket serverData)
  sendAll firstPlayerSock (encode ("OK" :: String))
  firstUserName    <- fmap (decode :: ByteString -> String) $ recv firstPlayerSock 20000
  putStrLn $ "name " ++ (show firstUserName)
  gen              <- getStdGen
  let q = (nullScore $ changeMode $ pushGen initGameState gen)
  _                <- (forkIO $ loop firstPlayerSock secondPlayerSock q conDuo firstUserName secondUserName)
  return ()

loopSolo :: Socket -> GameState -> SQ.Connection -> String -> IO ()
loopSolo socket1 state conSolo userName = do
  sendAll socket1 (encode $ show state)
  event1 <- fmap (read . decode) $ recv socket1 20000
  let state1 = update event1 (state)
  if (sBallLife state1 /= Dead)
    then
      loopSolo socket1 state1 conSolo userName
    else do
      sendAll socket1 (encode $ show state1)
      date     <- getCurrentTime
      timeZone <- getCurrentTimeZone
      let (TimeOfDay hour minute _) = localTimeOfDay $ utcToLocalTime timeZone date
      pushSoloGameData conSolo 1 (T.pack userName) (sScore state1) (T.pack $ (show $ toGregorian (utctDay date)) ++ " at " ++ (show hour ++ ":" ++ show minute))
      close socket1

loopFinishSolo :: Socket -> GameState -> Int -> SQ.Connection -> String -> String -> IO ()
loopFinishSolo socket1 state score conDuo losedName playingName = do
  sendAll socket1 (encode $ show state)
  event1 <- fmap (read . decode) $ recv socket1 20000
  let state1 = update event1 (state)
  if (sBallLife state1 /= Dead)
    then do
      if (sScore state1 > score)
        then do
          sendAll socket1 (encode $ show state1)
          date     <- getCurrentTime
          timeZone <- getCurrentTimeZone
          let (TimeOfDay hour minute _) = localTimeOfDay $ utcToLocalTime timeZone date
          pushDuoGameData conDuo 2 (T.pack playingName) (T.pack losedName) (sScore state1) score (T.pack $ (show $ toGregorian (utctDay date)) ++ " at " ++ (show hour ++ ":" ++ show minute))
          close socket1
        else do
          loopFinishSolo socket1 state1 score conDuo losedName playingName
    else do
      sendAll socket1 (encode $ show state1)
      date     <- getCurrentTime
      timeZone <- getCurrentTimeZone
      let (TimeOfDay hour minute _) = localTimeOfDay $ utcToLocalTime timeZone date
      pushDuoGameData conDuo 2 (T.pack playingName) (T.pack losedName) (sScore state1) score (T.pack $ (show $ toGregorian (utctDay date)) ++ " at " ++ (show hour ++ ":" ++ show minute))
      close socket1

loop :: Socket -> Socket -> GameState -> SQ.Connection -> String -> String -> IO ()
loop socket1 socket2 state conDuo firstPlayerName secondPlayerName = do
  sendAll socket1 (encode $ show state)
  sendAll socket2 (encode $ show (swap state))
  event1 <- fmap (read . decode) $ recv socket1 20000
  event2 <- fmap (read . decode) $ recv socket2 20000
  let state1 = update event1 state
  if (sBallLife state1 /= Dead)
    then do
      let state2 = update event2 (swap state1)
      if (sBallLife state2 /= Dead)
        then
          loop socket1 socket2 (swap state2) conDuo firstPlayerName secondPlayerName
        else do
          (loopFinishSolo socket1 (changeMode $ swap state2) (sScore state2) conDuo secondPlayerName firstPlayerName)
          close socket1
          sendAll socket2 (encode $ show state2)
          close socket2
    else do
      (loopFinishSolo socket2 (changeMode $ swap state1) (sScore state1) conDuo firstPlayerName secondPlayerName)
      close socket2
      sendAll socket1 (encode $ show state1)
      close socket1

swap :: GameState -> GameState
swap state =
  state { sBoard = sBoard2 state
        , sBoard2 = sBoard state
        , sBall = sBall2 state
        , sBall2 = sBall state
        , sMoveChanging = sMoveChanging2 state
        , sMoveChanging2 = sMoveChanging state
        , sScore = sScore2 state
        , sScore2 = sScore state
        , sBallStatus = sBallStatus2 state
        , sBallStatus2 = sBallStatus state
        , sBallLife = sBallLife2 state
        , sBallLife2 = sBallLife state
        , sDirection = sDirection2 state
        , sDirection2 = sDirection state
        , rectLengh = rectLengh2 state
        , rectLengh2 = rectLengh state
        , sCurPlayer = succ (sCurPlayer state)
        }

pushGen :: GameState -> StdGen -> GameState
pushGen state gen = state { sRandomGen = gen }

changeMode :: GameState -> GameState
changeMode state = state { sMode = if (sMode state == Dual) then Solo else Dual }

setBlocksPos :: GameState -> GameState
setBlocksPos state = state { sBlockYIndent = 200 }

nullScore :: GameState -> GameState
nullScore state = state { sScore2 = 0 }
