{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BD where

import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple
import qualified Data.Text as T

data DuoDB = DuoDB Int T.Text T.Text Int Int T.Text deriving (Read, Show, Eq)

instance FromRow DuoDB where
  fromRow = DuoDB <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DuoDB where
  toRow (DuoDB mode firstPlayerName secondPlayerName firstPlayerScore secondPlayerScore gameData) =
     toRow (mode, firstPlayerName, secondPlayerName, firstPlayerScore, secondPlayerScore, gameData)

data SoloDB = SoloDB Int T.Text Int T.Text deriving (Read, Show, Eq)

instance FromRow SoloDB where
 fromRow = SoloDB <$> field <*> field <*> field <*> field

instance ToRow SoloDB where
  toRow (SoloDB mode firstPlayerName firstPlayerScore gameData) =
    toRow (mode, firstPlayerName, firstPlayerScore, gameData)

pushSoloGameData :: Connection -> Int -> T.Text -> Int -> T.Text -> IO ()
pushSoloGameData conn mode firstPlayerName firstPlayerScore gameData =
  execute conn "INSERT INTO dataBaseSolo (mode, firstPlayerName, firstPlayerScore, gameData) VALUES (?, ?, ?, ?)" (SoloDB mode firstPlayerName firstPlayerScore gameData)

pushDuoGameData :: Connection -> Int -> T.Text -> T.Text -> Int -> Int -> T.Text -> IO ()
pushDuoGameData conn mode firstPlayerName secondPlayerName firstPlayerScore secondPlayerScore gameData =
  execute conn "INSERT INTO dataBaseDuo (mode, firstPlayerName, secondPlayerName, firstPlayerScore, secondPlayerScore, gameData) VALUES (?, ?, ?, ?, ?, ?)" (DuoDB mode firstPlayerName secondPlayerName firstPlayerScore secondPlayerScore gameData)

makeQuerySolo :: Connection -> T.Text -> IO [SoloDB]
makeQuerySolo conn userName = queryNamed conn ("SELECT mode, firstPlayerName, firstPlayerScore, gameData from dataBaseSolo WHERE firstPlayerName =:firstUserName") [":firstUserName" := userName] :: IO [SoloDB]

makeQueryDuoFirst :: Connection -> T.Text -> IO [DuoDB]
makeQueryDuoFirst conn userName = queryNamed conn ("SELECT mode, firstPlayerName, secondPlayerName, firstPlayerScore, secondPlayerScore, gameData from dataBaseDuo WHERE firstPlayerName =:firstUserName") [":firstUserName" := userName] :: IO [DuoDB]

makeQueryDuoSecond :: Connection -> T.Text -> IO [DuoDB]
makeQueryDuoSecond conn userName = queryNamed conn ("SELECT mode, firstPlayerName, secondPlayerName, firstPlayerScore, secondPlayerScore, gameData from dataBaseDuo WHERE secondPlayerName =:secondUserName") [":secondUserName" := userName] :: IO [DuoDB]
