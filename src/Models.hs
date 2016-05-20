{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson                  ( ToJSON, FromJSON, toJSON, parseJSON )
import GHC.Generics                ( Generic )
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Int                    ( Int64 )
import Data.Maybe                  ( fromMaybe, listToMaybe )
import Database.Persist.Sql
import Database.Persist.TH
import Config


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
StoredCrossword
    uuid String
    solved Bool
    deriving Eq Show Generic
StoredSquare
    storedCrosswordUuid String
    number Int Maybe
    guessedLetter String Maybe
    letter String Maybe
    fillable Bool
    y Int
    x Int
    deriving Eq Show Generic
|]

data Crossword = Crossword
  { rows :: [Square]
  , solved :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Crossword
instance FromJSON Crossword

data Square = Square
  { number :: Maybe Int
  , guessedLetter :: Maybe Char
  -- , letter :: Maybe Char
  , fillable :: Bool
  , coord :: (Int, Int)
  -- , crosswordId :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Square
instance FromJSON Square

storedCrosswordToCrossword :: [Square] -> StoredCrossword -> Crossword
storedCrosswordToCrossword rows StoredCrossword{..} =
  Crossword { rows = rows, solved = storedCrosswordSolved }

storedSquareToSquare :: StoredSquare -> Square
storedSquareToSquare StoredSquare{..} =
  Square { number = storedSquareNumber
         , guessedLetter = listToMaybe $ fromMaybe " " storedSquareGuessedLetter
         -- , letter = listToMaybe $ fromMaybe " " storedSquareLetter
         , fillable = storedSquareFillable
         , coord = (storedSquareX, storedSquareY)
         -- , crosswordId = storedSquareStoredCrosswordUuid
         }

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
