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

import Data.Aeson
import GHC.Generics                ( Generic )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Reader.Class
import Data.List                   ( groupBy, sortBy )
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
  { rows :: [Row]
  , solved :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Crossword
instance FromJSON Crossword

data Square = Square
  { number :: Maybe Int
  , guessedLetter :: Maybe Char
  , letter :: Maybe Char
  , fillable :: Bool
  , x :: Int
  , y :: Int
  -- , crosswordId :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Square
instance FromJSON Square

type Row = [Square]

storedCrosswordToCrossword :: [Square] -> StoredCrossword -> Crossword
storedCrosswordToCrossword squares StoredCrossword{..} =
  Crossword { rows = groupBy squaresInSameRow (sortBy compareY squares), solved = storedCrosswordSolved }

compareY :: Square -> Square -> Ordering
compareY s1 s2
  | y s1 < y s2  = LT
  | y s1 > y s2  = GT
  | otherwise    = EQ

squaresInSameRow :: Square -> Square -> Bool
squaresInSameRow square1 square2 =
  y square1 == y square2

storedSquareToSquare :: StoredSquare -> Square
storedSquareToSquare StoredSquare{..} =
  Square { number = storedSquareNumber
         , guessedLetter = listToMaybe $ fromMaybe " " storedSquareGuessedLetter
         , letter = listToMaybe $ fromMaybe " " storedSquareLetter
         , fillable = storedSquareFillable
         , x = storedSquareX
         , y = storedSquareY
         -- , crosswordId = storedSquareStoredCrosswordUuid
         }

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool
