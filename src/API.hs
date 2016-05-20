{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Data.Int                     ( Int64 )
import Database.Persist.Postgresql  ( get
                                    , insert
                                    , delete
                                    , replace
                                    , selectList
                                    , selectFirst
                                    , fromSqlKey
                                    , toSqlKey
                                    , Entity(..)
                                    , (==.)
                                    )
import Network.Wai                  ( Application )
import Servant
import Config                       ( Config(..) )
import Models


type API =
       "crosswords"
         :> Get '[JSON] [Crossword]
  :<|> "crosswords"
         :> Capture "uuid" String
         :> Get '[JSON] Crossword
  -- :<|> "crosswords"
  --        :> ReqBody '[JSON] Crossword
  --        :> Post '[JSON] Crossword
  -- :<|> "crosswords"
  --        :> Capture "id" CrosswordId
  --        :> Delete '[JSON] ()
  -- :<|> "crosswords"
  --        :> Capture "id" CrosswordId
  --        :> ReqBody '[JSON] Crossword
  --        :> Put '[JSON] Crossword

-- This is not available in servant-0.5 so define it ourselves:
type Handler = ExceptT ServantErr IO

-- The context in which our API actions will run:
type AppM = ReaderT Config Handler

api :: Proxy API
api = Proxy

app :: Config -> Application
app config = serve api (readerServer config)

readerServer :: Config -> Server API
readerServer config = enter (readerToHandler config) readerServerT

-- Allow the injecting of Config while still returning Handler:
readerToHandler :: Config -> AppM :~> Handler
readerToHandler config = Nat $ \x -> runReaderT x config

readerServerT :: ServerT API AppM
readerServerT = listCrosswords
           :<|> getCrossword
           -- :<|> createCrossword
           -- :<|> deleteCrossword
           -- :<|> updateCrossword


-- Crossword API:

listCrosswords :: ReaderT Config Handler [Crossword]
listCrosswords = do
    storedCrosswords <- runDb (selectList [] [])
    let crosswords = map (storedCrosswordToCrossword [] . entityVal) storedCrosswords
    return crosswords

getCrossword :: String -> AppM Crossword
getCrossword uuid = do
    maybeStoredCrossword <- runDb (selectFirst [StoredCrosswordUuid ==. uuid] [])
    maybeStoredSquares <- runDb (selectList [StoredSquareStoredCrosswordUuid ==. uuid] [])
    let
      maybeSquares = fmap (storedSquareToSquare . entityVal) maybeStoredSquares
      maybeCrossword = fmap (storedCrosswordToCrossword maybeSquares . entityVal) maybeStoredCrossword
    case maybeCrossword of
         Nothing -> throwError err404
         Just quiz -> return quiz

-- createCrossword :: Crossword -> AppM Crossword
-- createCrossword quiz = do
--     newCrosswordId <- runDb (insert (StoredCrossword (quizName quiz) (quizDescription quiz)))
--     maybeStoredCrossword <- runDb (selectFirst [StoredCrosswordId ==. newCrosswordId] [])
--     let maybeCrossword = fmap crosswordFromDb maybeStoredCrossword
--     case maybeCrossword of
--          Nothing -> throwError err404
--          Just quiz -> return quiz

-- deleteCrossword :: StoredCrosswordId -> AppM ()
-- deleteCrossword quizId = do
--   runDb (delete quizId)
--   return ()

-- updateCrossword :: StoredCrosswordId -> Crossword -> AppM Crossword
-- updateCrossword quizId quiz = do
--   runDb (replace quizId $ StoredCrossword (quizName quiz) (quizDescription quiz))
--   return quiz
