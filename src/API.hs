{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Api where

import Control.Monad.Except
import Control.Monad.Reader.Class
import Control.Monad.Reader         ( ReaderT, runReaderT )
import Data.Int                     ( Int64 )
import Data.Maybe                   ( maybeToList )
import Database.Persist.Postgresql  ( get
                                    , insert
                                    , delete
                                    , replace
                                    , updateWhere
                                    , selectList
                                    , selectFirst
                                    , fromSqlKey
                                    , toSqlKey
                                    , Entity(..)
                                    , (==.)
                                    , (=.)
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
  :<|> "crosswords"
         :> Capture "uuid" String
         :> ReqBody '[JSON] Crossword
         :> Put '[JSON] Crossword
  -- :<|> "crosswords"
  --        :> ReqBody '[JSON] Crossword
  --        :> Post '[JSON] Crossword
  -- :<|> "crosswords"
  --        :> Capture "id" CrosswordId
  --        :> Delete '[JSON] ()

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
           :<|> updateCrossword
           -- :<|> createCrossword
           -- :<|> deleteCrossword


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
         Just crossword -> return crossword

updateCrossword :: String -> Crossword -> AppM Crossword
updateCrossword uuid crossword = do
  _ <- mapM (updateStoredSquare uuid) $ squares crossword
  storedSquares <- runDb (selectList [StoredSquareStoredCrosswordUuid ==. uuid] [])
  storedCrossword <- runDb (selectFirst [StoredCrosswordUuid ==. uuid] [])
  let
    maybeSquares = fmap (storedSquareToSquare . entityVal) storedSquares
    maybeCrossword = fmap (storedCrosswordToCrossword maybeSquares . entityVal) storedCrossword
  case maybeCrossword of
    Nothing -> throwError err404
    Just c -> return c

updateStoredSquare :: String -> Square -> AppM ()
updateStoredSquare uuid square =
  runDb (updateWhere
          [ StoredSquareX ==. x square
          , StoredSquareY ==. y square
          , StoredSquareStoredCrosswordUuid ==. uuid
          ]
          [StoredSquareGuessedLetter =. (Just $ maybeToList $ guessedLetter square)])

-- createCrossword :: Crossword -> AppM Crossword
-- createCrossword crossword = do
--     newCrosswordId <- runDb (insert (StoredCrossword (crosswordName crossword) (crosswordDescription crossword)))
--     maybeStoredCrossword <- runDb (selectFirst [StoredCrosswordId ==. newCrosswordId] [])
--     let maybeCrossword = fmap crosswordFromDb maybeStoredCrossword
--     case maybeCrossword of
--          Nothing -> throwError err404
--          Just crossword -> return crossword

-- deleteCrossword :: StoredCrosswordId -> AppM ()
-- deleteCrossword crosswordId = do
--   runDb (delete crosswordId)
--   return ()
