{-# LANGUAGE OverloadedStrings #-}

module Config where

import Control.Monad.Logger                  ( NoLoggingT
                                             , LoggingT
                                             , runNoLoggingT
                                             , runStdoutLoggingT
                                             )
import Control.Monad.Trans.Maybe             ( runMaybeT
                                             , MaybeT (..)
                                             )
import Data.Monoid                           ( (<>) )
import Network.Wai                           ( Middleware )
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger  ( logStdout
                                             , logStdoutDev
                                             )
import System.Environment                    ( lookupEnv )
import Database.Persist.Postgresql           ( ConnectionPool
                                             , ConnectionString
                                             , createPostgresqlPool
                                             )
import qualified Data.ByteString.Char8 as BS


data Config = Config { getPool :: ConnectionPool
                     , getEnv  :: Environment
                     }

data Environment = Development
                 | Test
                 | Production
                 deriving (Eq, Show, Read)

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test        = runNoLoggingT testConnectionPool
makePool Development = runStdoutLoggingT devConnectionPool
makePool Production  = do
  pool <- runMaybeT $ do
    let keys = fmap BS.pack
          [ "host="
          , "port="
          , "user="
          , "password="
          , "dbname="
          ]
        envs =
          [ "PGHOST"
          , "PGPORT"
          , "PGUSER"
          , "PGPASS"
          , "PGDATABASE"
          ]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = mconcat . zipWith (<>) keys . fmap BS.pack $ envVars
    runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
  case pool of
    Nothing -> error "Database Configuration not present in environment."
    Just a -> return a

testConnectionPool = createPostgresqlPool (connStr Test) (envPool Test)
devConnectionPool = createPostgresqlPool (connStr Development) (envPool Development)

envPool :: Environment -> Int
envPool Test = 1
envPool Development = 1
envPool Production = 8

connStr :: Environment -> ConnectionString
connStr _ = "host=localhost dbname=crosswords user=crosswords password=crosswords port=5432"

middleware :: Middleware
middleware = cors $ const (Just corsPolicy)

corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy { corsOrigins = Nothing -- gives you /*
                     , corsMethods = ["GET", "POST", "PUT", "DELETE", "HEAD", "OPTION"]
                     , corsRequestHeaders = simpleHeaders -- adds "Content-Type" to defaults
                     , corsExposedHeaders = Nothing
                     , corsMaxAge = Nothing
                     , corsVaryOrigin = False
                     , corsRequireOrigin = False
                     , corsIgnoreFailures = False
                     }
