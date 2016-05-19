{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Aeson.Types
import GHC.Generics
import Servant


-- API Definition:

type CrosswordAPI = "crosswords" :> Get '[JSON] [Crossword]

-- The type returned by the Handler monad must match the second
-- argument of the HTTP method combinator used for out endpoint.
server :: Server CrosswordAPI
server = return [easyCrossword]

crosswordAPIProxy :: Proxy CrosswordAPI
crosswordAPIProxy = Proxy

-- Model:

data Crossword = Crossword
  { rows :: [Row]
  , solved :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Crossword

type Row = [Square]

data Square = Square
  { number :: Maybe Int
  , letter :: Maybe Char
  , fillable :: Bool
  , coord :: (Int, Int)
  } deriving (Eq, Show, Generic)

instance ToJSON Square

-- Sample Data:

easyCrossword :: Crossword
easyCrossword =
  Crossword
  { rows =
      [
        [ Square (Just 1) Nothing True  ( 0, 0 )
        , Square Nothing  Nothing True  ( 0, 1 )
        , Square (Just 2) Nothing True  ( 0, 2 )
        , Square Nothing  Nothing True  ( 0, 3 )
        , Square (Just 3) Nothing True  ( 0, 4 )
        ]
      , [ Square Nothing  Nothing True  ( 1, 0 )
        , Square Nothing  Nothing False ( 1, 1 )
        , Square Nothing  Nothing True  ( 1, 2 )
        , Square Nothing  Nothing False ( 1, 3 )
        , Square Nothing  Nothing True  ( 1, 4 )
        ]
      , [ Square (Just 4) Nothing True  ( 2, 0 )
        , Square Nothing  Nothing True  ( 2, 1 )
        , Square Nothing  Nothing True  ( 2, 2 )
        , Square Nothing  Nothing True  ( 2, 3 )
        , Square Nothing  Nothing True  ( 2, 4 )
        ]
      , [ Square Nothing  Nothing True  ( 3, 0 )
        , Square Nothing  Nothing False ( 3, 1 )
        , Square Nothing  Nothing True  ( 3, 2 )
        , Square Nothing  Nothing False ( 3, 3 )
        , Square Nothing  Nothing True  ( 3, 4 )
        ]
      , [ Square (Just 5) Nothing True  ( 4, 0 )
        , Square Nothing  Nothing True  ( 4, 1 )
        , Square Nothing  Nothing True  ( 4, 2 )
        , Square Nothing  Nothing True  ( 4, 3 )
        , Square Nothing  Nothing True  ( 4, 4 )
        ]
      ]
  , solved = False
  }

