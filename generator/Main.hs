{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Servant.Elm
import Elm          ( ElmType
                    , toElmType
                    , ElmTypeExpr( Primitive )
                    )
import Data.Int     ( Int64 )
import GHC.Generics ( Generic )
import Models
import Api


instance ElmType Crossword
instance ElmType Square

instance ElmType Int64 where
    toElmType _ = Primitive "Int"

spec :: Spec
spec =
  Spec ["Generated", "API"]
       (defElmImports : generateElmForAPI (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "elm"
