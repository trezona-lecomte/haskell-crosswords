module Main where

import API
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

main :: IO ()
main = run 8081 app

app :: Application
app = serve crosswordAPIProxy server
