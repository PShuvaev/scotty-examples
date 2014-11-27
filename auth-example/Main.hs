{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Web.Scotty as S
import Database.MySQL.Simple
import Handler
import BasicAuth

main = do
  conn <- connect defaultConnectInfo {  
                                        connectUser = "root",
                                        connectPassword = "root",
                                        connectDatabase = "lab"
                                      }
  S.scotty 3000 $ mapM_ ($ conn) [indexHandler, removeHandler, saveHandler, editHandler, logoutHandler, getLoginHandler, postLoginHandler]
