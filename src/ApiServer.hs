{-# LANGUAGE OverloadedStrings, DataKinds, MultiParamTypeClasses, TypeOperators, FlexibleInstances #-}

module ApiServer (apiServer) where

import Servant
    ( Application,
      Proxy(..),
      serve,
      type (:<|>)(..),
      Accept(contentType),
      MimeRender(..),
      type (:>),
      Get,
      Server, Capture, QueryParam, Handler)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Media ((//), (/:))
import Network.Wai.Handler.Warp (run)
import ApiServer.Middleware

data HTML

data DemoStruct = DemoStruct
  { fieldO1 :: Text
  , field02 :: Int
  } deriving Show

struct :: DemoStruct
struct = DemoStruct "Test" 1

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf8")

instance MimeRender HTML Text where
  mimeRender _ = encodeUtf8

instance {-# OVERLAPPABLE #-} Show a => MimeRender HTML a where
  mimeRender _ = encodeUtf8 . pack . show

type ServerAPI =
  Get '[HTML] Text
  :<|> "demo" :> "test" :> Get '[HTML] Text
  :<|> "users" :> Capture "userId" Int :> Get '[HTML] Text
  :<|> "queryparam" :> QueryParam "test" Int :> Get '[HTML] Text
  :<|> "struct" :> Get '[HTML] DemoStruct

serverRoutes :: Server ServerAPI
serverRoutes =
          return "From Home"
          :<|> return "From new endpoint"
          :<|> (\userId -> return $ "this user id is " <> (pack . show) userId)
          :<|> queryparam
          :<|> return struct
            where
              queryparam :: Maybe Int -> Handler Text
              queryparam (Just x) = return $ "your param value is " <> (pack . show) x
              queryparam Nothing = return "no params"

serverProxy :: Proxy ServerAPI
serverProxy = Proxy

router :: Application
router = serve serverProxy serverRoutes

apiServer :: IO ()
apiServer = run 8080 (middleware router)
