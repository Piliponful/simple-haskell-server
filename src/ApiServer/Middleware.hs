module ApiServer.Middleware (middleware) where
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import Network.Wai ( Application )

middleware :: Application -> Application
middleware = logStdoutDev . staticPolicy (addBase "static")