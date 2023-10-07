{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module CustomHandlerIF.API
  ( -- * Client and Server
    Config(..)
  , CustomHandlerIFBackend(..)
  , createCustomHandlerIFClient
  , runCustomHandlerIFServer
  , runCustomHandlerIFMiddlewareServer
  , runCustomHandlerIFClient
  , runCustomHandlerIFClientWithManager
  , callCustomHandlerIF
  , CustomHandlerIFClient
  , CustomHandlerIFClientError(..)
  -- ** Servant
  , CustomHandlerIFAPI
  -- ** Plain WAI Application
  , serverWaiApplicationCustomHandlerIF
  ) where

import           CustomHandlerIF.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serveWithContextT)
import           Servant.API                        hiding (addHeader)
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..), Application, Context (EmptyContext))
import           Servant.Server.StaticFiles         (serveDirectoryFileServer)
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for CustomHandlerIF.
type CustomHandlerIFAPI
    =    "healthcheck" :> Verb 'GET 200 '[JSON] Text -- 'healthcheckGet' route
    :<|> Raw


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype CustomHandlerIFClientError = CustomHandlerIFClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for CustomHandlerIF.
-- The backend can be used both for the client and the server. The client generated from the CustomHandlerIF OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createCustomHandlerIFClient@). Alternatively, provided
-- a backend, the API can be served using @runCustomHandlerIFMiddlewareServer@.
data CustomHandlerIFBackend m = CustomHandlerIFBackend
  { healthcheckGet :: m Text{- ^  -}
  }


newtype CustomHandlerIFClient a = CustomHandlerIFClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative CustomHandlerIFClient where
  pure x = CustomHandlerIFClient (\_ -> pure x)
  (CustomHandlerIFClient f) <*> (CustomHandlerIFClient x) =
    CustomHandlerIFClient (\env -> f env <*> x env)

instance Monad CustomHandlerIFClient where
  (CustomHandlerIFClient a) >>= f =
    CustomHandlerIFClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO CustomHandlerIFClient where
  liftIO io = CustomHandlerIFClient (\_ -> liftIO io)

createCustomHandlerIFClient :: CustomHandlerIFBackend CustomHandlerIFClient
createCustomHandlerIFClient = CustomHandlerIFBackend{..}
  where
    ((coerce -> healthcheckGet) :<|>
     _) = client (Proxy :: Proxy CustomHandlerIFAPI)

-- | Run requests in the CustomHandlerIFClient monad.
runCustomHandlerIFClient :: Config -> CustomHandlerIFClient a -> ExceptT ClientError IO a
runCustomHandlerIFClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runCustomHandlerIFClientWithManager manager clientConfig cl

-- | Run requests in the CustomHandlerIFClient monad using a custom manager.
runCustomHandlerIFClientWithManager :: Manager -> Config -> CustomHandlerIFClient a -> ExceptT ClientError IO a
runCustomHandlerIFClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a CustomHandlerIFClientError
callCustomHandlerIF
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> CustomHandlerIFClient a -> m a
callCustomHandlerIF env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (CustomHandlerIFClientError err)
    Right response -> pure response


requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the CustomHandlerIF server at the provided host and port.
runCustomHandlerIFServer
  :: (MonadIO m, MonadThrow m)
  => Config -> CustomHandlerIFBackend (ExceptT ServerError IO) -> m ()
runCustomHandlerIFServer config backend = runCustomHandlerIFMiddlewareServer config requestMiddlewareId backend

-- | Run the CustomHandlerIF server at the provided host and port.
runCustomHandlerIFMiddlewareServer
  :: (MonadIO m, MonadThrow m)
  => Config -> Middleware -> CustomHandlerIFBackend (ExceptT ServerError IO) -> m ()
runCustomHandlerIFMiddlewareServer Config{..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationCustomHandlerIF backend

-- | Plain "Network.Wai" Application for the CustomHandlerIF server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationCustomHandlerIF :: CustomHandlerIFBackend (ExceptT ServerError IO) -> Application
serverWaiApplicationCustomHandlerIF backend = serveWithContextT (Proxy :: Proxy CustomHandlerIFAPI) context id (serverFromBackend backend)
  where
    context = serverContext
    serverFromBackend CustomHandlerIFBackend{..} =
      (coerce healthcheckGet :<|>
       serveDirectoryFileServer "static")


serverContext :: Context ('[])
serverContext = EmptyContext
