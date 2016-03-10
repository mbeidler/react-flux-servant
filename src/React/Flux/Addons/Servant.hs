module React.Flux.Addons.Servant(
    HandleResponse
  , ApiRequestConfig(..)
  , request
  , HasAjaxRequest(..)
  , Request(..)
) where

import React.Flux
import Servant.Utils.Links
import Servant.API
import GHCJS.Types (JSVal, nullRef)
import GHCJS.Marshal (toJSVal_aeson, FromJSVal(..))
import GHC.TypeLits
import Data.Typeable (Proxy(..))
import Data.Aeson
import Data.JSString (JSString)
import Control.Arrow ((***))
import qualified Data.JSString as JSS
import qualified Data.JSString.Text as JSS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Request = Request {
    segments :: [JSString]
  , rHeaders :: [(JSString, JSString)]
  , rBody :: IO JSVal
}

type HandleResponse a = Either (Int,String) a -> IO [SomeStoreAction]

data ApiRequestConfig api = ApiRequestConfig
    { urlPrefix :: String
    , requestHeaders :: [(String,String)]
    }

request :: (IsElem endpoint api, HasAjaxRequest endpoint) => ApiRequestConfig api -> Proxy endpoint -> MkRequest endpoint
request (ApiRequestConfig p h) endpoint = toRequest endpoint (Request [JSS.pack p] h' (pure nullRef))
    where
        h' = map (JSS.pack *** JSS.pack) h

class HasAjaxRequest endpoint where
    type MkRequest endpoint
    toRequest :: Proxy endpoint -> Request -> MkRequest endpoint

instance (ToJSON a, HasAjaxRequest sub) => HasAjaxRequest (ReqBody '[JSON] a :> sub) where
    type MkRequest (ReqBody '[JSON] a :> sub) = a -> MkRequest sub
    toRequest _ r body = toRequest (Proxy :: Proxy sub) (r
        { rBody = toJSVal_aeson body >>= js_JSONstringify
        , rHeaders = rHeaders r ++ [("Content-Type", "application/json")]
        })

instance (KnownSymbol sym, HasAjaxRequest sub) => HasAjaxRequest (sym :> sub) where
    type MkRequest (sym :> sub) = MkRequest sub
    toRequest _ r = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [seg]})
        where
            seg = JSS.pack $ symbolVal (Proxy :: Proxy sym)

instance (ToHttpApiData v, HasAjaxRequest sub) => HasAjaxRequest (Capture sym v :> sub) where
    type MkRequest (Capture sym v :> sub) = v -> MkRequest sub
    toRequest _ r v = toRequest (Proxy :: Proxy sub) (r { segments = segments r ++ [v'] })
        where
            v' = JSS.pack $ T.unpack $ toUrlPiece v

instance (KnownSymbol sym, ToHttpApiData a, HasAjaxRequest sub) => HasAjaxRequest (Header sym a :> sub) where
    type MkRequest (Header sym a :> sub) = a -> MkRequest sub
    toRequest _ r a = toRequest (Proxy :: Proxy sub) (r { rHeaders = rHeaders r ++ [(sym',a')]})
        where
            sym' = JSS.pack $ symbolVal (Proxy :: Proxy sym)
            a' = JSS.pack $ T.unpack $ toUrlPiece a

instance (ReflectMethod m, FromJSON a) => HasAjaxRequest (Verb m s '[JSON] a) where
    type MkRequest (Verb m s '[JSON] a) = HandleResponse a -> IO ()
    toRequest _ r handler = do
        body <- rBody r
        let req = AjaxRequest
                  { reqMethod = JSS.textToJSString $ T.decodeUtf8 $ reflectMethod (Proxy :: Proxy m)
                  , reqURI = JSS.intercalate "/" (segments r)
                  , reqHeaders = rHeaders r ++ [("Accept", "application/json")]
                  , reqBody = body
                  }
        ajax req $ \resp ->
            if respStatus resp == 200
                then do
                    j <- js_JSONParse $ respResponseText resp
                    mv <- fromJSVal j
                    case mv of
                        Nothing -> handler $ Left (500, "Unable to convert response body")
                        Just v -> case fromJSON v of
                            Success v' -> handler $ Right v'
                            Error e -> handler $ Left (500, e)
                else handler $ Left (respStatus resp, JSS.unpack $ respResponseText resp)

foreign import javascript unsafe
    "JSON['parse']($1)"
    js_JSONParse :: JSString -> IO JSVal

foreign import javascript unsafe
    "JSON['stringify']($1)"
    js_JSONstringify :: JSVal -> IO JSVal
