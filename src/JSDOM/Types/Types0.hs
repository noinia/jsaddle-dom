{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.Types0 where

import qualified Data.Text as T (unpack, Text)
import Data.Coerce (coerce, Coercible)
import Language.Javascript.JSaddle
       (Object(..), valToBool, valNull, valToNumber, (!!), js, valToText,
        JSVal, JSString, JSM, maybeNullOrUndefined, maybeNullOrUndefined',
        valToStr, jsg, ToJSString(..), FromJSString(..), strToText, MakeObject(..),
        Nullable(..), freeFunction, instanceOf, JSContextRef,
        askJSM, runJSM, MonadJSM(..), liftJSM, strictEqual, function, js2)

import JSDOM.Types.Core
import JSDOM.Types.TypesCore
import JSDOM.Types.TypesHTML
import JSDOM.Types.TypesWebGL
import JSDOM.Types.TypesSVG
import JSDOM.Types.Types00

-- | Functions for this inteface are in "JSDOM.JsonWebKey".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/JsonWebKey Mozilla JsonWebKey documentation>
newtype JsonWebKey = JsonWebKey { unJsonWebKey :: JSVal }

instance PToJSVal JsonWebKey where
  pToJSVal = unJsonWebKey
  {-# INLINE pToJSVal #-}

instance PFromJSVal JsonWebKey where
  pFromJSVal = JsonWebKey
  {-# INLINE pFromJSVal #-}

instance ToJSVal JsonWebKey where
  toJSVal = return . unJsonWebKey
  {-# INLINE toJSVal #-}

instance FromJSVal JsonWebKey where
  fromJSVal v = fmap JsonWebKey <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . JsonWebKey
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject JsonWebKey where
  makeObject = makeObject . unJsonWebKey

instance IsGObject JsonWebKey where
  typeGType _ = gTypeJsonWebKey
  {-# INLINE typeGType #-}

noJsonWebKey :: Maybe JsonWebKey
noJsonWebKey = Nothing
{-# INLINE noJsonWebKey #-}

gTypeJsonWebKey :: JSM GType
gTypeJsonWebKey = GType . Object <$> jsg "JsonWebKey"

-- | Functions for this inteface are in "JSDOM.CryptoKey".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CryptoKey Mozilla CryptoKey documentation>
newtype CryptoKey = CryptoKey { unCryptoKey :: JSVal }

instance PToJSVal CryptoKey where
  pToJSVal = unCryptoKey
  {-# INLINE pToJSVal #-}

instance PFromJSVal CryptoKey where
  pFromJSVal = CryptoKey
  {-# INLINE pFromJSVal #-}

instance ToJSVal CryptoKey where
  toJSVal = return . unCryptoKey
  {-# INLINE toJSVal #-}

instance FromJSVal CryptoKey where
  fromJSVal v = fmap CryptoKey <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CryptoKey
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CryptoKey where
  makeObject = makeObject . unCryptoKey

instance IsGObject CryptoKey where
  typeGType _ = gTypeCryptoKey
  {-# INLINE typeGType #-}

noCryptoKey :: Maybe CryptoKey
noCryptoKey = Nothing
{-# INLINE noCryptoKey #-}

gTypeCryptoKey :: JSM GType
gTypeCryptoKey = GType . Object <$> jsg "CryptoKey"

-- | Functions for this inteface are in "JSDOM.CryptoKeyPair".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CryptoKeyPair Mozilla CryptoKeyPair documentation>
newtype CryptoKeyPair = CryptoKeyPair { unCryptoKeyPair :: JSVal }

instance PToJSVal CryptoKeyPair where
  pToJSVal = unCryptoKeyPair
  {-# INLINE pToJSVal #-}

instance PFromJSVal CryptoKeyPair where
  pFromJSVal = CryptoKeyPair
  {-# INLINE pFromJSVal #-}

instance ToJSVal CryptoKeyPair where
  toJSVal = return . unCryptoKeyPair
  {-# INLINE toJSVal #-}

instance FromJSVal CryptoKeyPair where
  fromJSVal v = fmap CryptoKeyPair <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CryptoKeyPair
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CryptoKeyPair where
  makeObject = makeObject . unCryptoKeyPair

instance IsGObject CryptoKeyPair where
  typeGType _ = gTypeCryptoKeyPair
  {-# INLINE typeGType #-}

noCryptoKeyPair :: Maybe CryptoKeyPair
noCryptoKeyPair = Nothing
{-# INLINE noCryptoKeyPair #-}

gTypeCryptoKeyPair :: JSM GType
gTypeCryptoKeyPair = GType . Object <$> jsg "CryptoKeyPair"

-- | Functions for this inteface are in "JSDOM.URLSearchParams".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/URLSearchParams Mozilla URLSearchParams documentation>
newtype URLSearchParams = URLSearchParams { unURLSearchParams :: JSVal }

instance PToJSVal URLSearchParams where
  pToJSVal = unURLSearchParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal URLSearchParams where
  pFromJSVal = URLSearchParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal URLSearchParams where
  toJSVal = return . unURLSearchParams
  {-# INLINE toJSVal #-}

instance FromJSVal URLSearchParams where
  fromJSVal v = fmap URLSearchParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . URLSearchParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject URLSearchParams where
  makeObject = makeObject . unURLSearchParams

instance IsGObject URLSearchParams where
  typeGType _ = gTypeURLSearchParams
  {-# INLINE typeGType #-}

noURLSearchParams :: Maybe URLSearchParams
noURLSearchParams = Nothing
{-# INLINE noURLSearchParams #-}

gTypeURLSearchParams :: JSM GType
gTypeURLSearchParams = GType . Object <$> jsg "URLSearchParams"

-- | Functions for this inteface are in "JSDOM.CanvasGradient".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasGradient Mozilla CanvasGradient documentation>
newtype CanvasGradient = CanvasGradient { unCanvasGradient :: JSVal }

instance PToJSVal CanvasGradient where
  pToJSVal = unCanvasGradient
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasGradient where
  pFromJSVal = CanvasGradient
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasGradient where
  toJSVal = return . unCanvasGradient
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasGradient where
  fromJSVal v = fmap CanvasGradient <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasGradient
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasGradient where
  makeObject = makeObject . unCanvasGradient

instance IsGObject CanvasGradient where
  typeGType _ = gTypeCanvasGradient
  {-# INLINE typeGType #-}

noCanvasGradient :: Maybe CanvasGradient
noCanvasGradient = Nothing
{-# INLINE noCanvasGradient #-}

gTypeCanvasGradient :: JSM GType
gTypeCanvasGradient = GType . Object <$> jsg "CanvasGradient"

-- | Functions for this inteface are in "JSDOM.CanvasPattern".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasPattern Mozilla CanvasPattern documentation>
newtype CanvasPattern = CanvasPattern { unCanvasPattern :: JSVal }

instance PToJSVal CanvasPattern where
  pToJSVal = unCanvasPattern
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasPattern where
  pFromJSVal = CanvasPattern
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasPattern where
  toJSVal = return . unCanvasPattern
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasPattern where
  fromJSVal v = fmap CanvasPattern <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasPattern
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasPattern where
  makeObject = makeObject . unCanvasPattern

instance IsGObject CanvasPattern where
  typeGType _ = gTypeCanvasPattern
  {-# INLINE typeGType #-}

noCanvasPattern :: Maybe CanvasPattern
noCanvasPattern = Nothing
{-# INLINE noCanvasPattern #-}

gTypeCanvasPattern :: JSM GType
gTypeCanvasPattern = GType . Object <$> jsg "CanvasPattern"

-- | Functions for this inteface are in "JSDOM.IDBCursor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBCursor Mozilla IDBCursor documentation>
newtype IDBCursor = IDBCursor { unIDBCursor :: JSVal }

instance PToJSVal IDBCursor where
  pToJSVal = unIDBCursor
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBCursor where
  pFromJSVal = IDBCursor
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBCursor where
  toJSVal = return . unIDBCursor
  {-# INLINE toJSVal #-}

instance FromJSVal IDBCursor where
  fromJSVal v = fmap IDBCursor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBCursor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBCursor where
  makeObject = makeObject . unIDBCursor

class (IsGObject o) => IsIDBCursor o
toIDBCursor :: IsIDBCursor o => o -> IDBCursor
toIDBCursor = IDBCursor . coerce

instance IsIDBCursor IDBCursor
instance IsGObject IDBCursor where
  typeGType _ = gTypeIDBCursor
  {-# INLINE typeGType #-}

noIDBCursor :: Maybe IDBCursor
noIDBCursor = Nothing
{-# INLINE noIDBCursor #-}

gTypeIDBCursor :: JSM GType
gTypeIDBCursor = GType . Object <$> jsg "IDBCursor"

-- | Functions for this inteface are in "JSDOM.IDBCursorWithValue".
-- Base interface functions are in:
--
--     * "JSDOM.IDBCursor"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBCursorWithValue Mozilla IDBCursorWithValue documentation>
newtype IDBCursorWithValue = IDBCursorWithValue { unIDBCursorWithValue :: JSVal }

instance PToJSVal IDBCursorWithValue where
  pToJSVal = unIDBCursorWithValue
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBCursorWithValue where
  pFromJSVal = IDBCursorWithValue
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBCursorWithValue where
  toJSVal = return . unIDBCursorWithValue
  {-# INLINE toJSVal #-}

instance FromJSVal IDBCursorWithValue where
  fromJSVal v = fmap IDBCursorWithValue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBCursorWithValue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBCursorWithValue where
  makeObject = makeObject . unIDBCursorWithValue

instance IsIDBCursor IDBCursorWithValue
instance IsGObject IDBCursorWithValue where
  typeGType _ = gTypeIDBCursorWithValue
  {-# INLINE typeGType #-}

noIDBCursorWithValue :: Maybe IDBCursorWithValue
noIDBCursorWithValue = Nothing
{-# INLINE noIDBCursorWithValue #-}

gTypeIDBCursorWithValue :: JSM GType
gTypeIDBCursorWithValue = GType . Object <$> jsg "IDBCursorWithValue"

-- | Functions for this inteface are in "JSDOM.IDBDatabase".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBDatabase Mozilla IDBDatabase documentation>
newtype IDBDatabase = IDBDatabase { unIDBDatabase :: JSVal }

instance PToJSVal IDBDatabase where
  pToJSVal = unIDBDatabase
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBDatabase where
  pFromJSVal = IDBDatabase
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBDatabase where
  toJSVal = return . unIDBDatabase
  {-# INLINE toJSVal #-}

instance FromJSVal IDBDatabase where
  fromJSVal v = fmap IDBDatabase <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBDatabase
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBDatabase where
  makeObject = makeObject . unIDBDatabase

instance IsEventTarget IDBDatabase
instance IsGObject IDBDatabase where
  typeGType _ = gTypeIDBDatabase
  {-# INLINE typeGType #-}

noIDBDatabase :: Maybe IDBDatabase
noIDBDatabase = Nothing
{-# INLINE noIDBDatabase #-}

gTypeIDBDatabase :: JSM GType
gTypeIDBDatabase = GType . Object <$> jsg "IDBDatabase"

-- | Functions for this inteface are in "JSDOM.IDBFactory".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBFactory Mozilla IDBFactory documentation>
newtype IDBFactory = IDBFactory { unIDBFactory :: JSVal }

instance PToJSVal IDBFactory where
  pToJSVal = unIDBFactory
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBFactory where
  pFromJSVal = IDBFactory
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBFactory where
  toJSVal = return . unIDBFactory
  {-# INLINE toJSVal #-}

instance FromJSVal IDBFactory where
  fromJSVal v = fmap IDBFactory <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBFactory
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBFactory where
  makeObject = makeObject . unIDBFactory

instance IsGObject IDBFactory where
  typeGType _ = gTypeIDBFactory
  {-# INLINE typeGType #-}

noIDBFactory :: Maybe IDBFactory
noIDBFactory = Nothing
{-# INLINE noIDBFactory #-}

gTypeIDBFactory :: JSM GType
gTypeIDBFactory = GType . Object <$> jsg "IDBFactory"

-- | Functions for this inteface are in "JSDOM.IDBIndex".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBIndex Mozilla IDBIndex documentation>
newtype IDBIndex = IDBIndex { unIDBIndex :: JSVal }

instance PToJSVal IDBIndex where
  pToJSVal = unIDBIndex
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBIndex where
  pFromJSVal = IDBIndex
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBIndex where
  toJSVal = return . unIDBIndex
  {-# INLINE toJSVal #-}

instance FromJSVal IDBIndex where
  fromJSVal v = fmap IDBIndex <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBIndex
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBIndex where
  makeObject = makeObject . unIDBIndex

instance IsGObject IDBIndex where
  typeGType _ = gTypeIDBIndex
  {-# INLINE typeGType #-}

noIDBIndex :: Maybe IDBIndex
noIDBIndex = Nothing
{-# INLINE noIDBIndex #-}

gTypeIDBIndex :: JSM GType
gTypeIDBIndex = GType . Object <$> jsg "IDBIndex"

-- | Functions for this inteface are in "JSDOM.IDBIndexParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBIndexParameters Mozilla IDBIndexParameters documentation>
newtype IDBIndexParameters = IDBIndexParameters { unIDBIndexParameters :: JSVal }

instance PToJSVal IDBIndexParameters where
  pToJSVal = unIDBIndexParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBIndexParameters where
  pFromJSVal = IDBIndexParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBIndexParameters where
  toJSVal = return . unIDBIndexParameters
  {-# INLINE toJSVal #-}

instance FromJSVal IDBIndexParameters where
  fromJSVal v = fmap IDBIndexParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBIndexParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBIndexParameters where
  makeObject = makeObject . unIDBIndexParameters

instance IsGObject IDBIndexParameters where
  typeGType _ = gTypeIDBIndexParameters
  {-# INLINE typeGType #-}

noIDBIndexParameters :: Maybe IDBIndexParameters
noIDBIndexParameters = Nothing
{-# INLINE noIDBIndexParameters #-}

gTypeIDBIndexParameters :: JSM GType
gTypeIDBIndexParameters = GType . Object <$> jsg "IDBIndexParameters"

-- | Functions for this inteface are in "JSDOM.IDBKeyRange".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBKeyRange Mozilla IDBKeyRange documentation>
newtype IDBKeyRange = IDBKeyRange { unIDBKeyRange :: JSVal }

instance PToJSVal IDBKeyRange where
  pToJSVal = unIDBKeyRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBKeyRange where
  pFromJSVal = IDBKeyRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBKeyRange where
  toJSVal = return . unIDBKeyRange
  {-# INLINE toJSVal #-}

instance FromJSVal IDBKeyRange where
  fromJSVal v = fmap IDBKeyRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBKeyRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBKeyRange where
  makeObject = makeObject . unIDBKeyRange

instance IsGObject IDBKeyRange where
  typeGType _ = gTypeIDBKeyRange
  {-# INLINE typeGType #-}

noIDBKeyRange :: Maybe IDBKeyRange
noIDBKeyRange = Nothing
{-# INLINE noIDBKeyRange #-}

gTypeIDBKeyRange :: JSM GType
gTypeIDBKeyRange = GType . Object <$> jsg "IDBKeyRange"

-- | Functions for this inteface are in "JSDOM.IDBObjectStore".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStore Mozilla IDBObjectStore documentation>
newtype IDBObjectStore = IDBObjectStore { unIDBObjectStore :: JSVal }

instance PToJSVal IDBObjectStore where
  pToJSVal = unIDBObjectStore
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBObjectStore where
  pFromJSVal = IDBObjectStore
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBObjectStore where
  toJSVal = return . unIDBObjectStore
  {-# INLINE toJSVal #-}

instance FromJSVal IDBObjectStore where
  fromJSVal v = fmap IDBObjectStore <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBObjectStore
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBObjectStore where
  makeObject = makeObject . unIDBObjectStore

instance IsGObject IDBObjectStore where
  typeGType _ = gTypeIDBObjectStore
  {-# INLINE typeGType #-}

noIDBObjectStore :: Maybe IDBObjectStore
noIDBObjectStore = Nothing
{-# INLINE noIDBObjectStore #-}

gTypeIDBObjectStore :: JSM GType
gTypeIDBObjectStore = GType . Object <$> jsg "IDBObjectStore"

-- | Functions for this inteface are in "JSDOM.IDBObjectStoreParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBObjectStoreParameters Mozilla IDBObjectStoreParameters documentation>
newtype IDBObjectStoreParameters = IDBObjectStoreParameters { unIDBObjectStoreParameters :: JSVal }

instance PToJSVal IDBObjectStoreParameters where
  pToJSVal = unIDBObjectStoreParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBObjectStoreParameters where
  pFromJSVal = IDBObjectStoreParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBObjectStoreParameters where
  toJSVal = return . unIDBObjectStoreParameters
  {-# INLINE toJSVal #-}

instance FromJSVal IDBObjectStoreParameters where
  fromJSVal v = fmap IDBObjectStoreParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBObjectStoreParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBObjectStoreParameters where
  makeObject = makeObject . unIDBObjectStoreParameters

instance IsGObject IDBObjectStoreParameters where
  typeGType _ = gTypeIDBObjectStoreParameters
  {-# INLINE typeGType #-}

noIDBObjectStoreParameters :: Maybe IDBObjectStoreParameters
noIDBObjectStoreParameters = Nothing
{-# INLINE noIDBObjectStoreParameters #-}

gTypeIDBObjectStoreParameters :: JSM GType
gTypeIDBObjectStoreParameters = GType . Object <$> jsg "IDBObjectStoreParameters"

-- | Functions for this inteface are in "JSDOM.IDBOpenDBRequest".
-- Base interface functions are in:
--
--     * "JSDOM.IDBRequest"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBOpenDBRequest Mozilla IDBOpenDBRequest documentation>
newtype IDBOpenDBRequest = IDBOpenDBRequest { unIDBOpenDBRequest :: JSVal }

instance PToJSVal IDBOpenDBRequest where
  pToJSVal = unIDBOpenDBRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBOpenDBRequest where
  pFromJSVal = IDBOpenDBRequest
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBOpenDBRequest where
  toJSVal = return . unIDBOpenDBRequest
  {-# INLINE toJSVal #-}

instance FromJSVal IDBOpenDBRequest where
  fromJSVal v = fmap IDBOpenDBRequest <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBOpenDBRequest
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBOpenDBRequest where
  makeObject = makeObject . unIDBOpenDBRequest

instance IsIDBRequest IDBOpenDBRequest
instance IsEventTarget IDBOpenDBRequest
instance IsGObject IDBOpenDBRequest where
  typeGType _ = gTypeIDBOpenDBRequest
  {-# INLINE typeGType #-}

noIDBOpenDBRequest :: Maybe IDBOpenDBRequest
noIDBOpenDBRequest = Nothing
{-# INLINE noIDBOpenDBRequest #-}

gTypeIDBOpenDBRequest :: JSM GType
gTypeIDBOpenDBRequest = GType . Object <$> jsg "IDBOpenDBRequest"

-- | Functions for this inteface are in "JSDOM.IDBRequest".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBRequest Mozilla IDBRequest documentation>
newtype IDBRequest = IDBRequest { unIDBRequest :: JSVal }

instance PToJSVal IDBRequest where
  pToJSVal = unIDBRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBRequest where
  pFromJSVal = IDBRequest
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBRequest where
  toJSVal = return . unIDBRequest
  {-# INLINE toJSVal #-}

instance FromJSVal IDBRequest where
  fromJSVal v = fmap IDBRequest <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBRequest
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBRequest where
  makeObject = makeObject . unIDBRequest

class (IsEventTarget o, IsGObject o) => IsIDBRequest o
toIDBRequest :: IsIDBRequest o => o -> IDBRequest
toIDBRequest = IDBRequest . coerce

instance IsIDBRequest IDBRequest
instance IsEventTarget IDBRequest
instance IsGObject IDBRequest where
  typeGType _ = gTypeIDBRequest
  {-# INLINE typeGType #-}

noIDBRequest :: Maybe IDBRequest
noIDBRequest = Nothing
{-# INLINE noIDBRequest #-}

gTypeIDBRequest :: JSM GType
gTypeIDBRequest = GType . Object <$> jsg "IDBRequest"

-- | Functions for this inteface are in "JSDOM.IDBTransaction".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBTransaction Mozilla IDBTransaction documentation>
newtype IDBTransaction = IDBTransaction { unIDBTransaction :: JSVal }

instance PToJSVal IDBTransaction where
  pToJSVal = unIDBTransaction
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBTransaction where
  pFromJSVal = IDBTransaction
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBTransaction where
  toJSVal = return . unIDBTransaction
  {-# INLINE toJSVal #-}

instance FromJSVal IDBTransaction where
  fromJSVal v = fmap IDBTransaction <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBTransaction
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBTransaction where
  makeObject = makeObject . unIDBTransaction

instance IsEventTarget IDBTransaction
instance IsGObject IDBTransaction where
  typeGType _ = gTypeIDBTransaction
  {-# INLINE typeGType #-}

noIDBTransaction :: Maybe IDBTransaction
noIDBTransaction = Nothing
{-# INLINE noIDBTransaction #-}

gTypeIDBTransaction :: JSM GType
gTypeIDBTransaction = GType . Object <$> jsg "IDBTransaction"

-- | Functions for this inteface are in "JSDOM.IDBVersionChangeEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBVersionChangeEvent Mozilla IDBVersionChangeEvent documentation>
newtype IDBVersionChangeEvent = IDBVersionChangeEvent { unIDBVersionChangeEvent :: JSVal }

instance PToJSVal IDBVersionChangeEvent where
  pToJSVal = unIDBVersionChangeEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBVersionChangeEvent where
  pFromJSVal = IDBVersionChangeEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBVersionChangeEvent where
  toJSVal = return . unIDBVersionChangeEvent
  {-# INLINE toJSVal #-}

instance FromJSVal IDBVersionChangeEvent where
  fromJSVal v = fmap IDBVersionChangeEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBVersionChangeEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBVersionChangeEvent where
  makeObject = makeObject . unIDBVersionChangeEvent

instance IsEvent IDBVersionChangeEvent
instance IsGObject IDBVersionChangeEvent where
  typeGType _ = gTypeIDBVersionChangeEvent
  {-# INLINE typeGType #-}

noIDBVersionChangeEvent :: Maybe IDBVersionChangeEvent
noIDBVersionChangeEvent = Nothing
{-# INLINE noIDBVersionChangeEvent #-}

gTypeIDBVersionChangeEvent :: JSM GType
gTypeIDBVersionChangeEvent = GType . Object <$> jsg "IDBVersionChangeEvent"

-- | Functions for this inteface are in "JSDOM.MediaStream".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/webkitMediaStream Mozilla webkitMediaStream documentation>
newtype MediaStream = MediaStream { unMediaStream :: JSVal }

instance PToJSVal MediaStream where
  pToJSVal = unMediaStream
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStream where
  pFromJSVal = MediaStream
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStream where
  toJSVal = return . unMediaStream
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStream where
  fromJSVal v = fmap MediaStream <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStream
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStream where
  makeObject = makeObject . unMediaStream

instance IsEventTarget MediaStream
instance IsGObject MediaStream where
  typeGType _ = gTypeMediaStream
  {-# INLINE typeGType #-}

noMediaStream :: Maybe MediaStream
noMediaStream = Nothing
{-# INLINE noMediaStream #-}

gTypeMediaStream :: JSM GType
gTypeMediaStream = GType . Object <$> jsg "webkitMediaStream"

-- | Functions for this inteface are in "JSDOM.MediaSource".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaSource Mozilla MediaSource documentation>
newtype MediaSource = MediaSource { unMediaSource :: JSVal }

instance PToJSVal MediaSource where
  pToJSVal = unMediaSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaSource where
  pFromJSVal = MediaSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaSource where
  toJSVal = return . unMediaSource
  {-# INLINE toJSVal #-}

instance FromJSVal MediaSource where
  fromJSVal v = fmap MediaSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaSource where
  makeObject = makeObject . unMediaSource

instance IsEventTarget MediaSource
instance IsGObject MediaSource where
  typeGType _ = gTypeMediaSource
  {-# INLINE typeGType #-}

noMediaSource :: Maybe MediaSource
noMediaSource = Nothing
{-# INLINE noMediaSource #-}

gTypeMediaSource :: JSM GType
gTypeMediaSource = GType . Object <$> jsg "MediaSource"

-- | Functions for this inteface are in "JSDOM.MediaStreamTrack".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrack Mozilla MediaStreamTrack documentation>
newtype MediaStreamTrack = MediaStreamTrack { unMediaStreamTrack :: JSVal }

instance PToJSVal MediaStreamTrack where
  pToJSVal = unMediaStreamTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamTrack where
  pFromJSVal = MediaStreamTrack
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamTrack where
  toJSVal = return . unMediaStreamTrack
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamTrack where
  fromJSVal v = fmap MediaStreamTrack <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamTrack
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamTrack where
  makeObject = makeObject . unMediaStreamTrack

class (IsEventTarget o, IsGObject o) => IsMediaStreamTrack o
toMediaStreamTrack :: IsMediaStreamTrack o => o -> MediaStreamTrack
toMediaStreamTrack = MediaStreamTrack . coerce

instance IsMediaStreamTrack MediaStreamTrack
instance IsEventTarget MediaStreamTrack
instance IsGObject MediaStreamTrack where
  typeGType _ = gTypeMediaStreamTrack
  {-# INLINE typeGType #-}

noMediaStreamTrack :: Maybe MediaStreamTrack
noMediaStreamTrack = Nothing
{-# INLINE noMediaStreamTrack #-}

gTypeMediaStreamTrack :: JSM GType
gTypeMediaStreamTrack = GType . Object <$> jsg "MediaStreamTrack"

-- | Functions for this inteface are in "JSDOM.CanvasCaptureMediaStreamTrack".
-- Base interface functions are in:
--
--     * "JSDOM.MediaStreamTrack"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasCaptureMediaStreamTrack Mozilla CanvasCaptureMediaStreamTrack documentation>
newtype CanvasCaptureMediaStreamTrack = CanvasCaptureMediaStreamTrack { unCanvasCaptureMediaStreamTrack :: JSVal }

instance PToJSVal CanvasCaptureMediaStreamTrack where
  pToJSVal = unCanvasCaptureMediaStreamTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasCaptureMediaStreamTrack where
  pFromJSVal = CanvasCaptureMediaStreamTrack
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasCaptureMediaStreamTrack where
  toJSVal = return . unCanvasCaptureMediaStreamTrack
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasCaptureMediaStreamTrack where
  fromJSVal v = fmap CanvasCaptureMediaStreamTrack <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasCaptureMediaStreamTrack
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasCaptureMediaStreamTrack where
  makeObject = makeObject . unCanvasCaptureMediaStreamTrack

instance IsMediaStreamTrack CanvasCaptureMediaStreamTrack
instance IsEventTarget CanvasCaptureMediaStreamTrack
instance IsGObject CanvasCaptureMediaStreamTrack where
  typeGType _ = gTypeCanvasCaptureMediaStreamTrack
  {-# INLINE typeGType #-}

noCanvasCaptureMediaStreamTrack :: Maybe CanvasCaptureMediaStreamTrack
noCanvasCaptureMediaStreamTrack = Nothing
{-# INLINE noCanvasCaptureMediaStreamTrack #-}

gTypeCanvasCaptureMediaStreamTrack :: JSM GType
gTypeCanvasCaptureMediaStreamTrack = GType . Object <$> jsg "CanvasCaptureMediaStreamTrack"

-- | Functions for this inteface are in "JSDOM.RTCIceCandidate".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidate Mozilla RTCIceCandidate documentation>
newtype RTCIceCandidate = RTCIceCandidate { unRTCIceCandidate :: JSVal }

instance PToJSVal RTCIceCandidate where
  pToJSVal = unRTCIceCandidate
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceCandidate where
  pFromJSVal = RTCIceCandidate
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceCandidate where
  toJSVal = return . unRTCIceCandidate
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceCandidate where
  fromJSVal v = fmap RTCIceCandidate <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceCandidate
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceCandidate where
  makeObject = makeObject . unRTCIceCandidate

instance IsGObject RTCIceCandidate where
  typeGType _ = gTypeRTCIceCandidate
  {-# INLINE typeGType #-}

noRTCIceCandidate :: Maybe RTCIceCandidate
noRTCIceCandidate = Nothing
{-# INLINE noRTCIceCandidate #-}

gTypeRTCIceCandidate :: JSM GType
gTypeRTCIceCandidate = GType . Object <$> jsg "RTCIceCandidate"

-- | Functions for this inteface are in "JSDOM.RTCIceCandidateEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidateEvent Mozilla RTCIceCandidateEvent documentation>
newtype RTCIceCandidateEvent = RTCIceCandidateEvent { unRTCIceCandidateEvent :: JSVal }

instance PToJSVal RTCIceCandidateEvent where
  pToJSVal = unRTCIceCandidateEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceCandidateEvent where
  pFromJSVal = RTCIceCandidateEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceCandidateEvent where
  toJSVal = return . unRTCIceCandidateEvent
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceCandidateEvent where
  fromJSVal v = fmap RTCIceCandidateEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceCandidateEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceCandidateEvent where
  makeObject = makeObject . unRTCIceCandidateEvent

instance IsEvent RTCIceCandidateEvent
instance IsGObject RTCIceCandidateEvent where
  typeGType _ = gTypeRTCIceCandidateEvent
  {-# INLINE typeGType #-}

noRTCIceCandidateEvent :: Maybe RTCIceCandidateEvent
noRTCIceCandidateEvent = Nothing
{-# INLINE noRTCIceCandidateEvent #-}

gTypeRTCIceCandidateEvent :: JSM GType
gTypeRTCIceCandidateEvent = GType . Object <$> jsg "RTCIceCandidateEvent"

-- | Functions for this inteface are in "JSDOM.RTCIceCandidateInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceCandidateInit Mozilla RTCIceCandidateInit documentation>
newtype RTCIceCandidateInit = RTCIceCandidateInit { unRTCIceCandidateInit :: JSVal }

instance PToJSVal RTCIceCandidateInit where
  pToJSVal = unRTCIceCandidateInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceCandidateInit where
  pFromJSVal = RTCIceCandidateInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceCandidateInit where
  toJSVal = return . unRTCIceCandidateInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceCandidateInit where
  fromJSVal v = fmap RTCIceCandidateInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceCandidateInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceCandidateInit where
  makeObject = makeObject . unRTCIceCandidateInit

instance IsGObject RTCIceCandidateInit where
  typeGType _ = gTypeRTCIceCandidateInit
  {-# INLINE typeGType #-}

noRTCIceCandidateInit :: Maybe RTCIceCandidateInit
noRTCIceCandidateInit = Nothing
{-# INLINE noRTCIceCandidateInit #-}

gTypeRTCIceCandidateInit :: JSM GType
gTypeRTCIceCandidateInit = GType . Object <$> jsg "RTCIceCandidateInit"

-- | Functions for this inteface are in "JSDOM.ProcessingInstruction".
-- Base interface functions are in:
--
--     * "JSDOM.CharacterData"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.ChildNode"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ProcessingInstruction Mozilla ProcessingInstruction documentation>
newtype ProcessingInstruction = ProcessingInstruction { unProcessingInstruction :: JSVal }

instance PToJSVal ProcessingInstruction where
  pToJSVal = unProcessingInstruction
  {-# INLINE pToJSVal #-}

instance PFromJSVal ProcessingInstruction where
  pFromJSVal = ProcessingInstruction
  {-# INLINE pFromJSVal #-}

instance ToJSVal ProcessingInstruction where
  toJSVal = return . unProcessingInstruction
  {-# INLINE toJSVal #-}

instance FromJSVal ProcessingInstruction where
  fromJSVal v = fmap ProcessingInstruction <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ProcessingInstruction
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ProcessingInstruction where
  makeObject = makeObject . unProcessingInstruction

instance IsCharacterData ProcessingInstruction
instance IsNode ProcessingInstruction
instance IsEventTarget ProcessingInstruction
instance IsNonDocumentTypeChildNode ProcessingInstruction
instance IsChildNode ProcessingInstruction
instance IsGObject ProcessingInstruction where
  typeGType _ = gTypeProcessingInstruction
  {-# INLINE typeGType #-}

noProcessingInstruction :: Maybe ProcessingInstruction
noProcessingInstruction = Nothing
{-# INLINE noProcessingInstruction #-}

gTypeProcessingInstruction :: JSM GType
gTypeProcessingInstruction = GType . Object <$> jsg "ProcessingInstruction"

-- | Functions for this inteface are in "JSDOM.MessagePort".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MessagePort Mozilla MessagePort documentation>
newtype MessagePort = MessagePort { unMessagePort :: JSVal }

instance PToJSVal MessagePort where
  pToJSVal = unMessagePort
  {-# INLINE pToJSVal #-}

instance PFromJSVal MessagePort where
  pFromJSVal = MessagePort
  {-# INLINE pFromJSVal #-}

instance ToJSVal MessagePort where
  toJSVal = return . unMessagePort
  {-# INLINE toJSVal #-}

instance FromJSVal MessagePort where
  fromJSVal v = fmap MessagePort <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MessagePort
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MessagePort where
  makeObject = makeObject . unMessagePort

instance IsEventTarget MessagePort
instance IsGObject MessagePort where
  typeGType _ = gTypeMessagePort
  {-# INLINE typeGType #-}

noMessagePort :: Maybe MessagePort
noMessagePort = Nothing
{-# INLINE noMessagePort #-}

gTypeMessagePort :: JSM GType
gTypeMessagePort = GType . Object <$> jsg "MessagePort"

-- | Functions for this inteface are in "JSDOM.GlobalCrypto".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GlobalCrypto Mozilla GlobalCrypto documentation>
newtype GlobalCrypto = GlobalCrypto { unGlobalCrypto :: JSVal }

instance PToJSVal GlobalCrypto where
  pToJSVal = unGlobalCrypto
  {-# INLINE pToJSVal #-}

instance PFromJSVal GlobalCrypto where
  pFromJSVal = GlobalCrypto
  {-# INLINE pFromJSVal #-}

instance ToJSVal GlobalCrypto where
  toJSVal = return . unGlobalCrypto
  {-# INLINE toJSVal #-}

instance FromJSVal GlobalCrypto where
  fromJSVal v = fmap GlobalCrypto <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GlobalCrypto
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GlobalCrypto where
  makeObject = makeObject . unGlobalCrypto

class (IsGObject o) => IsGlobalCrypto o
toGlobalCrypto :: IsGlobalCrypto o => o -> GlobalCrypto
toGlobalCrypto = GlobalCrypto . coerce

instance IsGlobalCrypto GlobalCrypto
instance IsGObject GlobalCrypto where
  typeGType _ = gTypeGlobalCrypto
  {-# INLINE typeGType #-}

noGlobalCrypto :: Maybe GlobalCrypto
noGlobalCrypto = Nothing
{-# INLINE noGlobalCrypto #-}

gTypeGlobalCrypto :: JSM GType
gTypeGlobalCrypto = GType . Object <$> jsg "GlobalCrypto"

-- | Functions for this inteface are in "JSDOM.GlobalPerformance".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GlobalPerformance Mozilla GlobalPerformance documentation>
newtype GlobalPerformance = GlobalPerformance { unGlobalPerformance :: JSVal }

instance PToJSVal GlobalPerformance where
  pToJSVal = unGlobalPerformance
  {-# INLINE pToJSVal #-}

instance PFromJSVal GlobalPerformance where
  pFromJSVal = GlobalPerformance
  {-# INLINE pFromJSVal #-}

instance ToJSVal GlobalPerformance where
  toJSVal = return . unGlobalPerformance
  {-# INLINE toJSVal #-}

instance FromJSVal GlobalPerformance where
  fromJSVal v = fmap GlobalPerformance <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GlobalPerformance
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GlobalPerformance where
  makeObject = makeObject . unGlobalPerformance

class (IsGObject o) => IsGlobalPerformance o
toGlobalPerformance :: IsGlobalPerformance o => o -> GlobalPerformance
toGlobalPerformance = GlobalPerformance . coerce

instance IsGlobalPerformance GlobalPerformance
instance IsGObject GlobalPerformance where
  typeGType _ = gTypeGlobalPerformance
  {-# INLINE typeGType #-}

noGlobalPerformance :: Maybe GlobalPerformance
noGlobalPerformance = Nothing
{-# INLINE noGlobalPerformance #-}

gTypeGlobalPerformance :: JSM GType
gTypeGlobalPerformance = GType . Object <$> jsg "GlobalPerformance"

-- | Functions for this inteface are in "JSDOM.WindowOrWorkerGlobalScope".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope Mozilla WindowOrWorkerGlobalScope documentation>
newtype WindowOrWorkerGlobalScope = WindowOrWorkerGlobalScope { unWindowOrWorkerGlobalScope :: JSVal }

instance PToJSVal WindowOrWorkerGlobalScope where
  pToJSVal = unWindowOrWorkerGlobalScope
  {-# INLINE pToJSVal #-}

instance PFromJSVal WindowOrWorkerGlobalScope where
  pFromJSVal = WindowOrWorkerGlobalScope
  {-# INLINE pFromJSVal #-}

instance ToJSVal WindowOrWorkerGlobalScope where
  toJSVal = return . unWindowOrWorkerGlobalScope
  {-# INLINE toJSVal #-}

instance FromJSVal WindowOrWorkerGlobalScope where
  fromJSVal v = fmap WindowOrWorkerGlobalScope <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WindowOrWorkerGlobalScope
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WindowOrWorkerGlobalScope where
  makeObject = makeObject . unWindowOrWorkerGlobalScope

class (IsGObject o) => IsWindowOrWorkerGlobalScope o
toWindowOrWorkerGlobalScope :: IsWindowOrWorkerGlobalScope o => o -> WindowOrWorkerGlobalScope
toWindowOrWorkerGlobalScope = WindowOrWorkerGlobalScope . coerce

instance IsWindowOrWorkerGlobalScope WindowOrWorkerGlobalScope
instance IsGObject WindowOrWorkerGlobalScope where
  typeGType _ = gTypeWindowOrWorkerGlobalScope
  {-# INLINE typeGType #-}

noWindowOrWorkerGlobalScope :: Maybe WindowOrWorkerGlobalScope
noWindowOrWorkerGlobalScope = Nothing
{-# INLINE noWindowOrWorkerGlobalScope #-}

gTypeWindowOrWorkerGlobalScope :: JSM GType
gTypeWindowOrWorkerGlobalScope = GType . Object <$> jsg "WindowOrWorkerGlobalScope"

-- | Functions for this inteface are in "JSDOM.ShadowRoot".
-- Base interface functions are in:
--
--     * "JSDOM.DocumentFragment"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonElementParentNode"
--     * "JSDOM.ParentNode"
--     * "JSDOM.DocumentOrShadowRoot"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ShadowRoot Mozilla ShadowRoot documentation>
newtype ShadowRoot = ShadowRoot { unShadowRoot :: JSVal }

instance PToJSVal ShadowRoot where
  pToJSVal = unShadowRoot
  {-# INLINE pToJSVal #-}

instance PFromJSVal ShadowRoot where
  pFromJSVal = ShadowRoot
  {-# INLINE pFromJSVal #-}

instance ToJSVal ShadowRoot where
  toJSVal = return . unShadowRoot
  {-# INLINE toJSVal #-}

instance FromJSVal ShadowRoot where
  fromJSVal v = fmap ShadowRoot <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ShadowRoot
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ShadowRoot where
  makeObject = makeObject . unShadowRoot

instance IsDocumentFragment ShadowRoot
instance IsNode ShadowRoot
instance IsEventTarget ShadowRoot
instance IsNonElementParentNode ShadowRoot
instance IsParentNode ShadowRoot
instance IsDocumentOrShadowRoot ShadowRoot
instance IsGObject ShadowRoot where
  typeGType _ = gTypeShadowRoot
  {-# INLINE typeGType #-}

noShadowRoot :: Maybe ShadowRoot
noShadowRoot = Nothing
{-# INLINE noShadowRoot #-}

gTypeShadowRoot :: JSM GType
gTypeShadowRoot = GType . Object <$> jsg "ShadowRoot"

-- | Functions for this inteface are in "JSDOM.Window".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--     * "JSDOM.WindowOrWorkerGlobalScope"
--     * "JSDOM.WindowEventHandlers"
--     * "JSDOM.GlobalPerformance"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.GlobalCrypto"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Window Mozilla Window documentation>
newtype Window = Window { unWindow :: JSVal }

instance PToJSVal Window where
  pToJSVal = unWindow
  {-# INLINE pToJSVal #-}

instance PFromJSVal Window where
  pFromJSVal = Window
  {-# INLINE pFromJSVal #-}

instance ToJSVal Window where
  toJSVal = return . unWindow
  {-# INLINE toJSVal #-}

instance FromJSVal Window where
  fromJSVal v = fmap Window <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Window
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Window where
  makeObject = makeObject . unWindow

instance IsEventTarget Window
instance IsWindowOrWorkerGlobalScope Window
instance IsWindowEventHandlers Window
instance IsGlobalPerformance Window
instance IsGlobalEventHandlers Window
instance IsGlobalCrypto Window
instance IsGObject Window where
  typeGType _ = gTypeWindow
  {-# INLINE typeGType #-}

noWindow :: Maybe Window
noWindow = Nothing
{-# INLINE noWindow #-}

gTypeWindow :: JSM GType
gTypeWindow = GType . Object <$> jsg "Window"

newtype AddEventListenerOptionsOrBool = AddEventListenerOptionsOrBool { unAddEventListenerOptionsOrBool :: JSVal }

instance PToJSVal AddEventListenerOptionsOrBool where
  pToJSVal = unAddEventListenerOptionsOrBool
  {-# INLINE pToJSVal #-}

instance PFromJSVal AddEventListenerOptionsOrBool where
  pFromJSVal = AddEventListenerOptionsOrBool
  {-# INLINE pFromJSVal #-}

instance ToJSVal AddEventListenerOptionsOrBool where
  toJSVal = return . unAddEventListenerOptionsOrBool
  {-# INLINE toJSVal #-}

instance FromJSVal AddEventListenerOptionsOrBool where
  fromJSVal v = fmap AddEventListenerOptionsOrBool <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AddEventListenerOptionsOrBool
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AddEventListenerOptionsOrBool where
  makeObject = makeObject . unAddEventListenerOptionsOrBool

class (FromJSVal o, ToJSVal o, PToJSVal o) => IsAddEventListenerOptionsOrBool o

toAddEventListenerOptionsOrBool :: IsAddEventListenerOptionsOrBool o => o -> AddEventListenerOptionsOrBool
toAddEventListenerOptionsOrBool = AddEventListenerOptionsOrBool . pToJSVal

instance IsAddEventListenerOptionsOrBool AddEventListenerOptionsOrBool
instance IsAddEventListenerOptionsOrBool Bool
instance IsAddEventListenerOptionsOrBool AddEventListenerOptions

newtype BinaryData = BinaryData { unBinaryData :: JSVal }

instance PToJSVal BinaryData where
  pToJSVal = unBinaryData
  {-# INLINE pToJSVal #-}

instance PFromJSVal BinaryData where
  pFromJSVal = BinaryData
  {-# INLINE pFromJSVal #-}

instance ToJSVal BinaryData where
  toJSVal = return . unBinaryData
  {-# INLINE toJSVal #-}

instance FromJSVal BinaryData where
  fromJSVal v = fmap BinaryData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BinaryData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BinaryData where
  makeObject = makeObject . unBinaryData

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsBinaryData o

toBinaryData :: IsBinaryData o => o -> BinaryData
toBinaryData = BinaryData . coerce

instance IsBinaryData BinaryData
instance IsBinaryData ArrayBuffer
instance IsBinaryData ArrayBufferView

newtype BlobPart = BlobPart { unBlobPart :: JSVal }

instance PToJSVal BlobPart where
  pToJSVal = unBlobPart
  {-# INLINE pToJSVal #-}

instance PFromJSVal BlobPart where
  pFromJSVal = BlobPart
  {-# INLINE pFromJSVal #-}

instance ToJSVal BlobPart where
  toJSVal = return . unBlobPart
  {-# INLINE toJSVal #-}

instance FromJSVal BlobPart where
  fromJSVal v = fmap BlobPart <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BlobPart
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BlobPart where
  makeObject = makeObject . unBlobPart

class (FromJSVal o, ToJSVal o) => IsBlobPart o

instance IsBlobPart BlobPart
instance IsBlobPart BinaryData
instance IsBlobPart BufferSource
instance IsBlobPart ArrayBufferView
instance IsBlobPart ArrayBuffer
instance IsBlobPart Blob
instance IsBlobPart File
instance IsBlobPart Text
instance IsBlobPart JSString
instance IsBlobPart String

newtype BodyInit = BodyInit { unBodyInit :: JSVal }

instance PToJSVal BodyInit where
  pToJSVal = unBodyInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal BodyInit where
  pFromJSVal = BodyInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal BodyInit where
  toJSVal = return . unBodyInit
  {-# INLINE toJSVal #-}

instance FromJSVal BodyInit where
  fromJSVal v = fmap BodyInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BodyInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BodyInit where
  makeObject = makeObject . unBodyInit

class (FromJSVal o, ToJSVal o) => IsBodyInit o

instance IsBodyInit BodyInit
instance IsBodyInit Text
instance IsBodyInit JSString
instance IsBodyInit String
instance IsBodyInit FormData
instance IsBodyInit BinaryData
instance IsBodyInit BufferSource
instance IsBodyInit ArrayBufferView
instance IsBodyInit ArrayBuffer
instance IsBodyInit Blob
instance IsBodyInit File

newtype BufferDataSource = BufferDataSource { unBufferDataSource :: JSVal }

instance PToJSVal BufferDataSource where
  pToJSVal = unBufferDataSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal BufferDataSource where
  pFromJSVal = BufferDataSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal BufferDataSource where
  toJSVal = return . unBufferDataSource
  {-# INLINE toJSVal #-}

instance FromJSVal BufferDataSource where
  fromJSVal v = fmap BufferDataSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BufferDataSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BufferDataSource where
  makeObject = makeObject . unBufferDataSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsBufferDataSource o

toBufferDataSource :: IsBufferDataSource o => o -> BufferDataSource
toBufferDataSource = BufferDataSource . coerce

instance IsBufferDataSource BufferDataSource
instance IsBufferDataSource ArrayBuffer
instance IsBufferDataSource ArrayBufferView

newtype BufferSource = BufferSource { unBufferSource :: JSVal }

instance PToJSVal BufferSource where
  pToJSVal = unBufferSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal BufferSource where
  pFromJSVal = BufferSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal BufferSource where
  toJSVal = return . unBufferSource
  {-# INLINE toJSVal #-}

instance FromJSVal BufferSource where
  fromJSVal v = fmap BufferSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BufferSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BufferSource where
  makeObject = makeObject . unBufferSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsBufferSource o

toBufferSource :: IsBufferSource o => o -> BufferSource
toBufferSource = BufferSource . coerce

instance IsBufferSource BufferSource
instance IsBufferSource ArrayBuffer
instance IsBufferSource ArrayBufferView

-- The remainder of this file is generated from IDL files using domconv-webkit-jsffi
newtype CanvasImageSource = CanvasImageSource { unCanvasImageSource :: JSVal }

instance PToJSVal CanvasImageSource where
  pToJSVal = unCanvasImageSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasImageSource where
  pFromJSVal = CanvasImageSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasImageSource where
  toJSVal = return . unCanvasImageSource
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasImageSource where
  fromJSVal v = fmap CanvasImageSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasImageSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasImageSource where
  makeObject = makeObject . unCanvasImageSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsCanvasImageSource o

toCanvasImageSource :: IsCanvasImageSource o => o -> CanvasImageSource
toCanvasImageSource = CanvasImageSource . coerce

instance IsCanvasImageSource CanvasImageSource
instance IsCanvasImageSource HTMLImageElement
instance IsCanvasImageSource HTMLVideoElement
instance IsCanvasImageSource HTMLCanvasElement

newtype CanvasStyle = CanvasStyle { unCanvasStyle :: JSVal }

instance PToJSVal CanvasStyle where
  pToJSVal = unCanvasStyle
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasStyle where
  pFromJSVal = CanvasStyle
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasStyle where
  toJSVal = return . unCanvasStyle
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasStyle where
  fromJSVal v = fmap CanvasStyle <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasStyle
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasStyle where
  makeObject = makeObject . unCanvasStyle

class (FromJSVal o, ToJSVal o) => IsCanvasStyle o

instance IsCanvasStyle CanvasStyle
instance IsCanvasStyle CanvasPattern
instance IsCanvasStyle CanvasGradient
instance IsCanvasStyle Text
instance IsCanvasStyle JSString
instance IsCanvasStyle String

newtype CredentialBodyType = CredentialBodyType { unCredentialBodyType :: JSVal }

instance PToJSVal CredentialBodyType where
  pToJSVal = unCredentialBodyType
  {-# INLINE pToJSVal #-}

instance PFromJSVal CredentialBodyType where
  pFromJSVal = CredentialBodyType
  {-# INLINE pFromJSVal #-}

instance ToJSVal CredentialBodyType where
  toJSVal = return . unCredentialBodyType
  {-# INLINE toJSVal #-}

instance FromJSVal CredentialBodyType where
  fromJSVal v = fmap CredentialBodyType <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CredentialBodyType
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CredentialBodyType where
  makeObject = makeObject . unCredentialBodyType

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsCredentialBodyType o

toCredentialBodyType :: IsCredentialBodyType o => o -> CredentialBodyType
toCredentialBodyType = CredentialBodyType . coerce

instance IsCredentialBodyType CredentialBodyType
instance IsCredentialBodyType URLSearchParams
instance IsCredentialBodyType FormData

newtype CryptoKeyOrKeyPair = CryptoKeyOrKeyPair { unCryptoKeyOrKeyPair :: JSVal }

instance PToJSVal CryptoKeyOrKeyPair where
  pToJSVal = unCryptoKeyOrKeyPair
  {-# INLINE pToJSVal #-}

instance PFromJSVal CryptoKeyOrKeyPair where
  pFromJSVal = CryptoKeyOrKeyPair
  {-# INLINE pFromJSVal #-}

instance ToJSVal CryptoKeyOrKeyPair where
  toJSVal = return . unCryptoKeyOrKeyPair
  {-# INLINE toJSVal #-}

instance FromJSVal CryptoKeyOrKeyPair where
  fromJSVal v = fmap CryptoKeyOrKeyPair <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CryptoKeyOrKeyPair
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CryptoKeyOrKeyPair where
  makeObject = makeObject . unCryptoKeyOrKeyPair

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsCryptoKeyOrKeyPair o

toCryptoKeyOrKeyPair :: IsCryptoKeyOrKeyPair o => o -> CryptoKeyOrKeyPair
toCryptoKeyOrKeyPair = CryptoKeyOrKeyPair . coerce

instance IsCryptoKeyOrKeyPair CryptoKeyOrKeyPair
instance IsCryptoKeyOrKeyPair CryptoKeyPair
instance IsCryptoKeyOrKeyPair CryptoKey

newtype EventListenerOptionsOrBool = EventListenerOptionsOrBool { unEventListenerOptionsOrBool :: JSVal }

instance PToJSVal EventListenerOptionsOrBool where
  pToJSVal = unEventListenerOptionsOrBool
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventListenerOptionsOrBool where
  pFromJSVal = EventListenerOptionsOrBool
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventListenerOptionsOrBool where
  toJSVal = return . unEventListenerOptionsOrBool
  {-# INLINE toJSVal #-}

instance FromJSVal EventListenerOptionsOrBool where
  fromJSVal v = fmap EventListenerOptionsOrBool <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventListenerOptionsOrBool
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventListenerOptionsOrBool where
  makeObject = makeObject . unEventListenerOptionsOrBool

class (FromJSVal o, ToJSVal o, PToJSVal o) => IsEventListenerOptionsOrBool o

toEventListenerOptionsOrBool :: IsEventListenerOptionsOrBool o => o -> EventListenerOptionsOrBool
toEventListenerOptionsOrBool = EventListenerOptionsOrBool . pToJSVal

instance IsEventListenerOptionsOrBool EventListenerOptionsOrBool
instance IsEventListenerOptionsOrBool Bool
instance IsEventListenerOptionsOrBool EventListenerOptions
instance IsEventListenerOptionsOrBool AddEventListenerOptions

newtype Float32List = Float32List { unFloat32List :: JSVal }

instance PToJSVal Float32List where
  pToJSVal = unFloat32List
  {-# INLINE pToJSVal #-}

instance PFromJSVal Float32List where
  pFromJSVal = Float32List
  {-# INLINE pFromJSVal #-}

instance ToJSVal Float32List where
  toJSVal = return . unFloat32List
  {-# INLINE toJSVal #-}

instance FromJSVal Float32List where
  fromJSVal v = fmap Float32List <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Float32List
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Float32List where
  makeObject = makeObject . unFloat32List

class (FromJSVal o, ToJSVal o) => IsFloat32List o

instance IsFloat32List Float32List
instance IsFloat32List [GLfloat]
instance IsFloat32List Float32Array

newtype HTMLCollectionOrElement = HTMLCollectionOrElement { unHTMLCollectionOrElement :: JSVal }

instance PToJSVal HTMLCollectionOrElement where
  pToJSVal = unHTMLCollectionOrElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLCollectionOrElement where
  pFromJSVal = HTMLCollectionOrElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLCollectionOrElement where
  toJSVal = return . unHTMLCollectionOrElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLCollectionOrElement where
  fromJSVal v = fmap HTMLCollectionOrElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLCollectionOrElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLCollectionOrElement where
  makeObject = makeObject . unHTMLCollectionOrElement

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsHTMLCollectionOrElement o

toHTMLCollectionOrElement :: IsHTMLCollectionOrElement o => o -> HTMLCollectionOrElement
toHTMLCollectionOrElement = HTMLCollectionOrElement . coerce

instance IsHTMLCollectionOrElement HTMLCollectionOrElement
instance IsHTMLCollectionOrElement Element
instance IsHTMLCollectionOrElement SVGViewElement
instance IsHTMLCollectionOrElement SVGVKernElement
instance IsHTMLCollectionOrElement SVGUseElement
instance IsHTMLCollectionOrElement SVGTitleElement
instance IsHTMLCollectionOrElement SVGTextPositioningElement
instance IsHTMLCollectionOrElement SVGTextPathElement
instance IsHTMLCollectionOrElement SVGTextElement
instance IsHTMLCollectionOrElement SVGTextContentElement
instance IsHTMLCollectionOrElement SVGTSpanElement
instance IsHTMLCollectionOrElement SVGTRefElement
instance IsHTMLCollectionOrElement SVGSymbolElement
instance IsHTMLCollectionOrElement SVGSwitchElement
instance IsHTMLCollectionOrElement SVGStyleElement
instance IsHTMLCollectionOrElement SVGStopElement
instance IsHTMLCollectionOrElement SVGSetElement
instance IsHTMLCollectionOrElement SVGScriptElement
instance IsHTMLCollectionOrElement SVGSVGElement
instance IsHTMLCollectionOrElement SVGRectElement
instance IsHTMLCollectionOrElement SVGRadialGradientElement
instance IsHTMLCollectionOrElement SVGPolylineElement
instance IsHTMLCollectionOrElement SVGPolygonElement
instance IsHTMLCollectionOrElement SVGPatternElement
instance IsHTMLCollectionOrElement SVGPathElement
instance IsHTMLCollectionOrElement SVGMissingGlyphElement
instance IsHTMLCollectionOrElement SVGMetadataElement
instance IsHTMLCollectionOrElement SVGMaskElement
instance IsHTMLCollectionOrElement SVGMarkerElement
instance IsHTMLCollectionOrElement SVGMPathElement
instance IsHTMLCollectionOrElement SVGLinearGradientElement
instance IsHTMLCollectionOrElement SVGLineElement
instance IsHTMLCollectionOrElement SVGImageElement
instance IsHTMLCollectionOrElement SVGHKernElement
instance IsHTMLCollectionOrElement SVGGraphicsElement
instance IsHTMLCollectionOrElement SVGGradientElement
instance IsHTMLCollectionOrElement SVGGlyphRefElement
instance IsHTMLCollectionOrElement SVGGlyphElement
instance IsHTMLCollectionOrElement SVGGElement
instance IsHTMLCollectionOrElement SVGForeignObjectElement
instance IsHTMLCollectionOrElement SVGFontFaceUriElement
instance IsHTMLCollectionOrElement SVGFontFaceSrcElement
instance IsHTMLCollectionOrElement SVGFontFaceNameElement
instance IsHTMLCollectionOrElement SVGFontFaceFormatElement
instance IsHTMLCollectionOrElement SVGFontFaceElement
instance IsHTMLCollectionOrElement SVGFontElement
instance IsHTMLCollectionOrElement SVGFilterElement
instance IsHTMLCollectionOrElement SVGFETurbulenceElement
instance IsHTMLCollectionOrElement SVGFETileElement
instance IsHTMLCollectionOrElement SVGFESpotLightElement
instance IsHTMLCollectionOrElement SVGFESpecularLightingElement
instance IsHTMLCollectionOrElement SVGFEPointLightElement
instance IsHTMLCollectionOrElement SVGFEOffsetElement
instance IsHTMLCollectionOrElement SVGFEMorphologyElement
instance IsHTMLCollectionOrElement SVGFEMergeNodeElement
instance IsHTMLCollectionOrElement SVGFEMergeElement
instance IsHTMLCollectionOrElement SVGFEImageElement
instance IsHTMLCollectionOrElement SVGFEGaussianBlurElement
instance IsHTMLCollectionOrElement SVGFEFuncRElement
instance IsHTMLCollectionOrElement SVGFEFuncGElement
instance IsHTMLCollectionOrElement SVGFEFuncBElement
instance IsHTMLCollectionOrElement SVGFEFuncAElement
instance IsHTMLCollectionOrElement SVGFEFloodElement
instance IsHTMLCollectionOrElement SVGFEDropShadowElement
instance IsHTMLCollectionOrElement SVGFEDistantLightElement
instance IsHTMLCollectionOrElement SVGFEDisplacementMapElement
instance IsHTMLCollectionOrElement SVGFEDiffuseLightingElement
instance IsHTMLCollectionOrElement SVGFEConvolveMatrixElement
instance IsHTMLCollectionOrElement SVGFECompositeElement
instance IsHTMLCollectionOrElement SVGFEComponentTransferElement
instance IsHTMLCollectionOrElement SVGFEColorMatrixElement
instance IsHTMLCollectionOrElement SVGFEBlendElement
instance IsHTMLCollectionOrElement SVGEllipseElement
instance IsHTMLCollectionOrElement SVGElement
instance IsHTMLCollectionOrElement SVGDescElement
instance IsHTMLCollectionOrElement SVGDefsElement
instance IsHTMLCollectionOrElement SVGCursorElement
instance IsHTMLCollectionOrElement SVGComponentTransferFunctionElement
instance IsHTMLCollectionOrElement SVGClipPathElement
instance IsHTMLCollectionOrElement SVGCircleElement
instance IsHTMLCollectionOrElement SVGAnimationElement
instance IsHTMLCollectionOrElement SVGAnimateTransformElement
instance IsHTMLCollectionOrElement SVGAnimateMotionElement
instance IsHTMLCollectionOrElement SVGAnimateElement
instance IsHTMLCollectionOrElement SVGAnimateColorElement
instance IsHTMLCollectionOrElement SVGAltGlyphItemElement
instance IsHTMLCollectionOrElement SVGAltGlyphElement
instance IsHTMLCollectionOrElement SVGAltGlyphDefElement
instance IsHTMLCollectionOrElement SVGAElement
instance IsHTMLCollectionOrElement HTMLVideoElement
instance IsHTMLCollectionOrElement HTMLUnknownElement
instance IsHTMLCollectionOrElement HTMLUListElement
instance IsHTMLCollectionOrElement HTMLTrackElement
instance IsHTMLCollectionOrElement HTMLTitleElement
instance IsHTMLCollectionOrElement HTMLTimeElement
instance IsHTMLCollectionOrElement HTMLTextAreaElement
instance IsHTMLCollectionOrElement HTMLTemplateElement
instance IsHTMLCollectionOrElement HTMLTableSectionElement
instance IsHTMLCollectionOrElement HTMLTableRowElement
instance IsHTMLCollectionOrElement HTMLTableElement
instance IsHTMLCollectionOrElement HTMLTableColElement
instance IsHTMLCollectionOrElement HTMLTableCellElement
instance IsHTMLCollectionOrElement HTMLTableCaptionElement
instance IsHTMLCollectionOrElement HTMLStyleElement
instance IsHTMLCollectionOrElement HTMLSpanElement
instance IsHTMLCollectionOrElement HTMLSourceElement
instance IsHTMLCollectionOrElement HTMLSlotElement
instance IsHTMLCollectionOrElement HTMLSelectElement
instance IsHTMLCollectionOrElement HTMLScriptElement
instance IsHTMLCollectionOrElement HTMLQuoteElement
instance IsHTMLCollectionOrElement HTMLProgressElement
instance IsHTMLCollectionOrElement HTMLPreElement
instance IsHTMLCollectionOrElement HTMLPictureElement
instance IsHTMLCollectionOrElement HTMLParamElement
instance IsHTMLCollectionOrElement HTMLParagraphElement
instance IsHTMLCollectionOrElement HTMLOutputElement
instance IsHTMLCollectionOrElement HTMLOptionElement
instance IsHTMLCollectionOrElement HTMLOptGroupElement
instance IsHTMLCollectionOrElement HTMLObjectElement
instance IsHTMLCollectionOrElement HTMLOListElement
instance IsHTMLCollectionOrElement HTMLModElement
instance IsHTMLCollectionOrElement HTMLMeterElement
instance IsHTMLCollectionOrElement HTMLMetaElement
instance IsHTMLCollectionOrElement HTMLMenuElement
instance IsHTMLCollectionOrElement HTMLMediaElement
instance IsHTMLCollectionOrElement HTMLMarqueeElement
instance IsHTMLCollectionOrElement HTMLMapElement
instance IsHTMLCollectionOrElement HTMLLinkElement
instance IsHTMLCollectionOrElement HTMLLegendElement
instance IsHTMLCollectionOrElement HTMLLabelElement
instance IsHTMLCollectionOrElement HTMLLIElement
instance IsHTMLCollectionOrElement HTMLKeygenElement
instance IsHTMLCollectionOrElement HTMLInputElement
instance IsHTMLCollectionOrElement HTMLImageElement
instance IsHTMLCollectionOrElement HTMLIFrameElement
instance IsHTMLCollectionOrElement HTMLHtmlElement
instance IsHTMLCollectionOrElement HTMLHeadingElement
instance IsHTMLCollectionOrElement HTMLHeadElement
instance IsHTMLCollectionOrElement HTMLHRElement
instance IsHTMLCollectionOrElement HTMLFrameSetElement
instance IsHTMLCollectionOrElement HTMLFrameElement
instance IsHTMLCollectionOrElement HTMLFormElement
instance IsHTMLCollectionOrElement HTMLFontElement
instance IsHTMLCollectionOrElement HTMLFieldSetElement
instance IsHTMLCollectionOrElement HTMLEmbedElement
instance IsHTMLCollectionOrElement HTMLElement
instance IsHTMLCollectionOrElement HTMLDivElement
instance IsHTMLCollectionOrElement HTMLDirectoryElement
instance IsHTMLCollectionOrElement HTMLDetailsElement
instance IsHTMLCollectionOrElement HTMLDataListElement
instance IsHTMLCollectionOrElement HTMLDataElement
instance IsHTMLCollectionOrElement HTMLDListElement
instance IsHTMLCollectionOrElement HTMLCanvasElement
instance IsHTMLCollectionOrElement HTMLButtonElement
instance IsHTMLCollectionOrElement HTMLBodyElement
instance IsHTMLCollectionOrElement HTMLBaseElement
instance IsHTMLCollectionOrElement HTMLBRElement
instance IsHTMLCollectionOrElement HTMLAudioElement
instance IsHTMLCollectionOrElement HTMLAttachmentElement
instance IsHTMLCollectionOrElement HTMLAreaElement
instance IsHTMLCollectionOrElement HTMLAppletElement
instance IsHTMLCollectionOrElement HTMLAnchorElement
instance IsHTMLCollectionOrElement HTMLCollection
instance IsHTMLCollectionOrElement HTMLOptionsCollection
instance IsHTMLCollectionOrElement HTMLFormControlsCollection

newtype HTMLElementOrLong = HTMLElementOrLong { unHTMLElementOrLong :: JSVal }

instance PToJSVal HTMLElementOrLong where
  pToJSVal = unHTMLElementOrLong
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLElementOrLong where
  pFromJSVal = HTMLElementOrLong
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLElementOrLong where
  toJSVal = return . unHTMLElementOrLong
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLElementOrLong where
  fromJSVal v = fmap HTMLElementOrLong <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLElementOrLong
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLElementOrLong where
  makeObject = makeObject . unHTMLElementOrLong

class (FromJSVal o, ToJSVal o) => IsHTMLElementOrLong o

instance IsHTMLElementOrLong HTMLElementOrLong
instance IsHTMLElementOrLong Int
instance IsHTMLElementOrLong HTMLElement
instance IsHTMLElementOrLong HTMLVideoElement
instance IsHTMLElementOrLong HTMLUnknownElement
instance IsHTMLElementOrLong HTMLUListElement
instance IsHTMLElementOrLong HTMLTrackElement
instance IsHTMLElementOrLong HTMLTitleElement
instance IsHTMLElementOrLong HTMLTimeElement
instance IsHTMLElementOrLong HTMLTextAreaElement
instance IsHTMLElementOrLong HTMLTemplateElement
instance IsHTMLElementOrLong HTMLTableSectionElement
instance IsHTMLElementOrLong HTMLTableRowElement
instance IsHTMLElementOrLong HTMLTableElement
instance IsHTMLElementOrLong HTMLTableColElement
instance IsHTMLElementOrLong HTMLTableCellElement
instance IsHTMLElementOrLong HTMLTableCaptionElement
instance IsHTMLElementOrLong HTMLStyleElement
instance IsHTMLElementOrLong HTMLSpanElement
instance IsHTMLElementOrLong HTMLSourceElement
instance IsHTMLElementOrLong HTMLSlotElement
instance IsHTMLElementOrLong HTMLSelectElement
instance IsHTMLElementOrLong HTMLScriptElement
instance IsHTMLElementOrLong HTMLQuoteElement
instance IsHTMLElementOrLong HTMLProgressElement
instance IsHTMLElementOrLong HTMLPreElement
instance IsHTMLElementOrLong HTMLPictureElement
instance IsHTMLElementOrLong HTMLParamElement
instance IsHTMLElementOrLong HTMLParagraphElement
instance IsHTMLElementOrLong HTMLOutputElement
instance IsHTMLElementOrLong HTMLOptionElement
instance IsHTMLElementOrLong HTMLOptGroupElement
instance IsHTMLElementOrLong HTMLObjectElement
instance IsHTMLElementOrLong HTMLOListElement
instance IsHTMLElementOrLong HTMLModElement
instance IsHTMLElementOrLong HTMLMeterElement
instance IsHTMLElementOrLong HTMLMetaElement
instance IsHTMLElementOrLong HTMLMenuElement
instance IsHTMLElementOrLong HTMLMediaElement
instance IsHTMLElementOrLong HTMLMarqueeElement
instance IsHTMLElementOrLong HTMLMapElement
instance IsHTMLElementOrLong HTMLLinkElement
instance IsHTMLElementOrLong HTMLLegendElement
instance IsHTMLElementOrLong HTMLLabelElement
instance IsHTMLElementOrLong HTMLLIElement
instance IsHTMLElementOrLong HTMLKeygenElement
instance IsHTMLElementOrLong HTMLInputElement
instance IsHTMLElementOrLong HTMLImageElement
instance IsHTMLElementOrLong HTMLIFrameElement
instance IsHTMLElementOrLong HTMLHtmlElement
instance IsHTMLElementOrLong HTMLHeadingElement
instance IsHTMLElementOrLong HTMLHeadElement
instance IsHTMLElementOrLong HTMLHRElement
instance IsHTMLElementOrLong HTMLFrameSetElement
instance IsHTMLElementOrLong HTMLFrameElement
instance IsHTMLElementOrLong HTMLFormElement
instance IsHTMLElementOrLong HTMLFontElement
instance IsHTMLElementOrLong HTMLFieldSetElement
instance IsHTMLElementOrLong HTMLEmbedElement
instance IsHTMLElementOrLong HTMLDivElement
instance IsHTMLElementOrLong HTMLDirectoryElement
instance IsHTMLElementOrLong HTMLDetailsElement
instance IsHTMLElementOrLong HTMLDataListElement
instance IsHTMLElementOrLong HTMLDataElement
instance IsHTMLElementOrLong HTMLDListElement
instance IsHTMLElementOrLong HTMLCanvasElement
instance IsHTMLElementOrLong HTMLButtonElement
instance IsHTMLElementOrLong HTMLBodyElement
instance IsHTMLElementOrLong HTMLBaseElement
instance IsHTMLElementOrLong HTMLBRElement
instance IsHTMLElementOrLong HTMLAudioElement
instance IsHTMLElementOrLong HTMLAttachmentElement
instance IsHTMLElementOrLong HTMLAreaElement
instance IsHTMLElementOrLong HTMLAppletElement
instance IsHTMLElementOrLong HTMLAnchorElement

newtype HTMLOptionElementOrGroup = HTMLOptionElementOrGroup { unHTMLOptionElementOrGroup :: JSVal }

instance PToJSVal HTMLOptionElementOrGroup where
  pToJSVal = unHTMLOptionElementOrGroup
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOptionElementOrGroup where
  pFromJSVal = HTMLOptionElementOrGroup
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOptionElementOrGroup where
  toJSVal = return . unHTMLOptionElementOrGroup
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOptionElementOrGroup where
  fromJSVal v = fmap HTMLOptionElementOrGroup <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOptionElementOrGroup
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOptionElementOrGroup where
  makeObject = makeObject . unHTMLOptionElementOrGroup

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsHTMLOptionElementOrGroup o

toHTMLOptionElementOrGroup :: IsHTMLOptionElementOrGroup o => o -> HTMLOptionElementOrGroup
toHTMLOptionElementOrGroup = HTMLOptionElementOrGroup . coerce

instance IsHTMLOptionElementOrGroup HTMLOptionElementOrGroup
instance IsHTMLOptionElementOrGroup HTMLOptGroupElement
instance IsHTMLOptionElementOrGroup HTMLOptionElement

newtype IDBCursorSource = IDBCursorSource { unIDBCursorSource :: JSVal }

instance PToJSVal IDBCursorSource where
  pToJSVal = unIDBCursorSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBCursorSource where
  pFromJSVal = IDBCursorSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBCursorSource where
  toJSVal = return . unIDBCursorSource
  {-# INLINE toJSVal #-}

instance FromJSVal IDBCursorSource where
  fromJSVal v = fmap IDBCursorSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBCursorSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBCursorSource where
  makeObject = makeObject . unIDBCursorSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsIDBCursorSource o

toIDBCursorSource :: IsIDBCursorSource o => o -> IDBCursorSource
toIDBCursorSource = IDBCursorSource . coerce

instance IsIDBCursorSource IDBCursorSource
instance IsIDBCursorSource IDBIndex
instance IsIDBCursorSource IDBObjectStore

newtype IDBKeyPath = IDBKeyPath { unIDBKeyPath :: JSVal }

instance PToJSVal IDBKeyPath where
  pToJSVal = unIDBKeyPath
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBKeyPath where
  pFromJSVal = IDBKeyPath
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBKeyPath where
  toJSVal = return . unIDBKeyPath
  {-# INLINE toJSVal #-}

instance FromJSVal IDBKeyPath where
  fromJSVal v = fmap IDBKeyPath <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBKeyPath
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBKeyPath where
  makeObject = makeObject . unIDBKeyPath

class (FromJSVal o, ToJSVal o) => IsIDBKeyPath o

instance IsIDBKeyPath IDBKeyPath
instance IsIDBKeyPath [Text]
instance IsIDBKeyPath [JSString]
instance IsIDBKeyPath [String]
instance IsIDBKeyPath Text
instance IsIDBKeyPath JSString
instance IsIDBKeyPath String

newtype IDBRequestResult = IDBRequestResult { unIDBRequestResult :: JSVal }

instance PToJSVal IDBRequestResult where
  pToJSVal = unIDBRequestResult
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBRequestResult where
  pFromJSVal = IDBRequestResult
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBRequestResult where
  toJSVal = return . unIDBRequestResult
  {-# INLINE toJSVal #-}

instance FromJSVal IDBRequestResult where
  fromJSVal v = fmap IDBRequestResult <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBRequestResult
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBRequestResult where
  makeObject = makeObject . unIDBRequestResult

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsIDBRequestResult o

toIDBRequestResult :: IsIDBRequestResult o => o -> IDBRequestResult
toIDBRequestResult = IDBRequestResult . coerce

instance IsIDBRequestResult IDBRequestResult
instance IsIDBRequestResult JSVal
instance IsIDBRequestResult IDBDatabase
instance IsIDBRequestResult IDBCursor
instance IsIDBRequestResult IDBCursorWithValue

newtype IDBRequestSource = IDBRequestSource { unIDBRequestSource :: JSVal }

instance PToJSVal IDBRequestSource where
  pToJSVal = unIDBRequestSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBRequestSource where
  pFromJSVal = IDBRequestSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBRequestSource where
  toJSVal = return . unIDBRequestSource
  {-# INLINE toJSVal #-}

instance FromJSVal IDBRequestSource where
  fromJSVal v = fmap IDBRequestSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBRequestSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBRequestSource where
  makeObject = makeObject . unIDBRequestSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsIDBRequestSource o

toIDBRequestSource :: IsIDBRequestSource o => o -> IDBRequestSource
toIDBRequestSource = IDBRequestSource . coerce

instance IsIDBRequestSource IDBRequestSource
instance IsIDBRequestSource IDBCursor
instance IsIDBRequestSource IDBCursorWithValue
instance IsIDBRequestSource IDBIndex
instance IsIDBRequestSource IDBObjectStore

newtype Int32List = Int32List { unInt32List :: JSVal }

instance PToJSVal Int32List where
  pToJSVal = unInt32List
  {-# INLINE pToJSVal #-}

instance PFromJSVal Int32List where
  pFromJSVal = Int32List
  {-# INLINE pFromJSVal #-}

instance ToJSVal Int32List where
  toJSVal = return . unInt32List
  {-# INLINE toJSVal #-}

instance FromJSVal Int32List where
  fromJSVal v = fmap Int32List <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Int32List
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Int32List where
  makeObject = makeObject . unInt32List

class (FromJSVal o, ToJSVal o) => IsInt32List o

instance IsInt32List Int32List
instance IsInt32List [GLint]
instance IsInt32List Int32Array

newtype KeyData = KeyData { unKeyData :: JSVal }

instance PToJSVal KeyData where
  pToJSVal = unKeyData
  {-# INLINE pToJSVal #-}

instance PFromJSVal KeyData where
  pFromJSVal = KeyData
  {-# INLINE pFromJSVal #-}

instance ToJSVal KeyData where
  toJSVal = return . unKeyData
  {-# INLINE toJSVal #-}

instance FromJSVal KeyData where
  fromJSVal v = fmap KeyData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . KeyData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject KeyData where
  makeObject = makeObject . unKeyData

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsKeyData o

toKeyData :: IsKeyData o => o -> KeyData
toKeyData = KeyData . coerce

instance IsKeyData KeyData
instance IsKeyData JsonWebKey
instance IsKeyData BinaryData
instance IsKeyData BufferSource
instance IsKeyData ArrayBufferView
instance IsKeyData ArrayBuffer

newtype MediaProvider = MediaProvider { unMediaProvider :: JSVal }

instance PToJSVal MediaProvider where
  pToJSVal = unMediaProvider
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaProvider where
  pFromJSVal = MediaProvider
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaProvider where
  toJSVal = return . unMediaProvider
  {-# INLINE toJSVal #-}

instance FromJSVal MediaProvider where
  fromJSVal v = fmap MediaProvider <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaProvider
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaProvider where
  makeObject = makeObject . unMediaProvider

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsMediaProvider o

toMediaProvider :: IsMediaProvider o => o -> MediaProvider
toMediaProvider = MediaProvider . coerce

instance IsMediaProvider MediaProvider
instance IsMediaProvider MediaStream
instance IsMediaProvider MediaSource
instance IsMediaProvider Blob
instance IsMediaProvider File

newtype MediaStreamTrackOrKind = MediaStreamTrackOrKind { unMediaStreamTrackOrKind :: JSVal }

instance PToJSVal MediaStreamTrackOrKind where
  pToJSVal = unMediaStreamTrackOrKind
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamTrackOrKind where
  pFromJSVal = MediaStreamTrackOrKind
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamTrackOrKind where
  toJSVal = return . unMediaStreamTrackOrKind
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamTrackOrKind where
  fromJSVal v = fmap MediaStreamTrackOrKind <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamTrackOrKind
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamTrackOrKind where
  makeObject = makeObject . unMediaStreamTrackOrKind

class (FromJSVal o, ToJSVal o) => IsMediaStreamTrackOrKind o

instance IsMediaStreamTrackOrKind MediaStreamTrackOrKind
instance IsMediaStreamTrackOrKind Text
instance IsMediaStreamTrackOrKind JSString
instance IsMediaStreamTrackOrKind String
instance IsMediaStreamTrackOrKind MediaStreamTrack
instance IsMediaStreamTrackOrKind CanvasCaptureMediaStreamTrack

newtype MessageEventSource = MessageEventSource { unMessageEventSource :: JSVal }

instance PToJSVal MessageEventSource where
  pToJSVal = unMessageEventSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal MessageEventSource where
  pFromJSVal = MessageEventSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal MessageEventSource where
  toJSVal = return . unMessageEventSource
  {-# INLINE toJSVal #-}

instance FromJSVal MessageEventSource where
  fromJSVal v = fmap MessageEventSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MessageEventSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MessageEventSource where
  makeObject = makeObject . unMessageEventSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsMessageEventSource o

toMessageEventSource :: IsMessageEventSource o => o -> MessageEventSource
toMessageEventSource = MessageEventSource . coerce

instance IsMessageEventSource MessageEventSource
instance IsMessageEventSource MessagePort
instance IsMessageEventSource Window

newtype NodeOrString = NodeOrString { unNodeOrString :: JSVal }

instance PToJSVal NodeOrString where
  pToJSVal = unNodeOrString
  {-# INLINE pToJSVal #-}

instance PFromJSVal NodeOrString where
  pFromJSVal = NodeOrString
  {-# INLINE pFromJSVal #-}

instance ToJSVal NodeOrString where
  toJSVal = return . unNodeOrString
  {-# INLINE toJSVal #-}

instance FromJSVal NodeOrString where
  fromJSVal v = fmap NodeOrString <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NodeOrString
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NodeOrString where
  makeObject = makeObject . unNodeOrString

class (FromJSVal o, ToJSVal o) => IsNodeOrString o

instance IsNodeOrString NodeOrString
instance IsNodeOrString Text
instance IsNodeOrString JSString
instance IsNodeOrString String
instance IsNodeOrString Node
instance IsNodeOrString XMLDocument
instance IsNodeOrString ShadowRoot
instance IsNodeOrString SVGViewElement
instance IsNodeOrString SVGVKernElement
instance IsNodeOrString SVGUseElement
instance IsNodeOrString SVGTitleElement
instance IsNodeOrString SVGTextPositioningElement
instance IsNodeOrString SVGTextPathElement
instance IsNodeOrString SVGTextElement
instance IsNodeOrString SVGTextContentElement
instance IsNodeOrString SVGTSpanElement
instance IsNodeOrString SVGTRefElement
instance IsNodeOrString SVGSymbolElement
instance IsNodeOrString SVGSwitchElement
instance IsNodeOrString SVGStyleElement
instance IsNodeOrString SVGStopElement
instance IsNodeOrString SVGSetElement
instance IsNodeOrString SVGScriptElement
instance IsNodeOrString SVGSVGElement
instance IsNodeOrString SVGRectElement
instance IsNodeOrString SVGRadialGradientElement
instance IsNodeOrString SVGPolylineElement
instance IsNodeOrString SVGPolygonElement
instance IsNodeOrString SVGPatternElement
instance IsNodeOrString SVGPathElement
instance IsNodeOrString SVGMissingGlyphElement
instance IsNodeOrString SVGMetadataElement
instance IsNodeOrString SVGMaskElement
instance IsNodeOrString SVGMarkerElement
instance IsNodeOrString SVGMPathElement
instance IsNodeOrString SVGLinearGradientElement
instance IsNodeOrString SVGLineElement
instance IsNodeOrString SVGImageElement
instance IsNodeOrString SVGHKernElement
instance IsNodeOrString SVGGraphicsElement
instance IsNodeOrString SVGGradientElement
instance IsNodeOrString SVGGlyphRefElement
instance IsNodeOrString SVGGlyphElement
instance IsNodeOrString SVGGElement
instance IsNodeOrString SVGForeignObjectElement
instance IsNodeOrString SVGFontFaceUriElement
instance IsNodeOrString SVGFontFaceSrcElement
instance IsNodeOrString SVGFontFaceNameElement
instance IsNodeOrString SVGFontFaceFormatElement
instance IsNodeOrString SVGFontFaceElement
instance IsNodeOrString SVGFontElement
instance IsNodeOrString SVGFilterElement
instance IsNodeOrString SVGFETurbulenceElement
instance IsNodeOrString SVGFETileElement
instance IsNodeOrString SVGFESpotLightElement
instance IsNodeOrString SVGFESpecularLightingElement
instance IsNodeOrString SVGFEPointLightElement
instance IsNodeOrString SVGFEOffsetElement
instance IsNodeOrString SVGFEMorphologyElement
instance IsNodeOrString SVGFEMergeNodeElement
instance IsNodeOrString SVGFEMergeElement
instance IsNodeOrString SVGFEImageElement
instance IsNodeOrString SVGFEGaussianBlurElement
instance IsNodeOrString SVGFEFuncRElement
instance IsNodeOrString SVGFEFuncGElement
instance IsNodeOrString SVGFEFuncBElement
instance IsNodeOrString SVGFEFuncAElement
instance IsNodeOrString SVGFEFloodElement
instance IsNodeOrString SVGFEDropShadowElement
instance IsNodeOrString SVGFEDistantLightElement
instance IsNodeOrString SVGFEDisplacementMapElement
instance IsNodeOrString SVGFEDiffuseLightingElement
instance IsNodeOrString SVGFEConvolveMatrixElement
instance IsNodeOrString SVGFECompositeElement
instance IsNodeOrString SVGFEComponentTransferElement
instance IsNodeOrString SVGFEColorMatrixElement
instance IsNodeOrString SVGFEBlendElement
instance IsNodeOrString SVGEllipseElement
instance IsNodeOrString SVGElement
instance IsNodeOrString SVGDescElement
instance IsNodeOrString SVGDefsElement
instance IsNodeOrString SVGCursorElement
instance IsNodeOrString SVGComponentTransferFunctionElement
instance IsNodeOrString SVGClipPathElement
instance IsNodeOrString SVGCircleElement
instance IsNodeOrString SVGAnimationElement
instance IsNodeOrString SVGAnimateTransformElement
instance IsNodeOrString SVGAnimateMotionElement
instance IsNodeOrString SVGAnimateElement
instance IsNodeOrString SVGAnimateColorElement
instance IsNodeOrString SVGAltGlyphItemElement
instance IsNodeOrString SVGAltGlyphElement
instance IsNodeOrString SVGAltGlyphDefElement
instance IsNodeOrString SVGAElement
instance IsNodeOrString ProcessingInstruction
instance IsNodeOrString HTMLVideoElement
instance IsNodeOrString HTMLUnknownElement
instance IsNodeOrString HTMLUListElement
instance IsNodeOrString HTMLTrackElement
instance IsNodeOrString HTMLTitleElement
instance IsNodeOrString HTMLTimeElement
instance IsNodeOrString HTMLTextAreaElement
instance IsNodeOrString HTMLTemplateElement
instance IsNodeOrString HTMLTableSectionElement
instance IsNodeOrString HTMLTableRowElement
instance IsNodeOrString HTMLTableElement
instance IsNodeOrString HTMLTableColElement
instance IsNodeOrString HTMLTableCellElement
instance IsNodeOrString HTMLTableCaptionElement
instance IsNodeOrString HTMLStyleElement
instance IsNodeOrString HTMLSpanElement
instance IsNodeOrString HTMLSourceElement
instance IsNodeOrString HTMLSlotElement
instance IsNodeOrString HTMLSelectElement
instance IsNodeOrString HTMLScriptElement
instance IsNodeOrString HTMLQuoteElement
instance IsNodeOrString HTMLProgressElement
instance IsNodeOrString HTMLPreElement
instance IsNodeOrString HTMLPictureElement
instance IsNodeOrString HTMLParamElement
instance IsNodeOrString HTMLParagraphElement
instance IsNodeOrString HTMLOutputElement
instance IsNodeOrString HTMLOptionElement
instance IsNodeOrString HTMLOptGroupElement
instance IsNodeOrString HTMLObjectElement
instance IsNodeOrString HTMLOListElement
instance IsNodeOrString HTMLModElement
instance IsNodeOrString HTMLMeterElement
instance IsNodeOrString HTMLMetaElement
instance IsNodeOrString HTMLMenuElement
instance IsNodeOrString HTMLMediaElement
instance IsNodeOrString HTMLMarqueeElement
instance IsNodeOrString HTMLMapElement
instance IsNodeOrString HTMLLinkElement
instance IsNodeOrString HTMLLegendElement
instance IsNodeOrString HTMLLabelElement
instance IsNodeOrString HTMLLIElement
instance IsNodeOrString HTMLKeygenElement
instance IsNodeOrString HTMLInputElement
instance IsNodeOrString HTMLImageElement
instance IsNodeOrString HTMLIFrameElement
instance IsNodeOrString HTMLHtmlElement
instance IsNodeOrString HTMLHeadingElement
instance IsNodeOrString HTMLHeadElement
instance IsNodeOrString HTMLHRElement
instance IsNodeOrString HTMLFrameSetElement
instance IsNodeOrString HTMLFrameElement
instance IsNodeOrString HTMLFormElement
instance IsNodeOrString HTMLFontElement
instance IsNodeOrString HTMLFieldSetElement
instance IsNodeOrString HTMLEmbedElement
instance IsNodeOrString HTMLElement
instance IsNodeOrString HTMLDocument
instance IsNodeOrString HTMLDivElement
instance IsNodeOrString HTMLDirectoryElement
instance IsNodeOrString HTMLDetailsElement
instance IsNodeOrString HTMLDataListElement
instance IsNodeOrString HTMLDataElement
instance IsNodeOrString HTMLDListElement
instance IsNodeOrString HTMLCanvasElement
instance IsNodeOrString HTMLButtonElement
instance IsNodeOrString HTMLBodyElement
instance IsNodeOrString HTMLBaseElement
instance IsNodeOrString HTMLBRElement
instance IsNodeOrString HTMLAudioElement
instance IsNodeOrString HTMLAttachmentElement
instance IsNodeOrString HTMLAreaElement
instance IsNodeOrString HTMLAppletElement
instance IsNodeOrString HTMLAnchorElement
instance IsNodeOrString Element
instance IsNodeOrString DocumentType
instance IsNodeOrString DocumentFragment
instance IsNodeOrString Document
instance IsNodeOrString Comment
instance IsNodeOrString CharacterData
instance IsNodeOrString CDATASection
instance IsNodeOrString Attr

newtype RTCIceCandidateOrInit = RTCIceCandidateOrInit { unRTCIceCandidateOrInit :: JSVal }

instance PToJSVal RTCIceCandidateOrInit where
  pToJSVal = unRTCIceCandidateOrInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceCandidateOrInit where
  pFromJSVal = RTCIceCandidateOrInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceCandidateOrInit where
  toJSVal = return . unRTCIceCandidateOrInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceCandidateOrInit where
  fromJSVal v = fmap RTCIceCandidateOrInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceCandidateOrInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceCandidateOrInit where
  makeObject = makeObject . unRTCIceCandidateOrInit

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsRTCIceCandidateOrInit o

toRTCIceCandidateOrInit :: IsRTCIceCandidateOrInit o => o -> RTCIceCandidateOrInit
toRTCIceCandidateOrInit = RTCIceCandidateOrInit . coerce

instance IsRTCIceCandidateOrInit RTCIceCandidateOrInit
instance IsRTCIceCandidateOrInit RTCIceCandidate
instance IsRTCIceCandidateOrInit RTCIceCandidateInit

newtype RadioNodeListOrElement = RadioNodeListOrElement { unRadioNodeListOrElement :: JSVal }

instance PToJSVal RadioNodeListOrElement where
  pToJSVal = unRadioNodeListOrElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal RadioNodeListOrElement where
  pFromJSVal = RadioNodeListOrElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal RadioNodeListOrElement where
  toJSVal = return . unRadioNodeListOrElement
  {-# INLINE toJSVal #-}

instance FromJSVal RadioNodeListOrElement where
  fromJSVal v = fmap RadioNodeListOrElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RadioNodeListOrElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RadioNodeListOrElement where
  makeObject = makeObject . unRadioNodeListOrElement

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsRadioNodeListOrElement o

toRadioNodeListOrElement :: IsRadioNodeListOrElement o => o -> RadioNodeListOrElement
toRadioNodeListOrElement = RadioNodeListOrElement . coerce

instance IsRadioNodeListOrElement RadioNodeListOrElement
instance IsRadioNodeListOrElement Element
instance IsRadioNodeListOrElement SVGViewElement
instance IsRadioNodeListOrElement SVGVKernElement
instance IsRadioNodeListOrElement SVGUseElement
instance IsRadioNodeListOrElement SVGTitleElement
instance IsRadioNodeListOrElement SVGTextPositioningElement
instance IsRadioNodeListOrElement SVGTextPathElement
instance IsRadioNodeListOrElement SVGTextElement
instance IsRadioNodeListOrElement SVGTextContentElement
instance IsRadioNodeListOrElement SVGTSpanElement
instance IsRadioNodeListOrElement SVGTRefElement
instance IsRadioNodeListOrElement SVGSymbolElement
instance IsRadioNodeListOrElement SVGSwitchElement
instance IsRadioNodeListOrElement SVGStyleElement
instance IsRadioNodeListOrElement SVGStopElement
instance IsRadioNodeListOrElement SVGSetElement
instance IsRadioNodeListOrElement SVGScriptElement
instance IsRadioNodeListOrElement SVGSVGElement
instance IsRadioNodeListOrElement SVGRectElement
instance IsRadioNodeListOrElement SVGRadialGradientElement
instance IsRadioNodeListOrElement SVGPolylineElement
instance IsRadioNodeListOrElement SVGPolygonElement
instance IsRadioNodeListOrElement SVGPatternElement
instance IsRadioNodeListOrElement SVGPathElement
instance IsRadioNodeListOrElement SVGMissingGlyphElement
instance IsRadioNodeListOrElement SVGMetadataElement
instance IsRadioNodeListOrElement SVGMaskElement
instance IsRadioNodeListOrElement SVGMarkerElement
instance IsRadioNodeListOrElement SVGMPathElement
instance IsRadioNodeListOrElement SVGLinearGradientElement
instance IsRadioNodeListOrElement SVGLineElement
instance IsRadioNodeListOrElement SVGImageElement
instance IsRadioNodeListOrElement SVGHKernElement
instance IsRadioNodeListOrElement SVGGraphicsElement
instance IsRadioNodeListOrElement SVGGradientElement
instance IsRadioNodeListOrElement SVGGlyphRefElement
instance IsRadioNodeListOrElement SVGGlyphElement
instance IsRadioNodeListOrElement SVGGElement
instance IsRadioNodeListOrElement SVGForeignObjectElement
instance IsRadioNodeListOrElement SVGFontFaceUriElement
instance IsRadioNodeListOrElement SVGFontFaceSrcElement
instance IsRadioNodeListOrElement SVGFontFaceNameElement
instance IsRadioNodeListOrElement SVGFontFaceFormatElement
instance IsRadioNodeListOrElement SVGFontFaceElement
instance IsRadioNodeListOrElement SVGFontElement
instance IsRadioNodeListOrElement SVGFilterElement
instance IsRadioNodeListOrElement SVGFETurbulenceElement
instance IsRadioNodeListOrElement SVGFETileElement
instance IsRadioNodeListOrElement SVGFESpotLightElement
instance IsRadioNodeListOrElement SVGFESpecularLightingElement
instance IsRadioNodeListOrElement SVGFEPointLightElement
instance IsRadioNodeListOrElement SVGFEOffsetElement
instance IsRadioNodeListOrElement SVGFEMorphologyElement
instance IsRadioNodeListOrElement SVGFEMergeNodeElement
instance IsRadioNodeListOrElement SVGFEMergeElement
instance IsRadioNodeListOrElement SVGFEImageElement
instance IsRadioNodeListOrElement SVGFEGaussianBlurElement
instance IsRadioNodeListOrElement SVGFEFuncRElement
instance IsRadioNodeListOrElement SVGFEFuncGElement
instance IsRadioNodeListOrElement SVGFEFuncBElement
instance IsRadioNodeListOrElement SVGFEFuncAElement
instance IsRadioNodeListOrElement SVGFEFloodElement
instance IsRadioNodeListOrElement SVGFEDropShadowElement
instance IsRadioNodeListOrElement SVGFEDistantLightElement
instance IsRadioNodeListOrElement SVGFEDisplacementMapElement
instance IsRadioNodeListOrElement SVGFEDiffuseLightingElement
instance IsRadioNodeListOrElement SVGFEConvolveMatrixElement
instance IsRadioNodeListOrElement SVGFECompositeElement
instance IsRadioNodeListOrElement SVGFEComponentTransferElement
instance IsRadioNodeListOrElement SVGFEColorMatrixElement
instance IsRadioNodeListOrElement SVGFEBlendElement
instance IsRadioNodeListOrElement SVGEllipseElement
instance IsRadioNodeListOrElement SVGElement
instance IsRadioNodeListOrElement SVGDescElement
instance IsRadioNodeListOrElement SVGDefsElement
instance IsRadioNodeListOrElement SVGCursorElement
instance IsRadioNodeListOrElement SVGComponentTransferFunctionElement
instance IsRadioNodeListOrElement SVGClipPathElement
instance IsRadioNodeListOrElement SVGCircleElement
instance IsRadioNodeListOrElement SVGAnimationElement
instance IsRadioNodeListOrElement SVGAnimateTransformElement
instance IsRadioNodeListOrElement SVGAnimateMotionElement
instance IsRadioNodeListOrElement SVGAnimateElement
instance IsRadioNodeListOrElement SVGAnimateColorElement
instance IsRadioNodeListOrElement SVGAltGlyphItemElement
instance IsRadioNodeListOrElement SVGAltGlyphElement
instance IsRadioNodeListOrElement SVGAltGlyphDefElement
instance IsRadioNodeListOrElement SVGAElement
instance IsRadioNodeListOrElement HTMLVideoElement
instance IsRadioNodeListOrElement HTMLUnknownElement
instance IsRadioNodeListOrElement HTMLUListElement
instance IsRadioNodeListOrElement HTMLTrackElement
instance IsRadioNodeListOrElement HTMLTitleElement
instance IsRadioNodeListOrElement HTMLTimeElement
instance IsRadioNodeListOrElement HTMLTextAreaElement
instance IsRadioNodeListOrElement HTMLTemplateElement
instance IsRadioNodeListOrElement HTMLTableSectionElement
instance IsRadioNodeListOrElement HTMLTableRowElement
instance IsRadioNodeListOrElement HTMLTableElement
instance IsRadioNodeListOrElement HTMLTableColElement
instance IsRadioNodeListOrElement HTMLTableCellElement
instance IsRadioNodeListOrElement HTMLTableCaptionElement
instance IsRadioNodeListOrElement HTMLStyleElement
instance IsRadioNodeListOrElement HTMLSpanElement
instance IsRadioNodeListOrElement HTMLSourceElement
instance IsRadioNodeListOrElement HTMLSlotElement
instance IsRadioNodeListOrElement HTMLSelectElement
instance IsRadioNodeListOrElement HTMLScriptElement
instance IsRadioNodeListOrElement HTMLQuoteElement
instance IsRadioNodeListOrElement HTMLProgressElement
instance IsRadioNodeListOrElement HTMLPreElement
instance IsRadioNodeListOrElement HTMLPictureElement
instance IsRadioNodeListOrElement HTMLParamElement
instance IsRadioNodeListOrElement HTMLParagraphElement
instance IsRadioNodeListOrElement HTMLOutputElement
instance IsRadioNodeListOrElement HTMLOptionElement
instance IsRadioNodeListOrElement HTMLOptGroupElement
instance IsRadioNodeListOrElement HTMLObjectElement
instance IsRadioNodeListOrElement HTMLOListElement
instance IsRadioNodeListOrElement HTMLModElement
instance IsRadioNodeListOrElement HTMLMeterElement
instance IsRadioNodeListOrElement HTMLMetaElement
instance IsRadioNodeListOrElement HTMLMenuElement
instance IsRadioNodeListOrElement HTMLMediaElement
instance IsRadioNodeListOrElement HTMLMarqueeElement
instance IsRadioNodeListOrElement HTMLMapElement
instance IsRadioNodeListOrElement HTMLLinkElement
instance IsRadioNodeListOrElement HTMLLegendElement
instance IsRadioNodeListOrElement HTMLLabelElement
instance IsRadioNodeListOrElement HTMLLIElement
instance IsRadioNodeListOrElement HTMLKeygenElement
instance IsRadioNodeListOrElement HTMLInputElement
instance IsRadioNodeListOrElement HTMLImageElement
instance IsRadioNodeListOrElement HTMLIFrameElement
instance IsRadioNodeListOrElement HTMLHtmlElement
instance IsRadioNodeListOrElement HTMLHeadingElement
instance IsRadioNodeListOrElement HTMLHeadElement
instance IsRadioNodeListOrElement HTMLHRElement
instance IsRadioNodeListOrElement HTMLFrameSetElement
instance IsRadioNodeListOrElement HTMLFrameElement
instance IsRadioNodeListOrElement HTMLFormElement
instance IsRadioNodeListOrElement HTMLFontElement
instance IsRadioNodeListOrElement HTMLFieldSetElement
instance IsRadioNodeListOrElement HTMLEmbedElement
instance IsRadioNodeListOrElement HTMLElement
instance IsRadioNodeListOrElement HTMLDivElement
instance IsRadioNodeListOrElement HTMLDirectoryElement
instance IsRadioNodeListOrElement HTMLDetailsElement
instance IsRadioNodeListOrElement HTMLDataListElement
instance IsRadioNodeListOrElement HTMLDataElement
instance IsRadioNodeListOrElement HTMLDListElement
instance IsRadioNodeListOrElement HTMLCanvasElement
instance IsRadioNodeListOrElement HTMLButtonElement
instance IsRadioNodeListOrElement HTMLBodyElement
instance IsRadioNodeListOrElement HTMLBaseElement
instance IsRadioNodeListOrElement HTMLBRElement
instance IsRadioNodeListOrElement HTMLAudioElement
instance IsRadioNodeListOrElement HTMLAttachmentElement
instance IsRadioNodeListOrElement HTMLAreaElement
instance IsRadioNodeListOrElement HTMLAppletElement
instance IsRadioNodeListOrElement HTMLAnchorElement
instance IsRadioNodeListOrElement RadioNodeList

newtype RenderingContext = RenderingContext { unRenderingContext :: JSVal }

instance PToJSVal RenderingContext where
  pToJSVal = unRenderingContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal RenderingContext where
  pFromJSVal = RenderingContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal RenderingContext where
  toJSVal = return . unRenderingContext
  {-# INLINE toJSVal #-}

instance FromJSVal RenderingContext where
  fromJSVal v = fmap RenderingContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RenderingContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RenderingContext where
  makeObject = makeObject . unRenderingContext

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsRenderingContext o

toRenderingContext :: IsRenderingContext o => o -> RenderingContext
toRenderingContext = RenderingContext . coerce

instance IsRenderingContext RenderingContext
instance IsRenderingContext WebGLRenderingContextBase
instance IsRenderingContext WebGLRenderingContext
instance IsRenderingContext WebGL2RenderingContext
instance IsRenderingContext CanvasRenderingContext2D

newtype SQLValue = SQLValue { unSQLValue :: JSVal }

instance PToJSVal SQLValue where
  pToJSVal = unSQLValue
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLValue where
  pFromJSVal = SQLValue
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLValue where
  toJSVal = return . unSQLValue
  {-# INLINE toJSVal #-}

instance FromJSVal SQLValue where
  fromJSVal v = fmap SQLValue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLValue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLValue where
  makeObject = makeObject . unSQLValue

class (FromJSVal o, ToJSVal o) => IsSQLValue o

instance IsSQLValue SQLValue
instance IsSQLValue (Maybe Text)
instance IsSQLValue (Maybe JSString)
instance IsSQLValue (Maybe String)
instance IsSQLValue Double

newtype StringOrArrayBuffer = StringOrArrayBuffer { unStringOrArrayBuffer :: JSVal }

instance PToJSVal StringOrArrayBuffer where
  pToJSVal = unStringOrArrayBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal StringOrArrayBuffer where
  pFromJSVal = StringOrArrayBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal StringOrArrayBuffer where
  toJSVal = return . unStringOrArrayBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal StringOrArrayBuffer where
  fromJSVal v = fmap StringOrArrayBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StringOrArrayBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StringOrArrayBuffer where
  makeObject = makeObject . unStringOrArrayBuffer

class (FromJSVal o, ToJSVal o) => IsStringOrArrayBuffer o

instance IsStringOrArrayBuffer StringOrArrayBuffer
instance IsStringOrArrayBuffer ArrayBuffer
instance IsStringOrArrayBuffer Text
instance IsStringOrArrayBuffer JSString
instance IsStringOrArrayBuffer String

newtype StringOrBinaryData = StringOrBinaryData { unStringOrBinaryData :: JSVal }

instance PToJSVal StringOrBinaryData where
  pToJSVal = unStringOrBinaryData
  {-# INLINE pToJSVal #-}

instance PFromJSVal StringOrBinaryData where
  pFromJSVal = StringOrBinaryData
  {-# INLINE pFromJSVal #-}

instance ToJSVal StringOrBinaryData where
  toJSVal = return . unStringOrBinaryData
  {-# INLINE toJSVal #-}

instance FromJSVal StringOrBinaryData where
  fromJSVal v = fmap StringOrBinaryData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StringOrBinaryData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StringOrBinaryData where
  makeObject = makeObject . unStringOrBinaryData

class (FromJSVal o, ToJSVal o) => IsStringOrBinaryData o

instance IsStringOrBinaryData StringOrBinaryData
instance IsStringOrBinaryData BinaryData
instance IsStringOrBinaryData BufferSource
instance IsStringOrBinaryData ArrayBufferView
instance IsStringOrBinaryData ArrayBuffer
instance IsStringOrBinaryData Text
instance IsStringOrBinaryData JSString
instance IsStringOrBinaryData String

newtype StringOrStrings = StringOrStrings { unStringOrStrings :: JSVal }

instance PToJSVal StringOrStrings where
  pToJSVal = unStringOrStrings
  {-# INLINE pToJSVal #-}

instance PFromJSVal StringOrStrings where
  pFromJSVal = StringOrStrings
  {-# INLINE pFromJSVal #-}

instance ToJSVal StringOrStrings where
  toJSVal = return . unStringOrStrings
  {-# INLINE toJSVal #-}

instance FromJSVal StringOrStrings where
  fromJSVal v = fmap StringOrStrings <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StringOrStrings
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StringOrStrings where
  makeObject = makeObject . unStringOrStrings

class (FromJSVal o, ToJSVal o) => IsStringOrStrings o

instance IsStringOrStrings StringOrStrings
instance IsStringOrStrings [Text]
instance IsStringOrStrings [JSString]
instance IsStringOrStrings [String]
instance IsStringOrStrings Text
instance IsStringOrStrings JSString
instance IsStringOrStrings String

newtype TexImageSource = TexImageSource { unTexImageSource :: JSVal }

instance PToJSVal TexImageSource where
  pToJSVal = unTexImageSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal TexImageSource where
  pFromJSVal = TexImageSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal TexImageSource where
  toJSVal = return . unTexImageSource
  {-# INLINE toJSVal #-}

instance FromJSVal TexImageSource where
  fromJSVal v = fmap TexImageSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TexImageSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TexImageSource where
  makeObject = makeObject . unTexImageSource

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsTexImageSource o

toTexImageSource :: IsTexImageSource o => o -> TexImageSource
toTexImageSource = TexImageSource . coerce

instance IsTexImageSource TexImageSource
instance IsTexImageSource ImageData
instance IsTexImageSource HTMLImageElement
instance IsTexImageSource HTMLVideoElement
instance IsTexImageSource HTMLCanvasElement

newtype Track = Track { unTrack :: JSVal }

instance PToJSVal Track where
  pToJSVal = unTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal Track where
  pFromJSVal = Track
  {-# INLINE pFromJSVal #-}

instance ToJSVal Track where
  toJSVal = return . unTrack
  {-# INLINE toJSVal #-}

instance FromJSVal Track where
  fromJSVal v = fmap Track <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Track
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Track where
  makeObject = makeObject . unTrack

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsTrack o

toTrack :: IsTrack o => o -> Track
toTrack = Track . coerce

instance IsTrack Track
instance IsTrack TextTrack
instance IsTrack AudioTrack
instance IsTrack VideoTrack

newtype URLSearchParamsInit = URLSearchParamsInit { unURLSearchParamsInit :: JSVal }

instance PToJSVal URLSearchParamsInit where
  pToJSVal = unURLSearchParamsInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal URLSearchParamsInit where
  pFromJSVal = URLSearchParamsInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal URLSearchParamsInit where
  toJSVal = return . unURLSearchParamsInit
  {-# INLINE toJSVal #-}

instance FromJSVal URLSearchParamsInit where
  fromJSVal v = fmap URLSearchParamsInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . URLSearchParamsInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject URLSearchParamsInit where
  makeObject = makeObject . unURLSearchParamsInit

class (FromJSVal o, ToJSVal o) => IsURLSearchParamsInit o

instance IsURLSearchParamsInit URLSearchParamsInit
instance IsURLSearchParamsInit Text
instance IsURLSearchParamsInit JSString
instance IsURLSearchParamsInit String
instance IsURLSearchParamsInit [[Text]]
instance IsURLSearchParamsInit [[JSString]]
instance IsURLSearchParamsInit [[String]]

newtype XMLHttpRequestBody = XMLHttpRequestBody { unXMLHttpRequestBody :: JSVal }

instance PToJSVal XMLHttpRequestBody where
  pToJSVal = unXMLHttpRequestBody
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequestBody where
  pFromJSVal = XMLHttpRequestBody
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLHttpRequestBody where
  toJSVal = return . unXMLHttpRequestBody
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequestBody where
  fromJSVal v = fmap XMLHttpRequestBody <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLHttpRequestBody
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLHttpRequestBody where
  makeObject = makeObject . unXMLHttpRequestBody

class (FromJSVal o, ToJSVal o) => IsXMLHttpRequestBody o

instance IsXMLHttpRequestBody XMLHttpRequestBody
instance IsXMLHttpRequestBody BodyInit
instance IsXMLHttpRequestBody Blob
instance IsXMLHttpRequestBody BinaryData
instance IsXMLHttpRequestBody BufferSource
instance IsXMLHttpRequestBody ArrayBufferView
instance IsXMLHttpRequestBody ArrayBuffer
instance IsXMLHttpRequestBody FormData
instance IsXMLHttpRequestBody Text
instance IsXMLHttpRequestBody JSString
instance IsXMLHttpRequestBody String
instance IsXMLHttpRequestBody Document
instance IsXMLHttpRequestBody XMLDocument
instance IsXMLHttpRequestBody HTMLDocument

