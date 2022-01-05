{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
-- For HasCallStack compatibility
{-# LANGUAGE ImplicitParams, KindSignatures #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module JSDOM.Types.Manual (
  -- * JavaScript Context and Monad
    JSContextRef(..), JSM, askJSM, runJSM, MonadJSM(..), liftJSM

  -- * DOM Context and Monad
  , DOMContext(..), DOM, askDOM, runDOM, MonadDOM(..), liftDOM

  -- * JavaScript Value
  , JSVal(..), ToJSVal(..), FromJSVal(..), PToJSVal(..), PFromJSVal(..)
  , integralToDoubleToJSVal, integralFromDoubleFromJSVal, integralFromDoubleFromJSValUnchecked

  -- * JavaScript String
  , JSString(..), ToJSString(..), FromJSString(..)
  , toMaybeJSString, fromMaybeJSString
  , noJSString

  -- * JavaScript Array
  , fromJSArray, fromJSArrayUnchecked

  -- * JavaScript Object
  , Object(..)

  -- * Nullable
  , Nullable(..), nullableToMaybe, maybeToNullable

  -- * DOM String
  , DOMString(..), ToDOMString(..), FromDOMString(..), IsDOMString, noDOMString
  , USVString(..), IsUSVString, noUSVString
  , ByteString(..), IsByteString, noByteString
  , CSSOMString(..), IsCSSOMString, noCSSOMString

  -- * Object
  , maybeNullOrUndefined, maybeNullOrUndefined', GType(..)
  , GObject(..), noGObject, IsGObject, toGObject, gTypeGObject, isA, objectToString
  , castTo, unsafeCastTo, uncheckedCastTo
  , strictEqual

  -- * TypedArray
  , RawTypedArray(RawTypedArray), unRawTypedArray, IsRawTypedArray, toRawTypedArray, noRawTypedArray

  , Function(Function), unFunction, IsFunction, toFunction, noFunction

  -- * Promise
  , PromiseRejected(..), noPromiseRejected, readPromise

  -- * Callbacks
  , Callback(..)
  , withCallback
  , AudioBufferCallback(..), noAudioBufferCallback
  , BlobCallback(..), noBlobCallback
  , DatabaseCallback(..), noDatabaseCallback
  , IntersectionObserverCallback(..), noIntersectionObserverCallback
  , MediaQueryListListener(..), noMediaQueryListListener
  , MediaStreamTrackSourcesCallback(..), noMediaStreamTrackSourcesCallback
  , NavigatorUserMediaErrorCallback(..), noNavigatorUserMediaErrorCallback
  , NavigatorUserMediaSuccessCallback(..), noNavigatorUserMediaSuccessCallback
  , NotificationPermissionCallback(..)
  , NodeFilter(..), noNodeFilter
  , PositionCallback(..), noPositionCallback
  , PositionErrorCallback(..), noPositionErrorCallback
  , PerformanceObserverCallback(..), noPerformanceObserverCallback
  , RequestAnimationFrameCallback(..), noRequestAnimationFrameCallback
  , RTCPeerConnectionErrorCallback(..), noRTCPeerConnectionErrorCallback
  , RTCSessionDescriptionCallback(..), noRTCSessionDescriptionCallback
  , RTCStatsCallback(..), noRTCStatsCallback
  , SQLStatementCallback(..), noSQLStatementCallback
  , SQLStatementErrorCallback(..), noSQLStatementErrorCallback
  , SQLTransactionCallback(..), noSQLTransactionCallback
  , SQLTransactionErrorCallback(..), noSQLTransactionErrorCallback
  , StorageErrorCallback(..), noStorageErrorCallback
  , StorageQuotaCallback(..), noStorageQuotaCallback
  , StorageUsageCallback(..), noStorageUsageCallback
  , StringCallback(..)
  , VoidCallback(..), noVoidCallback

  -- * Custom Types
  , DOMHighResTimeStamp, noDOMHighResTimeStamp
  , PerformanceEntryList, noPerformanceEntryList

  -- * Record Type
  , Record(Record), unRecord

  -- * Dictionaries
  , Dictionary(Dictionary), unDictionary, IsDictionary, toDictionary, noDictionary

  -- * Mutation Callback
  , MutationCallback(MutationCallback), unMutationCallback, IsMutationCallback, toMutationCallback, noMutationCallback

  -- * Date
  , Date(Date), unDate, IsDate, toDate, gTypeDate, noDate

  -- * Arrays
  , Array(Array), unArray, IsArray, toArray, gTypeArray, noArray
  , ObjectArray(ObjectArray), unObjectArray, IsObjectArray, toObjectArray, noObjectArray
  , ArrayBuffer(ArrayBuffer), unArrayBuffer, IsArrayBuffer, toArrayBuffer, gTypeArrayBuffer, noArrayBuffer
  , ArrayBufferView(ArrayBufferView), unArrayBufferView, IsArrayBufferView, toArrayBufferView, noArrayBufferView
  , Float32Array(Float32Array), unFloat32Array, IsFloat32Array, toFloat32Array, gTypeFloat32Array, noFloat32Array
  , Float64Array(Float64Array), unFloat64Array, IsFloat64Array, toFloat64Array, gTypeFloat64Array, noFloat64Array
  , Uint8Array(Uint8Array), unUint8Array, IsUint8Array, toUint8Array, gTypeUint8Array, noUint8Array
  , Uint8ClampedArray(Uint8ClampedArray), unUint8ClampedArray, IsUint8ClampedArray, toUint8ClampedArray, gTypeUint8ClampedArray, noUint8ClampedArray
  , Uint16Array(Uint16Array), unUint16Array, IsUint16Array, toUint16Array, gTypeUint16Array, noUint16Array
  , Uint32Array(Uint32Array), unUint32Array, IsUint32Array, toUint32Array, gTypeUint32Array, noUint32Array
  , Int8Array(Int8Array), unInt8Array, IsInt8Array, toInt8Array, gTypeInt8Array, noInt8Array
  , Int16Array(Int16Array), unInt16Array, IsInt16Array, toInt16Array, gTypeInt16Array, noInt16Array
  , Int32Array(Int32Array), unInt32Array, IsInt32Array, toInt32Array, gTypeInt32Array, noInt32Array

  -- * Geolocation
  , SerializedScriptValue(SerializedScriptValue), unSerializedScriptValue, IsSerializedScriptValue, toSerializedScriptValue, noSerializedScriptValue

  -- * Crypto
  , Algorithm(Algorithm), unAlgorithm, IsAlgorithm, toAlgorithm, noAlgorithm
  , CryptoOperationData(CryptoOperationData), unCryptoOperationData, IsCryptoOperationData, toCryptoOperationData, noCryptoOperationData

  -- * WebGL typedefs
  , GLenum(..), GLboolean(..), GLbitfield(..), GLbyte(..), GLshort(..), GLint(..), GLsizei(..)
  , GLintptr(..), GLsizeiptr(..), GLubyte(..), GLushort(..), GLuint(..), GLfloat(..), GLclampf(..)
  , GLint64, GLuint64
  , noGLenum, noGLboolean, noGLbitfield, noGLbyte, noGLshort, noGLint, noGLsizei
  , noGLintptr, noGLsizeiptr, noGLubyte, noGLushort, noGLuint, noGLfloat, noGLclampf
  , noGLint64, noGLuint64

  -- * Used for better error messages
  , HasCallStack

  , GlobalThis(GlobalThis), unGlobalThis, noGlobalThis

  -- * Interface types from IDL files
  ) where

import Prelude ()
import Prelude.Compat hiding((!!))
import qualified Data.Text as T (unpack, Text)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHCJS.Marshal (ToJSVal(..), FromJSVal(..))
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import Language.Javascript.JSaddle
       (Object(..), valToBool, valNull, valToNumber, (!!), js, valToText,
        JSVal, JSString, JSM, maybeNullOrUndefined, maybeNullOrUndefined',
        valToStr, jsg, ToJSString(..), FromJSString(..), strToText, MakeObject(..),
        Nullable(..), freeFunction, instanceOf, JSContextRef,
        askJSM, runJSM, MonadJSM(..), liftJSM, strictEqual, function, js2)
import qualified Language.Javascript.JSaddle as JSaddle (Function(..))
import Foreign.Ptr (nullPtr)
import Control.Lens.Operators ((^.))
import Data.Maybe (catMaybes)
import Language.Javascript.JSaddle.Classes (ToJSVal(..))
import Control.Monad ((>=>))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Data.Coerce (coerce, Coercible)
import Data.Typeable (Typeable)
import Control.Monad.Trans.Reader (ReaderT(..), ask)
import Control.Exception (bracket, Exception(..), throwIO)
#if MIN_VERSION_base(4,9,0)
import GHC.Stack (HasCallStack)
#else
import GHC.Exts (Constraint)
type HasCallStack = (() :: Constraint)
#endif

-- | This is the same as 'JSM' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
type DOM = JSM
-- | This is the same as 'JSContextRef' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
type DOMContext = JSContextRef
-- | This is the same as 'MonadJSM' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
type MonadDOM = MonadJSM

-- | This is the same as 'liftJSM' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
liftDOM :: MonadDOM m => DOM a -> m a
liftDOM = liftJSM

-- | This is the same as 'askJSM' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
askDOM :: MonadDOM m => m DOMContext
askDOM = askJSM

-- | This is the same as 'runJSM' except when using ghcjs-dom-webkit with GHC (instead of ghcjs-dom-jsaddle)
runDOM :: MonadIO m => DOM a -> DOMContext -> m a
runDOM = runJSM

newtype GType = GType Object

typeInstanceIsA :: ToJSVal value => value -> GType -> JSM Bool
typeInstanceIsA o (GType t) = o `instanceOf` t

-- | Safe but slow way to cast
--
-- > castTo Element x >>= \case
-- >     Nothing      -> error "Was not an element"
-- >     Just element -> ...
castTo :: forall obj obj' m. (Coercible obj JSVal, IsGObject obj', MonadJSM m) => (JSVal -> obj') -> obj -> m (Maybe obj')
castTo constructor obj = liftJSM $ do
  GType gtype <- typeGType (undefined :: obj')
  let jsval = coerce obj
  jsval `instanceOf` gtype >>= \case
    True  -> return . Just $ constructor jsval
    False -> return Nothing

-- | Unsafe way to cast.  Slow but if it fails an error message will
--   result and the message should be clear (uses HasCallStack).
--
-- > element <- unsafeCastTo Element x
unsafeCastTo :: forall obj obj' m. (HasCallStack, Coercible obj JSVal, IsGObject obj', MonadJSM m) => (JSVal -> obj') -> obj -> m obj'
unsafeCastTo constructor obj = liftJSM $ do
  GType gtype <- typeGType (undefined :: obj')
  let jsval = coerce obj
  jsval `instanceOf` gtype >>= \case
    True  -> return $ constructor jsval
    False -> do
      destType <- valToText (gtype ^. js "name")
      error $ "unsafeCastTo :: invalid conversion to "
        <> T.unpack destType <> " requested."

-- | Unsafe way to cast.  Fast but if it fails you program
--   will probably crash later on in some unpredictable way.
--
-- > element <- uncheckedCastTo Element x
uncheckedCastTo :: (Coercible obj JSVal, IsGObject obj') => (JSVal -> obj') -> obj -> obj'
uncheckedCastTo constructor = constructor . coerce

-- | Determine if this is an instance of a particular type
--
isA :: IsGObject o => o -> GType -> JSM Bool
isA obj = typeInstanceIsA (unGObject $ toGObject obj)

newtype GObject = GObject { unGObject :: JSVal }
noGObject :: Maybe GObject
noGObject = Nothing
{-# INLINE noGObject #-}

class (ToJSVal o, FromJSVal o, Coercible o JSVal) => IsGObject o where
  -- | Given object get the GType of the type.  The actual argument
  --   passed in is ignored.
  typeGType :: o -> JSM GType

-- | Safe upcast.
toGObject :: IsGObject o => o -> GObject
toGObject = GObject . coerce

fromJSArray :: FromJSVal o => JSVal -> JSM [Maybe o]
fromJSArray a = do
    l <- a ^. js "length" >>= valToNumber
    mapM (\i -> a !! i >>= fromJSVal) [0..round l - 1]

fromJSArrayUnchecked :: FromJSVal o => JSVal -> JSM [o]
fromJSArrayUnchecked = fromJSValUncheckedListOf

-- newtype Nullable a = Nullable JSVal

nullableToMaybe :: FromJSVal a => JSVal -> JSM (Maybe a)
nullableToMaybe = fromJSVal
{-# INLINE nullableToMaybe #-}
--
maybeToNullable :: ToJSVal a => Maybe a -> JSM JSVal
maybeToNullable Nothing = return valNull
maybeToNullable (Just a) = toJSVal a
{-# INLINE maybeToNullable #-}

instance PToJSVal GObject where
  pToJSVal = unGObject
  {-# INLINE pToJSVal #-}

instance PFromJSVal GObject where
  pFromJSVal = GObject
  {-# INLINE pFromJSVal #-}

instance ToJSVal GObject where
  toJSVal = return . unGObject
  {-# INLINE toJSVal #-}

instance FromJSVal GObject where
  fromJSVal val = fmap GObject <$> maybeNullOrUndefined val
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GObject
  {-# INLINE fromJSValUnchecked #-}

--instance IsGObject o => PToJSVal o where
--  pToJSVal = unGObject . toGObject
--  {-# INLINE pToJSVal #-}
--
--instance IsGObject o => PFromJSVal o where
--  pFromJSVal = unsafeCastGObject . GObject . castRef
--  {-# INLINE pFromJSVal #-}
--
--instance IsGObject o => ToJSVal o where
--  toJSVal = return . unGObject . toGObject
--  {-# INLINE toJSVal #-}
--
--instance IsGObject o => FromJSVal o where
--  fromJSVal = return . fmap (unsafeCastGObject . GObject . castRef) . maybeJSNullOrUndefined
--  {-# INLINE fromJSVal #-}

instance IsGObject GObject where
  typeGType _ = gTypeGObject
  {-# INLINE typeGType #-}

#ifdef ghcjs_HOST_OS
foreign import javascript unsafe "Object" gTypeGObject :: IO GType
#else
gTypeGObject :: JSM GType
gTypeGObject = GType . Object <$> jsg "Object"
#endif

objectToString :: (IsGObject self, FromJSString result) => self -> JSM result
objectToString self = fromJSValUnchecked (unGObject $ toGObject self)

-- | Fastest string type to use when you just
--   want to take a string from the DOM then
--   give it back as is.
type DOMString = JSString
noDOMString :: Maybe DOMString
noDOMString = Nothing
{-# INLINE noDOMString #-}
type CSSOMString = JSString
noCSSOMString :: Maybe CSSOMString
noCSSOMString = Nothing
{-# INLINE noCSSOMString #-}
type USVString = JSString
noUSVString :: Maybe USVString
noUSVString = Nothing
{-# INLINE noUSVString #-}
type ByteString = JSString
noByteString :: Maybe ByteString
noByteString = Nothing
{-# INLINE noByteString #-}

fromJSStringArray :: FromJSString s => JSVal -> JSM [s]
fromJSStringArray a = do
    l <- a ^. js "length" >>= valToNumber
    mapM (\i -> fromJSString <$> (a !! i >>= valToStr)) [0..round l - 1]

toMaybeJSString :: ToJSString a => Maybe a -> JSM JSVal
toMaybeJSString Nothing = return valNull
toMaybeJSString (Just a) = toJSVal (toJSString a)
{-# INLINE toMaybeJSString #-}

fromMaybeJSString :: FromJSString a => JSVal -> JSM (Maybe a)
fromMaybeJSString = maybeNullOrUndefined' (fmap fromJSString . valToStr)
{-# INLINE fromMaybeJSString #-}

integralToDoubleToJSVal :: Integral a => a -> JSM JSVal
integralToDoubleToJSVal a = toJSVal (fromIntegral a :: Double)

integralFromDoubleFromJSVal :: Integral a => JSVal -> JSM (Maybe a)
integralFromDoubleFromJSVal = fmap (fmap round) . (fromJSVal :: JSVal -> JSM (Maybe Double))

integralFromDoubleFromJSValUnchecked :: Integral a => JSVal -> JSM a
integralFromDoubleFromJSValUnchecked = fmap round . (fromJSValUnchecked :: JSVal -> JSM Double)

noJSString :: Maybe JSString
noJSString = Nothing
{-# INLINE noJSString #-}

type ToDOMString s = ToJSString s
type FromDOMString s = FromJSString s
type IsDOMString s = (ToDOMString s, FromDOMString s)
type IsCSSOMString s = (ToDOMString s, FromDOMString s)
type IsUSVString s = (ToDOMString s, FromDOMString s)
type IsByteString s = (ToDOMString s, FromDOMString s)

newtype RawTypedArray = RawTypedArray { unRawTypedArray :: JSVal }
noRawTypedArray :: Maybe RawTypedArray
noRawTypedArray = Nothing
{-# INLINE noRawTypedArray #-}

instance PToJSVal RawTypedArray where
  pToJSVal = unRawTypedArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal RawTypedArray where
  pFromJSVal = RawTypedArray
  {-# INLINE pFromJSVal #-}

instance ToJSVal RawTypedArray where
  toJSVal = return . unRawTypedArray
  {-# INLINE toJSVal #-}

instance FromJSVal RawTypedArray where
  fromJSVal v = fmap RawTypedArray <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RawTypedArray
  {-# INLINE fromJSValUnchecked #-}

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsRawTypedArray o

toRawTypedArray :: IsRawTypedArray o => o -> RawTypedArray
toRawTypedArray = RawTypedArray . coerce

newtype Function = Function { unFunction :: JSVal }
noFunction :: Maybe Function
noFunction = Nothing
{-# INLINE noFunction #-}

instance PToJSVal Function where
  pToJSVal = unFunction
  {-# INLINE pToJSVal #-}

instance PFromJSVal Function where
  pFromJSVal = Function
  {-# INLINE pFromJSVal #-}

instance ToJSVal Function where
  toJSVal = return . unFunction
  {-# INLINE toJSVal #-}

instance FromJSVal Function where
  fromJSVal v = fmap Function <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Function
  {-# INLINE fromJSValUnchecked #-}

class (FromJSVal o, ToJSVal o, PFromJSVal o, PToJSVal o, Coercible o JSVal) => IsFunction o

toFunction :: IsFunction o => o -> Function
toFunction = Function . coerce

instance IsFunction Function

-- Promise
newtype PromiseRejected = PromiseRejected { rejectionReason :: String } deriving (Typeable)
noPromiseRejected :: Maybe PromiseRejected
noPromiseRejected = Nothing
{-# INLINE noPromiseRejected #-}

instance Show PromiseRejected where
    show (PromiseRejected reason) = "A promise was rejected: " ++ reason
instance Exception PromiseRejected

readPromise :: JSVal -> JSM JSVal
readPromise promise = do
    resultMVar <- liftIO newEmptyMVar
    success <- function (\_ _ [result] -> liftIO . putMVar resultMVar $ Right result)
    error <- function (\_ _ [reason] -> liftIO . putMVar resultMVar $ Left reason)
    promise ^. js2 "then" success error
    result <- liftIO $ takeMVar resultMVar
    freeFunction success
    freeFunction error
    case result of
        Left reason -> do
          reason' <- fromJSVal reason
          let reason'' = case reason' of
                Just t -> T.unpack t
                Nothing -> "Unknown reason"
          liftIO . throwIO $ PromiseRejected reason''
        Right x -> return x

-- Callbacks
newtype Callback a = Callback JSaddle.Function

withCallback :: (MonadDOM m, Coercible c JSaddle.Function)
             => JSM c -> (c -> JSM a) -> m a
withCallback aquire f = do
    jsCtx <- askJSM
    liftIO $ bracket
        (runJSM aquire jsCtx)
        ((`runJSM` jsCtx) . freeFunction . coerce)
        (\t -> runJSM (f t) jsCtx)

newtype AudioBufferCallback = AudioBufferCallback (Callback (JSVal -> IO ()))
noAudioBufferCallback :: Maybe AudioBufferCallback
noAudioBufferCallback = Nothing
{-# INLINE noAudioBufferCallback #-}
instance ToJSVal AudioBufferCallback where toJSVal (AudioBufferCallback (Callback r)) = toJSVal r
newtype BlobCallback = BlobCallback (Callback (JSVal -> IO ()))
noBlobCallback :: Maybe BlobCallback
noBlobCallback = Nothing
{-# INLINE noBlobCallback #-}
instance ToJSVal BlobCallback where toJSVal (BlobCallback (Callback r)) = toJSVal r
newtype DatabaseCallback = DatabaseCallback (Callback (JSVal -> IO ()))
noDatabaseCallback :: Maybe DatabaseCallback
noDatabaseCallback = Nothing
{-# INLINE noDatabaseCallback #-}
instance ToJSVal DatabaseCallback where toJSVal (DatabaseCallback (Callback r)) = toJSVal r
newtype IntersectionObserverCallback = IntersectionObserverCallback (Callback (JSVal -> JSVal -> IO ()))
noIntersectionObserverCallback :: Maybe IntersectionObserverCallback
noIntersectionObserverCallback = Nothing
{-# INLINE noIntersectionObserverCallback #-}
instance ToJSVal IntersectionObserverCallback where toJSVal (IntersectionObserverCallback (Callback r)) = toJSVal r
newtype MediaQueryListListener = MediaQueryListListener (Callback (JSVal -> IO ()))
noMediaQueryListListener :: Maybe MediaQueryListListener
noMediaQueryListListener = Nothing
{-# INLINE noMediaQueryListListener #-}
instance ToJSVal MediaQueryListListener where toJSVal (MediaQueryListListener (Callback r)) = toJSVal r
newtype MediaStreamTrackSourcesCallback = MediaStreamTrackSourcesCallback (Callback (JSVal -> IO ()))
noMediaStreamTrackSourcesCallback :: Maybe MediaStreamTrackSourcesCallback
noMediaStreamTrackSourcesCallback = Nothing
{-# INLINE noMediaStreamTrackSourcesCallback #-}
instance ToJSVal MediaStreamTrackSourcesCallback where toJSVal (MediaStreamTrackSourcesCallback (Callback r)) = toJSVal r
newtype NavigatorUserMediaErrorCallback = NavigatorUserMediaErrorCallback (Callback (JSVal -> IO ()))
noNavigatorUserMediaErrorCallback :: Maybe NavigatorUserMediaErrorCallback
noNavigatorUserMediaErrorCallback = Nothing
{-# INLINE noNavigatorUserMediaErrorCallback #-}
instance ToJSVal NavigatorUserMediaErrorCallback where toJSVal (NavigatorUserMediaErrorCallback (Callback r)) = toJSVal r
newtype NavigatorUserMediaSuccessCallback = NavigatorUserMediaSuccessCallback (Callback (JSVal -> IO ()))
noNavigatorUserMediaSuccessCallback :: Maybe NavigatorUserMediaSuccessCallback
noNavigatorUserMediaSuccessCallback = Nothing
{-# INLINE noNavigatorUserMediaSuccessCallback #-}
instance ToJSVal NavigatorUserMediaSuccessCallback where toJSVal (NavigatorUserMediaSuccessCallback (Callback r)) = toJSVal r
newtype NotificationPermissionCallback permissions = NotificationPermissionCallback (Callback (JSVal -> IO ()))
instance ToJSVal (NotificationPermissionCallback permissions) where toJSVal (NotificationPermissionCallback (Callback r)) = toJSVal r
newtype NodeFilter = NodeFilter (Callback (JSVal -> IO ()))
noNodeFilter :: Maybe NodeFilter
noNodeFilter = Nothing
{-# INLINE noNodeFilter #-}
instance ToJSVal NodeFilter where toJSVal (NodeFilter (Callback r)) = toJSVal r
newtype PositionCallback = PositionCallback (Callback (JSVal -> IO ()))
noPositionCallback :: Maybe PositionCallback
noPositionCallback = Nothing
{-# INLINE noPositionCallback #-}
instance ToJSVal PositionCallback where toJSVal (PositionCallback (Callback r)) = toJSVal r
newtype PositionErrorCallback = PositionErrorCallback (Callback (JSVal -> IO ()))
noPositionErrorCallback :: Maybe PositionErrorCallback
noPositionErrorCallback = Nothing
{-# INLINE noPositionErrorCallback #-}
instance ToJSVal PositionErrorCallback where toJSVal (PositionErrorCallback (Callback r)) = toJSVal r
newtype PerformanceObserverCallback = PerformanceObserverCallback (Callback (JSVal -> JSVal -> IO ()))
noPerformanceObserverCallback :: Maybe PerformanceObserverCallback
noPerformanceObserverCallback = Nothing
{-# INLINE noPerformanceObserverCallback #-}
instance ToJSVal PerformanceObserverCallback where toJSVal (PerformanceObserverCallback (Callback r)) = toJSVal r
newtype RequestAnimationFrameCallback = RequestAnimationFrameCallback (Callback (JSVal -> IO ()))
noRequestAnimationFrameCallback :: Maybe RequestAnimationFrameCallback
noRequestAnimationFrameCallback = Nothing
{-# INLINE noRequestAnimationFrameCallback #-}
instance ToJSVal RequestAnimationFrameCallback where toJSVal (RequestAnimationFrameCallback (Callback r)) = toJSVal r
newtype RTCPeerConnectionErrorCallback = RTCPeerConnectionErrorCallback (Callback (JSVal -> IO ()))
noRTCPeerConnectionErrorCallback :: Maybe RTCPeerConnectionErrorCallback
noRTCPeerConnectionErrorCallback = Nothing
{-# INLINE noRTCPeerConnectionErrorCallback #-}
instance ToJSVal RTCPeerConnectionErrorCallback where toJSVal (RTCPeerConnectionErrorCallback (Callback r)) = toJSVal r
newtype RTCSessionDescriptionCallback = RTCSessionDescriptionCallback (Callback (JSVal -> IO ()))
noRTCSessionDescriptionCallback :: Maybe RTCSessionDescriptionCallback
noRTCSessionDescriptionCallback = Nothing
{-# INLINE noRTCSessionDescriptionCallback #-}
instance ToJSVal RTCSessionDescriptionCallback where toJSVal (RTCSessionDescriptionCallback (Callback r)) = toJSVal r
newtype RTCStatsCallback = RTCStatsCallback (Callback (JSVal -> IO ()))
noRTCStatsCallback :: Maybe RTCStatsCallback
noRTCStatsCallback = Nothing
{-# INLINE noRTCStatsCallback #-}
instance ToJSVal RTCStatsCallback where toJSVal (RTCStatsCallback (Callback r)) = toJSVal r
newtype SQLStatementCallback = SQLStatementCallback (Callback (JSVal -> JSVal -> IO ()))
noSQLStatementCallback :: Maybe SQLStatementCallback
noSQLStatementCallback = Nothing
{-# INLINE noSQLStatementCallback #-}
instance ToJSVal SQLStatementCallback where toJSVal (SQLStatementCallback (Callback r)) = toJSVal r
newtype SQLStatementErrorCallback = SQLStatementErrorCallback (Callback (JSVal -> JSVal -> IO ()))
noSQLStatementErrorCallback :: Maybe SQLStatementErrorCallback
noSQLStatementErrorCallback = Nothing
{-# INLINE noSQLStatementErrorCallback #-}
instance ToJSVal SQLStatementErrorCallback where toJSVal (SQLStatementErrorCallback (Callback r)) = toJSVal r
newtype SQLTransactionCallback = SQLTransactionCallback (Callback (JSVal -> IO ()))
noSQLTransactionCallback :: Maybe SQLTransactionCallback
noSQLTransactionCallback = Nothing
{-# INLINE noSQLTransactionCallback #-}
instance ToJSVal SQLTransactionCallback where toJSVal (SQLTransactionCallback (Callback r)) = toJSVal r
newtype SQLTransactionErrorCallback = SQLTransactionErrorCallback (Callback (JSVal -> IO ()))
noSQLTransactionErrorCallback :: Maybe SQLTransactionErrorCallback
noSQLTransactionErrorCallback = Nothing
{-# INLINE noSQLTransactionErrorCallback #-}
instance ToJSVal SQLTransactionErrorCallback where toJSVal (SQLTransactionErrorCallback (Callback r)) = toJSVal r
newtype StorageErrorCallback = StorageErrorCallback (Callback (JSVal -> IO ()))
noStorageErrorCallback :: Maybe StorageErrorCallback
noStorageErrorCallback = Nothing
{-# INLINE noStorageErrorCallback #-}
instance ToJSVal StorageErrorCallback where toJSVal (StorageErrorCallback (Callback r)) = toJSVal r
newtype StorageQuotaCallback = StorageQuotaCallback (Callback (JSVal -> IO ()))
noStorageQuotaCallback :: Maybe StorageQuotaCallback
noStorageQuotaCallback = Nothing
{-# INLINE noStorageQuotaCallback #-}
instance ToJSVal StorageQuotaCallback where toJSVal (StorageQuotaCallback (Callback r)) = toJSVal r
newtype StorageUsageCallback = StorageUsageCallback (Callback (JSVal -> JSVal -> IO ()))
noStorageUsageCallback :: Maybe StorageUsageCallback
noStorageUsageCallback = Nothing
{-# INLINE noStorageUsageCallback #-}
instance ToJSVal StorageUsageCallback where toJSVal (StorageUsageCallback (Callback r)) = toJSVal r
newtype StringCallback s = StringCallback (Callback (JSVal -> IO ()))
instance ToJSVal (StringCallback s) where toJSVal (StringCallback (Callback r)) = toJSVal r
newtype VoidCallback = VoidCallback (Callback (IO ()))
noVoidCallback :: Maybe VoidCallback
noVoidCallback = Nothing
{-# INLINE noVoidCallback #-}
instance ToJSVal VoidCallback where toJSVal (VoidCallback (Callback r)) = toJSVal r

-- Custom types
type DOMHighResTimeStamp = Double
noDOMHighResTimeStamp :: Maybe DOMHighResTimeStamp
noDOMHighResTimeStamp = Nothing
{-# INLINE noDOMHighResTimeStamp #-}
type PerformanceEntryList = [PerformanceEntry]
noPerformanceEntryList :: Maybe PerformanceEntryList
noPerformanceEntryList = Nothing
{-# INLINE noPerformanceEntryList #-}

-- Record Type
newtype Record key value = Record { unRecord :: JSVal }

instance PToJSVal (Record key value) where
  pToJSVal = unRecord
  {-# INLINE pToJSVal #-}

instance PFromJSVal (Record key value) where
  pFromJSVal = Record
  {-# INLINE pFromJSVal #-}

instance ToJSVal (Record key value) where
  toJSVal = return . unRecord
  {-# INLINE toJSVal #-}

instance FromJSVal (Record key value) where
  fromJSVal v = fmap Record <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Record
  {-# INLINE fromJSValUnchecked #-}

newtype SerializedScriptValue = SerializedScriptValue { unSerializedScriptValue :: JSVal }
noSerializedScriptValue :: Maybe SerializedScriptValue
noSerializedScriptValue = Nothing
{-# INLINE noSerializedScriptValue #-}

instance PToJSVal SerializedScriptValue where
  pToJSVal = unSerializedScriptValue
  {-# INLINE pToJSVal #-}

instance PFromJSVal SerializedScriptValue where
  pFromJSVal = SerializedScriptValue
  {-# INLINE pFromJSVal #-}

instance ToJSVal SerializedScriptValue where
  toJSVal = return . unSerializedScriptValue
  {-# INLINE toJSVal #-}

instance FromJSVal SerializedScriptValue where
  fromJSVal v = fmap SerializedScriptValue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SerializedScriptValue
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsSerializedScriptValue o
toSerializedScriptValue :: IsSerializedScriptValue o => o -> SerializedScriptValue
toSerializedScriptValue = SerializedScriptValue . coerce

instance IsSerializedScriptValue SerializedScriptValue
instance IsGObject SerializedScriptValue where
  typeGType _ = error "Unable to get the JavaScript type of SerializedScriptValue"

newtype Dictionary = Dictionary { unDictionary :: JSVal }
noDictionary :: Maybe Dictionary
noDictionary = Nothing
{-# INLINE noDictionary #-}

instance PToJSVal Dictionary where
  pToJSVal = unDictionary
  {-# INLINE pToJSVal #-}

instance PFromJSVal Dictionary where
  pFromJSVal = Dictionary
  {-# INLINE pFromJSVal #-}

instance ToJSVal Dictionary where
  toJSVal = return . unDictionary
  {-# INLINE toJSVal #-}

instance FromJSVal Dictionary where
  fromJSVal v = fmap Dictionary <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Dictionary
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsDictionary o
toDictionary :: IsDictionary o => o -> Dictionary
toDictionary = Dictionary . coerce

instance IsDictionary Dictionary
instance IsGObject Dictionary where
  typeGType _ = error "Unable to get the JavaScript type of Dictionary"

newtype MutationCallback = MutationCallback { unMutationCallback :: JSVal }
noMutationCallback :: Maybe MutationCallback
noMutationCallback = Nothing
{-# INLINE noMutationCallback #-}

instance PToJSVal MutationCallback where
  pToJSVal = unMutationCallback
  {-# INLINE pToJSVal #-}

instance PFromJSVal MutationCallback where
  pFromJSVal = MutationCallback
  {-# INLINE pFromJSVal #-}

instance ToJSVal MutationCallback where
  toJSVal = return . unMutationCallback
  {-# INLINE toJSVal #-}

instance FromJSVal MutationCallback where
  fromJSVal v = fmap MutationCallback <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MutationCallback
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsMutationCallback o
toMutationCallback :: IsMutationCallback o => o -> MutationCallback
toMutationCallback = MutationCallback . coerce

instance IsMutationCallback MutationCallback
instance IsGObject MutationCallback where
  typeGType _ = error "Unable to get the JavaScript type of MutationCallback"

newtype ArrayBuffer = ArrayBuffer { unArrayBuffer :: JSVal }
noArrayBuffer :: Maybe ArrayBuffer
noArrayBuffer = Nothing
{-# INLINE noArrayBuffer #-}

instance PToJSVal ArrayBuffer where
  pToJSVal = unArrayBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal ArrayBuffer where
  pFromJSVal = ArrayBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal ArrayBuffer where
  toJSVal = return . unArrayBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal ArrayBuffer where
  fromJSVal v = fmap ArrayBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ArrayBuffer
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsArrayBuffer o
toArrayBuffer :: IsArrayBuffer o => o -> ArrayBuffer
toArrayBuffer = ArrayBuffer . coerce

instance IsArrayBuffer ArrayBuffer
instance IsGObject ArrayBuffer where
    typeGType _ = gTypeArrayBuffer

gTypeArrayBuffer :: JSM GType
gTypeArrayBuffer = GType . Object <$> jsg "ArrayBuffer"

newtype Float32Array = Float32Array { unFloat32Array :: JSVal }
noFloat32Array :: Maybe Float32Array
noFloat32Array = Nothing
{-# INLINE noFloat32Array #-}

instance PToJSVal Float32Array where
  pToJSVal = unFloat32Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Float32Array where
  pFromJSVal = Float32Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Float32Array where
  toJSVal = return . unFloat32Array
  {-# INLINE toJSVal #-}

instance FromJSVal Float32Array where
  fromJSVal v = fmap Float32Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Float32Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsFloat32Array o
toFloat32Array :: IsFloat32Array o => o -> Float32Array
toFloat32Array = Float32Array . coerce

instance IsFloat32Array Float32Array
instance IsGObject Float32Array where
        typeGType _ = gTypeFloat32Array

gTypeFloat32Array :: JSM GType
gTypeFloat32Array = GType . Object <$> jsg "Float32Array"

newtype Float64Array = Float64Array { unFloat64Array :: JSVal }
noFloat64Array :: Maybe Float64Array
noFloat64Array = Nothing
{-# INLINE noFloat64Array #-}

instance PToJSVal Float64Array where
  pToJSVal = unFloat64Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Float64Array where
  pFromJSVal = Float64Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Float64Array where
  toJSVal = return . unFloat64Array
  {-# INLINE toJSVal #-}

instance FromJSVal Float64Array where
  fromJSVal v = fmap Float64Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Float64Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsFloat64Array o
toFloat64Array :: IsFloat64Array o => o -> Float64Array
toFloat64Array = Float64Array . coerce

instance IsFloat64Array Float64Array
instance IsGObject Float64Array where
    typeGType _ = gTypeFloat64Array

gTypeFloat64Array :: JSM GType
gTypeFloat64Array = GType . Object <$> jsg "Float64Array"

newtype Uint8Array = Uint8Array { unUint8Array :: JSVal }
noUint8Array :: Maybe Uint8Array
noUint8Array = Nothing
{-# INLINE noUint8Array #-}

instance PToJSVal Uint8Array where
  pToJSVal = unUint8Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Uint8Array where
  pFromJSVal = Uint8Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Uint8Array where
  toJSVal = return . unUint8Array
  {-# INLINE toJSVal #-}

instance FromJSVal Uint8Array where
  fromJSVal v = fmap Uint8Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Uint8Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsUint8Array o
toUint8Array :: IsUint8Array o => o -> Uint8Array
toUint8Array = Uint8Array . coerce

instance IsUint8Array Uint8Array
instance IsGObject Uint8Array where
    typeGType _ = gTypeUint8Array

gTypeUint8Array :: JSM GType
gTypeUint8Array = GType . Object <$> jsg "Uint8Array"

newtype Uint8ClampedArray = Uint8ClampedArray { unUint8ClampedArray :: JSVal }
noUint8ClampedArray :: Maybe Uint8ClampedArray
noUint8ClampedArray = Nothing
{-# INLINE noUint8ClampedArray #-}

instance PToJSVal Uint8ClampedArray where
  pToJSVal = unUint8ClampedArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal Uint8ClampedArray where
  pFromJSVal = Uint8ClampedArray
  {-# INLINE pFromJSVal #-}

instance ToJSVal Uint8ClampedArray where
  toJSVal = return . unUint8ClampedArray
  {-# INLINE toJSVal #-}

instance FromJSVal Uint8ClampedArray where
  fromJSVal v = fmap Uint8ClampedArray <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Uint8ClampedArray
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsUint8ClampedArray o
toUint8ClampedArray :: IsUint8ClampedArray o => o -> Uint8ClampedArray
toUint8ClampedArray = Uint8ClampedArray . coerce

instance IsUint8ClampedArray Uint8ClampedArray
instance IsGObject Uint8ClampedArray where
    typeGType _ = gTypeUint8ClampedArray

gTypeUint8ClampedArray :: JSM GType
gTypeUint8ClampedArray = GType . Object <$> jsg "Uint8ClampedArray"

newtype Uint16Array = Uint16Array { unUint16Array :: JSVal }
noUint16Array :: Maybe Uint16Array
noUint16Array = Nothing
{-# INLINE noUint16Array #-}

instance PToJSVal Uint16Array where
  pToJSVal = unUint16Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Uint16Array where
  pFromJSVal = Uint16Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Uint16Array where
  toJSVal = return . unUint16Array
  {-# INLINE toJSVal #-}

instance FromJSVal Uint16Array where
  fromJSVal v = fmap Uint16Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Uint16Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsUint16Array o
toUint16Array :: IsUint16Array o => o -> Uint16Array
toUint16Array = Uint16Array . coerce

instance IsUint16Array Uint16Array
instance IsGObject Uint16Array where
    typeGType _ = gTypeUint16Array

gTypeUint16Array :: JSM GType
gTypeUint16Array = GType . Object <$> jsg "Uint16Array"

newtype Uint32Array = Uint32Array { unUint32Array :: JSVal }
noUint32Array :: Maybe Uint32Array
noUint32Array = Nothing
{-# INLINE noUint32Array #-}

instance PToJSVal Uint32Array where
  pToJSVal = unUint32Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Uint32Array where
  pFromJSVal = Uint32Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Uint32Array where
  toJSVal = return . unUint32Array
  {-# INLINE toJSVal #-}

instance FromJSVal Uint32Array where
  fromJSVal v = fmap Uint32Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Uint32Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsUint32Array o
toUint32Array :: IsUint32Array o => o -> Uint32Array
toUint32Array = Uint32Array . coerce

instance IsUint32Array Uint32Array
instance IsGObject Uint32Array where
    typeGType _ = gTypeUint32Array

gTypeUint32Array :: JSM GType
gTypeUint32Array = GType . Object <$> jsg "Uint32Array"

newtype Int8Array = Int8Array { unInt8Array :: JSVal }
noInt8Array :: Maybe Int8Array
noInt8Array = Nothing
{-# INLINE noInt8Array #-}

instance PToJSVal Int8Array where
  pToJSVal = unInt8Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Int8Array where
  pFromJSVal = Int8Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Int8Array where
  toJSVal = return . unInt8Array
  {-# INLINE toJSVal #-}

instance FromJSVal Int8Array where
  fromJSVal v = fmap Int8Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Int8Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsInt8Array o
toInt8Array :: IsInt8Array o => o -> Int8Array
toInt8Array = Int8Array . coerce

instance IsInt8Array Int8Array
instance IsGObject Int8Array where
    typeGType _ = gTypeInt8Array

gTypeInt8Array :: JSM GType
gTypeInt8Array = GType . Object <$> jsg "Int8Array"

newtype Int16Array = Int16Array { unInt16Array :: JSVal }
noInt16Array :: Maybe Int16Array
noInt16Array = Nothing
{-# INLINE noInt16Array #-}

instance PToJSVal Int16Array where
  pToJSVal = unInt16Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Int16Array where
  pFromJSVal = Int16Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Int16Array where
  toJSVal = return . unInt16Array
  {-# INLINE toJSVal #-}

instance FromJSVal Int16Array where
  fromJSVal v = fmap Int16Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Int16Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsInt16Array o
toInt16Array :: IsInt16Array o => o -> Int16Array
toInt16Array = Int16Array . coerce

instance IsInt16Array Int16Array
instance IsGObject Int16Array where
    typeGType _ = gTypeInt16Array

gTypeInt16Array :: JSM GType
gTypeInt16Array = GType . Object <$> jsg "Int16Array"

newtype Int32Array = Int32Array { unInt32Array :: JSVal }
noInt32Array :: Maybe Int32Array
noInt32Array = Nothing
{-# INLINE noInt32Array #-}

instance PToJSVal Int32Array where
  pToJSVal = unInt32Array
  {-# INLINE pToJSVal #-}

instance PFromJSVal Int32Array where
  pFromJSVal = Int32Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Int32Array where
  toJSVal = return . unInt32Array
  {-# INLINE toJSVal #-}

instance FromJSVal Int32Array where
  fromJSVal v = fmap Int32Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Int32Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsInt32Array o
toInt32Array :: IsInt32Array o => o -> Int32Array
toInt32Array = Int32Array . coerce

instance IsInt32Array Int32Array
instance IsGObject Int32Array where
    typeGType _ = gTypeInt32Array

gTypeInt32Array :: JSM GType
gTypeInt32Array = GType . Object <$> jsg "Int32Array"

newtype ObjectArray = ObjectArray { unObjectArray :: JSVal }
noObjectArray :: Maybe ObjectArray
noObjectArray = Nothing
{-# INLINE noObjectArray #-}

instance PToJSVal ObjectArray where
  pToJSVal = unObjectArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal ObjectArray where
  pFromJSVal = ObjectArray
  {-# INLINE pFromJSVal #-}

instance ToJSVal ObjectArray where
  toJSVal = return . unObjectArray
  {-# INLINE toJSVal #-}

instance FromJSVal ObjectArray where
  fromJSVal v = fmap ObjectArray <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ObjectArray
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsObjectArray o
toObjectArray :: IsObjectArray o => o -> ObjectArray
toObjectArray = ObjectArray . coerce

instance IsObjectArray ObjectArray
instance IsGObject ObjectArray where
  typeGType _ = error "Unable to get the JavaScript type of ObjectArray"

newtype ArrayBufferView = ArrayBufferView { unArrayBufferView :: JSVal }
noArrayBufferView :: Maybe ArrayBufferView
noArrayBufferView = Nothing
{-# INLINE noArrayBufferView #-}

instance PToJSVal ArrayBufferView where
  pToJSVal = unArrayBufferView
  {-# INLINE pToJSVal #-}

instance PFromJSVal ArrayBufferView where
  pFromJSVal = ArrayBufferView
  {-# INLINE pFromJSVal #-}

instance ToJSVal ArrayBufferView where
  toJSVal = return . unArrayBufferView
  {-# INLINE toJSVal #-}

instance FromJSVal ArrayBufferView where
  fromJSVal v = fmap ArrayBufferView <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ArrayBufferView
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsArrayBufferView o
toArrayBufferView :: IsArrayBufferView o => o -> ArrayBufferView
toArrayBufferView = ArrayBufferView . coerce

instance IsArrayBufferView ArrayBufferView
instance IsGObject ArrayBufferView where
  typeGType _ = error "Unable to get the JavaScript type of ArrayBufferView"

newtype Array = Array { unArray :: JSVal }
noArray :: Maybe Array
noArray = Nothing
{-# INLINE noArray #-}

instance PToJSVal Array where
  pToJSVal = unArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal Array where
  pFromJSVal = Array
  {-# INLINE pFromJSVal #-}

instance ToJSVal Array where
  toJSVal = return . unArray
  {-# INLINE toJSVal #-}

instance FromJSVal Array where
  fromJSVal v = fmap Array <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Array
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsArray o
toArray :: IsArray o => o -> Array
toArray = Array . coerce

instance IsArray Array
instance IsGObject Array where
    typeGType _ = gTypeArray

gTypeArray :: JSM GType
gTypeArray = GType . Object <$> jsg "Array"

newtype Date = Date { unDate :: JSVal }
noDate :: Maybe Date
noDate = Nothing
{-# INLINE noDate #-}

instance PToJSVal Date where
  pToJSVal = unDate
  {-# INLINE pToJSVal #-}

instance PFromJSVal Date where
  pFromJSVal = Date
  {-# INLINE pFromJSVal #-}

instance ToJSVal Date where
  toJSVal = return . unDate
  {-# INLINE toJSVal #-}

instance FromJSVal Date where
  fromJSVal v = fmap Date <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Date
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsDate o
toDate :: IsDate o => o -> Date
toDate = Date . coerce

instance IsDate Date
instance IsGObject Date where
    typeGType _ = gTypeDate

gTypeDate :: JSM GType
gTypeDate = GType . Object <$> jsg "Date"

newtype Algorithm = Algorithm { unAlgorithm :: JSVal }
noAlgorithm :: Maybe Algorithm
noAlgorithm = Nothing
{-# INLINE noAlgorithm #-}

instance PToJSVal Algorithm where
  pToJSVal = unAlgorithm
  {-# INLINE pToJSVal #-}

instance PFromJSVal Algorithm where
  pFromJSVal = Algorithm
  {-# INLINE pFromJSVal #-}

instance ToJSVal Algorithm where
  toJSVal = return . unAlgorithm
  {-# INLINE toJSVal #-}

instance FromJSVal Algorithm where
  fromJSVal v = fmap Algorithm <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Algorithm
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsAlgorithm o
toAlgorithm :: IsAlgorithm o => o -> Algorithm
toAlgorithm = Algorithm . coerce

instance IsAlgorithm Algorithm
instance IsGObject Algorithm where
  typeGType _ = error "Unable to get the JavaScript type of Algorithm"

newtype CryptoOperationData = CryptoOperationData { unCryptoOperationData :: JSVal }
noCryptoOperationData :: Maybe CryptoOperationData
noCryptoOperationData = Nothing
{-# INLINE noCryptoOperationData #-}

instance PToJSVal CryptoOperationData where
  pToJSVal = unCryptoOperationData
  {-# INLINE pToJSVal #-}

instance PFromJSVal CryptoOperationData where
  pFromJSVal = CryptoOperationData
  {-# INLINE pFromJSVal #-}

instance ToJSVal CryptoOperationData where
  toJSVal = return . unCryptoOperationData
  {-# INLINE toJSVal #-}

instance FromJSVal CryptoOperationData where
  fromJSVal v = fmap CryptoOperationData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CryptoOperationData
  {-# INLINE fromJSValUnchecked #-}

class IsGObject o => IsCryptoOperationData o
toCryptoOperationData :: IsCryptoOperationData o => o -> CryptoOperationData
toCryptoOperationData = CryptoOperationData . coerce

instance IsCryptoOperationData CryptoOperationData
instance IsGObject CryptoOperationData where
  typeGType _ = error "Unable to get the JavaScript type of CryptoOperationData"
instance IsCryptoOperationData ArrayBuffer
instance IsCryptoOperationData ArrayBufferView

type GLenum = Word32
noGLenum :: Maybe GLenum
noGLenum = Nothing
{-# INLINE noGLenum #-}
type GLboolean = Bool
noGLboolean :: Maybe GLboolean
noGLboolean = Nothing
{-# INLINE noGLboolean #-}
type GLbitfield = Word32
noGLbitfield :: Maybe GLbitfield
noGLbitfield = Nothing
{-# INLINE noGLbitfield #-}
type GLbyte = Int8
noGLbyte :: Maybe GLbyte
noGLbyte = Nothing
{-# INLINE noGLbyte #-}
type GLshort = Int16
noGLshort :: Maybe GLshort
noGLshort = Nothing
{-# INLINE noGLshort #-}
type GLint = Int32
noGLint :: Maybe GLint
noGLint = Nothing
{-# INLINE noGLint #-}
type GLint64 = Int64
noGLint64 :: Maybe GLint64
noGLint64 = Nothing
{-# INLINE noGLint64 #-}
type GLsizei = Int32
noGLsizei :: Maybe GLsizei
noGLsizei = Nothing
{-# INLINE noGLsizei #-}
type GLintptr = Int64
noGLintptr :: Maybe GLintptr
noGLintptr = Nothing
{-# INLINE noGLintptr #-}
type GLsizeiptr = Int64
noGLsizeiptr :: Maybe GLsizeiptr
noGLsizeiptr = Nothing
{-# INLINE noGLsizeiptr #-}
type GLubyte = Word8
noGLubyte :: Maybe GLubyte
noGLubyte = Nothing
{-# INLINE noGLubyte #-}
type GLushort = Word16
noGLushort :: Maybe GLushort
noGLushort = Nothing
{-# INLINE noGLushort #-}
type GLuint = Word32
noGLuint :: Maybe GLuint
noGLuint = Nothing
{-# INLINE noGLuint #-}
type GLuint64 = Word64
noGLuint64 :: Maybe GLuint64
noGLuint64 = Nothing
{-# INLINE noGLuint64 #-}
type GLfloat = Double
noGLfloat :: Maybe GLfloat
noGLfloat = Nothing
{-# INLINE noGLfloat #-}
type GLclampf = Double
noGLclampf :: Maybe GLclampf
noGLclampf = Nothing
{-# INLINE noGLclampf #-}

-- This type is used to access the `globalThis` (see https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/globalThis)
newtype GlobalThis = GlobalThis { unGlobalThis :: JSVal }

instance PToJSVal GlobalThis where
  pToJSVal = unGlobalThis
  {-# INLINE pToJSVal #-}

instance PFromJSVal GlobalThis where
  pFromJSVal = GlobalThis
  {-# INLINE pFromJSVal #-}

instance ToJSVal GlobalThis where
  toJSVal = return . unGlobalThis
  {-# INLINE toJSVal #-}

instance FromJSVal GlobalThis where
  fromJSVal v = fmap GlobalThis <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GlobalThis
  {-# INLINE fromJSValUnchecked #-}

instance IsGObject GlobalThis where
  typeGType _ = error "Unable to get the JavaScript type of GlobalThis"

instance MakeObject GlobalThis where
  makeObject = makeObject . unGlobalThis

instance IsEventTarget GlobalThis
instance IsWindowOrWorkerGlobalScope GlobalThis
instance IsGlobalPerformance GlobalThis
instance IsGlobalEventHandlers GlobalThis
instance IsGlobalCrypto GlobalThis
noGlobalThis :: Maybe GlobalThis
noGlobalThis = Nothing
{-# INLINE noGlobalThis #-}
