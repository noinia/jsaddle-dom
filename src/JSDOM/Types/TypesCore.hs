module JSDOM.Types.TypesCore
  ( AddEventListenerOptions(AddEventListenerOptions), unAddEventListenerOptions, noAddEventListenerOptions, gTypeAddEventListenerOptions
  , EventListenerOptions(EventListenerOptions), unEventListenerOptions, IsEventListenerOptions, toEventListenerOptions, noEventListenerOptions, gTypeEventListenerOptions
  , Blob(Blob), unBlob, IsBlob, toBlob, noBlob, gTypeBlob
  , File(File), unFile, noFile, gTypeFile
  , FormData(FormData), unFormData, noFormData, gTypeFormData
  , Event(Event), unEvent, IsEvent, toEvent, noEvent, gTypeEvent
  , EventInit(EventInit), unEventInit, IsEventInit, toEventInit, noEventInit, gTypeEventInit
  , UIEvent(UIEvent), unUIEvent, IsUIEvent, toUIEvent, noUIEvent, gTypeUIEvent
  ) where

import qualified Data.Text as T (unpack, Text)
import Data.Coerce (coerce, Coercible)
import Language.Javascript.JSaddle
       (Object(..), valToBool, valNull, valToNumber, (!!), js, valToText,
        JSVal, JSString, JSM, maybeNullOrUndefined, maybeNullOrUndefined',
        valToStr, jsg, ToJSString(..), FromJSString(..), strToText, MakeObject(..),
        Nullable(..), freeFunction, instanceOf, JSContextRef,
        askJSM, runJSM, MonadJSM(..), liftJSM, strictEqual, function, js2)

import JSDOM.Types.Core

-- CORE GENERATED TYPES

-- | Functions for this inteface are in "JSDOM.UIEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/UIEvent Mozilla UIEvent documentation>
newtype UIEvent = UIEvent { unUIEvent :: JSVal }

instance PToJSVal UIEvent where
  pToJSVal = unUIEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal UIEvent where
  pFromJSVal = UIEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal UIEvent where
  toJSVal = return . unUIEvent
  {-# INLINE toJSVal #-}

instance FromJSVal UIEvent where
  fromJSVal v = fmap UIEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . UIEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject UIEvent where
  makeObject = makeObject . unUIEvent

class (IsEvent o, IsGObject o) => IsUIEvent o
toUIEvent :: IsUIEvent o => o -> UIEvent
toUIEvent = UIEvent . coerce

instance IsUIEvent UIEvent
instance IsEvent UIEvent
instance IsGObject UIEvent where
  typeGType _ = gTypeUIEvent
  {-# INLINE typeGType #-}

noUIEvent :: Maybe UIEvent
noUIEvent = Nothing
{-# INLINE noUIEvent #-}

gTypeUIEvent :: JSM GType
gTypeUIEvent = GType . Object <$> jsg "UIEvent"

-- | Functions for this inteface are in "JSDOM.Event".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Event Mozilla Event documentation>
newtype Event = Event { unEvent :: JSVal }

instance PToJSVal Event where
  pToJSVal = unEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal Event where
  pFromJSVal = Event
  {-# INLINE pFromJSVal #-}

instance ToJSVal Event where
  toJSVal = return . unEvent
  {-# INLINE toJSVal #-}

instance FromJSVal Event where
  fromJSVal v = fmap Event <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Event
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Event where
  makeObject = makeObject . unEvent

class (IsGObject o) => IsEvent o
toEvent :: IsEvent o => o -> Event
toEvent = Event . coerce

instance IsEvent Event
instance IsGObject Event where
  typeGType _ = gTypeEvent
  {-# INLINE typeGType #-}

noEvent :: Maybe Event
noEvent = Nothing
{-# INLINE noEvent #-}

gTypeEvent :: JSM GType
gTypeEvent = GType . Object <$> jsg "Event"

-- | Functions for this inteface are in "JSDOM.EventInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventInit Mozilla EventInit documentation>
newtype EventInit = EventInit { unEventInit :: JSVal }

instance PToJSVal EventInit where
  pToJSVal = unEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventInit where
  pFromJSVal = EventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventInit where
  toJSVal = return . unEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal EventInit where
  fromJSVal v = fmap EventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventInit where
  makeObject = makeObject . unEventInit

class (IsGObject o) => IsEventInit o
toEventInit :: IsEventInit o => o -> EventInit
toEventInit = EventInit . coerce

instance IsEventInit EventInit
instance IsGObject EventInit where
  typeGType _ = gTypeEventInit
  {-# INLINE typeGType #-}

noEventInit :: Maybe EventInit
noEventInit = Nothing
{-# INLINE noEventInit #-}

gTypeEventInit :: JSM GType
gTypeEventInit = GType . Object <$> jsg "EventInit"


-- | Functions for this inteface are in "JSDOM.AddEventListenerOptions".
-- Base interface functions are in:
--
--     * "JSDOM.EventListenerOptions"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AddEventListenerOptions Mozilla AddEventListenerOptions documentation>
newtype AddEventListenerOptions = AddEventListenerOptions { unAddEventListenerOptions :: JSVal }

instance PToJSVal AddEventListenerOptions where
  pToJSVal = unAddEventListenerOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal AddEventListenerOptions where
  pFromJSVal = AddEventListenerOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal AddEventListenerOptions where
  toJSVal = return . unAddEventListenerOptions
  {-# INLINE toJSVal #-}

instance FromJSVal AddEventListenerOptions where
  fromJSVal v = fmap AddEventListenerOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AddEventListenerOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AddEventListenerOptions where
  makeObject = makeObject . unAddEventListenerOptions

instance IsEventListenerOptions AddEventListenerOptions
instance IsGObject AddEventListenerOptions where
  typeGType _ = gTypeAddEventListenerOptions
  {-# INLINE typeGType #-}

noAddEventListenerOptions :: Maybe AddEventListenerOptions
noAddEventListenerOptions = Nothing
{-# INLINE noAddEventListenerOptions #-}

gTypeAddEventListenerOptions :: JSM GType
gTypeAddEventListenerOptions = GType . Object <$> jsg "AddEventListenerOptions"

-- | Functions for this inteface are in "JSDOM.EventListenerOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventListenerOptions Mozilla EventListenerOptions documentation>
newtype EventListenerOptions = EventListenerOptions { unEventListenerOptions :: JSVal }

instance PToJSVal EventListenerOptions where
  pToJSVal = unEventListenerOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventListenerOptions where
  pFromJSVal = EventListenerOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventListenerOptions where
  toJSVal = return . unEventListenerOptions
  {-# INLINE toJSVal #-}

instance FromJSVal EventListenerOptions where
  fromJSVal v = fmap EventListenerOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventListenerOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventListenerOptions where
  makeObject = makeObject . unEventListenerOptions

class (IsGObject o) => IsEventListenerOptions o
toEventListenerOptions :: IsEventListenerOptions o => o -> EventListenerOptions
toEventListenerOptions = EventListenerOptions . coerce

instance IsEventListenerOptions EventListenerOptions
instance IsGObject EventListenerOptions where
  typeGType _ = gTypeEventListenerOptions
  {-# INLINE typeGType #-}

noEventListenerOptions :: Maybe EventListenerOptions
noEventListenerOptions = Nothing
{-# INLINE noEventListenerOptions #-}

gTypeEventListenerOptions :: JSM GType
gTypeEventListenerOptions = GType . Object <$> jsg "EventListenerOptions"

-- | Functions for this inteface are in "JSDOM.File".
-- Base interface functions are in:
--
--     * "JSDOM.Blob"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/File Mozilla File documentation>
newtype File = File { unFile :: JSVal }

instance PToJSVal File where
  pToJSVal = unFile
  {-# INLINE pToJSVal #-}

instance PFromJSVal File where
  pFromJSVal = File
  {-# INLINE pFromJSVal #-}

instance ToJSVal File where
  toJSVal = return . unFile
  {-# INLINE toJSVal #-}

instance FromJSVal File where
  fromJSVal v = fmap File <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . File
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject File where
  makeObject = makeObject . unFile

instance IsBlob File
instance IsGObject File where
  typeGType _ = gTypeFile
  {-# INLINE typeGType #-}

noFile :: Maybe File
noFile = Nothing
{-# INLINE noFile #-}

gTypeFile :: JSM GType
gTypeFile = GType . Object <$> jsg "File"

-- | Functions for this inteface are in "JSDOM.FormData".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FormData Mozilla FormData documentation>
newtype FormData = FormData { unFormData :: JSVal }

instance PToJSVal FormData where
  pToJSVal = unFormData
  {-# INLINE pToJSVal #-}

instance PFromJSVal FormData where
  pFromJSVal = FormData
  {-# INLINE pFromJSVal #-}

instance ToJSVal FormData where
  toJSVal = return . unFormData
  {-# INLINE toJSVal #-}

instance FromJSVal FormData where
  fromJSVal v = fmap FormData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FormData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FormData where
  makeObject = makeObject . unFormData

instance IsGObject FormData where
  typeGType _ = gTypeFormData
  {-# INLINE typeGType #-}

noFormData :: Maybe FormData
noFormData = Nothing
{-# INLINE noFormData #-}

gTypeFormData :: JSM GType
gTypeFormData = GType . Object <$> jsg "FormData"

-- | Functions for this inteface are in "JSDOM.Blob".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Blob Mozilla Blob documentation>
newtype Blob = Blob { unBlob :: JSVal }

instance PToJSVal Blob where
  pToJSVal = unBlob
  {-# INLINE pToJSVal #-}

instance PFromJSVal Blob where
  pFromJSVal = Blob
  {-# INLINE pFromJSVal #-}

instance ToJSVal Blob where
  toJSVal = return . unBlob
  {-# INLINE toJSVal #-}

instance FromJSVal Blob where
  fromJSVal v = fmap Blob <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Blob
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Blob where
  makeObject = makeObject . unBlob

class (IsGObject o) => IsBlob o
toBlob :: IsBlob o => o -> Blob
toBlob = Blob . coerce

instance IsBlob Blob
instance IsGObject Blob where
  typeGType _ = gTypeBlob
  {-# INLINE typeGType #-}

noBlob :: Maybe Blob
noBlob = Nothing
{-# INLINE noBlob #-}

gTypeBlob :: JSM GType
gTypeBlob = GType . Object <$> jsg "Blob"
