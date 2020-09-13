{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.Types00 where

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

-- | Functions for this inteface are in "JSDOM.CanvasPath".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasPath Mozilla CanvasPath documentation>
newtype CanvasPath = CanvasPath { unCanvasPath :: JSVal }

instance PToJSVal CanvasPath where
  pToJSVal = unCanvasPath
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasPath where
  pFromJSVal = CanvasPath
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasPath where
  toJSVal = return . unCanvasPath
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasPath where
  fromJSVal v = fmap CanvasPath <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasPath
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasPath where
  makeObject = makeObject . unCanvasPath

class (IsGObject o) => IsCanvasPath o
toCanvasPath :: IsCanvasPath o => o -> CanvasPath
toCanvasPath = CanvasPath . coerce

instance IsCanvasPath CanvasPath
instance IsGObject CanvasPath where
  typeGType _ = gTypeCanvasPath
  {-# INLINE typeGType #-}

noCanvasPath :: Maybe CanvasPath
noCanvasPath = Nothing
{-# INLINE noCanvasPath #-}

gTypeCanvasPath :: JSM GType
gTypeCanvasPath = GType . Object <$> jsg "CanvasPath"


-- | Functions for this inteface are in "JSDOM.CanvasRenderingContext2D".
-- Base interface functions are in:
--
--     * "JSDOM.CanvasPath"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D Mozilla CanvasRenderingContext2D documentation>
newtype CanvasRenderingContext2D = CanvasRenderingContext2D { unCanvasRenderingContext2D :: JSVal }

instance PToJSVal CanvasRenderingContext2D where
  pToJSVal = unCanvasRenderingContext2D
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasRenderingContext2D where
  pFromJSVal = CanvasRenderingContext2D
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasRenderingContext2D where
  toJSVal = return . unCanvasRenderingContext2D
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasRenderingContext2D where
  fromJSVal v = fmap CanvasRenderingContext2D <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasRenderingContext2D
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasRenderingContext2D where
  makeObject = makeObject . unCanvasRenderingContext2D

instance IsCanvasPath CanvasRenderingContext2D
instance IsGObject CanvasRenderingContext2D where
  typeGType _ = gTypeCanvasRenderingContext2D
  {-# INLINE typeGType #-}

noCanvasRenderingContext2D :: Maybe CanvasRenderingContext2D
noCanvasRenderingContext2D = Nothing
{-# INLINE noCanvasRenderingContext2D #-}

gTypeCanvasRenderingContext2D :: JSM GType
gTypeCanvasRenderingContext2D = GType . Object <$> jsg "CanvasRenderingContext2D"
-- | Functions for this inteface are in "JSDOM.ImageData".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ImageData Mozilla ImageData documentation>
newtype ImageData = ImageData { unImageData :: JSVal }

instance PToJSVal ImageData where
  pToJSVal = unImageData
  {-# INLINE pToJSVal #-}

instance PFromJSVal ImageData where
  pFromJSVal = ImageData
  {-# INLINE pFromJSVal #-}

instance ToJSVal ImageData where
  toJSVal = return . unImageData
  {-# INLINE toJSVal #-}

instance FromJSVal ImageData where
  fromJSVal v = fmap ImageData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ImageData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ImageData where
  makeObject = makeObject . unImageData

instance IsGObject ImageData where
  typeGType _ = gTypeImageData
  {-# INLINE typeGType #-}

noImageData :: Maybe ImageData
noImageData = Nothing
{-# INLINE noImageData #-}

gTypeImageData :: JSM GType
gTypeImageData = GType . Object <$> jsg "ImageData"

-- | Functions for this inteface are in "JSDOM.AudioTrack".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioTrack Mozilla AudioTrack documentation>
newtype AudioTrack = AudioTrack { unAudioTrack :: JSVal }

instance PToJSVal AudioTrack where
  pToJSVal = unAudioTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioTrack where
  pFromJSVal = AudioTrack
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioTrack where
  toJSVal = return . unAudioTrack
  {-# INLINE toJSVal #-}

instance FromJSVal AudioTrack where
  fromJSVal v = fmap AudioTrack <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioTrack
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioTrack where
  makeObject = makeObject . unAudioTrack

instance IsGObject AudioTrack where
  typeGType _ = gTypeAudioTrack
  {-# INLINE typeGType #-}

noAudioTrack :: Maybe AudioTrack
noAudioTrack = Nothing
{-# INLINE noAudioTrack #-}

gTypeAudioTrack :: JSM GType
gTypeAudioTrack = GType . Object <$> jsg "AudioTrack"

-- | Functions for this inteface are in "JSDOM.VideoTrack".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VideoTrack Mozilla VideoTrack documentation>
newtype VideoTrack = VideoTrack { unVideoTrack :: JSVal }

instance PToJSVal VideoTrack where
  pToJSVal = unVideoTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal VideoTrack where
  pFromJSVal = VideoTrack
  {-# INLINE pFromJSVal #-}

instance ToJSVal VideoTrack where
  toJSVal = return . unVideoTrack
  {-# INLINE toJSVal #-}

instance FromJSVal VideoTrack where
  fromJSVal v = fmap VideoTrack <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VideoTrack
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VideoTrack where
  makeObject = makeObject . unVideoTrack

instance IsGObject VideoTrack where
  typeGType _ = gTypeVideoTrack
  {-# INLINE typeGType #-}

noVideoTrack :: Maybe VideoTrack
noVideoTrack = Nothing
{-# INLINE noVideoTrack #-}

gTypeVideoTrack :: JSM GType
gTypeVideoTrack = GType . Object <$> jsg "VideoTrack"

-- | Functions for this inteface are in "JSDOM.TextTrack".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextTrack Mozilla TextTrack documentation>
newtype TextTrack = TextTrack { unTextTrack :: JSVal }

instance PToJSVal TextTrack where
  pToJSVal = unTextTrack
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextTrack where
  pFromJSVal = TextTrack
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextTrack where
  toJSVal = return . unTextTrack
  {-# INLINE toJSVal #-}

instance FromJSVal TextTrack where
  fromJSVal v = fmap TextTrack <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextTrack
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextTrack where
  makeObject = makeObject . unTextTrack

instance IsEventTarget TextTrack
instance IsGObject TextTrack where
  typeGType _ = gTypeTextTrack
  {-# INLINE typeGType #-}

noTextTrack :: Maybe TextTrack
noTextTrack = Nothing
{-# INLINE noTextTrack #-}

gTypeTextTrack :: JSM GType
gTypeTextTrack = GType . Object <$> jsg "TextTrack"

-- | Functions for this inteface are in "JSDOM.XMLDocument".
-- Base interface functions are in:
--
--     * "JSDOM.Document"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.DocumentOrShadowRoot"
--     * "JSDOM.NonElementParentNode"
--     * "JSDOM.ParentNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLDocument Mozilla XMLDocument documentation>
newtype XMLDocument = XMLDocument { unXMLDocument :: JSVal }

instance PToJSVal XMLDocument where
  pToJSVal = unXMLDocument
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLDocument where
  pFromJSVal = XMLDocument
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLDocument where
  toJSVal = return . unXMLDocument
  {-# INLINE toJSVal #-}

instance FromJSVal XMLDocument where
  fromJSVal v = fmap XMLDocument <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLDocument
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLDocument where
  makeObject = makeObject . unXMLDocument

instance IsDocument XMLDocument
instance IsNode XMLDocument
instance IsEventTarget XMLDocument
instance IsGlobalEventHandlers XMLDocument
instance IsDocumentOrShadowRoot XMLDocument
instance IsNonElementParentNode XMLDocument
instance IsParentNode XMLDocument
instance IsDocumentAndElementEventHandlers XMLDocument
instance IsGObject XMLDocument where
  typeGType _ = gTypeXMLDocument
  {-# INLINE typeGType #-}

noXMLDocument :: Maybe XMLDocument
noXMLDocument = Nothing
{-# INLINE noXMLDocument #-}

gTypeXMLDocument :: JSM GType
gTypeXMLDocument = GType . Object <$> jsg "XMLDocument"

