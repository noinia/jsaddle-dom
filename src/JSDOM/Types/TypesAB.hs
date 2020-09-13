{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.TypesAB where

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
import JSDOM.Types.TypesSVG
import JSDOM.Types.TypesHTML
import JSDOM.Types.TypesWebGL
import JSDOM.Types.Types00
import JSDOM.Types.Types0

-- | Functions for this inteface are in "JSDOM.TextTrackCue".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextTrackCue Mozilla TextTrackCue documentation>
newtype TextTrackCue = TextTrackCue { unTextTrackCue :: JSVal }

instance PToJSVal TextTrackCue where
  pToJSVal = unTextTrackCue
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextTrackCue where
  pFromJSVal = TextTrackCue
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextTrackCue where
  toJSVal = return . unTextTrackCue
  {-# INLINE toJSVal #-}

instance FromJSVal TextTrackCue where
  fromJSVal v = fmap TextTrackCue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextTrackCue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextTrackCue where
  makeObject = makeObject . unTextTrackCue

class (IsEventTarget o, IsGObject o) => IsTextTrackCue o
toTextTrackCue :: IsTextTrackCue o => o -> TextTrackCue
toTextTrackCue = TextTrackCue . coerce

instance IsTextTrackCue TextTrackCue
instance IsEventTarget TextTrackCue
instance IsGObject TextTrackCue where
  typeGType _ = gTypeTextTrackCue
  {-# INLINE typeGType #-}

noTextTrackCue :: Maybe TextTrackCue
noTextTrackCue = Nothing
{-# INLINE noTextTrackCue #-}

gTypeTextTrackCue :: JSM GType
gTypeTextTrackCue = GType . Object <$> jsg "TextTrackCue"

-- | Functions for this inteface are in "JSDOM.StyleSheet".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StyleSheet Mozilla StyleSheet documentation>
newtype StyleSheet = StyleSheet { unStyleSheet :: JSVal }

instance PToJSVal StyleSheet where
  pToJSVal = unStyleSheet
  {-# INLINE pToJSVal #-}

instance PFromJSVal StyleSheet where
  pFromJSVal = StyleSheet
  {-# INLINE pFromJSVal #-}

instance ToJSVal StyleSheet where
  toJSVal = return . unStyleSheet
  {-# INLINE toJSVal #-}

instance FromJSVal StyleSheet where
  fromJSVal v = fmap StyleSheet <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StyleSheet
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StyleSheet where
  makeObject = makeObject . unStyleSheet

class (IsGObject o) => IsStyleSheet o
toStyleSheet :: IsStyleSheet o => o -> StyleSheet
toStyleSheet = StyleSheet . coerce

instance IsStyleSheet StyleSheet
instance IsGObject StyleSheet where
  typeGType _ = gTypeStyleSheet
  {-# INLINE typeGType #-}

noStyleSheet :: Maybe StyleSheet
noStyleSheet = Nothing
{-# INLINE noStyleSheet #-}

gTypeStyleSheet :: JSM GType
gTypeStyleSheet = GType . Object <$> jsg "StyleSheet"

-- | Functions for this inteface are in "JSDOM.LongRange".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/LongRange Mozilla LongRange documentation>
newtype LongRange = LongRange { unLongRange :: JSVal }

instance PToJSVal LongRange where
  pToJSVal = unLongRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal LongRange where
  pFromJSVal = LongRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal LongRange where
  toJSVal = return . unLongRange
  {-# INLINE toJSVal #-}

instance FromJSVal LongRange where
  fromJSVal v = fmap LongRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . LongRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject LongRange where
  makeObject = makeObject . unLongRange

class (IsGObject o) => IsLongRange o
toLongRange :: IsLongRange o => o -> LongRange
toLongRange = LongRange . coerce

instance IsLongRange LongRange
instance IsGObject LongRange where
  typeGType _ = gTypeLongRange
  {-# INLINE typeGType #-}

noLongRange :: Maybe LongRange
noLongRange = Nothing
{-# INLINE noLongRange #-}

gTypeLongRange :: JSM GType
gTypeLongRange = GType . Object <$> jsg "LongRange"


-- | Functions for this inteface are in "JSDOM.WorkerGlobalScope".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--     * "JSDOM.WindowOrWorkerGlobalScope"
--     * "JSDOM.GlobalPerformance"
--     * "JSDOM.GlobalCrypto"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WorkerGlobalScope Mozilla WorkerGlobalScope documentation>
newtype WorkerGlobalScope = WorkerGlobalScope { unWorkerGlobalScope :: JSVal }

instance PToJSVal WorkerGlobalScope where
  pToJSVal = unWorkerGlobalScope
  {-# INLINE pToJSVal #-}

instance PFromJSVal WorkerGlobalScope where
  pFromJSVal = WorkerGlobalScope
  {-# INLINE pFromJSVal #-}

instance ToJSVal WorkerGlobalScope where
  toJSVal = return . unWorkerGlobalScope
  {-# INLINE toJSVal #-}

instance FromJSVal WorkerGlobalScope where
  fromJSVal v = fmap WorkerGlobalScope <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WorkerGlobalScope
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WorkerGlobalScope where
  makeObject = makeObject . unWorkerGlobalScope

class (IsEventTarget o, IsWindowOrWorkerGlobalScope o, IsGlobalPerformance o, IsGlobalCrypto o, IsGObject o) => IsWorkerGlobalScope o
toWorkerGlobalScope :: IsWorkerGlobalScope o => o -> WorkerGlobalScope
toWorkerGlobalScope = WorkerGlobalScope . coerce

instance IsWorkerGlobalScope WorkerGlobalScope
instance IsEventTarget WorkerGlobalScope
instance IsWindowOrWorkerGlobalScope WorkerGlobalScope
instance IsGlobalPerformance WorkerGlobalScope
instance IsGlobalCrypto WorkerGlobalScope
instance IsGObject WorkerGlobalScope where
  typeGType _ = gTypeWorkerGlobalScope
  {-# INLINE typeGType #-}

noWorkerGlobalScope :: Maybe WorkerGlobalScope
noWorkerGlobalScope = Nothing
{-# INLINE noWorkerGlobalScope #-}

gTypeWorkerGlobalScope :: JSM GType
gTypeWorkerGlobalScope = GType . Object <$> jsg "WorkerGlobalScope"


-- | Functions for this inteface are in "JSDOM.UIEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/UIEventInit Mozilla UIEventInit documentation>
newtype UIEventInit = UIEventInit { unUIEventInit :: JSVal }

instance PToJSVal UIEventInit where
  pToJSVal = unUIEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal UIEventInit where
  pFromJSVal = UIEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal UIEventInit where
  toJSVal = return . unUIEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal UIEventInit where
  fromJSVal v = fmap UIEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . UIEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject UIEventInit where
  makeObject = makeObject . unUIEventInit

class (IsEventInit o, IsGObject o) => IsUIEventInit o
toUIEventInit :: IsUIEventInit o => o -> UIEventInit
toUIEventInit = UIEventInit . coerce

instance IsUIEventInit UIEventInit
instance IsEventInit UIEventInit
instance IsGObject UIEventInit where
  typeGType _ = gTypeUIEventInit
  {-# INLINE typeGType #-}

noUIEventInit :: Maybe UIEventInit
noUIEventInit = Nothing
{-# INLINE noUIEventInit #-}

gTypeUIEventInit :: JSM GType
gTypeUIEventInit = GType . Object <$> jsg "UIEventInit"

-- | Functions for this inteface are in "JSDOM.ANGLEInstancedArrays".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ANGLEInstancedArrays Mozilla ANGLEInstancedArrays documentation>
newtype ANGLEInstancedArrays = ANGLEInstancedArrays { unANGLEInstancedArrays :: JSVal }

instance PToJSVal ANGLEInstancedArrays where
  pToJSVal = unANGLEInstancedArrays
  {-# INLINE pToJSVal #-}

instance PFromJSVal ANGLEInstancedArrays where
  pFromJSVal = ANGLEInstancedArrays
  {-# INLINE pFromJSVal #-}

instance ToJSVal ANGLEInstancedArrays where
  toJSVal = return . unANGLEInstancedArrays
  {-# INLINE toJSVal #-}

instance FromJSVal ANGLEInstancedArrays where
  fromJSVal v = fmap ANGLEInstancedArrays <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ANGLEInstancedArrays
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ANGLEInstancedArrays where
  makeObject = makeObject . unANGLEInstancedArrays

instance IsGObject ANGLEInstancedArrays where
  typeGType _ = gTypeANGLEInstancedArrays
  {-# INLINE typeGType #-}

noANGLEInstancedArrays :: Maybe ANGLEInstancedArrays
noANGLEInstancedArrays = Nothing
{-# INLINE noANGLEInstancedArrays #-}

gTypeANGLEInstancedArrays :: JSM GType
gTypeANGLEInstancedArrays = GType . Object <$> jsg "ANGLEInstancedArrays"

-- | Functions for this inteface are in "JSDOM.AbstractWorker".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AbstractWorker Mozilla AbstractWorker documentation>
newtype AbstractWorker = AbstractWorker { unAbstractWorker :: JSVal }

instance PToJSVal AbstractWorker where
  pToJSVal = unAbstractWorker
  {-# INLINE pToJSVal #-}

instance PFromJSVal AbstractWorker where
  pFromJSVal = AbstractWorker
  {-# INLINE pFromJSVal #-}

instance ToJSVal AbstractWorker where
  toJSVal = return . unAbstractWorker
  {-# INLINE toJSVal #-}

instance FromJSVal AbstractWorker where
  fromJSVal v = fmap AbstractWorker <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AbstractWorker
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AbstractWorker where
  makeObject = makeObject . unAbstractWorker

class (IsGObject o) => IsAbstractWorker o
toAbstractWorker :: IsAbstractWorker o => o -> AbstractWorker
toAbstractWorker = AbstractWorker . coerce

instance IsAbstractWorker AbstractWorker
instance IsGObject AbstractWorker where
  typeGType _ = gTypeAbstractWorker
  {-# INLINE typeGType #-}

noAbstractWorker :: Maybe AbstractWorker
noAbstractWorker = Nothing
{-# INLINE noAbstractWorker #-}

gTypeAbstractWorker :: JSM GType
gTypeAbstractWorker = GType . Object <$> jsg "AbstractWorker"

-- | Functions for this inteface are in "JSDOM.Acceleration".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Acceleration Mozilla Acceleration documentation>
newtype Acceleration = Acceleration { unAcceleration :: JSVal }

instance PToJSVal Acceleration where
  pToJSVal = unAcceleration
  {-# INLINE pToJSVal #-}

instance PFromJSVal Acceleration where
  pFromJSVal = Acceleration
  {-# INLINE pFromJSVal #-}

instance ToJSVal Acceleration where
  toJSVal = return . unAcceleration
  {-# INLINE toJSVal #-}

instance FromJSVal Acceleration where
  fromJSVal v = fmap Acceleration <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Acceleration
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Acceleration where
  makeObject = makeObject . unAcceleration

instance IsGObject Acceleration where
  typeGType _ = gTypeAcceleration
  {-# INLINE typeGType #-}

noAcceleration :: Maybe Acceleration
noAcceleration = Nothing
{-# INLINE noAcceleration #-}

gTypeAcceleration :: JSM GType
gTypeAcceleration = GType . Object <$> jsg "Acceleration"

-- | Functions for this inteface are in "JSDOM.AesCbcCfbParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AesCbcCfbParams Mozilla AesCbcCfbParams documentation>
newtype AesCbcCfbParams = AesCbcCfbParams { unAesCbcCfbParams :: JSVal }

instance PToJSVal AesCbcCfbParams where
  pToJSVal = unAesCbcCfbParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal AesCbcCfbParams where
  pFromJSVal = AesCbcCfbParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal AesCbcCfbParams where
  toJSVal = return . unAesCbcCfbParams
  {-# INLINE toJSVal #-}

instance FromJSVal AesCbcCfbParams where
  fromJSVal v = fmap AesCbcCfbParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AesCbcCfbParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AesCbcCfbParams where
  makeObject = makeObject . unAesCbcCfbParams

instance IsCryptoAlgorithmParameters AesCbcCfbParams
instance IsGObject AesCbcCfbParams where
  typeGType _ = gTypeAesCbcCfbParams
  {-# INLINE typeGType #-}

noAesCbcCfbParams :: Maybe AesCbcCfbParams
noAesCbcCfbParams = Nothing
{-# INLINE noAesCbcCfbParams #-}

gTypeAesCbcCfbParams :: JSM GType
gTypeAesCbcCfbParams = GType . Object <$> jsg "AesCbcCfbParams"

-- | Functions for this inteface are in "JSDOM.AesCtrParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AesCtrParams Mozilla AesCtrParams documentation>
newtype AesCtrParams = AesCtrParams { unAesCtrParams :: JSVal }

instance PToJSVal AesCtrParams where
  pToJSVal = unAesCtrParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal AesCtrParams where
  pFromJSVal = AesCtrParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal AesCtrParams where
  toJSVal = return . unAesCtrParams
  {-# INLINE toJSVal #-}

instance FromJSVal AesCtrParams where
  fromJSVal v = fmap AesCtrParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AesCtrParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AesCtrParams where
  makeObject = makeObject . unAesCtrParams

instance IsCryptoAlgorithmParameters AesCtrParams
instance IsGObject AesCtrParams where
  typeGType _ = gTypeAesCtrParams
  {-# INLINE typeGType #-}

noAesCtrParams :: Maybe AesCtrParams
noAesCtrParams = Nothing
{-# INLINE noAesCtrParams #-}

gTypeAesCtrParams :: JSM GType
gTypeAesCtrParams = GType . Object <$> jsg "AesCtrParams"

-- | Functions for this inteface are in "JSDOM.AesGcmParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AesGcmParams Mozilla AesGcmParams documentation>
newtype AesGcmParams = AesGcmParams { unAesGcmParams :: JSVal }

instance PToJSVal AesGcmParams where
  pToJSVal = unAesGcmParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal AesGcmParams where
  pFromJSVal = AesGcmParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal AesGcmParams where
  toJSVal = return . unAesGcmParams
  {-# INLINE toJSVal #-}

instance FromJSVal AesGcmParams where
  fromJSVal v = fmap AesGcmParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AesGcmParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AesGcmParams where
  makeObject = makeObject . unAesGcmParams

instance IsCryptoAlgorithmParameters AesGcmParams
instance IsGObject AesGcmParams where
  typeGType _ = gTypeAesGcmParams
  {-# INLINE typeGType #-}

noAesGcmParams :: Maybe AesGcmParams
noAesGcmParams = Nothing
{-# INLINE noAesGcmParams #-}

gTypeAesGcmParams :: JSM GType
gTypeAesGcmParams = GType . Object <$> jsg "AesGcmParams"

-- | Functions for this inteface are in "JSDOM.AesKeyParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AesKeyParams Mozilla AesKeyParams documentation>
newtype AesKeyParams = AesKeyParams { unAesKeyParams :: JSVal }

instance PToJSVal AesKeyParams where
  pToJSVal = unAesKeyParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal AesKeyParams where
  pFromJSVal = AesKeyParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal AesKeyParams where
  toJSVal = return . unAesKeyParams
  {-# INLINE toJSVal #-}

instance FromJSVal AesKeyParams where
  fromJSVal v = fmap AesKeyParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AesKeyParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AesKeyParams where
  makeObject = makeObject . unAesKeyParams

instance IsCryptoAlgorithmParameters AesKeyParams
instance IsGObject AesKeyParams where
  typeGType _ = gTypeAesKeyParams
  {-# INLINE typeGType #-}

noAesKeyParams :: Maybe AesKeyParams
noAesKeyParams = Nothing
{-# INLINE noAesKeyParams #-}

gTypeAesKeyParams :: JSM GType
gTypeAesKeyParams = GType . Object <$> jsg "AesKeyParams"

-- | Functions for this inteface are in "JSDOM.AnalyserNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AnalyserNode Mozilla AnalyserNode documentation>
newtype AnalyserNode = AnalyserNode { unAnalyserNode :: JSVal }

instance PToJSVal AnalyserNode where
  pToJSVal = unAnalyserNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal AnalyserNode where
  pFromJSVal = AnalyserNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal AnalyserNode where
  toJSVal = return . unAnalyserNode
  {-# INLINE toJSVal #-}

instance FromJSVal AnalyserNode where
  fromJSVal v = fmap AnalyserNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AnalyserNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AnalyserNode where
  makeObject = makeObject . unAnalyserNode

instance IsAudioNode AnalyserNode
instance IsEventTarget AnalyserNode
instance IsGObject AnalyserNode where
  typeGType _ = gTypeAnalyserNode
  {-# INLINE typeGType #-}

noAnalyserNode :: Maybe AnalyserNode
noAnalyserNode = Nothing
{-# INLINE noAnalyserNode #-}

gTypeAnalyserNode :: JSM GType
gTypeAnalyserNode = GType . Object <$> jsg "AnalyserNode"

-- | Functions for this inteface are in "JSDOM.Animation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Animation Mozilla Animation documentation>
newtype Animation = Animation { unAnimation :: JSVal }

instance PToJSVal Animation where
  pToJSVal = unAnimation
  {-# INLINE pToJSVal #-}

instance PFromJSVal Animation where
  pFromJSVal = Animation
  {-# INLINE pFromJSVal #-}

instance ToJSVal Animation where
  toJSVal = return . unAnimation
  {-# INLINE toJSVal #-}

instance FromJSVal Animation where
  fromJSVal v = fmap Animation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Animation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Animation where
  makeObject = makeObject . unAnimation

instance IsGObject Animation where
  typeGType _ = gTypeAnimation
  {-# INLINE typeGType #-}

noAnimation :: Maybe Animation
noAnimation = Nothing
{-# INLINE noAnimation #-}

gTypeAnimation :: JSM GType
gTypeAnimation = GType . Object <$> jsg "Animation"

-- | Functions for this inteface are in "JSDOM.AnimationEffect".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEffect Mozilla AnimationEffect documentation>
newtype AnimationEffect = AnimationEffect { unAnimationEffect :: JSVal }

instance PToJSVal AnimationEffect where
  pToJSVal = unAnimationEffect
  {-# INLINE pToJSVal #-}

instance PFromJSVal AnimationEffect where
  pFromJSVal = AnimationEffect
  {-# INLINE pFromJSVal #-}

instance ToJSVal AnimationEffect where
  toJSVal = return . unAnimationEffect
  {-# INLINE toJSVal #-}

instance FromJSVal AnimationEffect where
  fromJSVal v = fmap AnimationEffect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AnimationEffect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AnimationEffect where
  makeObject = makeObject . unAnimationEffect

class (IsGObject o) => IsAnimationEffect o
toAnimationEffect :: IsAnimationEffect o => o -> AnimationEffect
toAnimationEffect = AnimationEffect . coerce

instance IsAnimationEffect AnimationEffect
instance IsGObject AnimationEffect where
  typeGType _ = gTypeAnimationEffect
  {-# INLINE typeGType #-}

noAnimationEffect :: Maybe AnimationEffect
noAnimationEffect = Nothing
{-# INLINE noAnimationEffect #-}

gTypeAnimationEffect :: JSM GType
gTypeAnimationEffect = GType . Object <$> jsg "AnimationEffect"

-- | Functions for this inteface are in "JSDOM.AnimationEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEvent Mozilla AnimationEvent documentation>
newtype AnimationEvent = AnimationEvent { unAnimationEvent :: JSVal }

instance PToJSVal AnimationEvent where
  pToJSVal = unAnimationEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal AnimationEvent where
  pFromJSVal = AnimationEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal AnimationEvent where
  toJSVal = return . unAnimationEvent
  {-# INLINE toJSVal #-}

instance FromJSVal AnimationEvent where
  fromJSVal v = fmap AnimationEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AnimationEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AnimationEvent where
  makeObject = makeObject . unAnimationEvent

instance IsEvent AnimationEvent
instance IsGObject AnimationEvent where
  typeGType _ = gTypeAnimationEvent
  {-# INLINE typeGType #-}

noAnimationEvent :: Maybe AnimationEvent
noAnimationEvent = Nothing
{-# INLINE noAnimationEvent #-}

gTypeAnimationEvent :: JSM GType
gTypeAnimationEvent = GType . Object <$> jsg "AnimationEvent"

-- | Functions for this inteface are in "JSDOM.AnimationEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AnimationEventInit Mozilla AnimationEventInit documentation>
newtype AnimationEventInit = AnimationEventInit { unAnimationEventInit :: JSVal }

instance PToJSVal AnimationEventInit where
  pToJSVal = unAnimationEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal AnimationEventInit where
  pFromJSVal = AnimationEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal AnimationEventInit where
  toJSVal = return . unAnimationEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal AnimationEventInit where
  fromJSVal v = fmap AnimationEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AnimationEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AnimationEventInit where
  makeObject = makeObject . unAnimationEventInit

instance IsEventInit AnimationEventInit
instance IsGObject AnimationEventInit where
  typeGType _ = gTypeAnimationEventInit
  {-# INLINE typeGType #-}

noAnimationEventInit :: Maybe AnimationEventInit
noAnimationEventInit = Nothing
{-# INLINE noAnimationEventInit #-}

gTypeAnimationEventInit :: JSM GType
gTypeAnimationEventInit = GType . Object <$> jsg "AnimationEventInit"

-- | Functions for this inteface are in "JSDOM.AnimationTimeline".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AnimationTimeline Mozilla AnimationTimeline documentation>
newtype AnimationTimeline = AnimationTimeline { unAnimationTimeline :: JSVal }

instance PToJSVal AnimationTimeline where
  pToJSVal = unAnimationTimeline
  {-# INLINE pToJSVal #-}

instance PFromJSVal AnimationTimeline where
  pFromJSVal = AnimationTimeline
  {-# INLINE pFromJSVal #-}

instance ToJSVal AnimationTimeline where
  toJSVal = return . unAnimationTimeline
  {-# INLINE toJSVal #-}

instance FromJSVal AnimationTimeline where
  fromJSVal v = fmap AnimationTimeline <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AnimationTimeline
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AnimationTimeline where
  makeObject = makeObject . unAnimationTimeline

class (IsGObject o) => IsAnimationTimeline o
toAnimationTimeline :: IsAnimationTimeline o => o -> AnimationTimeline
toAnimationTimeline = AnimationTimeline . coerce

instance IsAnimationTimeline AnimationTimeline
instance IsGObject AnimationTimeline where
  typeGType _ = gTypeAnimationTimeline
  {-# INLINE typeGType #-}

noAnimationTimeline :: Maybe AnimationTimeline
noAnimationTimeline = Nothing
{-# INLINE noAnimationTimeline #-}

gTypeAnimationTimeline :: JSM GType
gTypeAnimationTimeline = GType . Object <$> jsg "AnimationTimeline"

-- | Functions for this inteface are in "JSDOM.ApplePayError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayError Mozilla ApplePayError documentation>
newtype ApplePayError = ApplePayError { unApplePayError :: JSVal }

instance PToJSVal ApplePayError where
  pToJSVal = unApplePayError
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayError where
  pFromJSVal = ApplePayError
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayError where
  toJSVal = return . unApplePayError
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayError where
  fromJSVal v = fmap ApplePayError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayError where
  makeObject = makeObject . unApplePayError

instance IsGObject ApplePayError where
  typeGType _ = gTypeApplePayError
  {-# INLINE typeGType #-}

noApplePayError :: Maybe ApplePayError
noApplePayError = Nothing
{-# INLINE noApplePayError #-}

gTypeApplePayError :: JSM GType
gTypeApplePayError = GType . Object <$> jsg "ApplePayError"

-- | Functions for this inteface are in "JSDOM.ApplePayLineItem".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayLineItem Mozilla ApplePayLineItem documentation>
newtype ApplePayLineItem = ApplePayLineItem { unApplePayLineItem :: JSVal }

instance PToJSVal ApplePayLineItem where
  pToJSVal = unApplePayLineItem
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayLineItem where
  pFromJSVal = ApplePayLineItem
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayLineItem where
  toJSVal = return . unApplePayLineItem
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayLineItem where
  fromJSVal v = fmap ApplePayLineItem <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayLineItem
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayLineItem where
  makeObject = makeObject . unApplePayLineItem

instance IsGObject ApplePayLineItem where
  typeGType _ = gTypeApplePayLineItem
  {-# INLINE typeGType #-}

noApplePayLineItem :: Maybe ApplePayLineItem
noApplePayLineItem = Nothing
{-# INLINE noApplePayLineItem #-}

gTypeApplePayLineItem :: JSM GType
gTypeApplePayLineItem = GType . Object <$> jsg "ApplePayLineItem"

-- | Functions for this inteface are in "JSDOM.ApplePayPayment".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPayment Mozilla ApplePayPayment documentation>
newtype ApplePayPayment = ApplePayPayment { unApplePayPayment :: JSVal }

instance PToJSVal ApplePayPayment where
  pToJSVal = unApplePayPayment
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPayment where
  pFromJSVal = ApplePayPayment
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPayment where
  toJSVal = return . unApplePayPayment
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPayment where
  fromJSVal v = fmap ApplePayPayment <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPayment
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPayment where
  makeObject = makeObject . unApplePayPayment

instance IsGObject ApplePayPayment where
  typeGType _ = gTypeApplePayPayment
  {-# INLINE typeGType #-}

noApplePayPayment :: Maybe ApplePayPayment
noApplePayPayment = Nothing
{-# INLINE noApplePayPayment #-}

gTypeApplePayPayment :: JSM GType
gTypeApplePayPayment = GType . Object <$> jsg "ApplePayPayment"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentAuthorizationResult".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentAuthorizationResult Mozilla ApplePayPaymentAuthorizationResult documentation>
newtype ApplePayPaymentAuthorizationResult = ApplePayPaymentAuthorizationResult { unApplePayPaymentAuthorizationResult :: JSVal }

instance PToJSVal ApplePayPaymentAuthorizationResult where
  pToJSVal = unApplePayPaymentAuthorizationResult
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentAuthorizationResult where
  pFromJSVal = ApplePayPaymentAuthorizationResult
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentAuthorizationResult where
  toJSVal = return . unApplePayPaymentAuthorizationResult
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentAuthorizationResult where
  fromJSVal v = fmap ApplePayPaymentAuthorizationResult <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentAuthorizationResult
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentAuthorizationResult where
  makeObject = makeObject . unApplePayPaymentAuthorizationResult

instance IsGObject ApplePayPaymentAuthorizationResult where
  typeGType _ = gTypeApplePayPaymentAuthorizationResult
  {-# INLINE typeGType #-}

noApplePayPaymentAuthorizationResult :: Maybe ApplePayPaymentAuthorizationResult
noApplePayPaymentAuthorizationResult = Nothing
{-# INLINE noApplePayPaymentAuthorizationResult #-}

gTypeApplePayPaymentAuthorizationResult :: JSM GType
gTypeApplePayPaymentAuthorizationResult = GType . Object <$> jsg "ApplePayPaymentAuthorizationResult"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentAuthorizedEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentAuthorizedEvent Mozilla ApplePayPaymentAuthorizedEvent documentation>
newtype ApplePayPaymentAuthorizedEvent = ApplePayPaymentAuthorizedEvent { unApplePayPaymentAuthorizedEvent :: JSVal }

instance PToJSVal ApplePayPaymentAuthorizedEvent where
  pToJSVal = unApplePayPaymentAuthorizedEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentAuthorizedEvent where
  pFromJSVal = ApplePayPaymentAuthorizedEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentAuthorizedEvent where
  toJSVal = return . unApplePayPaymentAuthorizedEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentAuthorizedEvent where
  fromJSVal v = fmap ApplePayPaymentAuthorizedEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentAuthorizedEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentAuthorizedEvent where
  makeObject = makeObject . unApplePayPaymentAuthorizedEvent

instance IsEvent ApplePayPaymentAuthorizedEvent
instance IsGObject ApplePayPaymentAuthorizedEvent where
  typeGType _ = gTypeApplePayPaymentAuthorizedEvent
  {-# INLINE typeGType #-}

noApplePayPaymentAuthorizedEvent :: Maybe ApplePayPaymentAuthorizedEvent
noApplePayPaymentAuthorizedEvent = Nothing
{-# INLINE noApplePayPaymentAuthorizedEvent #-}

gTypeApplePayPaymentAuthorizedEvent :: JSM GType
gTypeApplePayPaymentAuthorizedEvent = GType . Object <$> jsg "ApplePayPaymentAuthorizedEvent"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentContact".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentContact Mozilla ApplePayPaymentContact documentation>
newtype ApplePayPaymentContact = ApplePayPaymentContact { unApplePayPaymentContact :: JSVal }

instance PToJSVal ApplePayPaymentContact where
  pToJSVal = unApplePayPaymentContact
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentContact where
  pFromJSVal = ApplePayPaymentContact
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentContact where
  toJSVal = return . unApplePayPaymentContact
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentContact where
  fromJSVal v = fmap ApplePayPaymentContact <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentContact
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentContact where
  makeObject = makeObject . unApplePayPaymentContact

instance IsGObject ApplePayPaymentContact where
  typeGType _ = gTypeApplePayPaymentContact
  {-# INLINE typeGType #-}

noApplePayPaymentContact :: Maybe ApplePayPaymentContact
noApplePayPaymentContact = Nothing
{-# INLINE noApplePayPaymentContact #-}

gTypeApplePayPaymentContact :: JSM GType
gTypeApplePayPaymentContact = GType . Object <$> jsg "ApplePayPaymentContact"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentMethod".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentMethod Mozilla ApplePayPaymentMethod documentation>
newtype ApplePayPaymentMethod = ApplePayPaymentMethod { unApplePayPaymentMethod :: JSVal }

instance PToJSVal ApplePayPaymentMethod where
  pToJSVal = unApplePayPaymentMethod
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentMethod where
  pFromJSVal = ApplePayPaymentMethod
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentMethod where
  toJSVal = return . unApplePayPaymentMethod
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentMethod where
  fromJSVal v = fmap ApplePayPaymentMethod <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentMethod
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentMethod where
  makeObject = makeObject . unApplePayPaymentMethod

instance IsGObject ApplePayPaymentMethod where
  typeGType _ = gTypeApplePayPaymentMethod
  {-# INLINE typeGType #-}

noApplePayPaymentMethod :: Maybe ApplePayPaymentMethod
noApplePayPaymentMethod = Nothing
{-# INLINE noApplePayPaymentMethod #-}

gTypeApplePayPaymentMethod :: JSM GType
gTypeApplePayPaymentMethod = GType . Object <$> jsg "ApplePayPaymentMethod"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentMethodSelectedEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentMethodSelectedEvent Mozilla ApplePayPaymentMethodSelectedEvent documentation>
newtype ApplePayPaymentMethodSelectedEvent = ApplePayPaymentMethodSelectedEvent { unApplePayPaymentMethodSelectedEvent :: JSVal }

instance PToJSVal ApplePayPaymentMethodSelectedEvent where
  pToJSVal = unApplePayPaymentMethodSelectedEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentMethodSelectedEvent where
  pFromJSVal = ApplePayPaymentMethodSelectedEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentMethodSelectedEvent where
  toJSVal = return . unApplePayPaymentMethodSelectedEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentMethodSelectedEvent where
  fromJSVal v = fmap ApplePayPaymentMethodSelectedEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentMethodSelectedEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentMethodSelectedEvent where
  makeObject = makeObject . unApplePayPaymentMethodSelectedEvent

instance IsEvent ApplePayPaymentMethodSelectedEvent
instance IsGObject ApplePayPaymentMethodSelectedEvent where
  typeGType _ = gTypeApplePayPaymentMethodSelectedEvent
  {-# INLINE typeGType #-}

noApplePayPaymentMethodSelectedEvent :: Maybe ApplePayPaymentMethodSelectedEvent
noApplePayPaymentMethodSelectedEvent = Nothing
{-# INLINE noApplePayPaymentMethodSelectedEvent #-}

gTypeApplePayPaymentMethodSelectedEvent :: JSM GType
gTypeApplePayPaymentMethodSelectedEvent = GType . Object <$> jsg "ApplePayPaymentMethodSelectedEvent"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentMethodUpdate".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentMethodUpdate Mozilla ApplePayPaymentMethodUpdate documentation>
newtype ApplePayPaymentMethodUpdate = ApplePayPaymentMethodUpdate { unApplePayPaymentMethodUpdate :: JSVal }

instance PToJSVal ApplePayPaymentMethodUpdate where
  pToJSVal = unApplePayPaymentMethodUpdate
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentMethodUpdate where
  pFromJSVal = ApplePayPaymentMethodUpdate
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentMethodUpdate where
  toJSVal = return . unApplePayPaymentMethodUpdate
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentMethodUpdate where
  fromJSVal v = fmap ApplePayPaymentMethodUpdate <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentMethodUpdate
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentMethodUpdate where
  makeObject = makeObject . unApplePayPaymentMethodUpdate

instance IsGObject ApplePayPaymentMethodUpdate where
  typeGType _ = gTypeApplePayPaymentMethodUpdate
  {-# INLINE typeGType #-}

noApplePayPaymentMethodUpdate :: Maybe ApplePayPaymentMethodUpdate
noApplePayPaymentMethodUpdate = Nothing
{-# INLINE noApplePayPaymentMethodUpdate #-}

gTypeApplePayPaymentMethodUpdate :: JSM GType
gTypeApplePayPaymentMethodUpdate = GType . Object <$> jsg "ApplePayPaymentMethodUpdate"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentPass".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentPass Mozilla ApplePayPaymentPass documentation>
newtype ApplePayPaymentPass = ApplePayPaymentPass { unApplePayPaymentPass :: JSVal }

instance PToJSVal ApplePayPaymentPass where
  pToJSVal = unApplePayPaymentPass
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentPass where
  pFromJSVal = ApplePayPaymentPass
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentPass where
  toJSVal = return . unApplePayPaymentPass
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentPass where
  fromJSVal v = fmap ApplePayPaymentPass <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentPass
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentPass where
  makeObject = makeObject . unApplePayPaymentPass

instance IsGObject ApplePayPaymentPass where
  typeGType _ = gTypeApplePayPaymentPass
  {-# INLINE typeGType #-}

noApplePayPaymentPass :: Maybe ApplePayPaymentPass
noApplePayPaymentPass = Nothing
{-# INLINE noApplePayPaymentPass #-}

gTypeApplePayPaymentPass :: JSM GType
gTypeApplePayPaymentPass = GType . Object <$> jsg "ApplePayPaymentPass"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentRequest".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentRequest Mozilla ApplePayPaymentRequest documentation>
newtype ApplePayPaymentRequest = ApplePayPaymentRequest { unApplePayPaymentRequest :: JSVal }

instance PToJSVal ApplePayPaymentRequest where
  pToJSVal = unApplePayPaymentRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentRequest where
  pFromJSVal = ApplePayPaymentRequest
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentRequest where
  toJSVal = return . unApplePayPaymentRequest
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentRequest where
  fromJSVal v = fmap ApplePayPaymentRequest <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentRequest
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentRequest where
  makeObject = makeObject . unApplePayPaymentRequest

instance IsGObject ApplePayPaymentRequest where
  typeGType _ = gTypeApplePayPaymentRequest
  {-# INLINE typeGType #-}

noApplePayPaymentRequest :: Maybe ApplePayPaymentRequest
noApplePayPaymentRequest = Nothing
{-# INLINE noApplePayPaymentRequest #-}

gTypeApplePayPaymentRequest :: JSM GType
gTypeApplePayPaymentRequest = GType . Object <$> jsg "ApplePayPaymentRequest"

-- | Functions for this inteface are in "JSDOM.ApplePayPaymentToken".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayPaymentToken Mozilla ApplePayPaymentToken documentation>
newtype ApplePayPaymentToken = ApplePayPaymentToken { unApplePayPaymentToken :: JSVal }

instance PToJSVal ApplePayPaymentToken where
  pToJSVal = unApplePayPaymentToken
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayPaymentToken where
  pFromJSVal = ApplePayPaymentToken
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayPaymentToken where
  toJSVal = return . unApplePayPaymentToken
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayPaymentToken where
  fromJSVal v = fmap ApplePayPaymentToken <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayPaymentToken
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayPaymentToken where
  makeObject = makeObject . unApplePayPaymentToken

instance IsGObject ApplePayPaymentToken where
  typeGType _ = gTypeApplePayPaymentToken
  {-# INLINE typeGType #-}

noApplePayPaymentToken :: Maybe ApplePayPaymentToken
noApplePayPaymentToken = Nothing
{-# INLINE noApplePayPaymentToken #-}

gTypeApplePayPaymentToken :: JSM GType
gTypeApplePayPaymentToken = GType . Object <$> jsg "ApplePayPaymentToken"

-- | Functions for this inteface are in "JSDOM.ApplePaySession".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePaySession Mozilla ApplePaySession documentation>
newtype ApplePaySession = ApplePaySession { unApplePaySession :: JSVal }

instance PToJSVal ApplePaySession where
  pToJSVal = unApplePaySession
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePaySession where
  pFromJSVal = ApplePaySession
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePaySession where
  toJSVal = return . unApplePaySession
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePaySession where
  fromJSVal v = fmap ApplePaySession <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePaySession
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePaySession where
  makeObject = makeObject . unApplePaySession

instance IsEventTarget ApplePaySession
instance IsGObject ApplePaySession where
  typeGType _ = gTypeApplePaySession
  {-# INLINE typeGType #-}

noApplePaySession :: Maybe ApplePaySession
noApplePaySession = Nothing
{-# INLINE noApplePaySession #-}

gTypeApplePaySession :: JSM GType
gTypeApplePaySession = GType . Object <$> jsg "ApplePaySession"

-- | Functions for this inteface are in "JSDOM.ApplePayShippingContactSelectedEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayShippingContactSelectedEvent Mozilla ApplePayShippingContactSelectedEvent documentation>
newtype ApplePayShippingContactSelectedEvent = ApplePayShippingContactSelectedEvent { unApplePayShippingContactSelectedEvent :: JSVal }

instance PToJSVal ApplePayShippingContactSelectedEvent where
  pToJSVal = unApplePayShippingContactSelectedEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayShippingContactSelectedEvent where
  pFromJSVal = ApplePayShippingContactSelectedEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayShippingContactSelectedEvent where
  toJSVal = return . unApplePayShippingContactSelectedEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayShippingContactSelectedEvent where
  fromJSVal v = fmap ApplePayShippingContactSelectedEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayShippingContactSelectedEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayShippingContactSelectedEvent where
  makeObject = makeObject . unApplePayShippingContactSelectedEvent

instance IsEvent ApplePayShippingContactSelectedEvent
instance IsGObject ApplePayShippingContactSelectedEvent where
  typeGType _ = gTypeApplePayShippingContactSelectedEvent
  {-# INLINE typeGType #-}

noApplePayShippingContactSelectedEvent :: Maybe ApplePayShippingContactSelectedEvent
noApplePayShippingContactSelectedEvent = Nothing
{-# INLINE noApplePayShippingContactSelectedEvent #-}

gTypeApplePayShippingContactSelectedEvent :: JSM GType
gTypeApplePayShippingContactSelectedEvent = GType . Object <$> jsg "ApplePayShippingContactSelectedEvent"

-- | Functions for this inteface are in "JSDOM.ApplePayShippingContactUpdate".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayShippingContactUpdate Mozilla ApplePayShippingContactUpdate documentation>
newtype ApplePayShippingContactUpdate = ApplePayShippingContactUpdate { unApplePayShippingContactUpdate :: JSVal }

instance PToJSVal ApplePayShippingContactUpdate where
  pToJSVal = unApplePayShippingContactUpdate
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayShippingContactUpdate where
  pFromJSVal = ApplePayShippingContactUpdate
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayShippingContactUpdate where
  toJSVal = return . unApplePayShippingContactUpdate
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayShippingContactUpdate where
  fromJSVal v = fmap ApplePayShippingContactUpdate <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayShippingContactUpdate
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayShippingContactUpdate where
  makeObject = makeObject . unApplePayShippingContactUpdate

instance IsGObject ApplePayShippingContactUpdate where
  typeGType _ = gTypeApplePayShippingContactUpdate
  {-# INLINE typeGType #-}

noApplePayShippingContactUpdate :: Maybe ApplePayShippingContactUpdate
noApplePayShippingContactUpdate = Nothing
{-# INLINE noApplePayShippingContactUpdate #-}

gTypeApplePayShippingContactUpdate :: JSM GType
gTypeApplePayShippingContactUpdate = GType . Object <$> jsg "ApplePayShippingContactUpdate"

-- | Functions for this inteface are in "JSDOM.ApplePayShippingMethod".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayShippingMethod Mozilla ApplePayShippingMethod documentation>
newtype ApplePayShippingMethod = ApplePayShippingMethod { unApplePayShippingMethod :: JSVal }

instance PToJSVal ApplePayShippingMethod where
  pToJSVal = unApplePayShippingMethod
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayShippingMethod where
  pFromJSVal = ApplePayShippingMethod
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayShippingMethod where
  toJSVal = return . unApplePayShippingMethod
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayShippingMethod where
  fromJSVal v = fmap ApplePayShippingMethod <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayShippingMethod
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayShippingMethod where
  makeObject = makeObject . unApplePayShippingMethod

instance IsGObject ApplePayShippingMethod where
  typeGType _ = gTypeApplePayShippingMethod
  {-# INLINE typeGType #-}

noApplePayShippingMethod :: Maybe ApplePayShippingMethod
noApplePayShippingMethod = Nothing
{-# INLINE noApplePayShippingMethod #-}

gTypeApplePayShippingMethod :: JSM GType
gTypeApplePayShippingMethod = GType . Object <$> jsg "ApplePayShippingMethod"

-- | Functions for this inteface are in "JSDOM.ApplePayShippingMethodSelectedEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayShippingMethodSelectedEvent Mozilla ApplePayShippingMethodSelectedEvent documentation>
newtype ApplePayShippingMethodSelectedEvent = ApplePayShippingMethodSelectedEvent { unApplePayShippingMethodSelectedEvent :: JSVal }

instance PToJSVal ApplePayShippingMethodSelectedEvent where
  pToJSVal = unApplePayShippingMethodSelectedEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayShippingMethodSelectedEvent where
  pFromJSVal = ApplePayShippingMethodSelectedEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayShippingMethodSelectedEvent where
  toJSVal = return . unApplePayShippingMethodSelectedEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayShippingMethodSelectedEvent where
  fromJSVal v = fmap ApplePayShippingMethodSelectedEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayShippingMethodSelectedEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayShippingMethodSelectedEvent where
  makeObject = makeObject . unApplePayShippingMethodSelectedEvent

instance IsEvent ApplePayShippingMethodSelectedEvent
instance IsGObject ApplePayShippingMethodSelectedEvent where
  typeGType _ = gTypeApplePayShippingMethodSelectedEvent
  {-# INLINE typeGType #-}

noApplePayShippingMethodSelectedEvent :: Maybe ApplePayShippingMethodSelectedEvent
noApplePayShippingMethodSelectedEvent = Nothing
{-# INLINE noApplePayShippingMethodSelectedEvent #-}

gTypeApplePayShippingMethodSelectedEvent :: JSM GType
gTypeApplePayShippingMethodSelectedEvent = GType . Object <$> jsg "ApplePayShippingMethodSelectedEvent"

-- | Functions for this inteface are in "JSDOM.ApplePayShippingMethodUpdate".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayShippingMethodUpdate Mozilla ApplePayShippingMethodUpdate documentation>
newtype ApplePayShippingMethodUpdate = ApplePayShippingMethodUpdate { unApplePayShippingMethodUpdate :: JSVal }

instance PToJSVal ApplePayShippingMethodUpdate where
  pToJSVal = unApplePayShippingMethodUpdate
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayShippingMethodUpdate where
  pFromJSVal = ApplePayShippingMethodUpdate
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayShippingMethodUpdate where
  toJSVal = return . unApplePayShippingMethodUpdate
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayShippingMethodUpdate where
  fromJSVal v = fmap ApplePayShippingMethodUpdate <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayShippingMethodUpdate
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayShippingMethodUpdate where
  makeObject = makeObject . unApplePayShippingMethodUpdate

instance IsGObject ApplePayShippingMethodUpdate where
  typeGType _ = gTypeApplePayShippingMethodUpdate
  {-# INLINE typeGType #-}

noApplePayShippingMethodUpdate :: Maybe ApplePayShippingMethodUpdate
noApplePayShippingMethodUpdate = Nothing
{-# INLINE noApplePayShippingMethodUpdate #-}

gTypeApplePayShippingMethodUpdate :: JSM GType
gTypeApplePayShippingMethodUpdate = GType . Object <$> jsg "ApplePayShippingMethodUpdate"

-- | Functions for this inteface are in "JSDOM.ApplePayValidateMerchantEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplePayValidateMerchantEvent Mozilla ApplePayValidateMerchantEvent documentation>
newtype ApplePayValidateMerchantEvent = ApplePayValidateMerchantEvent { unApplePayValidateMerchantEvent :: JSVal }

instance PToJSVal ApplePayValidateMerchantEvent where
  pToJSVal = unApplePayValidateMerchantEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplePayValidateMerchantEvent where
  pFromJSVal = ApplePayValidateMerchantEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplePayValidateMerchantEvent where
  toJSVal = return . unApplePayValidateMerchantEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ApplePayValidateMerchantEvent where
  fromJSVal v = fmap ApplePayValidateMerchantEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplePayValidateMerchantEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplePayValidateMerchantEvent where
  makeObject = makeObject . unApplePayValidateMerchantEvent

instance IsEvent ApplePayValidateMerchantEvent
instance IsGObject ApplePayValidateMerchantEvent where
  typeGType _ = gTypeApplePayValidateMerchantEvent
  {-# INLINE typeGType #-}

noApplePayValidateMerchantEvent :: Maybe ApplePayValidateMerchantEvent
noApplePayValidateMerchantEvent = Nothing
{-# INLINE noApplePayValidateMerchantEvent #-}

gTypeApplePayValidateMerchantEvent :: JSM GType
gTypeApplePayValidateMerchantEvent = GType . Object <$> jsg "ApplePayValidateMerchantEvent"

-- | Functions for this inteface are in "JSDOM.ApplicationCache".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ApplicationCache Mozilla ApplicationCache documentation>
newtype ApplicationCache = ApplicationCache { unApplicationCache :: JSVal }

instance PToJSVal ApplicationCache where
  pToJSVal = unApplicationCache
  {-# INLINE pToJSVal #-}

instance PFromJSVal ApplicationCache where
  pFromJSVal = ApplicationCache
  {-# INLINE pFromJSVal #-}

instance ToJSVal ApplicationCache where
  toJSVal = return . unApplicationCache
  {-# INLINE toJSVal #-}

instance FromJSVal ApplicationCache where
  fromJSVal v = fmap ApplicationCache <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ApplicationCache
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ApplicationCache where
  makeObject = makeObject . unApplicationCache

instance IsEventTarget ApplicationCache
instance IsGObject ApplicationCache where
  typeGType _ = gTypeApplicationCache
  {-# INLINE typeGType #-}

noApplicationCache :: Maybe ApplicationCache
noApplicationCache = Nothing
{-# INLINE noApplicationCache #-}

gTypeApplicationCache :: JSM GType
gTypeApplicationCache = GType . Object <$> jsg "ApplicationCache"

-- | Functions for this inteface are in "JSDOM.AssignedNodesOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AssignedNodesOptions Mozilla AssignedNodesOptions documentation>
newtype AssignedNodesOptions = AssignedNodesOptions { unAssignedNodesOptions :: JSVal }

instance PToJSVal AssignedNodesOptions where
  pToJSVal = unAssignedNodesOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal AssignedNodesOptions where
  pFromJSVal = AssignedNodesOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal AssignedNodesOptions where
  toJSVal = return . unAssignedNodesOptions
  {-# INLINE toJSVal #-}

instance FromJSVal AssignedNodesOptions where
  fromJSVal v = fmap AssignedNodesOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AssignedNodesOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AssignedNodesOptions where
  makeObject = makeObject . unAssignedNodesOptions

instance IsGObject AssignedNodesOptions where
  typeGType _ = gTypeAssignedNodesOptions
  {-# INLINE typeGType #-}

noAssignedNodesOptions :: Maybe AssignedNodesOptions
noAssignedNodesOptions = Nothing
{-# INLINE noAssignedNodesOptions #-}

gTypeAssignedNodesOptions :: JSM GType
gTypeAssignedNodesOptions = GType . Object <$> jsg "AssignedNodesOptions"

-- | Functions for this inteface are in "JSDOM.AudioBuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioBuffer Mozilla AudioBuffer documentation>
newtype AudioBuffer = AudioBuffer { unAudioBuffer :: JSVal }

instance PToJSVal AudioBuffer where
  pToJSVal = unAudioBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioBuffer where
  pFromJSVal = AudioBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioBuffer where
  toJSVal = return . unAudioBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal AudioBuffer where
  fromJSVal v = fmap AudioBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioBuffer where
  makeObject = makeObject . unAudioBuffer

instance IsGObject AudioBuffer where
  typeGType _ = gTypeAudioBuffer
  {-# INLINE typeGType #-}

noAudioBuffer :: Maybe AudioBuffer
noAudioBuffer = Nothing
{-# INLINE noAudioBuffer #-}

gTypeAudioBuffer :: JSM GType
gTypeAudioBuffer = GType . Object <$> jsg "AudioBuffer"

-- | Functions for this inteface are in "JSDOM.AudioBufferSourceNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioBufferSourceNode Mozilla AudioBufferSourceNode documentation>
newtype AudioBufferSourceNode = AudioBufferSourceNode { unAudioBufferSourceNode :: JSVal }

instance PToJSVal AudioBufferSourceNode where
  pToJSVal = unAudioBufferSourceNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioBufferSourceNode where
  pFromJSVal = AudioBufferSourceNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioBufferSourceNode where
  toJSVal = return . unAudioBufferSourceNode
  {-# INLINE toJSVal #-}

instance FromJSVal AudioBufferSourceNode where
  fromJSVal v = fmap AudioBufferSourceNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioBufferSourceNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioBufferSourceNode where
  makeObject = makeObject . unAudioBufferSourceNode

instance IsAudioNode AudioBufferSourceNode
instance IsEventTarget AudioBufferSourceNode
instance IsGObject AudioBufferSourceNode where
  typeGType _ = gTypeAudioBufferSourceNode
  {-# INLINE typeGType #-}

noAudioBufferSourceNode :: Maybe AudioBufferSourceNode
noAudioBufferSourceNode = Nothing
{-# INLINE noAudioBufferSourceNode #-}

gTypeAudioBufferSourceNode :: JSM GType
gTypeAudioBufferSourceNode = GType . Object <$> jsg "AudioBufferSourceNode"

-- | Functions for this inteface are in "JSDOM.AudioContext".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioContext Mozilla AudioContext documentation>
newtype AudioContext = AudioContext { unAudioContext :: JSVal }

instance PToJSVal AudioContext where
  pToJSVal = unAudioContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioContext where
  pFromJSVal = AudioContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioContext where
  toJSVal = return . unAudioContext
  {-# INLINE toJSVal #-}

instance FromJSVal AudioContext where
  fromJSVal v = fmap AudioContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioContext where
  makeObject = makeObject . unAudioContext

class (IsEventTarget o, IsGObject o) => IsAudioContext o
toAudioContext :: IsAudioContext o => o -> AudioContext
toAudioContext = AudioContext . coerce

instance IsAudioContext AudioContext
instance IsEventTarget AudioContext
instance IsGObject AudioContext where
  typeGType _ = gTypeAudioContext
  {-# INLINE typeGType #-}

noAudioContext :: Maybe AudioContext
noAudioContext = Nothing
{-# INLINE noAudioContext #-}

gTypeAudioContext :: JSM GType
gTypeAudioContext = GType . Object <$> jsg "AudioContext"

-- | Functions for this inteface are in "JSDOM.AudioDestinationNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioDestinationNode Mozilla AudioDestinationNode documentation>
newtype AudioDestinationNode = AudioDestinationNode { unAudioDestinationNode :: JSVal }

instance PToJSVal AudioDestinationNode where
  pToJSVal = unAudioDestinationNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioDestinationNode where
  pFromJSVal = AudioDestinationNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioDestinationNode where
  toJSVal = return . unAudioDestinationNode
  {-# INLINE toJSVal #-}

instance FromJSVal AudioDestinationNode where
  fromJSVal v = fmap AudioDestinationNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioDestinationNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioDestinationNode where
  makeObject = makeObject . unAudioDestinationNode

instance IsAudioNode AudioDestinationNode
instance IsEventTarget AudioDestinationNode
instance IsGObject AudioDestinationNode where
  typeGType _ = gTypeAudioDestinationNode
  {-# INLINE typeGType #-}

noAudioDestinationNode :: Maybe AudioDestinationNode
noAudioDestinationNode = Nothing
{-# INLINE noAudioDestinationNode #-}

gTypeAudioDestinationNode :: JSM GType
gTypeAudioDestinationNode = GType . Object <$> jsg "AudioDestinationNode"

-- | Functions for this inteface are in "JSDOM.AudioListener".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioListener Mozilla AudioListener documentation>
newtype AudioListener = AudioListener { unAudioListener :: JSVal }

instance PToJSVal AudioListener where
  pToJSVal = unAudioListener
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioListener where
  pFromJSVal = AudioListener
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioListener where
  toJSVal = return . unAudioListener
  {-# INLINE toJSVal #-}

instance FromJSVal AudioListener where
  fromJSVal v = fmap AudioListener <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioListener
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioListener where
  makeObject = makeObject . unAudioListener

instance IsGObject AudioListener where
  typeGType _ = gTypeAudioListener
  {-# INLINE typeGType #-}

noAudioListener :: Maybe AudioListener
noAudioListener = Nothing
{-# INLINE noAudioListener #-}

gTypeAudioListener :: JSM GType
gTypeAudioListener = GType . Object <$> jsg "AudioListener"

-- | Functions for this inteface are in "JSDOM.AudioNode".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioNode Mozilla AudioNode documentation>
newtype AudioNode = AudioNode { unAudioNode :: JSVal }

instance PToJSVal AudioNode where
  pToJSVal = unAudioNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioNode where
  pFromJSVal = AudioNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioNode where
  toJSVal = return . unAudioNode
  {-# INLINE toJSVal #-}

instance FromJSVal AudioNode where
  fromJSVal v = fmap AudioNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioNode where
  makeObject = makeObject . unAudioNode

class (IsEventTarget o, IsGObject o) => IsAudioNode o
toAudioNode :: IsAudioNode o => o -> AudioNode
toAudioNode = AudioNode . coerce

instance IsAudioNode AudioNode
instance IsEventTarget AudioNode
instance IsGObject AudioNode where
  typeGType _ = gTypeAudioNode
  {-# INLINE typeGType #-}

noAudioNode :: Maybe AudioNode
noAudioNode = Nothing
{-# INLINE noAudioNode #-}

gTypeAudioNode :: JSM GType
gTypeAudioNode = GType . Object <$> jsg "AudioNode"

-- | Functions for this inteface are in "JSDOM.AudioParam".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioParam Mozilla AudioParam documentation>
newtype AudioParam = AudioParam { unAudioParam :: JSVal }

instance PToJSVal AudioParam where
  pToJSVal = unAudioParam
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioParam where
  pFromJSVal = AudioParam
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioParam where
  toJSVal = return . unAudioParam
  {-# INLINE toJSVal #-}

instance FromJSVal AudioParam where
  fromJSVal v = fmap AudioParam <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioParam
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioParam where
  makeObject = makeObject . unAudioParam

instance IsGObject AudioParam where
  typeGType _ = gTypeAudioParam
  {-# INLINE typeGType #-}

noAudioParam :: Maybe AudioParam
noAudioParam = Nothing
{-# INLINE noAudioParam #-}

gTypeAudioParam :: JSM GType
gTypeAudioParam = GType . Object <$> jsg "AudioParam"

-- | Functions for this inteface are in "JSDOM.AudioProcessingEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioProcessingEvent Mozilla AudioProcessingEvent documentation>
newtype AudioProcessingEvent = AudioProcessingEvent { unAudioProcessingEvent :: JSVal }

instance PToJSVal AudioProcessingEvent where
  pToJSVal = unAudioProcessingEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioProcessingEvent where
  pFromJSVal = AudioProcessingEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioProcessingEvent where
  toJSVal = return . unAudioProcessingEvent
  {-# INLINE toJSVal #-}

instance FromJSVal AudioProcessingEvent where
  fromJSVal v = fmap AudioProcessingEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioProcessingEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioProcessingEvent where
  makeObject = makeObject . unAudioProcessingEvent

instance IsEvent AudioProcessingEvent
instance IsGObject AudioProcessingEvent where
  typeGType _ = gTypeAudioProcessingEvent
  {-# INLINE typeGType #-}

noAudioProcessingEvent :: Maybe AudioProcessingEvent
noAudioProcessingEvent = Nothing
{-# INLINE noAudioProcessingEvent #-}

gTypeAudioProcessingEvent :: JSM GType
gTypeAudioProcessingEvent = GType . Object <$> jsg "AudioProcessingEvent"

-- | Functions for this inteface are in "JSDOM.AudioTrackList".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AudioTrackList Mozilla AudioTrackList documentation>
newtype AudioTrackList = AudioTrackList { unAudioTrackList :: JSVal }

instance PToJSVal AudioTrackList where
  pToJSVal = unAudioTrackList
  {-# INLINE pToJSVal #-}

instance PFromJSVal AudioTrackList where
  pFromJSVal = AudioTrackList
  {-# INLINE pFromJSVal #-}

instance ToJSVal AudioTrackList where
  toJSVal = return . unAudioTrackList
  {-# INLINE toJSVal #-}

instance FromJSVal AudioTrackList where
  fromJSVal v = fmap AudioTrackList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AudioTrackList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AudioTrackList where
  makeObject = makeObject . unAudioTrackList

instance IsEventTarget AudioTrackList
instance IsGObject AudioTrackList where
  typeGType _ = gTypeAudioTrackList
  {-# INLINE typeGType #-}

noAudioTrackList :: Maybe AudioTrackList
noAudioTrackList = Nothing
{-# INLINE noAudioTrackList #-}

gTypeAudioTrackList :: JSM GType
gTypeAudioTrackList = GType . Object <$> jsg "AudioTrackList"

-- | Functions for this inteface are in "JSDOM.AutocompleteErrorEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AutocompleteErrorEvent Mozilla AutocompleteErrorEvent documentation>
newtype AutocompleteErrorEvent = AutocompleteErrorEvent { unAutocompleteErrorEvent :: JSVal }

instance PToJSVal AutocompleteErrorEvent where
  pToJSVal = unAutocompleteErrorEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal AutocompleteErrorEvent where
  pFromJSVal = AutocompleteErrorEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal AutocompleteErrorEvent where
  toJSVal = return . unAutocompleteErrorEvent
  {-# INLINE toJSVal #-}

instance FromJSVal AutocompleteErrorEvent where
  fromJSVal v = fmap AutocompleteErrorEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AutocompleteErrorEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AutocompleteErrorEvent where
  makeObject = makeObject . unAutocompleteErrorEvent

instance IsEvent AutocompleteErrorEvent
instance IsGObject AutocompleteErrorEvent where
  typeGType _ = gTypeAutocompleteErrorEvent
  {-# INLINE typeGType #-}

noAutocompleteErrorEvent :: Maybe AutocompleteErrorEvent
noAutocompleteErrorEvent = Nothing
{-# INLINE noAutocompleteErrorEvent #-}

gTypeAutocompleteErrorEvent :: JSM GType
gTypeAutocompleteErrorEvent = GType . Object <$> jsg "AutocompleteErrorEvent"

-- | Functions for this inteface are in "JSDOM.AutocompleteErrorEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/AutocompleteErrorEventInit Mozilla AutocompleteErrorEventInit documentation>
newtype AutocompleteErrorEventInit = AutocompleteErrorEventInit { unAutocompleteErrorEventInit :: JSVal }

instance PToJSVal AutocompleteErrorEventInit where
  pToJSVal = unAutocompleteErrorEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal AutocompleteErrorEventInit where
  pFromJSVal = AutocompleteErrorEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal AutocompleteErrorEventInit where
  toJSVal = return . unAutocompleteErrorEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal AutocompleteErrorEventInit where
  fromJSVal v = fmap AutocompleteErrorEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . AutocompleteErrorEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject AutocompleteErrorEventInit where
  makeObject = makeObject . unAutocompleteErrorEventInit

instance IsEventInit AutocompleteErrorEventInit
instance IsGObject AutocompleteErrorEventInit where
  typeGType _ = gTypeAutocompleteErrorEventInit
  {-# INLINE typeGType #-}

noAutocompleteErrorEventInit :: Maybe AutocompleteErrorEventInit
noAutocompleteErrorEventInit = Nothing
{-# INLINE noAutocompleteErrorEventInit #-}

gTypeAutocompleteErrorEventInit :: JSM GType
gTypeAutocompleteErrorEventInit = GType . Object <$> jsg "AutocompleteErrorEventInit"

-- | Functions for this inteface are in "JSDOM.BarProp".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BarProp Mozilla BarProp documentation>
newtype BarProp = BarProp { unBarProp :: JSVal }

instance PToJSVal BarProp where
  pToJSVal = unBarProp
  {-# INLINE pToJSVal #-}

instance PFromJSVal BarProp where
  pFromJSVal = BarProp
  {-# INLINE pFromJSVal #-}

instance ToJSVal BarProp where
  toJSVal = return . unBarProp
  {-# INLINE toJSVal #-}

instance FromJSVal BarProp where
  fromJSVal v = fmap BarProp <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BarProp
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BarProp where
  makeObject = makeObject . unBarProp

instance IsGObject BarProp where
  typeGType _ = gTypeBarProp
  {-# INLINE typeGType #-}

noBarProp :: Maybe BarProp
noBarProp = Nothing
{-# INLINE noBarProp #-}

gTypeBarProp :: JSM GType
gTypeBarProp = GType . Object <$> jsg "BarProp"

-- | Functions for this inteface are in "JSDOM.BasicCredential".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BasicCredential Mozilla BasicCredential documentation>
newtype BasicCredential = BasicCredential { unBasicCredential :: JSVal }

instance PToJSVal BasicCredential where
  pToJSVal = unBasicCredential
  {-# INLINE pToJSVal #-}

instance PFromJSVal BasicCredential where
  pFromJSVal = BasicCredential
  {-# INLINE pFromJSVal #-}

instance ToJSVal BasicCredential where
  toJSVal = return . unBasicCredential
  {-# INLINE toJSVal #-}

instance FromJSVal BasicCredential where
  fromJSVal v = fmap BasicCredential <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BasicCredential
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BasicCredential where
  makeObject = makeObject . unBasicCredential

class (IsGObject o) => IsBasicCredential o
toBasicCredential :: IsBasicCredential o => o -> BasicCredential
toBasicCredential = BasicCredential . coerce

instance IsBasicCredential BasicCredential
instance IsGObject BasicCredential where
  typeGType _ = gTypeBasicCredential
  {-# INLINE typeGType #-}

noBasicCredential :: Maybe BasicCredential
noBasicCredential = Nothing
{-# INLINE noBasicCredential #-}

gTypeBasicCredential :: JSM GType
gTypeBasicCredential = GType . Object <$> jsg "BasicCredential"

-- | Functions for this inteface are in "JSDOM.BeforeLoadEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BeforeLoadEvent Mozilla BeforeLoadEvent documentation>
newtype BeforeLoadEvent = BeforeLoadEvent { unBeforeLoadEvent :: JSVal }

instance PToJSVal BeforeLoadEvent where
  pToJSVal = unBeforeLoadEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal BeforeLoadEvent where
  pFromJSVal = BeforeLoadEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal BeforeLoadEvent where
  toJSVal = return . unBeforeLoadEvent
  {-# INLINE toJSVal #-}

instance FromJSVal BeforeLoadEvent where
  fromJSVal v = fmap BeforeLoadEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BeforeLoadEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BeforeLoadEvent where
  makeObject = makeObject . unBeforeLoadEvent

instance IsEvent BeforeLoadEvent
instance IsGObject BeforeLoadEvent where
  typeGType _ = gTypeBeforeLoadEvent
  {-# INLINE typeGType #-}

noBeforeLoadEvent :: Maybe BeforeLoadEvent
noBeforeLoadEvent = Nothing
{-# INLINE noBeforeLoadEvent #-}

gTypeBeforeLoadEvent :: JSM GType
gTypeBeforeLoadEvent = GType . Object <$> jsg "BeforeLoadEvent"

-- | Functions for this inteface are in "JSDOM.BeforeLoadEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BeforeLoadEventInit Mozilla BeforeLoadEventInit documentation>
newtype BeforeLoadEventInit = BeforeLoadEventInit { unBeforeLoadEventInit :: JSVal }

instance PToJSVal BeforeLoadEventInit where
  pToJSVal = unBeforeLoadEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal BeforeLoadEventInit where
  pFromJSVal = BeforeLoadEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal BeforeLoadEventInit where
  toJSVal = return . unBeforeLoadEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal BeforeLoadEventInit where
  fromJSVal v = fmap BeforeLoadEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BeforeLoadEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BeforeLoadEventInit where
  makeObject = makeObject . unBeforeLoadEventInit

instance IsEventInit BeforeLoadEventInit
instance IsGObject BeforeLoadEventInit where
  typeGType _ = gTypeBeforeLoadEventInit
  {-# INLINE typeGType #-}

noBeforeLoadEventInit :: Maybe BeforeLoadEventInit
noBeforeLoadEventInit = Nothing
{-# INLINE noBeforeLoadEventInit #-}

gTypeBeforeLoadEventInit :: JSM GType
gTypeBeforeLoadEventInit = GType . Object <$> jsg "BeforeLoadEventInit"

-- | Functions for this inteface are in "JSDOM.BeforeUnloadEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BeforeUnloadEvent Mozilla BeforeUnloadEvent documentation>
newtype BeforeUnloadEvent = BeforeUnloadEvent { unBeforeUnloadEvent :: JSVal }

instance PToJSVal BeforeUnloadEvent where
  pToJSVal = unBeforeUnloadEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal BeforeUnloadEvent where
  pFromJSVal = BeforeUnloadEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal BeforeUnloadEvent where
  toJSVal = return . unBeforeUnloadEvent
  {-# INLINE toJSVal #-}

instance FromJSVal BeforeUnloadEvent where
  fromJSVal v = fmap BeforeUnloadEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BeforeUnloadEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BeforeUnloadEvent where
  makeObject = makeObject . unBeforeUnloadEvent

instance IsEvent BeforeUnloadEvent
instance IsGObject BeforeUnloadEvent where
  typeGType _ = gTypeBeforeUnloadEvent
  {-# INLINE typeGType #-}

noBeforeUnloadEvent :: Maybe BeforeUnloadEvent
noBeforeUnloadEvent = Nothing
{-# INLINE noBeforeUnloadEvent #-}

gTypeBeforeUnloadEvent :: JSM GType
gTypeBeforeUnloadEvent = GType . Object <$> jsg "BeforeUnloadEvent"

-- | Functions for this inteface are in "JSDOM.BiquadFilterNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BiquadFilterNode Mozilla BiquadFilterNode documentation>
newtype BiquadFilterNode = BiquadFilterNode { unBiquadFilterNode :: JSVal }

instance PToJSVal BiquadFilterNode where
  pToJSVal = unBiquadFilterNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal BiquadFilterNode where
  pFromJSVal = BiquadFilterNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal BiquadFilterNode where
  toJSVal = return . unBiquadFilterNode
  {-# INLINE toJSVal #-}

instance FromJSVal BiquadFilterNode where
  fromJSVal v = fmap BiquadFilterNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BiquadFilterNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BiquadFilterNode where
  makeObject = makeObject . unBiquadFilterNode

instance IsAudioNode BiquadFilterNode
instance IsEventTarget BiquadFilterNode
instance IsGObject BiquadFilterNode where
  typeGType _ = gTypeBiquadFilterNode
  {-# INLINE typeGType #-}

noBiquadFilterNode :: Maybe BiquadFilterNode
noBiquadFilterNode = Nothing
{-# INLINE noBiquadFilterNode #-}

gTypeBiquadFilterNode :: JSM GType
gTypeBiquadFilterNode = GType . Object <$> jsg "BiquadFilterNode"

-- | Functions for this inteface are in "JSDOM.BlobPropertyBag".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/BlobPropertyBag Mozilla BlobPropertyBag documentation>
newtype BlobPropertyBag = BlobPropertyBag { unBlobPropertyBag :: JSVal }

instance PToJSVal BlobPropertyBag where
  pToJSVal = unBlobPropertyBag
  {-# INLINE pToJSVal #-}

instance PFromJSVal BlobPropertyBag where
  pFromJSVal = BlobPropertyBag
  {-# INLINE pFromJSVal #-}

instance ToJSVal BlobPropertyBag where
  toJSVal = return . unBlobPropertyBag
  {-# INLINE toJSVal #-}

instance FromJSVal BlobPropertyBag where
  fromJSVal v = fmap BlobPropertyBag <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . BlobPropertyBag
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject BlobPropertyBag where
  makeObject = makeObject . unBlobPropertyBag

class (IsGObject o) => IsBlobPropertyBag o
toBlobPropertyBag :: IsBlobPropertyBag o => o -> BlobPropertyBag
toBlobPropertyBag = BlobPropertyBag . coerce

instance IsBlobPropertyBag BlobPropertyBag
instance IsGObject BlobPropertyBag where
  typeGType _ = gTypeBlobPropertyBag
  {-# INLINE typeGType #-}

noBlobPropertyBag :: Maybe BlobPropertyBag
noBlobPropertyBag = Nothing
{-# INLINE noBlobPropertyBag #-}

gTypeBlobPropertyBag :: JSM GType
gTypeBlobPropertyBag = GType . Object <$> jsg "BlobPropertyBag"

-- | Functions for this inteface are in "JSDOM.Body".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Body Mozilla Body documentation>
newtype Body = Body { unBody :: JSVal }

instance PToJSVal Body where
  pToJSVal = unBody
  {-# INLINE pToJSVal #-}

instance PFromJSVal Body where
  pFromJSVal = Body
  {-# INLINE pFromJSVal #-}

instance ToJSVal Body where
  toJSVal = return . unBody
  {-# INLINE toJSVal #-}

instance FromJSVal Body where
  fromJSVal v = fmap Body <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Body
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Body where
  makeObject = makeObject . unBody

class (IsGObject o) => IsBody o
toBody :: IsBody o => o -> Body
toBody = Body . coerce

instance IsBody Body
instance IsGObject Body where
  typeGType _ = gTypeBody
  {-# INLINE typeGType #-}

noBody :: Maybe Body
noBody = Nothing
{-# INLINE noBody #-}

gTypeBody :: JSM GType
gTypeBody = GType . Object <$> jsg "Body"

-- | Functions for this inteface are in "JSDOM.ByteLengthQueuingStrategy".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ByteLengthQueuingStrategy Mozilla ByteLengthQueuingStrategy documentation>
newtype ByteLengthQueuingStrategy = ByteLengthQueuingStrategy { unByteLengthQueuingStrategy :: JSVal }

instance PToJSVal ByteLengthQueuingStrategy where
  pToJSVal = unByteLengthQueuingStrategy
  {-# INLINE pToJSVal #-}

instance PFromJSVal ByteLengthQueuingStrategy where
  pFromJSVal = ByteLengthQueuingStrategy
  {-# INLINE pFromJSVal #-}

instance ToJSVal ByteLengthQueuingStrategy where
  toJSVal = return . unByteLengthQueuingStrategy
  {-# INLINE toJSVal #-}

instance FromJSVal ByteLengthQueuingStrategy where
  fromJSVal v = fmap ByteLengthQueuingStrategy <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ByteLengthQueuingStrategy
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ByteLengthQueuingStrategy where
  makeObject = makeObject . unByteLengthQueuingStrategy

instance IsGObject ByteLengthQueuingStrategy where
  typeGType _ = gTypeByteLengthQueuingStrategy
  {-# INLINE typeGType #-}

noByteLengthQueuingStrategy :: Maybe ByteLengthQueuingStrategy
noByteLengthQueuingStrategy = Nothing
{-# INLINE noByteLengthQueuingStrategy #-}

gTypeByteLengthQueuingStrategy :: JSM GType
gTypeByteLengthQueuingStrategy = GType . Object <$> jsg "ByteLengthQueuingStrategy"


-- | Functions for this inteface are in "JSDOM.CSS".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSS Mozilla CSS documentation>
newtype CSS = CSS { unCSS :: JSVal }

instance PToJSVal CSS where
  pToJSVal = unCSS
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSS where
  pFromJSVal = CSS
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSS where
  toJSVal = return . unCSS
  {-# INLINE toJSVal #-}

instance FromJSVal CSS where
  fromJSVal v = fmap CSS <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSS
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSS where
  makeObject = makeObject . unCSS

instance IsGObject CSS where
  typeGType _ = gTypeCSS
  {-# INLINE typeGType #-}

noCSS :: Maybe CSS
noCSS = Nothing
{-# INLINE noCSS #-}

gTypeCSS :: JSM GType
gTypeCSS = GType . Object <$> jsg "CSS"

-- | Functions for this inteface are in "JSDOM.CSSFontFaceLoadEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSFontFaceLoadEvent Mozilla CSSFontFaceLoadEvent documentation>
newtype CSSFontFaceLoadEvent = CSSFontFaceLoadEvent { unCSSFontFaceLoadEvent :: JSVal }

instance PToJSVal CSSFontFaceLoadEvent where
  pToJSVal = unCSSFontFaceLoadEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSFontFaceLoadEvent where
  pFromJSVal = CSSFontFaceLoadEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSFontFaceLoadEvent where
  toJSVal = return . unCSSFontFaceLoadEvent
  {-# INLINE toJSVal #-}

instance FromJSVal CSSFontFaceLoadEvent where
  fromJSVal v = fmap CSSFontFaceLoadEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSFontFaceLoadEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSFontFaceLoadEvent where
  makeObject = makeObject . unCSSFontFaceLoadEvent

instance IsEvent CSSFontFaceLoadEvent
instance IsGObject CSSFontFaceLoadEvent where
  typeGType _ = gTypeCSSFontFaceLoadEvent
  {-# INLINE typeGType #-}

noCSSFontFaceLoadEvent :: Maybe CSSFontFaceLoadEvent
noCSSFontFaceLoadEvent = Nothing
{-# INLINE noCSSFontFaceLoadEvent #-}

gTypeCSSFontFaceLoadEvent :: JSM GType
gTypeCSSFontFaceLoadEvent = GType . Object <$> jsg "CSSFontFaceLoadEvent"

-- | Functions for this inteface are in "JSDOM.CSSFontFaceLoadEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSFontFaceLoadEventInit Mozilla CSSFontFaceLoadEventInit documentation>
newtype CSSFontFaceLoadEventInit = CSSFontFaceLoadEventInit { unCSSFontFaceLoadEventInit :: JSVal }

instance PToJSVal CSSFontFaceLoadEventInit where
  pToJSVal = unCSSFontFaceLoadEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSFontFaceLoadEventInit where
  pFromJSVal = CSSFontFaceLoadEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSFontFaceLoadEventInit where
  toJSVal = return . unCSSFontFaceLoadEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal CSSFontFaceLoadEventInit where
  fromJSVal v = fmap CSSFontFaceLoadEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSFontFaceLoadEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSFontFaceLoadEventInit where
  makeObject = makeObject . unCSSFontFaceLoadEventInit

instance IsEventInit CSSFontFaceLoadEventInit
instance IsGObject CSSFontFaceLoadEventInit where
  typeGType _ = gTypeCSSFontFaceLoadEventInit
  {-# INLINE typeGType #-}

noCSSFontFaceLoadEventInit :: Maybe CSSFontFaceLoadEventInit
noCSSFontFaceLoadEventInit = Nothing
{-# INLINE noCSSFontFaceLoadEventInit #-}

gTypeCSSFontFaceLoadEventInit :: JSM GType
gTypeCSSFontFaceLoadEventInit = GType . Object <$> jsg "CSSFontFaceLoadEventInit"

-- | Functions for this inteface are in "JSDOM.CSSFontFaceRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSFontFaceRule Mozilla CSSFontFaceRule documentation>
newtype CSSFontFaceRule = CSSFontFaceRule { unCSSFontFaceRule :: JSVal }

instance PToJSVal CSSFontFaceRule where
  pToJSVal = unCSSFontFaceRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSFontFaceRule where
  pFromJSVal = CSSFontFaceRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSFontFaceRule where
  toJSVal = return . unCSSFontFaceRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSFontFaceRule where
  fromJSVal v = fmap CSSFontFaceRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSFontFaceRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSFontFaceRule where
  makeObject = makeObject . unCSSFontFaceRule

instance IsCSSRule CSSFontFaceRule
instance IsGObject CSSFontFaceRule where
  typeGType _ = gTypeCSSFontFaceRule
  {-# INLINE typeGType #-}

noCSSFontFaceRule :: Maybe CSSFontFaceRule
noCSSFontFaceRule = Nothing
{-# INLINE noCSSFontFaceRule #-}

gTypeCSSFontFaceRule :: JSM GType
gTypeCSSFontFaceRule = GType . Object <$> jsg "CSSFontFaceRule"

-- | Functions for this inteface are in "JSDOM.CSSImportRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSImportRule Mozilla CSSImportRule documentation>
newtype CSSImportRule = CSSImportRule { unCSSImportRule :: JSVal }

instance PToJSVal CSSImportRule where
  pToJSVal = unCSSImportRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSImportRule where
  pFromJSVal = CSSImportRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSImportRule where
  toJSVal = return . unCSSImportRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSImportRule where
  fromJSVal v = fmap CSSImportRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSImportRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSImportRule where
  makeObject = makeObject . unCSSImportRule

instance IsCSSRule CSSImportRule
instance IsGObject CSSImportRule where
  typeGType _ = gTypeCSSImportRule
  {-# INLINE typeGType #-}

noCSSImportRule :: Maybe CSSImportRule
noCSSImportRule = Nothing
{-# INLINE noCSSImportRule #-}

gTypeCSSImportRule :: JSM GType
gTypeCSSImportRule = GType . Object <$> jsg "CSSImportRule"

-- | Functions for this inteface are in "JSDOM.CSSKeyframeRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSKeyframeRule Mozilla CSSKeyframeRule documentation>
newtype CSSKeyframeRule = CSSKeyframeRule { unCSSKeyframeRule :: JSVal }

instance PToJSVal CSSKeyframeRule where
  pToJSVal = unCSSKeyframeRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSKeyframeRule where
  pFromJSVal = CSSKeyframeRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSKeyframeRule where
  toJSVal = return . unCSSKeyframeRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSKeyframeRule where
  fromJSVal v = fmap CSSKeyframeRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSKeyframeRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSKeyframeRule where
  makeObject = makeObject . unCSSKeyframeRule

instance IsCSSRule CSSKeyframeRule
instance IsGObject CSSKeyframeRule where
  typeGType _ = gTypeCSSKeyframeRule
  {-# INLINE typeGType #-}

noCSSKeyframeRule :: Maybe CSSKeyframeRule
noCSSKeyframeRule = Nothing
{-# INLINE noCSSKeyframeRule #-}

gTypeCSSKeyframeRule :: JSM GType
gTypeCSSKeyframeRule = GType . Object <$> jsg "CSSKeyframeRule"

-- | Functions for this inteface are in "JSDOM.CSSKeyframesRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSKeyframesRule Mozilla CSSKeyframesRule documentation>
newtype CSSKeyframesRule = CSSKeyframesRule { unCSSKeyframesRule :: JSVal }

instance PToJSVal CSSKeyframesRule where
  pToJSVal = unCSSKeyframesRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSKeyframesRule where
  pFromJSVal = CSSKeyframesRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSKeyframesRule where
  toJSVal = return . unCSSKeyframesRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSKeyframesRule where
  fromJSVal v = fmap CSSKeyframesRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSKeyframesRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSKeyframesRule where
  makeObject = makeObject . unCSSKeyframesRule

instance IsCSSRule CSSKeyframesRule
instance IsGObject CSSKeyframesRule where
  typeGType _ = gTypeCSSKeyframesRule
  {-# INLINE typeGType #-}

noCSSKeyframesRule :: Maybe CSSKeyframesRule
noCSSKeyframesRule = Nothing
{-# INLINE noCSSKeyframesRule #-}

gTypeCSSKeyframesRule :: JSM GType
gTypeCSSKeyframesRule = GType . Object <$> jsg "CSSKeyframesRule"

-- | Functions for this inteface are in "JSDOM.CSSMediaRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSMediaRule Mozilla CSSMediaRule documentation>
newtype CSSMediaRule = CSSMediaRule { unCSSMediaRule :: JSVal }

instance PToJSVal CSSMediaRule where
  pToJSVal = unCSSMediaRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSMediaRule where
  pFromJSVal = CSSMediaRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSMediaRule where
  toJSVal = return . unCSSMediaRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSMediaRule where
  fromJSVal v = fmap CSSMediaRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSMediaRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSMediaRule where
  makeObject = makeObject . unCSSMediaRule

instance IsCSSRule CSSMediaRule
instance IsGObject CSSMediaRule where
  typeGType _ = gTypeCSSMediaRule
  {-# INLINE typeGType #-}

noCSSMediaRule :: Maybe CSSMediaRule
noCSSMediaRule = Nothing
{-# INLINE noCSSMediaRule #-}

gTypeCSSMediaRule :: JSM GType
gTypeCSSMediaRule = GType . Object <$> jsg "CSSMediaRule"

-- | Functions for this inteface are in "JSDOM.CSSNamespaceRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSNamespaceRule Mozilla CSSNamespaceRule documentation>
newtype CSSNamespaceRule = CSSNamespaceRule { unCSSNamespaceRule :: JSVal }

instance PToJSVal CSSNamespaceRule where
  pToJSVal = unCSSNamespaceRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSNamespaceRule where
  pFromJSVal = CSSNamespaceRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSNamespaceRule where
  toJSVal = return . unCSSNamespaceRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSNamespaceRule where
  fromJSVal v = fmap CSSNamespaceRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSNamespaceRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSNamespaceRule where
  makeObject = makeObject . unCSSNamespaceRule

instance IsCSSRule CSSNamespaceRule
instance IsGObject CSSNamespaceRule where
  typeGType _ = gTypeCSSNamespaceRule
  {-# INLINE typeGType #-}

noCSSNamespaceRule :: Maybe CSSNamespaceRule
noCSSNamespaceRule = Nothing
{-# INLINE noCSSNamespaceRule #-}

gTypeCSSNamespaceRule :: JSM GType
gTypeCSSNamespaceRule = GType . Object <$> jsg "CSSNamespaceRule"

-- | Functions for this inteface are in "JSDOM.CSSPageRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSPageRule Mozilla CSSPageRule documentation>
newtype CSSPageRule = CSSPageRule { unCSSPageRule :: JSVal }

instance PToJSVal CSSPageRule where
  pToJSVal = unCSSPageRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSPageRule where
  pFromJSVal = CSSPageRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSPageRule where
  toJSVal = return . unCSSPageRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSPageRule where
  fromJSVal v = fmap CSSPageRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSPageRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSPageRule where
  makeObject = makeObject . unCSSPageRule

instance IsCSSRule CSSPageRule
instance IsGObject CSSPageRule where
  typeGType _ = gTypeCSSPageRule
  {-# INLINE typeGType #-}

noCSSPageRule :: Maybe CSSPageRule
noCSSPageRule = Nothing
{-# INLINE noCSSPageRule #-}

gTypeCSSPageRule :: JSM GType
gTypeCSSPageRule = GType . Object <$> jsg "CSSPageRule"

-- | Functions for this inteface are in "JSDOM.CSSPrimitiveValue".
-- Base interface functions are in:
--
--     * "JSDOM.CSSValue"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSPrimitiveValue Mozilla CSSPrimitiveValue documentation>
newtype CSSPrimitiveValue = CSSPrimitiveValue { unCSSPrimitiveValue :: JSVal }

instance PToJSVal CSSPrimitiveValue where
  pToJSVal = unCSSPrimitiveValue
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSPrimitiveValue where
  pFromJSVal = CSSPrimitiveValue
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSPrimitiveValue where
  toJSVal = return . unCSSPrimitiveValue
  {-# INLINE toJSVal #-}

instance FromJSVal CSSPrimitiveValue where
  fromJSVal v = fmap CSSPrimitiveValue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSPrimitiveValue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSPrimitiveValue where
  makeObject = makeObject . unCSSPrimitiveValue

instance IsCSSValue CSSPrimitiveValue
instance IsGObject CSSPrimitiveValue where
  typeGType _ = gTypeCSSPrimitiveValue
  {-# INLINE typeGType #-}

noCSSPrimitiveValue :: Maybe CSSPrimitiveValue
noCSSPrimitiveValue = Nothing
{-# INLINE noCSSPrimitiveValue #-}

gTypeCSSPrimitiveValue :: JSM GType
gTypeCSSPrimitiveValue = GType . Object <$> jsg "CSSPrimitiveValue"

-- | Functions for this inteface are in "JSDOM.CSSRule".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSRule Mozilla CSSRule documentation>
newtype CSSRule = CSSRule { unCSSRule :: JSVal }

instance PToJSVal CSSRule where
  pToJSVal = unCSSRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSRule where
  pFromJSVal = CSSRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSRule where
  toJSVal = return . unCSSRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSRule where
  fromJSVal v = fmap CSSRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSRule where
  makeObject = makeObject . unCSSRule

class (IsGObject o) => IsCSSRule o
toCSSRule :: IsCSSRule o => o -> CSSRule
toCSSRule = CSSRule . coerce

instance IsCSSRule CSSRule
instance IsGObject CSSRule where
  typeGType _ = gTypeCSSRule
  {-# INLINE typeGType #-}

noCSSRule :: Maybe CSSRule
noCSSRule = Nothing
{-# INLINE noCSSRule #-}

gTypeCSSRule :: JSM GType
gTypeCSSRule = GType . Object <$> jsg "CSSRule"

-- | Functions for this inteface are in "JSDOM.CSSRuleList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSRuleList Mozilla CSSRuleList documentation>
newtype CSSRuleList = CSSRuleList { unCSSRuleList :: JSVal }

instance PToJSVal CSSRuleList where
  pToJSVal = unCSSRuleList
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSRuleList where
  pFromJSVal = CSSRuleList
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSRuleList where
  toJSVal = return . unCSSRuleList
  {-# INLINE toJSVal #-}

instance FromJSVal CSSRuleList where
  fromJSVal v = fmap CSSRuleList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSRuleList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSRuleList where
  makeObject = makeObject . unCSSRuleList

instance IsGObject CSSRuleList where
  typeGType _ = gTypeCSSRuleList
  {-# INLINE typeGType #-}

noCSSRuleList :: Maybe CSSRuleList
noCSSRuleList = Nothing
{-# INLINE noCSSRuleList #-}

gTypeCSSRuleList :: JSM GType
gTypeCSSRuleList = GType . Object <$> jsg "CSSRuleList"

-- | Functions for this inteface are in "JSDOM.CSSStyleDeclaration".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration Mozilla CSSStyleDeclaration documentation>
newtype CSSStyleDeclaration = CSSStyleDeclaration { unCSSStyleDeclaration :: JSVal }

instance PToJSVal CSSStyleDeclaration where
  pToJSVal = unCSSStyleDeclaration
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSStyleDeclaration where
  pFromJSVal = CSSStyleDeclaration
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSStyleDeclaration where
  toJSVal = return . unCSSStyleDeclaration
  {-# INLINE toJSVal #-}

instance FromJSVal CSSStyleDeclaration where
  fromJSVal v = fmap CSSStyleDeclaration <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSStyleDeclaration
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSStyleDeclaration where
  makeObject = makeObject . unCSSStyleDeclaration

instance IsGObject CSSStyleDeclaration where
  typeGType _ = gTypeCSSStyleDeclaration
  {-# INLINE typeGType #-}

noCSSStyleDeclaration :: Maybe CSSStyleDeclaration
noCSSStyleDeclaration = Nothing
{-# INLINE noCSSStyleDeclaration #-}

gTypeCSSStyleDeclaration :: JSM GType
gTypeCSSStyleDeclaration = GType . Object <$> jsg "CSSStyleDeclaration"

-- | Functions for this inteface are in "JSDOM.CSSStyleRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleRule Mozilla CSSStyleRule documentation>
newtype CSSStyleRule = CSSStyleRule { unCSSStyleRule :: JSVal }

instance PToJSVal CSSStyleRule where
  pToJSVal = unCSSStyleRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSStyleRule where
  pFromJSVal = CSSStyleRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSStyleRule where
  toJSVal = return . unCSSStyleRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSStyleRule where
  fromJSVal v = fmap CSSStyleRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSStyleRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSStyleRule where
  makeObject = makeObject . unCSSStyleRule

instance IsCSSRule CSSStyleRule
instance IsGObject CSSStyleRule where
  typeGType _ = gTypeCSSStyleRule
  {-# INLINE typeGType #-}

noCSSStyleRule :: Maybe CSSStyleRule
noCSSStyleRule = Nothing
{-# INLINE noCSSStyleRule #-}

gTypeCSSStyleRule :: JSM GType
gTypeCSSStyleRule = GType . Object <$> jsg "CSSStyleRule"

-- | Functions for this inteface are in "JSDOM.CSSStyleSheet".
-- Base interface functions are in:
--
--     * "JSDOM.StyleSheet"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleSheet Mozilla CSSStyleSheet documentation>
newtype CSSStyleSheet = CSSStyleSheet { unCSSStyleSheet :: JSVal }

instance PToJSVal CSSStyleSheet where
  pToJSVal = unCSSStyleSheet
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSStyleSheet where
  pFromJSVal = CSSStyleSheet
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSStyleSheet where
  toJSVal = return . unCSSStyleSheet
  {-# INLINE toJSVal #-}

instance FromJSVal CSSStyleSheet where
  fromJSVal v = fmap CSSStyleSheet <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSStyleSheet
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSStyleSheet where
  makeObject = makeObject . unCSSStyleSheet

instance IsStyleSheet CSSStyleSheet
instance IsGObject CSSStyleSheet where
  typeGType _ = gTypeCSSStyleSheet
  {-# INLINE typeGType #-}

noCSSStyleSheet :: Maybe CSSStyleSheet
noCSSStyleSheet = Nothing
{-# INLINE noCSSStyleSheet #-}

gTypeCSSStyleSheet :: JSM GType
gTypeCSSStyleSheet = GType . Object <$> jsg "CSSStyleSheet"

-- | Functions for this inteface are in "JSDOM.CSSSupportsRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSSupportsRule Mozilla CSSSupportsRule documentation>
newtype CSSSupportsRule = CSSSupportsRule { unCSSSupportsRule :: JSVal }

instance PToJSVal CSSSupportsRule where
  pToJSVal = unCSSSupportsRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSSupportsRule where
  pFromJSVal = CSSSupportsRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSSupportsRule where
  toJSVal = return . unCSSSupportsRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSSupportsRule where
  fromJSVal v = fmap CSSSupportsRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSSupportsRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSSupportsRule where
  makeObject = makeObject . unCSSSupportsRule

instance IsCSSRule CSSSupportsRule
instance IsGObject CSSSupportsRule where
  typeGType _ = gTypeCSSSupportsRule
  {-# INLINE typeGType #-}

noCSSSupportsRule :: Maybe CSSSupportsRule
noCSSSupportsRule = Nothing
{-# INLINE noCSSSupportsRule #-}

gTypeCSSSupportsRule :: JSM GType
gTypeCSSSupportsRule = GType . Object <$> jsg "CSSSupportsRule"

-- | Functions for this inteface are in "JSDOM.CSSUnknownRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSUnknownRule Mozilla CSSUnknownRule documentation>
newtype CSSUnknownRule = CSSUnknownRule { unCSSUnknownRule :: JSVal }

instance PToJSVal CSSUnknownRule where
  pToJSVal = unCSSUnknownRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSUnknownRule where
  pFromJSVal = CSSUnknownRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSUnknownRule where
  toJSVal = return . unCSSUnknownRule
  {-# INLINE toJSVal #-}

instance FromJSVal CSSUnknownRule where
  fromJSVal v = fmap CSSUnknownRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSUnknownRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSUnknownRule where
  makeObject = makeObject . unCSSUnknownRule

instance IsCSSRule CSSUnknownRule
instance IsGObject CSSUnknownRule where
  typeGType _ = gTypeCSSUnknownRule
  {-# INLINE typeGType #-}

noCSSUnknownRule :: Maybe CSSUnknownRule
noCSSUnknownRule = Nothing
{-# INLINE noCSSUnknownRule #-}

gTypeCSSUnknownRule :: JSM GType
gTypeCSSUnknownRule = GType . Object <$> jsg "CSSUnknownRule"

-- | Functions for this inteface are in "JSDOM.CSSValue".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSValue Mozilla CSSValue documentation>
newtype CSSValue = CSSValue { unCSSValue :: JSVal }

instance PToJSVal CSSValue where
  pToJSVal = unCSSValue
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSValue where
  pFromJSVal = CSSValue
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSValue where
  toJSVal = return . unCSSValue
  {-# INLINE toJSVal #-}

instance FromJSVal CSSValue where
  fromJSVal v = fmap CSSValue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSValue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSValue where
  makeObject = makeObject . unCSSValue

class (IsGObject o) => IsCSSValue o
toCSSValue :: IsCSSValue o => o -> CSSValue
toCSSValue = CSSValue . coerce

instance IsCSSValue CSSValue
instance IsGObject CSSValue where
  typeGType _ = gTypeCSSValue
  {-# INLINE typeGType #-}

noCSSValue :: Maybe CSSValue
noCSSValue = Nothing
{-# INLINE noCSSValue #-}

gTypeCSSValue :: JSM GType
gTypeCSSValue = GType . Object <$> jsg "CSSValue"

-- | Functions for this inteface are in "JSDOM.CSSValueList".
-- Base interface functions are in:
--
--     * "JSDOM.CSSValue"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CSSValueList Mozilla CSSValueList documentation>
newtype CSSValueList = CSSValueList { unCSSValueList :: JSVal }

instance PToJSVal CSSValueList where
  pToJSVal = unCSSValueList
  {-# INLINE pToJSVal #-}

instance PFromJSVal CSSValueList where
  pFromJSVal = CSSValueList
  {-# INLINE pFromJSVal #-}

instance ToJSVal CSSValueList where
  toJSVal = return . unCSSValueList
  {-# INLINE toJSVal #-}

instance FromJSVal CSSValueList where
  fromJSVal v = fmap CSSValueList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CSSValueList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CSSValueList where
  makeObject = makeObject . unCSSValueList

instance IsCSSValue CSSValueList
instance IsGObject CSSValueList where
  typeGType _ = gTypeCSSValueList
  {-# INLINE typeGType #-}

noCSSValueList :: Maybe CSSValueList
noCSSValueList = Nothing
{-# INLINE noCSSValueList #-}

gTypeCSSValueList :: JSM GType
gTypeCSSValueList = GType . Object <$> jsg "CSSValueList"

-- | Functions for this inteface are in "JSDOM.CanvasProxy".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CanvasProxy Mozilla CanvasProxy documentation>
newtype CanvasProxy = CanvasProxy { unCanvasProxy :: JSVal }

instance PToJSVal CanvasProxy where
  pToJSVal = unCanvasProxy
  {-# INLINE pToJSVal #-}

instance PFromJSVal CanvasProxy where
  pFromJSVal = CanvasProxy
  {-# INLINE pFromJSVal #-}

instance ToJSVal CanvasProxy where
  toJSVal = return . unCanvasProxy
  {-# INLINE toJSVal #-}

instance FromJSVal CanvasProxy where
  fromJSVal v = fmap CanvasProxy <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CanvasProxy
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CanvasProxy where
  makeObject = makeObject . unCanvasProxy

instance IsGObject CanvasProxy where
  typeGType _ = gTypeCanvasProxy
  {-# INLINE typeGType #-}

noCanvasProxy :: Maybe CanvasProxy
noCanvasProxy = Nothing
{-# INLINE noCanvasProxy #-}

gTypeCanvasProxy :: JSM GType
gTypeCanvasProxy = GType . Object <$> jsg "CanvasProxy"

-- | Functions for this inteface are in "JSDOM.ChannelMergerNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ChannelMergerNode Mozilla ChannelMergerNode documentation>
newtype ChannelMergerNode = ChannelMergerNode { unChannelMergerNode :: JSVal }

instance PToJSVal ChannelMergerNode where
  pToJSVal = unChannelMergerNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ChannelMergerNode where
  pFromJSVal = ChannelMergerNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ChannelMergerNode where
  toJSVal = return . unChannelMergerNode
  {-# INLINE toJSVal #-}

instance FromJSVal ChannelMergerNode where
  fromJSVal v = fmap ChannelMergerNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ChannelMergerNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ChannelMergerNode where
  makeObject = makeObject . unChannelMergerNode

instance IsAudioNode ChannelMergerNode
instance IsEventTarget ChannelMergerNode
instance IsGObject ChannelMergerNode where
  typeGType _ = gTypeChannelMergerNode
  {-# INLINE typeGType #-}

noChannelMergerNode :: Maybe ChannelMergerNode
noChannelMergerNode = Nothing
{-# INLINE noChannelMergerNode #-}

gTypeChannelMergerNode :: JSM GType
gTypeChannelMergerNode = GType . Object <$> jsg "ChannelMergerNode"

-- | Functions for this inteface are in "JSDOM.ChannelSplitterNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ChannelSplitterNode Mozilla ChannelSplitterNode documentation>
newtype ChannelSplitterNode = ChannelSplitterNode { unChannelSplitterNode :: JSVal }

instance PToJSVal ChannelSplitterNode where
  pToJSVal = unChannelSplitterNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ChannelSplitterNode where
  pFromJSVal = ChannelSplitterNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ChannelSplitterNode where
  toJSVal = return . unChannelSplitterNode
  {-# INLINE toJSVal #-}

instance FromJSVal ChannelSplitterNode where
  fromJSVal v = fmap ChannelSplitterNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ChannelSplitterNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ChannelSplitterNode where
  makeObject = makeObject . unChannelSplitterNode

instance IsAudioNode ChannelSplitterNode
instance IsEventTarget ChannelSplitterNode
instance IsGObject ChannelSplitterNode where
  typeGType _ = gTypeChannelSplitterNode
  {-# INLINE typeGType #-}

noChannelSplitterNode :: Maybe ChannelSplitterNode
noChannelSplitterNode = Nothing
{-# INLINE noChannelSplitterNode #-}

gTypeChannelSplitterNode :: JSM GType
gTypeChannelSplitterNode = GType . Object <$> jsg "ChannelSplitterNode"

-- | Functions for this inteface are in "JSDOM.ClipboardEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ClipboardEvent Mozilla ClipboardEvent documentation>
newtype ClipboardEvent = ClipboardEvent { unClipboardEvent :: JSVal }

instance PToJSVal ClipboardEvent where
  pToJSVal = unClipboardEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ClipboardEvent where
  pFromJSVal = ClipboardEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ClipboardEvent where
  toJSVal = return . unClipboardEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ClipboardEvent where
  fromJSVal v = fmap ClipboardEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ClipboardEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ClipboardEvent where
  makeObject = makeObject . unClipboardEvent

instance IsEvent ClipboardEvent
instance IsGObject ClipboardEvent where
  typeGType _ = gTypeClipboardEvent
  {-# INLINE typeGType #-}

noClipboardEvent :: Maybe ClipboardEvent
noClipboardEvent = Nothing
{-# INLINE noClipboardEvent #-}

gTypeClipboardEvent :: JSM GType
gTypeClipboardEvent = GType . Object <$> jsg "ClipboardEvent"

-- | Functions for this inteface are in "JSDOM.ClipboardEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ClipboardEventInit Mozilla ClipboardEventInit documentation>
newtype ClipboardEventInit = ClipboardEventInit { unClipboardEventInit :: JSVal }

instance PToJSVal ClipboardEventInit where
  pToJSVal = unClipboardEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal ClipboardEventInit where
  pFromJSVal = ClipboardEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal ClipboardEventInit where
  toJSVal = return . unClipboardEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal ClipboardEventInit where
  fromJSVal v = fmap ClipboardEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ClipboardEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ClipboardEventInit where
  makeObject = makeObject . unClipboardEventInit

instance IsEventInit ClipboardEventInit
instance IsGObject ClipboardEventInit where
  typeGType _ = gTypeClipboardEventInit
  {-# INLINE typeGType #-}

noClipboardEventInit :: Maybe ClipboardEventInit
noClipboardEventInit = Nothing
{-# INLINE noClipboardEventInit #-}

gTypeClipboardEventInit :: JSM GType
gTypeClipboardEventInit = GType . Object <$> jsg "ClipboardEventInit"

-- | Functions for this inteface are in "JSDOM.CloseEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent Mozilla CloseEvent documentation>
newtype CloseEvent = CloseEvent { unCloseEvent :: JSVal }

instance PToJSVal CloseEvent where
  pToJSVal = unCloseEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal CloseEvent where
  pFromJSVal = CloseEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal CloseEvent where
  toJSVal = return . unCloseEvent
  {-# INLINE toJSVal #-}

instance FromJSVal CloseEvent where
  fromJSVal v = fmap CloseEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CloseEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CloseEvent where
  makeObject = makeObject . unCloseEvent

instance IsEvent CloseEvent
instance IsGObject CloseEvent where
  typeGType _ = gTypeCloseEvent
  {-# INLINE typeGType #-}

noCloseEvent :: Maybe CloseEvent
noCloseEvent = Nothing
{-# INLINE noCloseEvent #-}

gTypeCloseEvent :: JSM GType
gTypeCloseEvent = GType . Object <$> jsg "CloseEvent"

-- | Functions for this inteface are in "JSDOM.CloseEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CloseEventInit Mozilla CloseEventInit documentation>
newtype CloseEventInit = CloseEventInit { unCloseEventInit :: JSVal }

instance PToJSVal CloseEventInit where
  pToJSVal = unCloseEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal CloseEventInit where
  pFromJSVal = CloseEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal CloseEventInit where
  toJSVal = return . unCloseEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal CloseEventInit where
  fromJSVal v = fmap CloseEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CloseEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CloseEventInit where
  makeObject = makeObject . unCloseEventInit

instance IsEventInit CloseEventInit
instance IsGObject CloseEventInit where
  typeGType _ = gTypeCloseEventInit
  {-# INLINE typeGType #-}

noCloseEventInit :: Maybe CloseEventInit
noCloseEventInit = Nothing
{-# INLINE noCloseEventInit #-}

gTypeCloseEventInit :: JSM GType
gTypeCloseEventInit = GType . Object <$> jsg "CloseEventInit"

-- | Functions for this inteface are in "JSDOM.CommandLineAPIHost".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CommandLineAPIHost Mozilla CommandLineAPIHost documentation>
newtype CommandLineAPIHost = CommandLineAPIHost { unCommandLineAPIHost :: JSVal }

instance PToJSVal CommandLineAPIHost where
  pToJSVal = unCommandLineAPIHost
  {-# INLINE pToJSVal #-}

instance PFromJSVal CommandLineAPIHost where
  pFromJSVal = CommandLineAPIHost
  {-# INLINE pFromJSVal #-}

instance ToJSVal CommandLineAPIHost where
  toJSVal = return . unCommandLineAPIHost
  {-# INLINE toJSVal #-}

instance FromJSVal CommandLineAPIHost where
  fromJSVal v = fmap CommandLineAPIHost <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CommandLineAPIHost
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CommandLineAPIHost where
  makeObject = makeObject . unCommandLineAPIHost

instance IsGObject CommandLineAPIHost where
  typeGType _ = gTypeCommandLineAPIHost
  {-# INLINE typeGType #-}

noCommandLineAPIHost :: Maybe CommandLineAPIHost
noCommandLineAPIHost = Nothing
{-# INLINE noCommandLineAPIHost #-}

gTypeCommandLineAPIHost :: JSM GType
gTypeCommandLineAPIHost = GType . Object <$> jsg "CommandLineAPIHost"


-- | Functions for this inteface are in "JSDOM.CompositionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CompositionEvent Mozilla CompositionEvent documentation>
newtype CompositionEvent = CompositionEvent { unCompositionEvent :: JSVal }

instance PToJSVal CompositionEvent where
  pToJSVal = unCompositionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal CompositionEvent where
  pFromJSVal = CompositionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal CompositionEvent where
  toJSVal = return . unCompositionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal CompositionEvent where
  fromJSVal v = fmap CompositionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CompositionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CompositionEvent where
  makeObject = makeObject . unCompositionEvent

instance IsUIEvent CompositionEvent
instance IsEvent CompositionEvent
instance IsGObject CompositionEvent where
  typeGType _ = gTypeCompositionEvent
  {-# INLINE typeGType #-}

noCompositionEvent :: Maybe CompositionEvent
noCompositionEvent = Nothing
{-# INLINE noCompositionEvent #-}

gTypeCompositionEvent :: JSM GType
gTypeCompositionEvent = GType . Object <$> jsg "CompositionEvent"

-- | Functions for this inteface are in "JSDOM.CompositionEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CompositionEventInit Mozilla CompositionEventInit documentation>
newtype CompositionEventInit = CompositionEventInit { unCompositionEventInit :: JSVal }

instance PToJSVal CompositionEventInit where
  pToJSVal = unCompositionEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal CompositionEventInit where
  pFromJSVal = CompositionEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal CompositionEventInit where
  toJSVal = return . unCompositionEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal CompositionEventInit where
  fromJSVal v = fmap CompositionEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CompositionEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CompositionEventInit where
  makeObject = makeObject . unCompositionEventInit

instance IsUIEventInit CompositionEventInit
instance IsEventInit CompositionEventInit
instance IsGObject CompositionEventInit where
  typeGType _ = gTypeCompositionEventInit
  {-# INLINE typeGType #-}

noCompositionEventInit :: Maybe CompositionEventInit
noCompositionEventInit = Nothing
{-# INLINE noCompositionEventInit #-}

gTypeCompositionEventInit :: JSM GType
gTypeCompositionEventInit = GType . Object <$> jsg "CompositionEventInit"

-- | Functions for this inteface are in "JSDOM.ConstrainBooleanParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ConstrainBooleanParameters Mozilla ConstrainBooleanParameters documentation>
newtype ConstrainBooleanParameters = ConstrainBooleanParameters { unConstrainBooleanParameters :: JSVal }

instance PToJSVal ConstrainBooleanParameters where
  pToJSVal = unConstrainBooleanParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal ConstrainBooleanParameters where
  pFromJSVal = ConstrainBooleanParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal ConstrainBooleanParameters where
  toJSVal = return . unConstrainBooleanParameters
  {-# INLINE toJSVal #-}

instance FromJSVal ConstrainBooleanParameters where
  fromJSVal v = fmap ConstrainBooleanParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ConstrainBooleanParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ConstrainBooleanParameters where
  makeObject = makeObject . unConstrainBooleanParameters

instance IsGObject ConstrainBooleanParameters where
  typeGType _ = gTypeConstrainBooleanParameters
  {-# INLINE typeGType #-}

noConstrainBooleanParameters :: Maybe ConstrainBooleanParameters
noConstrainBooleanParameters = Nothing
{-# INLINE noConstrainBooleanParameters #-}

gTypeConstrainBooleanParameters :: JSM GType
gTypeConstrainBooleanParameters = GType . Object <$> jsg "ConstrainBooleanParameters"

-- | Functions for this inteface are in "JSDOM.ConstrainDOMStringParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ConstrainDOMStringParameters Mozilla ConstrainDOMStringParameters documentation>
newtype ConstrainDOMStringParameters = ConstrainDOMStringParameters { unConstrainDOMStringParameters :: JSVal }

instance PToJSVal ConstrainDOMStringParameters where
  pToJSVal = unConstrainDOMStringParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal ConstrainDOMStringParameters where
  pFromJSVal = ConstrainDOMStringParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal ConstrainDOMStringParameters where
  toJSVal = return . unConstrainDOMStringParameters
  {-# INLINE toJSVal #-}

instance FromJSVal ConstrainDOMStringParameters where
  fromJSVal v = fmap ConstrainDOMStringParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ConstrainDOMStringParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ConstrainDOMStringParameters where
  makeObject = makeObject . unConstrainDOMStringParameters

instance IsGObject ConstrainDOMStringParameters where
  typeGType _ = gTypeConstrainDOMStringParameters
  {-# INLINE typeGType #-}

noConstrainDOMStringParameters :: Maybe ConstrainDOMStringParameters
noConstrainDOMStringParameters = Nothing
{-# INLINE noConstrainDOMStringParameters #-}

gTypeConstrainDOMStringParameters :: JSM GType
gTypeConstrainDOMStringParameters = GType . Object <$> jsg "ConstrainDOMStringParameters"

-- | Functions for this inteface are in "JSDOM.ConstrainDoubleRange".
-- Base interface functions are in:
--
--     * "JSDOM.DoubleRange"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ConstrainDoubleRange Mozilla ConstrainDoubleRange documentation>
newtype ConstrainDoubleRange = ConstrainDoubleRange { unConstrainDoubleRange :: JSVal }

instance PToJSVal ConstrainDoubleRange where
  pToJSVal = unConstrainDoubleRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal ConstrainDoubleRange where
  pFromJSVal = ConstrainDoubleRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal ConstrainDoubleRange where
  toJSVal = return . unConstrainDoubleRange
  {-# INLINE toJSVal #-}

instance FromJSVal ConstrainDoubleRange where
  fromJSVal v = fmap ConstrainDoubleRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ConstrainDoubleRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ConstrainDoubleRange where
  makeObject = makeObject . unConstrainDoubleRange

instance IsDoubleRange ConstrainDoubleRange
instance IsGObject ConstrainDoubleRange where
  typeGType _ = gTypeConstrainDoubleRange
  {-# INLINE typeGType #-}

noConstrainDoubleRange :: Maybe ConstrainDoubleRange
noConstrainDoubleRange = Nothing
{-# INLINE noConstrainDoubleRange #-}

gTypeConstrainDoubleRange :: JSM GType
gTypeConstrainDoubleRange = GType . Object <$> jsg "ConstrainDoubleRange"

-- | Functions for this inteface are in "JSDOM.ConstrainLongRange".
-- Base interface functions are in:
--
--     * "JSDOM.LongRange"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ConstrainLongRange Mozilla ConstrainLongRange documentation>
newtype ConstrainLongRange = ConstrainLongRange { unConstrainLongRange :: JSVal }

instance PToJSVal ConstrainLongRange where
  pToJSVal = unConstrainLongRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal ConstrainLongRange where
  pFromJSVal = ConstrainLongRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal ConstrainLongRange where
  toJSVal = return . unConstrainLongRange
  {-# INLINE toJSVal #-}

instance FromJSVal ConstrainLongRange where
  fromJSVal v = fmap ConstrainLongRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ConstrainLongRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ConstrainLongRange where
  makeObject = makeObject . unConstrainLongRange

instance IsLongRange ConstrainLongRange
instance IsGObject ConstrainLongRange where
  typeGType _ = gTypeConstrainLongRange
  {-# INLINE typeGType #-}

noConstrainLongRange :: Maybe ConstrainLongRange
noConstrainLongRange = Nothing
{-# INLINE noConstrainLongRange #-}

gTypeConstrainLongRange :: JSM GType
gTypeConstrainLongRange = GType . Object <$> jsg "ConstrainLongRange"

-- | Functions for this inteface are in "JSDOM.ConvolverNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ConvolverNode Mozilla ConvolverNode documentation>
newtype ConvolverNode = ConvolverNode { unConvolverNode :: JSVal }

instance PToJSVal ConvolverNode where
  pToJSVal = unConvolverNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ConvolverNode where
  pFromJSVal = ConvolverNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ConvolverNode where
  toJSVal = return . unConvolverNode
  {-# INLINE toJSVal #-}

instance FromJSVal ConvolverNode where
  fromJSVal v = fmap ConvolverNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ConvolverNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ConvolverNode where
  makeObject = makeObject . unConvolverNode

instance IsAudioNode ConvolverNode
instance IsEventTarget ConvolverNode
instance IsGObject ConvolverNode where
  typeGType _ = gTypeConvolverNode
  {-# INLINE typeGType #-}

noConvolverNode :: Maybe ConvolverNode
noConvolverNode = Nothing
{-# INLINE noConvolverNode #-}

gTypeConvolverNode :: JSM GType
gTypeConvolverNode = GType . Object <$> jsg "ConvolverNode"

-- | Functions for this inteface are in "JSDOM.Coordinates".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Coordinates Mozilla Coordinates documentation>
newtype Coordinates = Coordinates { unCoordinates :: JSVal }

instance PToJSVal Coordinates where
  pToJSVal = unCoordinates
  {-# INLINE pToJSVal #-}

instance PFromJSVal Coordinates where
  pFromJSVal = Coordinates
  {-# INLINE pFromJSVal #-}

instance ToJSVal Coordinates where
  toJSVal = return . unCoordinates
  {-# INLINE toJSVal #-}

instance FromJSVal Coordinates where
  fromJSVal v = fmap Coordinates <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Coordinates
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Coordinates where
  makeObject = makeObject . unCoordinates

instance IsGObject Coordinates where
  typeGType _ = gTypeCoordinates
  {-# INLINE typeGType #-}

noCoordinates :: Maybe Coordinates
noCoordinates = Nothing
{-# INLINE noCoordinates #-}

gTypeCoordinates :: JSM GType
gTypeCoordinates = GType . Object <$> jsg "Coordinates"

-- | Functions for this inteface are in "JSDOM.CountQueuingStrategy".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CountQueuingStrategy Mozilla CountQueuingStrategy documentation>
newtype CountQueuingStrategy = CountQueuingStrategy { unCountQueuingStrategy :: JSVal }

instance PToJSVal CountQueuingStrategy where
  pToJSVal = unCountQueuingStrategy
  {-# INLINE pToJSVal #-}

instance PFromJSVal CountQueuingStrategy where
  pFromJSVal = CountQueuingStrategy
  {-# INLINE pFromJSVal #-}

instance ToJSVal CountQueuingStrategy where
  toJSVal = return . unCountQueuingStrategy
  {-# INLINE toJSVal #-}

instance FromJSVal CountQueuingStrategy where
  fromJSVal v = fmap CountQueuingStrategy <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CountQueuingStrategy
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CountQueuingStrategy where
  makeObject = makeObject . unCountQueuingStrategy

instance IsGObject CountQueuingStrategy where
  typeGType _ = gTypeCountQueuingStrategy
  {-# INLINE typeGType #-}

noCountQueuingStrategy :: Maybe CountQueuingStrategy
noCountQueuingStrategy = Nothing
{-# INLINE noCountQueuingStrategy #-}

gTypeCountQueuingStrategy :: JSM GType
gTypeCountQueuingStrategy = GType . Object <$> jsg "CountQueuingStrategy"

-- | Functions for this inteface are in "JSDOM.Counter".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Counter Mozilla Counter documentation>
newtype Counter = Counter { unCounter :: JSVal }

instance PToJSVal Counter where
  pToJSVal = unCounter
  {-# INLINE pToJSVal #-}

instance PFromJSVal Counter where
  pFromJSVal = Counter
  {-# INLINE pFromJSVal #-}

instance ToJSVal Counter where
  toJSVal = return . unCounter
  {-# INLINE toJSVal #-}

instance FromJSVal Counter where
  fromJSVal v = fmap Counter <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Counter
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Counter where
  makeObject = makeObject . unCounter

instance IsGObject Counter where
  typeGType _ = gTypeCounter
  {-# INLINE typeGType #-}

noCounter :: Maybe Counter
noCounter = Nothing
{-# INLINE noCounter #-}

gTypeCounter :: JSM GType
gTypeCounter = GType . Object <$> jsg "Counter"

-- | Functions for this inteface are in "JSDOM.CredentialData".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CredentialData Mozilla CredentialData documentation>
newtype CredentialData = CredentialData { unCredentialData :: JSVal }

instance PToJSVal CredentialData where
  pToJSVal = unCredentialData
  {-# INLINE pToJSVal #-}

instance PFromJSVal CredentialData where
  pFromJSVal = CredentialData
  {-# INLINE pFromJSVal #-}

instance ToJSVal CredentialData where
  toJSVal = return . unCredentialData
  {-# INLINE toJSVal #-}

instance FromJSVal CredentialData where
  fromJSVal v = fmap CredentialData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CredentialData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CredentialData where
  makeObject = makeObject . unCredentialData

class (IsGObject o) => IsCredentialData o
toCredentialData :: IsCredentialData o => o -> CredentialData
toCredentialData = CredentialData . coerce

instance IsCredentialData CredentialData
instance IsGObject CredentialData where
  typeGType _ = gTypeCredentialData
  {-# INLINE typeGType #-}

noCredentialData :: Maybe CredentialData
noCredentialData = Nothing
{-# INLINE noCredentialData #-}

gTypeCredentialData :: JSM GType
gTypeCredentialData = GType . Object <$> jsg "CredentialData"

-- | Functions for this inteface are in "JSDOM.Crypto".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Crypto Mozilla Crypto documentation>
newtype Crypto = Crypto { unCrypto :: JSVal }

instance PToJSVal Crypto where
  pToJSVal = unCrypto
  {-# INLINE pToJSVal #-}

instance PFromJSVal Crypto where
  pFromJSVal = Crypto
  {-# INLINE pFromJSVal #-}

instance ToJSVal Crypto where
  toJSVal = return . unCrypto
  {-# INLINE toJSVal #-}

instance FromJSVal Crypto where
  fromJSVal v = fmap Crypto <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Crypto
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Crypto where
  makeObject = makeObject . unCrypto

instance IsGObject Crypto where
  typeGType _ = gTypeCrypto
  {-# INLINE typeGType #-}

noCrypto :: Maybe Crypto
noCrypto = Nothing
{-# INLINE noCrypto #-}

gTypeCrypto :: JSM GType
gTypeCrypto = GType . Object <$> jsg "Crypto"

-- | Functions for this inteface are in "JSDOM.CryptoAlgorithmParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CryptoAlgorithmParameters Mozilla CryptoAlgorithmParameters documentation>
newtype CryptoAlgorithmParameters = CryptoAlgorithmParameters { unCryptoAlgorithmParameters :: JSVal }

instance PToJSVal CryptoAlgorithmParameters where
  pToJSVal = unCryptoAlgorithmParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal CryptoAlgorithmParameters where
  pFromJSVal = CryptoAlgorithmParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal CryptoAlgorithmParameters where
  toJSVal = return . unCryptoAlgorithmParameters
  {-# INLINE toJSVal #-}

instance FromJSVal CryptoAlgorithmParameters where
  fromJSVal v = fmap CryptoAlgorithmParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CryptoAlgorithmParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CryptoAlgorithmParameters where
  makeObject = makeObject . unCryptoAlgorithmParameters

class (IsGObject o) => IsCryptoAlgorithmParameters o
toCryptoAlgorithmParameters :: IsCryptoAlgorithmParameters o => o -> CryptoAlgorithmParameters
toCryptoAlgorithmParameters = CryptoAlgorithmParameters . coerce

instance IsCryptoAlgorithmParameters CryptoAlgorithmParameters
instance IsGObject CryptoAlgorithmParameters where
  typeGType _ = gTypeCryptoAlgorithmParameters
  {-# INLINE typeGType #-}

noCryptoAlgorithmParameters :: Maybe CryptoAlgorithmParameters
noCryptoAlgorithmParameters = Nothing
{-# INLINE noCryptoAlgorithmParameters #-}

gTypeCryptoAlgorithmParameters :: JSM GType
gTypeCryptoAlgorithmParameters = GType . Object <$> jsg "CryptoAlgorithmParameters"


-- | Functions for this inteface are in "JSDOM.CustomElementRegistry".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry Mozilla CustomElementRegistry documentation>
newtype CustomElementRegistry = CustomElementRegistry { unCustomElementRegistry :: JSVal }

instance PToJSVal CustomElementRegistry where
  pToJSVal = unCustomElementRegistry
  {-# INLINE pToJSVal #-}

instance PFromJSVal CustomElementRegistry where
  pFromJSVal = CustomElementRegistry
  {-# INLINE pFromJSVal #-}

instance ToJSVal CustomElementRegistry where
  toJSVal = return . unCustomElementRegistry
  {-# INLINE toJSVal #-}

instance FromJSVal CustomElementRegistry where
  fromJSVal v = fmap CustomElementRegistry <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CustomElementRegistry
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CustomElementRegistry where
  makeObject = makeObject . unCustomElementRegistry

instance IsGObject CustomElementRegistry where
  typeGType _ = gTypeCustomElementRegistry
  {-# INLINE typeGType #-}

noCustomElementRegistry :: Maybe CustomElementRegistry
noCustomElementRegistry = Nothing
{-# INLINE noCustomElementRegistry #-}

gTypeCustomElementRegistry :: JSM GType
gTypeCustomElementRegistry = GType . Object <$> jsg "CustomElementRegistry"

-- | Functions for this inteface are in "JSDOM.CustomEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent Mozilla CustomEvent documentation>
newtype CustomEvent = CustomEvent { unCustomEvent :: JSVal }

instance PToJSVal CustomEvent where
  pToJSVal = unCustomEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal CustomEvent where
  pFromJSVal = CustomEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal CustomEvent where
  toJSVal = return . unCustomEvent
  {-# INLINE toJSVal #-}

instance FromJSVal CustomEvent where
  fromJSVal v = fmap CustomEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CustomEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CustomEvent where
  makeObject = makeObject . unCustomEvent

instance IsEvent CustomEvent
instance IsGObject CustomEvent where
  typeGType _ = gTypeCustomEvent
  {-# INLINE typeGType #-}

noCustomEvent :: Maybe CustomEvent
noCustomEvent = Nothing
{-# INLINE noCustomEvent #-}

gTypeCustomEvent :: JSM GType
gTypeCustomEvent = GType . Object <$> jsg "CustomEvent"

-- | Functions for this inteface are in "JSDOM.CustomEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CustomEventInit Mozilla CustomEventInit documentation>
newtype CustomEventInit = CustomEventInit { unCustomEventInit :: JSVal }

instance PToJSVal CustomEventInit where
  pToJSVal = unCustomEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal CustomEventInit where
  pFromJSVal = CustomEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal CustomEventInit where
  toJSVal = return . unCustomEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal CustomEventInit where
  fromJSVal v = fmap CustomEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CustomEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CustomEventInit where
  makeObject = makeObject . unCustomEventInit

instance IsEventInit CustomEventInit
instance IsGObject CustomEventInit where
  typeGType _ = gTypeCustomEventInit
  {-# INLINE typeGType #-}

noCustomEventInit :: Maybe CustomEventInit
noCustomEventInit = Nothing
{-# INLINE noCustomEventInit #-}

gTypeCustomEventInit :: JSM GType
gTypeCustomEventInit = GType . Object <$> jsg "CustomEventInit"

-- | Functions for this inteface are in "JSDOM.DOMError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMError Mozilla DOMError documentation>
newtype DOMError = DOMError { unDOMError :: JSVal }

instance PToJSVal DOMError where
  pToJSVal = unDOMError
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMError where
  pFromJSVal = DOMError
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMError where
  toJSVal = return . unDOMError
  {-# INLINE toJSVal #-}

instance FromJSVal DOMError where
  fromJSVal v = fmap DOMError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMError where
  makeObject = makeObject . unDOMError

class (IsGObject o) => IsDOMError o
toDOMError :: IsDOMError o => o -> DOMError
toDOMError = DOMError . coerce

instance IsDOMError DOMError
instance IsGObject DOMError where
  typeGType _ = gTypeDOMError
  {-# INLINE typeGType #-}

noDOMError :: Maybe DOMError
noDOMError = Nothing
{-# INLINE noDOMError #-}

gTypeDOMError :: JSM GType
gTypeDOMError = GType . Object <$> jsg "DOMError"

-- | Functions for this inteface are in "JSDOM.DOMException".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMException Mozilla DOMException documentation>
newtype DOMException = DOMException { unDOMException :: JSVal }

instance PToJSVal DOMException where
  pToJSVal = unDOMException
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMException where
  pFromJSVal = DOMException
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMException where
  toJSVal = return . unDOMException
  {-# INLINE toJSVal #-}

instance FromJSVal DOMException where
  fromJSVal v = fmap DOMException <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMException
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMException where
  makeObject = makeObject . unDOMException

instance IsGObject DOMException where
  typeGType _ = gTypeDOMException
  {-# INLINE typeGType #-}

noDOMException :: Maybe DOMException
noDOMException = Nothing
{-# INLINE noDOMException #-}

gTypeDOMException :: JSM GType
gTypeDOMException = GType . Object <$> jsg "DOMException"

-- | Functions for this inteface are in "JSDOM.DOMImplementation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMImplementation Mozilla DOMImplementation documentation>
newtype DOMImplementation = DOMImplementation { unDOMImplementation :: JSVal }

instance PToJSVal DOMImplementation where
  pToJSVal = unDOMImplementation
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMImplementation where
  pFromJSVal = DOMImplementation
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMImplementation where
  toJSVal = return . unDOMImplementation
  {-# INLINE toJSVal #-}

instance FromJSVal DOMImplementation where
  fromJSVal v = fmap DOMImplementation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMImplementation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMImplementation where
  makeObject = makeObject . unDOMImplementation

instance IsGObject DOMImplementation where
  typeGType _ = gTypeDOMImplementation
  {-# INLINE typeGType #-}

noDOMImplementation :: Maybe DOMImplementation
noDOMImplementation = Nothing
{-# INLINE noDOMImplementation #-}

gTypeDOMImplementation :: JSM GType
gTypeDOMImplementation = GType . Object <$> jsg "DOMImplementation"

-- | Functions for this inteface are in "JSDOM.DOMNamedFlowCollection".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitNamedFlowCollection Mozilla WebKitNamedFlowCollection documentation>
newtype DOMNamedFlowCollection = DOMNamedFlowCollection { unDOMNamedFlowCollection :: JSVal }

instance PToJSVal DOMNamedFlowCollection where
  pToJSVal = unDOMNamedFlowCollection
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMNamedFlowCollection where
  pFromJSVal = DOMNamedFlowCollection
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMNamedFlowCollection where
  toJSVal = return . unDOMNamedFlowCollection
  {-# INLINE toJSVal #-}

instance FromJSVal DOMNamedFlowCollection where
  fromJSVal v = fmap DOMNamedFlowCollection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMNamedFlowCollection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMNamedFlowCollection where
  makeObject = makeObject . unDOMNamedFlowCollection

instance IsGObject DOMNamedFlowCollection where
  typeGType _ = gTypeDOMNamedFlowCollection
  {-# INLINE typeGType #-}

noDOMNamedFlowCollection :: Maybe DOMNamedFlowCollection
noDOMNamedFlowCollection = Nothing
{-# INLINE noDOMNamedFlowCollection #-}

gTypeDOMNamedFlowCollection :: JSM GType
gTypeDOMNamedFlowCollection = GType . Object <$> jsg "WebKitNamedFlowCollection"

-- | Functions for this inteface are in "JSDOM.DOMParser".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMParser Mozilla DOMParser documentation>
newtype DOMParser = DOMParser { unDOMParser :: JSVal }

instance PToJSVal DOMParser where
  pToJSVal = unDOMParser
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMParser where
  pFromJSVal = DOMParser
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMParser where
  toJSVal = return . unDOMParser
  {-# INLINE toJSVal #-}

instance FromJSVal DOMParser where
  fromJSVal v = fmap DOMParser <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMParser
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMParser where
  makeObject = makeObject . unDOMParser

instance IsGObject DOMParser where
  typeGType _ = gTypeDOMParser
  {-# INLINE typeGType #-}

noDOMParser :: Maybe DOMParser
noDOMParser = Nothing
{-# INLINE noDOMParser #-}

gTypeDOMParser :: JSM GType
gTypeDOMParser = GType . Object <$> jsg "DOMParser"

-- | Functions for this inteface are in "JSDOM.DOMPoint".
-- Base interface functions are in:
--
--     * "JSDOM.DOMPointReadOnly"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMPoint Mozilla DOMPoint documentation>
newtype DOMPoint = DOMPoint { unDOMPoint :: JSVal }

instance PToJSVal DOMPoint where
  pToJSVal = unDOMPoint
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMPoint where
  pFromJSVal = DOMPoint
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMPoint where
  toJSVal = return . unDOMPoint
  {-# INLINE toJSVal #-}

instance FromJSVal DOMPoint where
  fromJSVal v = fmap DOMPoint <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMPoint
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMPoint where
  makeObject = makeObject . unDOMPoint

instance IsDOMPointReadOnly DOMPoint
instance IsGObject DOMPoint where
  typeGType _ = gTypeDOMPoint
  {-# INLINE typeGType #-}

noDOMPoint :: Maybe DOMPoint
noDOMPoint = Nothing
{-# INLINE noDOMPoint #-}

gTypeDOMPoint :: JSM GType
gTypeDOMPoint = GType . Object <$> jsg "DOMPoint"

-- | Functions for this inteface are in "JSDOM.DOMPointInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMPointInit Mozilla DOMPointInit documentation>
newtype DOMPointInit = DOMPointInit { unDOMPointInit :: JSVal }

instance PToJSVal DOMPointInit where
  pToJSVal = unDOMPointInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMPointInit where
  pFromJSVal = DOMPointInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMPointInit where
  toJSVal = return . unDOMPointInit
  {-# INLINE toJSVal #-}

instance FromJSVal DOMPointInit where
  fromJSVal v = fmap DOMPointInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMPointInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMPointInit where
  makeObject = makeObject . unDOMPointInit

instance IsGObject DOMPointInit where
  typeGType _ = gTypeDOMPointInit
  {-# INLINE typeGType #-}

noDOMPointInit :: Maybe DOMPointInit
noDOMPointInit = Nothing
{-# INLINE noDOMPointInit #-}

gTypeDOMPointInit :: JSM GType
gTypeDOMPointInit = GType . Object <$> jsg "DOMPointInit"

-- | Functions for this inteface are in "JSDOM.DOMPointReadOnly".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMPointReadOnly Mozilla DOMPointReadOnly documentation>
newtype DOMPointReadOnly = DOMPointReadOnly { unDOMPointReadOnly :: JSVal }

instance PToJSVal DOMPointReadOnly where
  pToJSVal = unDOMPointReadOnly
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMPointReadOnly where
  pFromJSVal = DOMPointReadOnly
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMPointReadOnly where
  toJSVal = return . unDOMPointReadOnly
  {-# INLINE toJSVal #-}

instance FromJSVal DOMPointReadOnly where
  fromJSVal v = fmap DOMPointReadOnly <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMPointReadOnly
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMPointReadOnly where
  makeObject = makeObject . unDOMPointReadOnly

class (IsGObject o) => IsDOMPointReadOnly o
toDOMPointReadOnly :: IsDOMPointReadOnly o => o -> DOMPointReadOnly
toDOMPointReadOnly = DOMPointReadOnly . coerce

instance IsDOMPointReadOnly DOMPointReadOnly
instance IsGObject DOMPointReadOnly where
  typeGType _ = gTypeDOMPointReadOnly
  {-# INLINE typeGType #-}

noDOMPointReadOnly :: Maybe DOMPointReadOnly
noDOMPointReadOnly = Nothing
{-# INLINE noDOMPointReadOnly #-}

gTypeDOMPointReadOnly :: JSM GType
gTypeDOMPointReadOnly = GType . Object <$> jsg "DOMPointReadOnly"

-- | Functions for this inteface are in "JSDOM.DOMRect".
-- Base interface functions are in:
--
--     * "JSDOM.DOMRectReadOnly"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMRect Mozilla DOMRect documentation>
newtype DOMRect = DOMRect { unDOMRect :: JSVal }

instance PToJSVal DOMRect where
  pToJSVal = unDOMRect
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMRect where
  pFromJSVal = DOMRect
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMRect where
  toJSVal = return . unDOMRect
  {-# INLINE toJSVal #-}

instance FromJSVal DOMRect where
  fromJSVal v = fmap DOMRect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMRect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMRect where
  makeObject = makeObject . unDOMRect

instance IsDOMRectReadOnly DOMRect
instance IsGObject DOMRect where
  typeGType _ = gTypeDOMRect
  {-# INLINE typeGType #-}

noDOMRect :: Maybe DOMRect
noDOMRect = Nothing
{-# INLINE noDOMRect #-}

gTypeDOMRect :: JSM GType
gTypeDOMRect = GType . Object <$> jsg "DOMRect"

-- | Functions for this inteface are in "JSDOM.DOMRectInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMRectInit Mozilla DOMRectInit documentation>
newtype DOMRectInit = DOMRectInit { unDOMRectInit :: JSVal }

instance PToJSVal DOMRectInit where
  pToJSVal = unDOMRectInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMRectInit where
  pFromJSVal = DOMRectInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMRectInit where
  toJSVal = return . unDOMRectInit
  {-# INLINE toJSVal #-}

instance FromJSVal DOMRectInit where
  fromJSVal v = fmap DOMRectInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMRectInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMRectInit where
  makeObject = makeObject . unDOMRectInit

instance IsGObject DOMRectInit where
  typeGType _ = gTypeDOMRectInit
  {-# INLINE typeGType #-}

noDOMRectInit :: Maybe DOMRectInit
noDOMRectInit = Nothing
{-# INLINE noDOMRectInit #-}

gTypeDOMRectInit :: JSM GType
gTypeDOMRectInit = GType . Object <$> jsg "DOMRectInit"

-- | Functions for this inteface are in "JSDOM.DOMRectReadOnly".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMRectReadOnly Mozilla DOMRectReadOnly documentation>
newtype DOMRectReadOnly = DOMRectReadOnly { unDOMRectReadOnly :: JSVal }

instance PToJSVal DOMRectReadOnly where
  pToJSVal = unDOMRectReadOnly
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMRectReadOnly where
  pFromJSVal = DOMRectReadOnly
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMRectReadOnly where
  toJSVal = return . unDOMRectReadOnly
  {-# INLINE toJSVal #-}

instance FromJSVal DOMRectReadOnly where
  fromJSVal v = fmap DOMRectReadOnly <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMRectReadOnly
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMRectReadOnly where
  makeObject = makeObject . unDOMRectReadOnly

class (IsGObject o) => IsDOMRectReadOnly o
toDOMRectReadOnly :: IsDOMRectReadOnly o => o -> DOMRectReadOnly
toDOMRectReadOnly = DOMRectReadOnly . coerce

instance IsDOMRectReadOnly DOMRectReadOnly
instance IsGObject DOMRectReadOnly where
  typeGType _ = gTypeDOMRectReadOnly
  {-# INLINE typeGType #-}

noDOMRectReadOnly :: Maybe DOMRectReadOnly
noDOMRectReadOnly = Nothing
{-# INLINE noDOMRectReadOnly #-}

gTypeDOMRectReadOnly :: JSM GType
gTypeDOMRectReadOnly = GType . Object <$> jsg "DOMRectReadOnly"

-- | Functions for this inteface are in "JSDOM.DOMStringList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMStringList Mozilla DOMStringList documentation>
newtype DOMStringList = DOMStringList { unDOMStringList :: JSVal }

instance PToJSVal DOMStringList where
  pToJSVal = unDOMStringList
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMStringList where
  pFromJSVal = DOMStringList
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMStringList where
  toJSVal = return . unDOMStringList
  {-# INLINE toJSVal #-}

instance FromJSVal DOMStringList where
  fromJSVal v = fmap DOMStringList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMStringList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMStringList where
  makeObject = makeObject . unDOMStringList

instance IsGObject DOMStringList where
  typeGType _ = gTypeDOMStringList
  {-# INLINE typeGType #-}

noDOMStringList :: Maybe DOMStringList
noDOMStringList = Nothing
{-# INLINE noDOMStringList #-}

gTypeDOMStringList :: JSM GType
gTypeDOMStringList = GType . Object <$> jsg "DOMStringList"

-- | Functions for this inteface are in "JSDOM.DOMStringMap".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMStringMap Mozilla DOMStringMap documentation>
newtype DOMStringMap = DOMStringMap { unDOMStringMap :: JSVal }

instance PToJSVal DOMStringMap where
  pToJSVal = unDOMStringMap
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMStringMap where
  pFromJSVal = DOMStringMap
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMStringMap where
  toJSVal = return . unDOMStringMap
  {-# INLINE toJSVal #-}

instance FromJSVal DOMStringMap where
  fromJSVal v = fmap DOMStringMap <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMStringMap
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMStringMap where
  makeObject = makeObject . unDOMStringMap

instance IsGObject DOMStringMap where
  typeGType _ = gTypeDOMStringMap
  {-# INLINE typeGType #-}

noDOMStringMap :: Maybe DOMStringMap
noDOMStringMap = Nothing
{-# INLINE noDOMStringMap #-}

gTypeDOMStringMap :: JSM GType
gTypeDOMStringMap = GType . Object <$> jsg "DOMStringMap"

-- | Functions for this inteface are in "JSDOM.DOMTokenList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DOMTokenList Mozilla DOMTokenList documentation>
newtype DOMTokenList = DOMTokenList { unDOMTokenList :: JSVal }

instance PToJSVal DOMTokenList where
  pToJSVal = unDOMTokenList
  {-# INLINE pToJSVal #-}

instance PFromJSVal DOMTokenList where
  pFromJSVal = DOMTokenList
  {-# INLINE pFromJSVal #-}

instance ToJSVal DOMTokenList where
  toJSVal = return . unDOMTokenList
  {-# INLINE toJSVal #-}

instance FromJSVal DOMTokenList where
  fromJSVal v = fmap DOMTokenList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DOMTokenList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DOMTokenList where
  makeObject = makeObject . unDOMTokenList

instance IsGObject DOMTokenList where
  typeGType _ = gTypeDOMTokenList
  {-# INLINE typeGType #-}

noDOMTokenList :: Maybe DOMTokenList
noDOMTokenList = Nothing
{-# INLINE noDOMTokenList #-}

gTypeDOMTokenList :: JSM GType
gTypeDOMTokenList = GType . Object <$> jsg "DOMTokenList"

-- | Functions for this inteface are in "JSDOM.DataCue".
-- Base interface functions are in:
--
--     * "JSDOM.TextTrackCue"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitDataCue Mozilla WebKitDataCue documentation>
newtype DataCue = DataCue { unDataCue :: JSVal }

instance PToJSVal DataCue where
  pToJSVal = unDataCue
  {-# INLINE pToJSVal #-}

instance PFromJSVal DataCue where
  pFromJSVal = DataCue
  {-# INLINE pFromJSVal #-}

instance ToJSVal DataCue where
  toJSVal = return . unDataCue
  {-# INLINE toJSVal #-}

instance FromJSVal DataCue where
  fromJSVal v = fmap DataCue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DataCue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DataCue where
  makeObject = makeObject . unDataCue

instance IsTextTrackCue DataCue
instance IsEventTarget DataCue
instance IsGObject DataCue where
  typeGType _ = gTypeDataCue
  {-# INLINE typeGType #-}

noDataCue :: Maybe DataCue
noDataCue = Nothing
{-# INLINE noDataCue #-}

gTypeDataCue :: JSM GType
gTypeDataCue = GType . Object <$> jsg "WebKitDataCue"

-- | Functions for this inteface are in "JSDOM.DataTransfer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransfer Mozilla DataTransfer documentation>
newtype DataTransfer = DataTransfer { unDataTransfer :: JSVal }

instance PToJSVal DataTransfer where
  pToJSVal = unDataTransfer
  {-# INLINE pToJSVal #-}

instance PFromJSVal DataTransfer where
  pFromJSVal = DataTransfer
  {-# INLINE pFromJSVal #-}

instance ToJSVal DataTransfer where
  toJSVal = return . unDataTransfer
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransfer where
  fromJSVal v = fmap DataTransfer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DataTransfer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DataTransfer where
  makeObject = makeObject . unDataTransfer

instance IsGObject DataTransfer where
  typeGType _ = gTypeDataTransfer
  {-# INLINE typeGType #-}

noDataTransfer :: Maybe DataTransfer
noDataTransfer = Nothing
{-# INLINE noDataTransfer #-}

gTypeDataTransfer :: JSM GType
gTypeDataTransfer = GType . Object <$> jsg "DataTransfer"

-- | Functions for this inteface are in "JSDOM.DataTransferItem".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItem Mozilla DataTransferItem documentation>
newtype DataTransferItem = DataTransferItem { unDataTransferItem :: JSVal }

instance PToJSVal DataTransferItem where
  pToJSVal = unDataTransferItem
  {-# INLINE pToJSVal #-}

instance PFromJSVal DataTransferItem where
  pFromJSVal = DataTransferItem
  {-# INLINE pFromJSVal #-}

instance ToJSVal DataTransferItem where
  toJSVal = return . unDataTransferItem
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransferItem where
  fromJSVal v = fmap DataTransferItem <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DataTransferItem
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DataTransferItem where
  makeObject = makeObject . unDataTransferItem

instance IsGObject DataTransferItem where
  typeGType _ = gTypeDataTransferItem
  {-# INLINE typeGType #-}

noDataTransferItem :: Maybe DataTransferItem
noDataTransferItem = Nothing
{-# INLINE noDataTransferItem #-}

gTypeDataTransferItem :: JSM GType
gTypeDataTransferItem = GType . Object <$> jsg "DataTransferItem"

-- | Functions for this inteface are in "JSDOM.DataTransferItemList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DataTransferItemList Mozilla DataTransferItemList documentation>
newtype DataTransferItemList = DataTransferItemList { unDataTransferItemList :: JSVal }

instance PToJSVal DataTransferItemList where
  pToJSVal = unDataTransferItemList
  {-# INLINE pToJSVal #-}

instance PFromJSVal DataTransferItemList where
  pFromJSVal = DataTransferItemList
  {-# INLINE pFromJSVal #-}

instance ToJSVal DataTransferItemList where
  toJSVal = return . unDataTransferItemList
  {-# INLINE toJSVal #-}

instance FromJSVal DataTransferItemList where
  fromJSVal v = fmap DataTransferItemList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DataTransferItemList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DataTransferItemList where
  makeObject = makeObject . unDataTransferItemList

instance IsGObject DataTransferItemList where
  typeGType _ = gTypeDataTransferItemList
  {-# INLINE typeGType #-}

noDataTransferItemList :: Maybe DataTransferItemList
noDataTransferItemList = Nothing
{-# INLINE noDataTransferItemList #-}

gTypeDataTransferItemList :: JSM GType
gTypeDataTransferItemList = GType . Object <$> jsg "DataTransferItemList"

-- | Functions for this inteface are in "JSDOM.Database".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Database Mozilla Database documentation>
newtype Database = Database { unDatabase :: JSVal }

instance PToJSVal Database where
  pToJSVal = unDatabase
  {-# INLINE pToJSVal #-}

instance PFromJSVal Database where
  pFromJSVal = Database
  {-# INLINE pFromJSVal #-}

instance ToJSVal Database where
  toJSVal = return . unDatabase
  {-# INLINE toJSVal #-}

instance FromJSVal Database where
  fromJSVal v = fmap Database <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Database
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Database where
  makeObject = makeObject . unDatabase

instance IsGObject Database where
  typeGType _ = gTypeDatabase
  {-# INLINE typeGType #-}

noDatabase :: Maybe Database
noDatabase = Nothing
{-# INLINE noDatabase #-}

gTypeDatabase :: JSM GType
gTypeDatabase = GType . Object <$> jsg "Database"

-- | Functions for this inteface are in "JSDOM.DedicatedWorkerGlobalScope".
-- Base interface functions are in:
--
--     * "JSDOM.WorkerGlobalScope"
--     * "JSDOM.EventTarget"
--     * "JSDOM.WindowOrWorkerGlobalScope"
--     * "JSDOM.GlobalPerformance"
--     * "JSDOM.GlobalCrypto"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DedicatedWorkerGlobalScope Mozilla DedicatedWorkerGlobalScope documentation>
newtype DedicatedWorkerGlobalScope = DedicatedWorkerGlobalScope { unDedicatedWorkerGlobalScope :: JSVal }

instance PToJSVal DedicatedWorkerGlobalScope where
  pToJSVal = unDedicatedWorkerGlobalScope
  {-# INLINE pToJSVal #-}

instance PFromJSVal DedicatedWorkerGlobalScope where
  pFromJSVal = DedicatedWorkerGlobalScope
  {-# INLINE pFromJSVal #-}

instance ToJSVal DedicatedWorkerGlobalScope where
  toJSVal = return . unDedicatedWorkerGlobalScope
  {-# INLINE toJSVal #-}

instance FromJSVal DedicatedWorkerGlobalScope where
  fromJSVal v = fmap DedicatedWorkerGlobalScope <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DedicatedWorkerGlobalScope
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DedicatedWorkerGlobalScope where
  makeObject = makeObject . unDedicatedWorkerGlobalScope

instance IsWorkerGlobalScope DedicatedWorkerGlobalScope
instance IsEventTarget DedicatedWorkerGlobalScope
instance IsWindowOrWorkerGlobalScope DedicatedWorkerGlobalScope
instance IsGlobalPerformance DedicatedWorkerGlobalScope
instance IsGlobalCrypto DedicatedWorkerGlobalScope
instance IsGObject DedicatedWorkerGlobalScope where
  typeGType _ = gTypeDedicatedWorkerGlobalScope
  {-# INLINE typeGType #-}

noDedicatedWorkerGlobalScope :: Maybe DedicatedWorkerGlobalScope
noDedicatedWorkerGlobalScope = Nothing
{-# INLINE noDedicatedWorkerGlobalScope #-}

gTypeDedicatedWorkerGlobalScope :: JSM GType
gTypeDedicatedWorkerGlobalScope = GType . Object <$> jsg "DedicatedWorkerGlobalScope"

-- | Functions for this inteface are in "JSDOM.DelayNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DelayNode Mozilla DelayNode documentation>
newtype DelayNode = DelayNode { unDelayNode :: JSVal }

instance PToJSVal DelayNode where
  pToJSVal = unDelayNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal DelayNode where
  pFromJSVal = DelayNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal DelayNode where
  toJSVal = return . unDelayNode
  {-# INLINE toJSVal #-}

instance FromJSVal DelayNode where
  fromJSVal v = fmap DelayNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DelayNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DelayNode where
  makeObject = makeObject . unDelayNode

instance IsAudioNode DelayNode
instance IsEventTarget DelayNode
instance IsGObject DelayNode where
  typeGType _ = gTypeDelayNode
  {-# INLINE typeGType #-}

noDelayNode :: Maybe DelayNode
noDelayNode = Nothing
{-# INLINE noDelayNode #-}

gTypeDelayNode :: JSM GType
gTypeDelayNode = GType . Object <$> jsg "DelayNode"

-- | Functions for this inteface are in "JSDOM.DeviceMotionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DeviceMotionEvent Mozilla DeviceMotionEvent documentation>
newtype DeviceMotionEvent = DeviceMotionEvent { unDeviceMotionEvent :: JSVal }

instance PToJSVal DeviceMotionEvent where
  pToJSVal = unDeviceMotionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal DeviceMotionEvent where
  pFromJSVal = DeviceMotionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal DeviceMotionEvent where
  toJSVal = return . unDeviceMotionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal DeviceMotionEvent where
  fromJSVal v = fmap DeviceMotionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DeviceMotionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DeviceMotionEvent where
  makeObject = makeObject . unDeviceMotionEvent

instance IsEvent DeviceMotionEvent
instance IsGObject DeviceMotionEvent where
  typeGType _ = gTypeDeviceMotionEvent
  {-# INLINE typeGType #-}

noDeviceMotionEvent :: Maybe DeviceMotionEvent
noDeviceMotionEvent = Nothing
{-# INLINE noDeviceMotionEvent #-}

gTypeDeviceMotionEvent :: JSM GType
gTypeDeviceMotionEvent = GType . Object <$> jsg "DeviceMotionEvent"

-- | Functions for this inteface are in "JSDOM.DeviceOrientationEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DeviceOrientationEvent Mozilla DeviceOrientationEvent documentation>
newtype DeviceOrientationEvent = DeviceOrientationEvent { unDeviceOrientationEvent :: JSVal }

instance PToJSVal DeviceOrientationEvent where
  pToJSVal = unDeviceOrientationEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal DeviceOrientationEvent where
  pFromJSVal = DeviceOrientationEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal DeviceOrientationEvent where
  toJSVal = return . unDeviceOrientationEvent
  {-# INLINE toJSVal #-}

instance FromJSVal DeviceOrientationEvent where
  fromJSVal v = fmap DeviceOrientationEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DeviceOrientationEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DeviceOrientationEvent where
  makeObject = makeObject . unDeviceOrientationEvent

instance IsEvent DeviceOrientationEvent
instance IsGObject DeviceOrientationEvent where
  typeGType _ = gTypeDeviceOrientationEvent
  {-# INLINE typeGType #-}

noDeviceOrientationEvent :: Maybe DeviceOrientationEvent
noDeviceOrientationEvent = Nothing
{-# INLINE noDeviceOrientationEvent #-}

gTypeDeviceOrientationEvent :: JSM GType
gTypeDeviceOrientationEvent = GType . Object <$> jsg "DeviceOrientationEvent"

-- | Functions for this inteface are in "JSDOM.DeviceProximityEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DeviceProximityEvent Mozilla DeviceProximityEvent documentation>
newtype DeviceProximityEvent = DeviceProximityEvent { unDeviceProximityEvent :: JSVal }

instance PToJSVal DeviceProximityEvent where
  pToJSVal = unDeviceProximityEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal DeviceProximityEvent where
  pFromJSVal = DeviceProximityEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal DeviceProximityEvent where
  toJSVal = return . unDeviceProximityEvent
  {-# INLINE toJSVal #-}

instance FromJSVal DeviceProximityEvent where
  fromJSVal v = fmap DeviceProximityEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DeviceProximityEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DeviceProximityEvent where
  makeObject = makeObject . unDeviceProximityEvent

instance IsEvent DeviceProximityEvent
instance IsGObject DeviceProximityEvent where
  typeGType _ = gTypeDeviceProximityEvent
  {-# INLINE typeGType #-}

noDeviceProximityEvent :: Maybe DeviceProximityEvent
noDeviceProximityEvent = Nothing
{-# INLINE noDeviceProximityEvent #-}

gTypeDeviceProximityEvent :: JSM GType
gTypeDeviceProximityEvent = GType . Object <$> jsg "DeviceProximityEvent"

-- | Functions for this inteface are in "JSDOM.DeviceProximityEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DeviceProximityEventInit Mozilla DeviceProximityEventInit documentation>
newtype DeviceProximityEventInit = DeviceProximityEventInit { unDeviceProximityEventInit :: JSVal }

instance PToJSVal DeviceProximityEventInit where
  pToJSVal = unDeviceProximityEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal DeviceProximityEventInit where
  pFromJSVal = DeviceProximityEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal DeviceProximityEventInit where
  toJSVal = return . unDeviceProximityEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal DeviceProximityEventInit where
  fromJSVal v = fmap DeviceProximityEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DeviceProximityEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DeviceProximityEventInit where
  makeObject = makeObject . unDeviceProximityEventInit

instance IsEventInit DeviceProximityEventInit
instance IsGObject DeviceProximityEventInit where
  typeGType _ = gTypeDeviceProximityEventInit
  {-# INLINE typeGType #-}

noDeviceProximityEventInit :: Maybe DeviceProximityEventInit
noDeviceProximityEventInit = Nothing
{-# INLINE noDeviceProximityEventInit #-}

gTypeDeviceProximityEventInit :: JSM GType
gTypeDeviceProximityEventInit = GType . Object <$> jsg "DeviceProximityEventInit"


-- | Functions for this inteface are in "JSDOM.DocumentTimeline".
-- Base interface functions are in:
--
--     * "JSDOM.AnimationTimeline"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DocumentTimeline Mozilla DocumentTimeline documentation>
newtype DocumentTimeline = DocumentTimeline { unDocumentTimeline :: JSVal }

instance PToJSVal DocumentTimeline where
  pToJSVal = unDocumentTimeline
  {-# INLINE pToJSVal #-}

instance PFromJSVal DocumentTimeline where
  pFromJSVal = DocumentTimeline
  {-# INLINE pFromJSVal #-}

instance ToJSVal DocumentTimeline where
  toJSVal = return . unDocumentTimeline
  {-# INLINE toJSVal #-}

instance FromJSVal DocumentTimeline where
  fromJSVal v = fmap DocumentTimeline <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DocumentTimeline
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DocumentTimeline where
  makeObject = makeObject . unDocumentTimeline

instance IsAnimationTimeline DocumentTimeline
instance IsGObject DocumentTimeline where
  typeGType _ = gTypeDocumentTimeline
  {-# INLINE typeGType #-}

noDocumentTimeline :: Maybe DocumentTimeline
noDocumentTimeline = Nothing
{-# INLINE noDocumentTimeline #-}

gTypeDocumentTimeline :: JSM GType
gTypeDocumentTimeline = GType . Object <$> jsg "DocumentTimeline"

-- | Functions for this inteface are in "JSDOM.DoubleRange".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DoubleRange Mozilla DoubleRange documentation>
newtype DoubleRange = DoubleRange { unDoubleRange :: JSVal }

instance PToJSVal DoubleRange where
  pToJSVal = unDoubleRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal DoubleRange where
  pFromJSVal = DoubleRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal DoubleRange where
  toJSVal = return . unDoubleRange
  {-# INLINE toJSVal #-}

instance FromJSVal DoubleRange where
  fromJSVal v = fmap DoubleRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DoubleRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DoubleRange where
  makeObject = makeObject . unDoubleRange

class (IsGObject o) => IsDoubleRange o
toDoubleRange :: IsDoubleRange o => o -> DoubleRange
toDoubleRange = DoubleRange . coerce

instance IsDoubleRange DoubleRange
instance IsGObject DoubleRange where
  typeGType _ = gTypeDoubleRange
  {-# INLINE typeGType #-}

noDoubleRange :: Maybe DoubleRange
noDoubleRange = Nothing
{-# INLINE noDoubleRange #-}

gTypeDoubleRange :: JSM GType
gTypeDoubleRange = GType . Object <$> jsg "DoubleRange"

-- | Functions for this inteface are in "JSDOM.DynamicsCompressorNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DynamicsCompressorNode Mozilla DynamicsCompressorNode documentation>
newtype DynamicsCompressorNode = DynamicsCompressorNode { unDynamicsCompressorNode :: JSVal }

instance PToJSVal DynamicsCompressorNode where
  pToJSVal = unDynamicsCompressorNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal DynamicsCompressorNode where
  pFromJSVal = DynamicsCompressorNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal DynamicsCompressorNode where
  toJSVal = return . unDynamicsCompressorNode
  {-# INLINE toJSVal #-}

instance FromJSVal DynamicsCompressorNode where
  fromJSVal v = fmap DynamicsCompressorNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DynamicsCompressorNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DynamicsCompressorNode where
  makeObject = makeObject . unDynamicsCompressorNode

instance IsAudioNode DynamicsCompressorNode
instance IsEventTarget DynamicsCompressorNode
instance IsGObject DynamicsCompressorNode where
  typeGType _ = gTypeDynamicsCompressorNode
  {-# INLINE typeGType #-}

noDynamicsCompressorNode :: Maybe DynamicsCompressorNode
noDynamicsCompressorNode = Nothing
{-# INLINE noDynamicsCompressorNode #-}

gTypeDynamicsCompressorNode :: JSM GType
gTypeDynamicsCompressorNode = GType . Object <$> jsg "DynamicsCompressorNode"

-- | Functions for this inteface are in "JSDOM.EXTBlendMinMax".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EXTBlendMinMax Mozilla EXTBlendMinMax documentation>
newtype EXTBlendMinMax = EXTBlendMinMax { unEXTBlendMinMax :: JSVal }

instance PToJSVal EXTBlendMinMax where
  pToJSVal = unEXTBlendMinMax
  {-# INLINE pToJSVal #-}

instance PFromJSVal EXTBlendMinMax where
  pFromJSVal = EXTBlendMinMax
  {-# INLINE pFromJSVal #-}

instance ToJSVal EXTBlendMinMax where
  toJSVal = return . unEXTBlendMinMax
  {-# INLINE toJSVal #-}

instance FromJSVal EXTBlendMinMax where
  fromJSVal v = fmap EXTBlendMinMax <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EXTBlendMinMax
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EXTBlendMinMax where
  makeObject = makeObject . unEXTBlendMinMax

instance IsGObject EXTBlendMinMax where
  typeGType _ = gTypeEXTBlendMinMax
  {-# INLINE typeGType #-}

noEXTBlendMinMax :: Maybe EXTBlendMinMax
noEXTBlendMinMax = Nothing
{-# INLINE noEXTBlendMinMax #-}

gTypeEXTBlendMinMax :: JSM GType
gTypeEXTBlendMinMax = GType . Object <$> jsg "EXTBlendMinMax"

-- | Functions for this inteface are in "JSDOM.EXTFragDepth".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EXTFragDepth Mozilla EXTFragDepth documentation>
newtype EXTFragDepth = EXTFragDepth { unEXTFragDepth :: JSVal }

instance PToJSVal EXTFragDepth where
  pToJSVal = unEXTFragDepth
  {-# INLINE pToJSVal #-}

instance PFromJSVal EXTFragDepth where
  pFromJSVal = EXTFragDepth
  {-# INLINE pFromJSVal #-}

instance ToJSVal EXTFragDepth where
  toJSVal = return . unEXTFragDepth
  {-# INLINE toJSVal #-}

instance FromJSVal EXTFragDepth where
  fromJSVal v = fmap EXTFragDepth <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EXTFragDepth
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EXTFragDepth where
  makeObject = makeObject . unEXTFragDepth

instance IsGObject EXTFragDepth where
  typeGType _ = gTypeEXTFragDepth
  {-# INLINE typeGType #-}

noEXTFragDepth :: Maybe EXTFragDepth
noEXTFragDepth = Nothing
{-# INLINE noEXTFragDepth #-}

gTypeEXTFragDepth :: JSM GType
gTypeEXTFragDepth = GType . Object <$> jsg "EXTFragDepth"

-- | Functions for this inteface are in "JSDOM.EXTShaderTextureLOD".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EXTShaderTextureLOD Mozilla EXTShaderTextureLOD documentation>
newtype EXTShaderTextureLOD = EXTShaderTextureLOD { unEXTShaderTextureLOD :: JSVal }

instance PToJSVal EXTShaderTextureLOD where
  pToJSVal = unEXTShaderTextureLOD
  {-# INLINE pToJSVal #-}

instance PFromJSVal EXTShaderTextureLOD where
  pFromJSVal = EXTShaderTextureLOD
  {-# INLINE pFromJSVal #-}

instance ToJSVal EXTShaderTextureLOD where
  toJSVal = return . unEXTShaderTextureLOD
  {-# INLINE toJSVal #-}

instance FromJSVal EXTShaderTextureLOD where
  fromJSVal v = fmap EXTShaderTextureLOD <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EXTShaderTextureLOD
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EXTShaderTextureLOD where
  makeObject = makeObject . unEXTShaderTextureLOD

instance IsGObject EXTShaderTextureLOD where
  typeGType _ = gTypeEXTShaderTextureLOD
  {-# INLINE typeGType #-}

noEXTShaderTextureLOD :: Maybe EXTShaderTextureLOD
noEXTShaderTextureLOD = Nothing
{-# INLINE noEXTShaderTextureLOD #-}

gTypeEXTShaderTextureLOD :: JSM GType
gTypeEXTShaderTextureLOD = GType . Object <$> jsg "EXTShaderTextureLOD"

-- | Functions for this inteface are in "JSDOM.EXTTextureFilterAnisotropic".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EXTTextureFilterAnisotropic Mozilla EXTTextureFilterAnisotropic documentation>
newtype EXTTextureFilterAnisotropic = EXTTextureFilterAnisotropic { unEXTTextureFilterAnisotropic :: JSVal }

instance PToJSVal EXTTextureFilterAnisotropic where
  pToJSVal = unEXTTextureFilterAnisotropic
  {-# INLINE pToJSVal #-}

instance PFromJSVal EXTTextureFilterAnisotropic where
  pFromJSVal = EXTTextureFilterAnisotropic
  {-# INLINE pFromJSVal #-}

instance ToJSVal EXTTextureFilterAnisotropic where
  toJSVal = return . unEXTTextureFilterAnisotropic
  {-# INLINE toJSVal #-}

instance FromJSVal EXTTextureFilterAnisotropic where
  fromJSVal v = fmap EXTTextureFilterAnisotropic <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EXTTextureFilterAnisotropic
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EXTTextureFilterAnisotropic where
  makeObject = makeObject . unEXTTextureFilterAnisotropic

instance IsGObject EXTTextureFilterAnisotropic where
  typeGType _ = gTypeEXTTextureFilterAnisotropic
  {-# INLINE typeGType #-}

noEXTTextureFilterAnisotropic :: Maybe EXTTextureFilterAnisotropic
noEXTTextureFilterAnisotropic = Nothing
{-# INLINE noEXTTextureFilterAnisotropic #-}

gTypeEXTTextureFilterAnisotropic :: JSM GType
gTypeEXTTextureFilterAnisotropic = GType . Object <$> jsg "EXTTextureFilterAnisotropic"

-- | Functions for this inteface are in "JSDOM.EXTsRGB".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EXTsRGB Mozilla EXTsRGB documentation>
newtype EXTsRGB = EXTsRGB { unEXTsRGB :: JSVal }

instance PToJSVal EXTsRGB where
  pToJSVal = unEXTsRGB
  {-# INLINE pToJSVal #-}

instance PFromJSVal EXTsRGB where
  pFromJSVal = EXTsRGB
  {-# INLINE pFromJSVal #-}

instance ToJSVal EXTsRGB where
  toJSVal = return . unEXTsRGB
  {-# INLINE toJSVal #-}

instance FromJSVal EXTsRGB where
  fromJSVal v = fmap EXTsRGB <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EXTsRGB
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EXTsRGB where
  makeObject = makeObject . unEXTsRGB

instance IsGObject EXTsRGB where
  typeGType _ = gTypeEXTsRGB
  {-# INLINE typeGType #-}

noEXTsRGB :: Maybe EXTsRGB
noEXTsRGB = Nothing
{-# INLINE noEXTsRGB #-}

gTypeEXTsRGB :: JSM GType
gTypeEXTsRGB = GType . Object <$> jsg "EXTsRGB"

-- | Functions for this inteface are in "JSDOM.EcKeyParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EcKeyParams Mozilla EcKeyParams documentation>
newtype EcKeyParams = EcKeyParams { unEcKeyParams :: JSVal }

instance PToJSVal EcKeyParams where
  pToJSVal = unEcKeyParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal EcKeyParams where
  pFromJSVal = EcKeyParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal EcKeyParams where
  toJSVal = return . unEcKeyParams
  {-# INLINE toJSVal #-}

instance FromJSVal EcKeyParams where
  fromJSVal v = fmap EcKeyParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EcKeyParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EcKeyParams where
  makeObject = makeObject . unEcKeyParams

instance IsCryptoAlgorithmParameters EcKeyParams
instance IsGObject EcKeyParams where
  typeGType _ = gTypeEcKeyParams
  {-# INLINE typeGType #-}

noEcKeyParams :: Maybe EcKeyParams
noEcKeyParams = Nothing
{-# INLINE noEcKeyParams #-}

gTypeEcKeyParams :: JSM GType
gTypeEcKeyParams = GType . Object <$> jsg "EcKeyParams"

-- | Functions for this inteface are in "JSDOM.EcdhKeyDeriveParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EcdhKeyDeriveParams Mozilla EcdhKeyDeriveParams documentation>
newtype EcdhKeyDeriveParams = EcdhKeyDeriveParams { unEcdhKeyDeriveParams :: JSVal }

instance PToJSVal EcdhKeyDeriveParams where
  pToJSVal = unEcdhKeyDeriveParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal EcdhKeyDeriveParams where
  pFromJSVal = EcdhKeyDeriveParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal EcdhKeyDeriveParams where
  toJSVal = return . unEcdhKeyDeriveParams
  {-# INLINE toJSVal #-}

instance FromJSVal EcdhKeyDeriveParams where
  fromJSVal v = fmap EcdhKeyDeriveParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EcdhKeyDeriveParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EcdhKeyDeriveParams where
  makeObject = makeObject . unEcdhKeyDeriveParams

instance IsCryptoAlgorithmParameters EcdhKeyDeriveParams
instance IsGObject EcdhKeyDeriveParams where
  typeGType _ = gTypeEcdhKeyDeriveParams
  {-# INLINE typeGType #-}

noEcdhKeyDeriveParams :: Maybe EcdhKeyDeriveParams
noEcdhKeyDeriveParams = Nothing
{-# INLINE noEcdhKeyDeriveParams #-}

gTypeEcdhKeyDeriveParams :: JSM GType
gTypeEcdhKeyDeriveParams = GType . Object <$> jsg "EcdhKeyDeriveParams"

-- | Functions for this inteface are in "JSDOM.EcdsaParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EcdsaParams Mozilla EcdsaParams documentation>
newtype EcdsaParams = EcdsaParams { unEcdsaParams :: JSVal }

instance PToJSVal EcdsaParams where
  pToJSVal = unEcdsaParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal EcdsaParams where
  pFromJSVal = EcdsaParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal EcdsaParams where
  toJSVal = return . unEcdsaParams
  {-# INLINE toJSVal #-}

instance FromJSVal EcdsaParams where
  fromJSVal v = fmap EcdsaParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EcdsaParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EcdsaParams where
  makeObject = makeObject . unEcdsaParams

instance IsCryptoAlgorithmParameters EcdsaParams
instance IsGObject EcdsaParams where
  typeGType _ = gTypeEcdsaParams
  {-# INLINE typeGType #-}

noEcdsaParams :: Maybe EcdsaParams
noEcdsaParams = Nothing
{-# INLINE noEcdsaParams #-}

gTypeEcdsaParams :: JSM GType
gTypeEcdsaParams = GType . Object <$> jsg "EcdsaParams"

-- | Functions for this inteface are in "JSDOM.ErrorEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEvent Mozilla ErrorEvent documentation>
newtype ErrorEvent = ErrorEvent { unErrorEvent :: JSVal }

instance PToJSVal ErrorEvent where
  pToJSVal = unErrorEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ErrorEvent where
  pFromJSVal = ErrorEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ErrorEvent where
  toJSVal = return . unErrorEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ErrorEvent where
  fromJSVal v = fmap ErrorEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ErrorEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ErrorEvent where
  makeObject = makeObject . unErrorEvent

instance IsEvent ErrorEvent
instance IsGObject ErrorEvent where
  typeGType _ = gTypeErrorEvent
  {-# INLINE typeGType #-}

noErrorEvent :: Maybe ErrorEvent
noErrorEvent = Nothing
{-# INLINE noErrorEvent #-}

gTypeErrorEvent :: JSM GType
gTypeErrorEvent = GType . Object <$> jsg "ErrorEvent"

-- | Functions for this inteface are in "JSDOM.ErrorEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ErrorEventInit Mozilla ErrorEventInit documentation>
newtype ErrorEventInit = ErrorEventInit { unErrorEventInit :: JSVal }

instance PToJSVal ErrorEventInit where
  pToJSVal = unErrorEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal ErrorEventInit where
  pFromJSVal = ErrorEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal ErrorEventInit where
  toJSVal = return . unErrorEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal ErrorEventInit where
  fromJSVal v = fmap ErrorEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ErrorEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ErrorEventInit where
  makeObject = makeObject . unErrorEventInit

instance IsEventInit ErrorEventInit
instance IsGObject ErrorEventInit where
  typeGType _ = gTypeErrorEventInit
  {-# INLINE typeGType #-}

noErrorEventInit :: Maybe ErrorEventInit
noErrorEventInit = Nothing
{-# INLINE noErrorEventInit #-}

gTypeErrorEventInit :: JSM GType
gTypeErrorEventInit = GType . Object <$> jsg "ErrorEventInit"

-- | Functions for this inteface are in "JSDOM.EventListener".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventListener Mozilla EventListener documentation>
newtype EventListener = EventListener { unEventListener :: JSVal }

instance PToJSVal EventListener where
  pToJSVal = unEventListener
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventListener where
  pFromJSVal = EventListener
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventListener where
  toJSVal = return . unEventListener
  {-# INLINE toJSVal #-}

instance FromJSVal EventListener where
  fromJSVal v = fmap EventListener <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventListener
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventListener where
  makeObject = makeObject . unEventListener

instance IsGObject EventListener where
  typeGType _ = gTypeEventListener
  {-# INLINE typeGType #-}

noEventListener :: Maybe EventListener
noEventListener = Nothing
{-# INLINE noEventListener #-}

gTypeEventListener :: JSM GType
gTypeEventListener = GType . Object <$> jsg "EventListener"

-- | Functions for this inteface are in "JSDOM.EventModifierInit".
-- Base interface functions are in:
--
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventModifierInit Mozilla EventModifierInit documentation>
newtype EventModifierInit = EventModifierInit { unEventModifierInit :: JSVal }

instance PToJSVal EventModifierInit where
  pToJSVal = unEventModifierInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventModifierInit where
  pFromJSVal = EventModifierInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventModifierInit where
  toJSVal = return . unEventModifierInit
  {-# INLINE toJSVal #-}

instance FromJSVal EventModifierInit where
  fromJSVal v = fmap EventModifierInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventModifierInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventModifierInit where
  makeObject = makeObject . unEventModifierInit

class (IsUIEventInit o, IsEventInit o, IsGObject o) => IsEventModifierInit o
toEventModifierInit :: IsEventModifierInit o => o -> EventModifierInit
toEventModifierInit = EventModifierInit . coerce

instance IsEventModifierInit EventModifierInit
instance IsUIEventInit EventModifierInit
instance IsEventInit EventModifierInit
instance IsGObject EventModifierInit where
  typeGType _ = gTypeEventModifierInit
  {-# INLINE typeGType #-}

noEventModifierInit :: Maybe EventModifierInit
noEventModifierInit = Nothing
{-# INLINE noEventModifierInit #-}

gTypeEventModifierInit :: JSM GType
gTypeEventModifierInit = GType . Object <$> jsg "EventModifierInit"

-- | Functions for this inteface are in "JSDOM.EventSource".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventSource Mozilla EventSource documentation>
newtype EventSource = EventSource { unEventSource :: JSVal }

instance PToJSVal EventSource where
  pToJSVal = unEventSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventSource where
  pFromJSVal = EventSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventSource where
  toJSVal = return . unEventSource
  {-# INLINE toJSVal #-}

instance FromJSVal EventSource where
  fromJSVal v = fmap EventSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventSource where
  makeObject = makeObject . unEventSource

instance IsEventTarget EventSource
instance IsGObject EventSource where
  typeGType _ = gTypeEventSource
  {-# INLINE typeGType #-}

noEventSource :: Maybe EventSource
noEventSource = Nothing
{-# INLINE noEventSource #-}

gTypeEventSource :: JSM GType
gTypeEventSource = GType . Object <$> jsg "EventSource"

-- | Functions for this inteface are in "JSDOM.EventSourceInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventSourceInit Mozilla EventSourceInit documentation>
newtype EventSourceInit = EventSourceInit { unEventSourceInit :: JSVal }

instance PToJSVal EventSourceInit where
  pToJSVal = unEventSourceInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventSourceInit where
  pFromJSVal = EventSourceInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventSourceInit where
  toJSVal = return . unEventSourceInit
  {-# INLINE toJSVal #-}

instance FromJSVal EventSourceInit where
  fromJSVal v = fmap EventSourceInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventSourceInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventSourceInit where
  makeObject = makeObject . unEventSourceInit

instance IsGObject EventSourceInit where
  typeGType _ = gTypeEventSourceInit
  {-# INLINE typeGType #-}

noEventSourceInit :: Maybe EventSourceInit
noEventSourceInit = Nothing
{-# INLINE noEventSourceInit #-}

gTypeEventSourceInit :: JSM GType
gTypeEventSourceInit = GType . Object <$> jsg "EventSourceInit"

-- | Functions for this inteface are in "JSDOM.FileError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FileError Mozilla FileError documentation>
newtype FileError = FileError { unFileError :: JSVal }

instance PToJSVal FileError where
  pToJSVal = unFileError
  {-# INLINE pToJSVal #-}

instance PFromJSVal FileError where
  pFromJSVal = FileError
  {-# INLINE pFromJSVal #-}

instance ToJSVal FileError where
  toJSVal = return . unFileError
  {-# INLINE toJSVal #-}

instance FromJSVal FileError where
  fromJSVal v = fmap FileError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FileError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FileError where
  makeObject = makeObject . unFileError

instance IsGObject FileError where
  typeGType _ = gTypeFileError
  {-# INLINE typeGType #-}

noFileError :: Maybe FileError
noFileError = Nothing
{-# INLINE noFileError #-}

gTypeFileError :: JSM GType
gTypeFileError = GType . Object <$> jsg "FileError"

-- | Functions for this inteface are in "JSDOM.FileException".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FileException Mozilla FileException documentation>
newtype FileException = FileException { unFileException :: JSVal }

instance PToJSVal FileException where
  pToJSVal = unFileException
  {-# INLINE pToJSVal #-}

instance PFromJSVal FileException where
  pFromJSVal = FileException
  {-# INLINE pFromJSVal #-}

instance ToJSVal FileException where
  toJSVal = return . unFileException
  {-# INLINE toJSVal #-}

instance FromJSVal FileException where
  fromJSVal v = fmap FileException <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FileException
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FileException where
  makeObject = makeObject . unFileException

instance IsGObject FileException where
  typeGType _ = gTypeFileException
  {-# INLINE typeGType #-}

noFileException :: Maybe FileException
noFileException = Nothing
{-# INLINE noFileException #-}

gTypeFileException :: JSM GType
gTypeFileException = GType . Object <$> jsg "FileException"

-- | Functions for this inteface are in "JSDOM.FileList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FileList Mozilla FileList documentation>
newtype FileList = FileList { unFileList :: JSVal }

instance PToJSVal FileList where
  pToJSVal = unFileList
  {-# INLINE pToJSVal #-}

instance PFromJSVal FileList where
  pFromJSVal = FileList
  {-# INLINE pFromJSVal #-}

instance ToJSVal FileList where
  toJSVal = return . unFileList
  {-# INLINE toJSVal #-}

instance FromJSVal FileList where
  fromJSVal v = fmap FileList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FileList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FileList where
  makeObject = makeObject . unFileList

instance IsGObject FileList where
  typeGType _ = gTypeFileList
  {-# INLINE typeGType #-}

noFileList :: Maybe FileList
noFileList = Nothing
{-# INLINE noFileList #-}

gTypeFileList :: JSM GType
gTypeFileList = GType . Object <$> jsg "FileList"

-- | Functions for this inteface are in "JSDOM.FilePropertyBag".
-- Base interface functions are in:
--
--     * "JSDOM.BlobPropertyBag"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FilePropertyBag Mozilla FilePropertyBag documentation>
newtype FilePropertyBag = FilePropertyBag { unFilePropertyBag :: JSVal }

instance PToJSVal FilePropertyBag where
  pToJSVal = unFilePropertyBag
  {-# INLINE pToJSVal #-}

instance PFromJSVal FilePropertyBag where
  pFromJSVal = FilePropertyBag
  {-# INLINE pFromJSVal #-}

instance ToJSVal FilePropertyBag where
  toJSVal = return . unFilePropertyBag
  {-# INLINE toJSVal #-}

instance FromJSVal FilePropertyBag where
  fromJSVal v = fmap FilePropertyBag <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FilePropertyBag
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FilePropertyBag where
  makeObject = makeObject . unFilePropertyBag

instance IsBlobPropertyBag FilePropertyBag
instance IsGObject FilePropertyBag where
  typeGType _ = gTypeFilePropertyBag
  {-# INLINE typeGType #-}

noFilePropertyBag :: Maybe FilePropertyBag
noFilePropertyBag = Nothing
{-# INLINE noFilePropertyBag #-}

gTypeFilePropertyBag :: JSM GType
gTypeFilePropertyBag = GType . Object <$> jsg "FilePropertyBag"

-- | Functions for this inteface are in "JSDOM.FileReader".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FileReader Mozilla FileReader documentation>
newtype FileReader = FileReader { unFileReader :: JSVal }

instance PToJSVal FileReader where
  pToJSVal = unFileReader
  {-# INLINE pToJSVal #-}

instance PFromJSVal FileReader where
  pFromJSVal = FileReader
  {-# INLINE pFromJSVal #-}

instance ToJSVal FileReader where
  toJSVal = return . unFileReader
  {-# INLINE toJSVal #-}

instance FromJSVal FileReader where
  fromJSVal v = fmap FileReader <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FileReader
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FileReader where
  makeObject = makeObject . unFileReader

instance IsEventTarget FileReader
instance IsGObject FileReader where
  typeGType _ = gTypeFileReader
  {-# INLINE typeGType #-}

noFileReader :: Maybe FileReader
noFileReader = Nothing
{-# INLINE noFileReader #-}

gTypeFileReader :: JSM GType
gTypeFileReader = GType . Object <$> jsg "FileReader"

-- | Functions for this inteface are in "JSDOM.FileReaderSync".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FileReaderSync Mozilla FileReaderSync documentation>
newtype FileReaderSync = FileReaderSync { unFileReaderSync :: JSVal }

instance PToJSVal FileReaderSync where
  pToJSVal = unFileReaderSync
  {-# INLINE pToJSVal #-}

instance PFromJSVal FileReaderSync where
  pFromJSVal = FileReaderSync
  {-# INLINE pFromJSVal #-}

instance ToJSVal FileReaderSync where
  toJSVal = return . unFileReaderSync
  {-# INLINE toJSVal #-}

instance FromJSVal FileReaderSync where
  fromJSVal v = fmap FileReaderSync <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FileReaderSync
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FileReaderSync where
  makeObject = makeObject . unFileReaderSync

instance IsGObject FileReaderSync where
  typeGType _ = gTypeFileReaderSync
  {-# INLINE typeGType #-}

noFileReaderSync :: Maybe FileReaderSync
noFileReaderSync = Nothing
{-# INLINE noFileReaderSync #-}

gTypeFileReaderSync :: JSM GType
gTypeFileReaderSync = GType . Object <$> jsg "FileReaderSync"

-- | Functions for this inteface are in "JSDOM.FocusEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FocusEvent Mozilla FocusEvent documentation>
newtype FocusEvent = FocusEvent { unFocusEvent :: JSVal }

instance PToJSVal FocusEvent where
  pToJSVal = unFocusEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal FocusEvent where
  pFromJSVal = FocusEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal FocusEvent where
  toJSVal = return . unFocusEvent
  {-# INLINE toJSVal #-}

instance FromJSVal FocusEvent where
  fromJSVal v = fmap FocusEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FocusEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FocusEvent where
  makeObject = makeObject . unFocusEvent

instance IsUIEvent FocusEvent
instance IsEvent FocusEvent
instance IsGObject FocusEvent where
  typeGType _ = gTypeFocusEvent
  {-# INLINE typeGType #-}

noFocusEvent :: Maybe FocusEvent
noFocusEvent = Nothing
{-# INLINE noFocusEvent #-}

gTypeFocusEvent :: JSM GType
gTypeFocusEvent = GType . Object <$> jsg "FocusEvent"

-- | Functions for this inteface are in "JSDOM.FocusEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FocusEventInit Mozilla FocusEventInit documentation>
newtype FocusEventInit = FocusEventInit { unFocusEventInit :: JSVal }

instance PToJSVal FocusEventInit where
  pToJSVal = unFocusEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal FocusEventInit where
  pFromJSVal = FocusEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal FocusEventInit where
  toJSVal = return . unFocusEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal FocusEventInit where
  fromJSVal v = fmap FocusEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FocusEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FocusEventInit where
  makeObject = makeObject . unFocusEventInit

instance IsUIEventInit FocusEventInit
instance IsEventInit FocusEventInit
instance IsGObject FocusEventInit where
  typeGType _ = gTypeFocusEventInit
  {-# INLINE typeGType #-}

noFocusEventInit :: Maybe FocusEventInit
noFocusEventInit = Nothing
{-# INLINE noFocusEventInit #-}

gTypeFocusEventInit :: JSM GType
gTypeFocusEventInit = GType . Object <$> jsg "FocusEventInit"

-- | Functions for this inteface are in "JSDOM.FontFace".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FontFace Mozilla FontFace documentation>
newtype FontFace = FontFace { unFontFace :: JSVal }

instance PToJSVal FontFace where
  pToJSVal = unFontFace
  {-# INLINE pToJSVal #-}

instance PFromJSVal FontFace where
  pFromJSVal = FontFace
  {-# INLINE pFromJSVal #-}

instance ToJSVal FontFace where
  toJSVal = return . unFontFace
  {-# INLINE toJSVal #-}

instance FromJSVal FontFace where
  fromJSVal v = fmap FontFace <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FontFace
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FontFace where
  makeObject = makeObject . unFontFace

instance IsGObject FontFace where
  typeGType _ = gTypeFontFace
  {-# INLINE typeGType #-}

noFontFace :: Maybe FontFace
noFontFace = Nothing
{-# INLINE noFontFace #-}

gTypeFontFace :: JSM GType
gTypeFontFace = GType . Object <$> jsg "FontFace"

-- | Functions for this inteface are in "JSDOM.FontFaceDescriptors".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FontFaceDescriptors Mozilla FontFaceDescriptors documentation>
newtype FontFaceDescriptors = FontFaceDescriptors { unFontFaceDescriptors :: JSVal }

instance PToJSVal FontFaceDescriptors where
  pToJSVal = unFontFaceDescriptors
  {-# INLINE pToJSVal #-}

instance PFromJSVal FontFaceDescriptors where
  pFromJSVal = FontFaceDescriptors
  {-# INLINE pFromJSVal #-}

instance ToJSVal FontFaceDescriptors where
  toJSVal = return . unFontFaceDescriptors
  {-# INLINE toJSVal #-}

instance FromJSVal FontFaceDescriptors where
  fromJSVal v = fmap FontFaceDescriptors <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FontFaceDescriptors
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FontFaceDescriptors where
  makeObject = makeObject . unFontFaceDescriptors

instance IsGObject FontFaceDescriptors where
  typeGType _ = gTypeFontFaceDescriptors
  {-# INLINE typeGType #-}

noFontFaceDescriptors :: Maybe FontFaceDescriptors
noFontFaceDescriptors = Nothing
{-# INLINE noFontFaceDescriptors #-}

gTypeFontFaceDescriptors :: JSM GType
gTypeFontFaceDescriptors = GType . Object <$> jsg "FontFaceDescriptors"

-- | Functions for this inteface are in "JSDOM.FontFaceSet".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/FontFaceSet Mozilla FontFaceSet documentation>
newtype FontFaceSet = FontFaceSet { unFontFaceSet :: JSVal }

instance PToJSVal FontFaceSet where
  pToJSVal = unFontFaceSet
  {-# INLINE pToJSVal #-}

instance PFromJSVal FontFaceSet where
  pFromJSVal = FontFaceSet
  {-# INLINE pFromJSVal #-}

instance ToJSVal FontFaceSet where
  toJSVal = return . unFontFaceSet
  {-# INLINE toJSVal #-}

instance FromJSVal FontFaceSet where
  fromJSVal v = fmap FontFaceSet <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . FontFaceSet
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject FontFaceSet where
  makeObject = makeObject . unFontFaceSet

instance IsEventTarget FontFaceSet
instance IsGObject FontFaceSet where
  typeGType _ = gTypeFontFaceSet
  {-# INLINE typeGType #-}

noFontFaceSet :: Maybe FontFaceSet
noFontFaceSet = Nothing
{-# INLINE noFontFaceSet #-}

gTypeFontFaceSet :: JSM GType
gTypeFontFaceSet = GType . Object <$> jsg "FontFaceSet"

-- | Functions for this inteface are in "JSDOM.GainNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GainNode Mozilla GainNode documentation>
newtype GainNode = GainNode { unGainNode :: JSVal }

instance PToJSVal GainNode where
  pToJSVal = unGainNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal GainNode where
  pFromJSVal = GainNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal GainNode where
  toJSVal = return . unGainNode
  {-# INLINE toJSVal #-}

instance FromJSVal GainNode where
  fromJSVal v = fmap GainNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GainNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GainNode where
  makeObject = makeObject . unGainNode

instance IsAudioNode GainNode
instance IsEventTarget GainNode
instance IsGObject GainNode where
  typeGType _ = gTypeGainNode
  {-# INLINE typeGType #-}

noGainNode :: Maybe GainNode
noGainNode = Nothing
{-# INLINE noGainNode #-}

gTypeGainNode :: JSM GType
gTypeGainNode = GType . Object <$> jsg "GainNode"

-- | Functions for this inteface are in "JSDOM.Gamepad".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Gamepad Mozilla Gamepad documentation>
newtype Gamepad = Gamepad { unGamepad :: JSVal }

instance PToJSVal Gamepad where
  pToJSVal = unGamepad
  {-# INLINE pToJSVal #-}

instance PFromJSVal Gamepad where
  pFromJSVal = Gamepad
  {-# INLINE pFromJSVal #-}

instance ToJSVal Gamepad where
  toJSVal = return . unGamepad
  {-# INLINE toJSVal #-}

instance FromJSVal Gamepad where
  fromJSVal v = fmap Gamepad <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Gamepad
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Gamepad where
  makeObject = makeObject . unGamepad

instance IsGObject Gamepad where
  typeGType _ = gTypeGamepad
  {-# INLINE typeGType #-}

noGamepad :: Maybe Gamepad
noGamepad = Nothing
{-# INLINE noGamepad #-}

gTypeGamepad :: JSM GType
gTypeGamepad = GType . Object <$> jsg "Gamepad"

-- | Functions for this inteface are in "JSDOM.GamepadButton".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GamepadButton Mozilla GamepadButton documentation>
newtype GamepadButton = GamepadButton { unGamepadButton :: JSVal }

instance PToJSVal GamepadButton where
  pToJSVal = unGamepadButton
  {-# INLINE pToJSVal #-}

instance PFromJSVal GamepadButton where
  pFromJSVal = GamepadButton
  {-# INLINE pFromJSVal #-}

instance ToJSVal GamepadButton where
  toJSVal = return . unGamepadButton
  {-# INLINE toJSVal #-}

instance FromJSVal GamepadButton where
  fromJSVal v = fmap GamepadButton <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GamepadButton
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GamepadButton where
  makeObject = makeObject . unGamepadButton

instance IsGObject GamepadButton where
  typeGType _ = gTypeGamepadButton
  {-# INLINE typeGType #-}

noGamepadButton :: Maybe GamepadButton
noGamepadButton = Nothing
{-# INLINE noGamepadButton #-}

gTypeGamepadButton :: JSM GType
gTypeGamepadButton = GType . Object <$> jsg "GamepadButton"

-- | Functions for this inteface are in "JSDOM.GamepadEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GamepadEvent Mozilla GamepadEvent documentation>
newtype GamepadEvent = GamepadEvent { unGamepadEvent :: JSVal }

instance PToJSVal GamepadEvent where
  pToJSVal = unGamepadEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal GamepadEvent where
  pFromJSVal = GamepadEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal GamepadEvent where
  toJSVal = return . unGamepadEvent
  {-# INLINE toJSVal #-}

instance FromJSVal GamepadEvent where
  fromJSVal v = fmap GamepadEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GamepadEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GamepadEvent where
  makeObject = makeObject . unGamepadEvent

instance IsEvent GamepadEvent
instance IsGObject GamepadEvent where
  typeGType _ = gTypeGamepadEvent
  {-# INLINE typeGType #-}

noGamepadEvent :: Maybe GamepadEvent
noGamepadEvent = Nothing
{-# INLINE noGamepadEvent #-}

gTypeGamepadEvent :: JSM GType
gTypeGamepadEvent = GType . Object <$> jsg "GamepadEvent"

-- | Functions for this inteface are in "JSDOM.GamepadEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GamepadEventInit Mozilla GamepadEventInit documentation>
newtype GamepadEventInit = GamepadEventInit { unGamepadEventInit :: JSVal }

instance PToJSVal GamepadEventInit where
  pToJSVal = unGamepadEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal GamepadEventInit where
  pFromJSVal = GamepadEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal GamepadEventInit where
  toJSVal = return . unGamepadEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal GamepadEventInit where
  fromJSVal v = fmap GamepadEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GamepadEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GamepadEventInit where
  makeObject = makeObject . unGamepadEventInit

instance IsEventInit GamepadEventInit
instance IsGObject GamepadEventInit where
  typeGType _ = gTypeGamepadEventInit
  {-# INLINE typeGType #-}

noGamepadEventInit :: Maybe GamepadEventInit
noGamepadEventInit = Nothing
{-# INLINE noGamepadEventInit #-}

gTypeGamepadEventInit :: JSM GType
gTypeGamepadEventInit = GType . Object <$> jsg "GamepadEventInit"

-- | Functions for this inteface are in "JSDOM.Geolocation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Geolocation Mozilla Geolocation documentation>
newtype Geolocation = Geolocation { unGeolocation :: JSVal }

instance PToJSVal Geolocation where
  pToJSVal = unGeolocation
  {-# INLINE pToJSVal #-}

instance PFromJSVal Geolocation where
  pFromJSVal = Geolocation
  {-# INLINE pFromJSVal #-}

instance ToJSVal Geolocation where
  toJSVal = return . unGeolocation
  {-# INLINE toJSVal #-}

instance FromJSVal Geolocation where
  fromJSVal v = fmap Geolocation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Geolocation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Geolocation where
  makeObject = makeObject . unGeolocation

instance IsGObject Geolocation where
  typeGType _ = gTypeGeolocation
  {-# INLINE typeGType #-}

noGeolocation :: Maybe Geolocation
noGeolocation = Nothing
{-# INLINE noGeolocation #-}

gTypeGeolocation :: JSM GType
gTypeGeolocation = GType . Object <$> jsg "Geolocation"

-- | Functions for this inteface are in "JSDOM.Geoposition".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Geoposition Mozilla Geoposition documentation>
newtype Geoposition = Geoposition { unGeoposition :: JSVal }

instance PToJSVal Geoposition where
  pToJSVal = unGeoposition
  {-# INLINE pToJSVal #-}

instance PFromJSVal Geoposition where
  pFromJSVal = Geoposition
  {-# INLINE pFromJSVal #-}

instance ToJSVal Geoposition where
  toJSVal = return . unGeoposition
  {-# INLINE toJSVal #-}

instance FromJSVal Geoposition where
  fromJSVal v = fmap Geoposition <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Geoposition
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Geoposition where
  makeObject = makeObject . unGeoposition

instance IsGObject Geoposition where
  typeGType _ = gTypeGeoposition
  {-# INLINE typeGType #-}

noGeoposition :: Maybe Geoposition
noGeoposition = Nothing
{-# INLINE noGeoposition #-}

gTypeGeoposition :: JSM GType
gTypeGeoposition = GType . Object <$> jsg "Geoposition"

-- | Functions for this inteface are in "JSDOM.GetRootNodeOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GetRootNodeOptions Mozilla GetRootNodeOptions documentation>
newtype GetRootNodeOptions = GetRootNodeOptions { unGetRootNodeOptions :: JSVal }

instance PToJSVal GetRootNodeOptions where
  pToJSVal = unGetRootNodeOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal GetRootNodeOptions where
  pFromJSVal = GetRootNodeOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal GetRootNodeOptions where
  toJSVal = return . unGetRootNodeOptions
  {-# INLINE toJSVal #-}

instance FromJSVal GetRootNodeOptions where
  fromJSVal v = fmap GetRootNodeOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GetRootNodeOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GetRootNodeOptions where
  makeObject = makeObject . unGetRootNodeOptions

instance IsGObject GetRootNodeOptions where
  typeGType _ = gTypeGetRootNodeOptions
  {-# INLINE typeGType #-}

noGetRootNodeOptions :: Maybe GetRootNodeOptions
noGetRootNodeOptions = Nothing
{-# INLINE noGetRootNodeOptions #-}

gTypeGetRootNodeOptions :: JSM GType
gTypeGetRootNodeOptions = GType . Object <$> jsg "GetRootNodeOptions"

-- | Functions for this inteface are in "JSDOM.HTMLAllCollection".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAllCollection Mozilla HTMLAllCollection documentation>
newtype HTMLAllCollection = HTMLAllCollection { unHTMLAllCollection :: JSVal }

instance PToJSVal HTMLAllCollection where
  pToJSVal = unHTMLAllCollection
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAllCollection where
  pFromJSVal = HTMLAllCollection
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAllCollection where
  toJSVal = return . unHTMLAllCollection
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAllCollection where
  fromJSVal v = fmap HTMLAllCollection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAllCollection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAllCollection where
  makeObject = makeObject . unHTMLAllCollection

instance IsGObject HTMLAllCollection where
  typeGType _ = gTypeHTMLAllCollection
  {-# INLINE typeGType #-}

noHTMLAllCollection :: Maybe HTMLAllCollection
noHTMLAllCollection = Nothing
{-# INLINE noHTMLAllCollection #-}

gTypeHTMLAllCollection :: JSM GType
gTypeHTMLAllCollection = GType . Object <$> jsg "HTMLAllCollection"


-- | Functions for this inteface are in "JSDOM.HashChangeEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HashChangeEvent Mozilla HashChangeEvent documentation>
newtype HashChangeEvent = HashChangeEvent { unHashChangeEvent :: JSVal }

instance PToJSVal HashChangeEvent where
  pToJSVal = unHashChangeEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal HashChangeEvent where
  pFromJSVal = HashChangeEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal HashChangeEvent where
  toJSVal = return . unHashChangeEvent
  {-# INLINE toJSVal #-}

instance FromJSVal HashChangeEvent where
  fromJSVal v = fmap HashChangeEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HashChangeEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HashChangeEvent where
  makeObject = makeObject . unHashChangeEvent

instance IsEvent HashChangeEvent
instance IsGObject HashChangeEvent where
  typeGType _ = gTypeHashChangeEvent
  {-# INLINE typeGType #-}

noHashChangeEvent :: Maybe HashChangeEvent
noHashChangeEvent = Nothing
{-# INLINE noHashChangeEvent #-}

gTypeHashChangeEvent :: JSM GType
gTypeHashChangeEvent = GType . Object <$> jsg "HashChangeEvent"

-- | Functions for this inteface are in "JSDOM.HashChangeEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HashChangeEventInit Mozilla HashChangeEventInit documentation>
newtype HashChangeEventInit = HashChangeEventInit { unHashChangeEventInit :: JSVal }

instance PToJSVal HashChangeEventInit where
  pToJSVal = unHashChangeEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal HashChangeEventInit where
  pFromJSVal = HashChangeEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal HashChangeEventInit where
  toJSVal = return . unHashChangeEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal HashChangeEventInit where
  fromJSVal v = fmap HashChangeEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HashChangeEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HashChangeEventInit where
  makeObject = makeObject . unHashChangeEventInit

instance IsEventInit HashChangeEventInit
instance IsGObject HashChangeEventInit where
  typeGType _ = gTypeHashChangeEventInit
  {-# INLINE typeGType #-}

noHashChangeEventInit :: Maybe HashChangeEventInit
noHashChangeEventInit = Nothing
{-# INLINE noHashChangeEventInit #-}

gTypeHashChangeEventInit :: JSM GType
gTypeHashChangeEventInit = GType . Object <$> jsg "HashChangeEventInit"

-- | Functions for this inteface are in "JSDOM.Headers".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Headers Mozilla Headers documentation>
newtype Headers = Headers { unHeaders :: JSVal }

instance PToJSVal Headers where
  pToJSVal = unHeaders
  {-# INLINE pToJSVal #-}

instance PFromJSVal Headers where
  pFromJSVal = Headers
  {-# INLINE pFromJSVal #-}

instance ToJSVal Headers where
  toJSVal = return . unHeaders
  {-# INLINE toJSVal #-}

instance FromJSVal Headers where
  fromJSVal v = fmap Headers <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Headers
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Headers where
  makeObject = makeObject . unHeaders

instance IsGObject Headers where
  typeGType _ = gTypeHeaders
  {-# INLINE typeGType #-}

noHeaders :: Maybe Headers
noHeaders = Nothing
{-# INLINE noHeaders #-}

gTypeHeaders :: JSM GType
gTypeHeaders = GType . Object <$> jsg "Headers"

-- | Functions for this inteface are in "JSDOM.History".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/History Mozilla History documentation>
newtype History = History { unHistory :: JSVal }

instance PToJSVal History where
  pToJSVal = unHistory
  {-# INLINE pToJSVal #-}

instance PFromJSVal History where
  pFromJSVal = History
  {-# INLINE pFromJSVal #-}

instance ToJSVal History where
  toJSVal = return . unHistory
  {-# INLINE toJSVal #-}

instance FromJSVal History where
  fromJSVal v = fmap History <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . History
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject History where
  makeObject = makeObject . unHistory

instance IsGObject History where
  typeGType _ = gTypeHistory
  {-# INLINE typeGType #-}

noHistory :: Maybe History
noHistory = Nothing
{-# INLINE noHistory #-}

gTypeHistory :: JSM GType
gTypeHistory = GType . Object <$> jsg "History"

-- | Functions for this inteface are in "JSDOM.HkdfParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HkdfParams Mozilla HkdfParams documentation>
newtype HkdfParams = HkdfParams { unHkdfParams :: JSVal }

instance PToJSVal HkdfParams where
  pToJSVal = unHkdfParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal HkdfParams where
  pFromJSVal = HkdfParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal HkdfParams where
  toJSVal = return . unHkdfParams
  {-# INLINE toJSVal #-}

instance FromJSVal HkdfParams where
  fromJSVal v = fmap HkdfParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HkdfParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HkdfParams where
  makeObject = makeObject . unHkdfParams

instance IsCryptoAlgorithmParameters HkdfParams
instance IsGObject HkdfParams where
  typeGType _ = gTypeHkdfParams
  {-# INLINE typeGType #-}

noHkdfParams :: Maybe HkdfParams
noHkdfParams = Nothing
{-# INLINE noHkdfParams #-}

gTypeHkdfParams :: JSM GType
gTypeHkdfParams = GType . Object <$> jsg "HkdfParams"

-- | Functions for this inteface are in "JSDOM.HmacKeyParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HmacKeyParams Mozilla HmacKeyParams documentation>
newtype HmacKeyParams = HmacKeyParams { unHmacKeyParams :: JSVal }

instance PToJSVal HmacKeyParams where
  pToJSVal = unHmacKeyParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal HmacKeyParams where
  pFromJSVal = HmacKeyParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal HmacKeyParams where
  toJSVal = return . unHmacKeyParams
  {-# INLINE toJSVal #-}

instance FromJSVal HmacKeyParams where
  fromJSVal v = fmap HmacKeyParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HmacKeyParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HmacKeyParams where
  makeObject = makeObject . unHmacKeyParams

instance IsCryptoAlgorithmParameters HmacKeyParams
instance IsGObject HmacKeyParams where
  typeGType _ = gTypeHmacKeyParams
  {-# INLINE typeGType #-}

noHmacKeyParams :: Maybe HmacKeyParams
noHmacKeyParams = Nothing
{-# INLINE noHmacKeyParams #-}

gTypeHmacKeyParams :: JSM GType
gTypeHmacKeyParams = GType . Object <$> jsg "HmacKeyParams"

-- | Functions for this inteface are in "JSDOM.IDBVersionChangeEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IDBVersionChangeEventInit Mozilla IDBVersionChangeEventInit documentation>
newtype IDBVersionChangeEventInit = IDBVersionChangeEventInit { unIDBVersionChangeEventInit :: JSVal }

instance PToJSVal IDBVersionChangeEventInit where
  pToJSVal = unIDBVersionChangeEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal IDBVersionChangeEventInit where
  pFromJSVal = IDBVersionChangeEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal IDBVersionChangeEventInit where
  toJSVal = return . unIDBVersionChangeEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal IDBVersionChangeEventInit where
  fromJSVal v = fmap IDBVersionChangeEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IDBVersionChangeEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IDBVersionChangeEventInit where
  makeObject = makeObject . unIDBVersionChangeEventInit

instance IsEventInit IDBVersionChangeEventInit
instance IsGObject IDBVersionChangeEventInit where
  typeGType _ = gTypeIDBVersionChangeEventInit
  {-# INLINE typeGType #-}

noIDBVersionChangeEventInit :: Maybe IDBVersionChangeEventInit
noIDBVersionChangeEventInit = Nothing
{-# INLINE noIDBVersionChangeEventInit #-}

gTypeIDBVersionChangeEventInit :: JSM GType
gTypeIDBVersionChangeEventInit = GType . Object <$> jsg "IDBVersionChangeEventInit"

-- | Functions for this inteface are in "JSDOM.InputEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/InputEvent Mozilla InputEvent documentation>
newtype InputEvent = InputEvent { unInputEvent :: JSVal }

instance PToJSVal InputEvent where
  pToJSVal = unInputEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal InputEvent where
  pFromJSVal = InputEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal InputEvent where
  toJSVal = return . unInputEvent
  {-# INLINE toJSVal #-}

instance FromJSVal InputEvent where
  fromJSVal v = fmap InputEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . InputEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject InputEvent where
  makeObject = makeObject . unInputEvent

instance IsUIEvent InputEvent
instance IsEvent InputEvent
instance IsGObject InputEvent where
  typeGType _ = gTypeInputEvent
  {-# INLINE typeGType #-}

noInputEvent :: Maybe InputEvent
noInputEvent = Nothing
{-# INLINE noInputEvent #-}

gTypeInputEvent :: JSM GType
gTypeInputEvent = GType . Object <$> jsg "InputEvent"

-- | Functions for this inteface are in "JSDOM.InputEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/InputEventInit Mozilla InputEventInit documentation>
newtype InputEventInit = InputEventInit { unInputEventInit :: JSVal }

instance PToJSVal InputEventInit where
  pToJSVal = unInputEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal InputEventInit where
  pFromJSVal = InputEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal InputEventInit where
  toJSVal = return . unInputEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal InputEventInit where
  fromJSVal v = fmap InputEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . InputEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject InputEventInit where
  makeObject = makeObject . unInputEventInit

instance IsUIEventInit InputEventInit
instance IsEventInit InputEventInit
instance IsGObject InputEventInit where
  typeGType _ = gTypeInputEventInit
  {-# INLINE typeGType #-}

noInputEventInit :: Maybe InputEventInit
noInputEventInit = Nothing
{-# INLINE noInputEventInit #-}

gTypeInputEventInit :: JSM GType
gTypeInputEventInit = GType . Object <$> jsg "InputEventInit"

-- | Functions for this inteface are in "JSDOM.InspectorFrontendHost".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/InspectorFrontendHost Mozilla InspectorFrontendHost documentation>
newtype InspectorFrontendHost = InspectorFrontendHost { unInspectorFrontendHost :: JSVal }

instance PToJSVal InspectorFrontendHost where
  pToJSVal = unInspectorFrontendHost
  {-# INLINE pToJSVal #-}

instance PFromJSVal InspectorFrontendHost where
  pFromJSVal = InspectorFrontendHost
  {-# INLINE pFromJSVal #-}

instance ToJSVal InspectorFrontendHost where
  toJSVal = return . unInspectorFrontendHost
  {-# INLINE toJSVal #-}

instance FromJSVal InspectorFrontendHost where
  fromJSVal v = fmap InspectorFrontendHost <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . InspectorFrontendHost
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject InspectorFrontendHost where
  makeObject = makeObject . unInspectorFrontendHost

instance IsGObject InspectorFrontendHost where
  typeGType _ = gTypeInspectorFrontendHost
  {-# INLINE typeGType #-}

noInspectorFrontendHost :: Maybe InspectorFrontendHost
noInspectorFrontendHost = Nothing
{-# INLINE noInspectorFrontendHost #-}

gTypeInspectorFrontendHost :: JSM GType
gTypeInspectorFrontendHost = GType . Object <$> jsg "InspectorFrontendHost"

-- | Functions for this inteface are in "JSDOM.IntersectionObserver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserver Mozilla IntersectionObserver documentation>
newtype IntersectionObserver = IntersectionObserver { unIntersectionObserver :: JSVal }

instance PToJSVal IntersectionObserver where
  pToJSVal = unIntersectionObserver
  {-# INLINE pToJSVal #-}

instance PFromJSVal IntersectionObserver where
  pFromJSVal = IntersectionObserver
  {-# INLINE pFromJSVal #-}

instance ToJSVal IntersectionObserver where
  toJSVal = return . unIntersectionObserver
  {-# INLINE toJSVal #-}

instance FromJSVal IntersectionObserver where
  fromJSVal v = fmap IntersectionObserver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IntersectionObserver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IntersectionObserver where
  makeObject = makeObject . unIntersectionObserver

instance IsGObject IntersectionObserver where
  typeGType _ = gTypeIntersectionObserver
  {-# INLINE typeGType #-}

noIntersectionObserver :: Maybe IntersectionObserver
noIntersectionObserver = Nothing
{-# INLINE noIntersectionObserver #-}

gTypeIntersectionObserver :: JSM GType
gTypeIntersectionObserver = GType . Object <$> jsg "IntersectionObserver"

-- | Functions for this inteface are in "JSDOM.IntersectionObserverEntry".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntry Mozilla IntersectionObserverEntry documentation>
newtype IntersectionObserverEntry = IntersectionObserverEntry { unIntersectionObserverEntry :: JSVal }

instance PToJSVal IntersectionObserverEntry where
  pToJSVal = unIntersectionObserverEntry
  {-# INLINE pToJSVal #-}

instance PFromJSVal IntersectionObserverEntry where
  pFromJSVal = IntersectionObserverEntry
  {-# INLINE pFromJSVal #-}

instance ToJSVal IntersectionObserverEntry where
  toJSVal = return . unIntersectionObserverEntry
  {-# INLINE toJSVal #-}

instance FromJSVal IntersectionObserverEntry where
  fromJSVal v = fmap IntersectionObserverEntry <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IntersectionObserverEntry
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IntersectionObserverEntry where
  makeObject = makeObject . unIntersectionObserverEntry

instance IsGObject IntersectionObserverEntry where
  typeGType _ = gTypeIntersectionObserverEntry
  {-# INLINE typeGType #-}

noIntersectionObserverEntry :: Maybe IntersectionObserverEntry
noIntersectionObserverEntry = Nothing
{-# INLINE noIntersectionObserverEntry #-}

gTypeIntersectionObserverEntry :: JSM GType
gTypeIntersectionObserverEntry = GType . Object <$> jsg "IntersectionObserverEntry"

-- | Functions for this inteface are in "JSDOM.IntersectionObserverEntryInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverEntryInit Mozilla IntersectionObserverEntryInit documentation>
newtype IntersectionObserverEntryInit = IntersectionObserverEntryInit { unIntersectionObserverEntryInit :: JSVal }

instance PToJSVal IntersectionObserverEntryInit where
  pToJSVal = unIntersectionObserverEntryInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal IntersectionObserverEntryInit where
  pFromJSVal = IntersectionObserverEntryInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal IntersectionObserverEntryInit where
  toJSVal = return . unIntersectionObserverEntryInit
  {-# INLINE toJSVal #-}

instance FromJSVal IntersectionObserverEntryInit where
  fromJSVal v = fmap IntersectionObserverEntryInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IntersectionObserverEntryInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IntersectionObserverEntryInit where
  makeObject = makeObject . unIntersectionObserverEntryInit

instance IsGObject IntersectionObserverEntryInit where
  typeGType _ = gTypeIntersectionObserverEntryInit
  {-# INLINE typeGType #-}

noIntersectionObserverEntryInit :: Maybe IntersectionObserverEntryInit
noIntersectionObserverEntryInit = Nothing
{-# INLINE noIntersectionObserverEntryInit #-}

gTypeIntersectionObserverEntryInit :: JSM GType
gTypeIntersectionObserverEntryInit = GType . Object <$> jsg "IntersectionObserverEntryInit"

-- | Functions for this inteface are in "JSDOM.IntersectionObserverInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/IntersectionObserverInit Mozilla IntersectionObserverInit documentation>
newtype IntersectionObserverInit = IntersectionObserverInit { unIntersectionObserverInit :: JSVal }

instance PToJSVal IntersectionObserverInit where
  pToJSVal = unIntersectionObserverInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal IntersectionObserverInit where
  pFromJSVal = IntersectionObserverInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal IntersectionObserverInit where
  toJSVal = return . unIntersectionObserverInit
  {-# INLINE toJSVal #-}

instance FromJSVal IntersectionObserverInit where
  fromJSVal v = fmap IntersectionObserverInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . IntersectionObserverInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject IntersectionObserverInit where
  makeObject = makeObject . unIntersectionObserverInit

instance IsGObject IntersectionObserverInit where
  typeGType _ = gTypeIntersectionObserverInit
  {-# INLINE typeGType #-}

noIntersectionObserverInit :: Maybe IntersectionObserverInit
noIntersectionObserverInit = Nothing
{-# INLINE noIntersectionObserverInit #-}

gTypeIntersectionObserverInit :: JSM GType
gTypeIntersectionObserverInit = GType . Object <$> jsg "IntersectionObserverInit"

