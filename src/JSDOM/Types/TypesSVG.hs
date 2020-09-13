{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.TypesSVG where

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

-- | Functions for this inteface are in "JSDOM.SVGAElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAElement Mozilla SVGAElement documentation>
newtype SVGAElement = SVGAElement { unSVGAElement :: JSVal }

instance PToJSVal SVGAElement where
  pToJSVal = unSVGAElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAElement where
  pFromJSVal = SVGAElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAElement where
  toJSVal = return . unSVGAElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAElement where
  fromJSVal v = fmap SVGAElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAElement where
  makeObject = makeObject . unSVGAElement

instance IsSVGGraphicsElement SVGAElement
instance IsSVGElement SVGAElement
instance IsElement SVGAElement
instance IsNode SVGAElement
instance IsEventTarget SVGAElement
instance IsSlotable SVGAElement
instance IsParentNode SVGAElement
instance IsNonDocumentTypeChildNode SVGAElement
instance IsDocumentAndElementEventHandlers SVGAElement
instance IsChildNode SVGAElement
instance IsAnimatable SVGAElement
instance IsGlobalEventHandlers SVGAElement
instance IsElementCSSInlineStyle SVGAElement
instance IsSVGTests SVGAElement
instance IsSVGURIReference SVGAElement
instance IsSVGExternalResourcesRequired SVGAElement
instance IsGObject SVGAElement where
  typeGType _ = gTypeSVGAElement
  {-# INLINE typeGType #-}

noSVGAElement :: Maybe SVGAElement
noSVGAElement = Nothing
{-# INLINE noSVGAElement #-}

gTypeSVGAElement :: JSM GType
gTypeSVGAElement = GType . Object <$> jsg "SVGAElement"

-- | Functions for this inteface are in "JSDOM.SVGAltGlyphDefElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAltGlyphDefElement Mozilla SVGAltGlyphDefElement documentation>
newtype SVGAltGlyphDefElement = SVGAltGlyphDefElement { unSVGAltGlyphDefElement :: JSVal }

instance PToJSVal SVGAltGlyphDefElement where
  pToJSVal = unSVGAltGlyphDefElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAltGlyphDefElement where
  pFromJSVal = SVGAltGlyphDefElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAltGlyphDefElement where
  toJSVal = return . unSVGAltGlyphDefElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAltGlyphDefElement where
  fromJSVal v = fmap SVGAltGlyphDefElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAltGlyphDefElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAltGlyphDefElement where
  makeObject = makeObject . unSVGAltGlyphDefElement

instance IsSVGElement SVGAltGlyphDefElement
instance IsElement SVGAltGlyphDefElement
instance IsNode SVGAltGlyphDefElement
instance IsEventTarget SVGAltGlyphDefElement
instance IsSlotable SVGAltGlyphDefElement
instance IsParentNode SVGAltGlyphDefElement
instance IsNonDocumentTypeChildNode SVGAltGlyphDefElement
instance IsDocumentAndElementEventHandlers SVGAltGlyphDefElement
instance IsChildNode SVGAltGlyphDefElement
instance IsAnimatable SVGAltGlyphDefElement
instance IsGlobalEventHandlers SVGAltGlyphDefElement
instance IsElementCSSInlineStyle SVGAltGlyphDefElement
instance IsGObject SVGAltGlyphDefElement where
  typeGType _ = gTypeSVGAltGlyphDefElement
  {-# INLINE typeGType #-}

noSVGAltGlyphDefElement :: Maybe SVGAltGlyphDefElement
noSVGAltGlyphDefElement = Nothing
{-# INLINE noSVGAltGlyphDefElement #-}

gTypeSVGAltGlyphDefElement :: JSM GType
gTypeSVGAltGlyphDefElement = GType . Object <$> jsg "SVGAltGlyphDefElement"

-- | Functions for this inteface are in "JSDOM.SVGAltGlyphElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextPositioningElement"
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--     * "JSDOM.SVGURIReference"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAltGlyphElement Mozilla SVGAltGlyphElement documentation>
newtype SVGAltGlyphElement = SVGAltGlyphElement { unSVGAltGlyphElement :: JSVal }

instance PToJSVal SVGAltGlyphElement where
  pToJSVal = unSVGAltGlyphElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAltGlyphElement where
  pFromJSVal = SVGAltGlyphElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAltGlyphElement where
  toJSVal = return . unSVGAltGlyphElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAltGlyphElement where
  fromJSVal v = fmap SVGAltGlyphElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAltGlyphElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAltGlyphElement where
  makeObject = makeObject . unSVGAltGlyphElement

instance IsSVGTextPositioningElement SVGAltGlyphElement
instance IsSVGTextContentElement SVGAltGlyphElement
instance IsSVGGraphicsElement SVGAltGlyphElement
instance IsSVGElement SVGAltGlyphElement
instance IsElement SVGAltGlyphElement
instance IsNode SVGAltGlyphElement
instance IsEventTarget SVGAltGlyphElement
instance IsSlotable SVGAltGlyphElement
instance IsParentNode SVGAltGlyphElement
instance IsNonDocumentTypeChildNode SVGAltGlyphElement
instance IsDocumentAndElementEventHandlers SVGAltGlyphElement
instance IsChildNode SVGAltGlyphElement
instance IsAnimatable SVGAltGlyphElement
instance IsGlobalEventHandlers SVGAltGlyphElement
instance IsElementCSSInlineStyle SVGAltGlyphElement
instance IsSVGTests SVGAltGlyphElement
instance IsSVGExternalResourcesRequired SVGAltGlyphElement
instance IsSVGURIReference SVGAltGlyphElement
instance IsGObject SVGAltGlyphElement where
  typeGType _ = gTypeSVGAltGlyphElement
  {-# INLINE typeGType #-}

noSVGAltGlyphElement :: Maybe SVGAltGlyphElement
noSVGAltGlyphElement = Nothing
{-# INLINE noSVGAltGlyphElement #-}

gTypeSVGAltGlyphElement :: JSM GType
gTypeSVGAltGlyphElement = GType . Object <$> jsg "SVGAltGlyphElement"

-- | Functions for this inteface are in "JSDOM.SVGAltGlyphItemElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAltGlyphItemElement Mozilla SVGAltGlyphItemElement documentation>
newtype SVGAltGlyphItemElement = SVGAltGlyphItemElement { unSVGAltGlyphItemElement :: JSVal }

instance PToJSVal SVGAltGlyphItemElement where
  pToJSVal = unSVGAltGlyphItemElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAltGlyphItemElement where
  pFromJSVal = SVGAltGlyphItemElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAltGlyphItemElement where
  toJSVal = return . unSVGAltGlyphItemElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAltGlyphItemElement where
  fromJSVal v = fmap SVGAltGlyphItemElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAltGlyphItemElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAltGlyphItemElement where
  makeObject = makeObject . unSVGAltGlyphItemElement

instance IsSVGElement SVGAltGlyphItemElement
instance IsElement SVGAltGlyphItemElement
instance IsNode SVGAltGlyphItemElement
instance IsEventTarget SVGAltGlyphItemElement
instance IsSlotable SVGAltGlyphItemElement
instance IsParentNode SVGAltGlyphItemElement
instance IsNonDocumentTypeChildNode SVGAltGlyphItemElement
instance IsDocumentAndElementEventHandlers SVGAltGlyphItemElement
instance IsChildNode SVGAltGlyphItemElement
instance IsAnimatable SVGAltGlyphItemElement
instance IsGlobalEventHandlers SVGAltGlyphItemElement
instance IsElementCSSInlineStyle SVGAltGlyphItemElement
instance IsGObject SVGAltGlyphItemElement where
  typeGType _ = gTypeSVGAltGlyphItemElement
  {-# INLINE typeGType #-}

noSVGAltGlyphItemElement :: Maybe SVGAltGlyphItemElement
noSVGAltGlyphItemElement = Nothing
{-# INLINE noSVGAltGlyphItemElement #-}

gTypeSVGAltGlyphItemElement :: JSM GType
gTypeSVGAltGlyphItemElement = GType . Object <$> jsg "SVGAltGlyphItemElement"

-- | Functions for this inteface are in "JSDOM.SVGAngle".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAngle Mozilla SVGAngle documentation>
newtype SVGAngle = SVGAngle { unSVGAngle :: JSVal }

instance PToJSVal SVGAngle where
  pToJSVal = unSVGAngle
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAngle where
  pFromJSVal = SVGAngle
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAngle where
  toJSVal = return . unSVGAngle
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAngle where
  fromJSVal v = fmap SVGAngle <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAngle
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAngle where
  makeObject = makeObject . unSVGAngle

instance IsGObject SVGAngle where
  typeGType _ = gTypeSVGAngle
  {-# INLINE typeGType #-}

noSVGAngle :: Maybe SVGAngle
noSVGAngle = Nothing
{-# INLINE noSVGAngle #-}

gTypeSVGAngle :: JSM GType
gTypeSVGAngle = GType . Object <$> jsg "SVGAngle"

-- | Functions for this inteface are in "JSDOM.SVGAnimateColorElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGAnimationElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimateColorElement Mozilla SVGAnimateColorElement documentation>
newtype SVGAnimateColorElement = SVGAnimateColorElement { unSVGAnimateColorElement :: JSVal }

instance PToJSVal SVGAnimateColorElement where
  pToJSVal = unSVGAnimateColorElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimateColorElement where
  pFromJSVal = SVGAnimateColorElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimateColorElement where
  toJSVal = return . unSVGAnimateColorElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimateColorElement where
  fromJSVal v = fmap SVGAnimateColorElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimateColorElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimateColorElement where
  makeObject = makeObject . unSVGAnimateColorElement

instance IsSVGAnimationElement SVGAnimateColorElement
instance IsSVGElement SVGAnimateColorElement
instance IsElement SVGAnimateColorElement
instance IsNode SVGAnimateColorElement
instance IsEventTarget SVGAnimateColorElement
instance IsSlotable SVGAnimateColorElement
instance IsParentNode SVGAnimateColorElement
instance IsNonDocumentTypeChildNode SVGAnimateColorElement
instance IsDocumentAndElementEventHandlers SVGAnimateColorElement
instance IsChildNode SVGAnimateColorElement
instance IsAnimatable SVGAnimateColorElement
instance IsGlobalEventHandlers SVGAnimateColorElement
instance IsElementCSSInlineStyle SVGAnimateColorElement
instance IsSVGTests SVGAnimateColorElement
instance IsSVGExternalResourcesRequired SVGAnimateColorElement
instance IsGObject SVGAnimateColorElement where
  typeGType _ = gTypeSVGAnimateColorElement
  {-# INLINE typeGType #-}

noSVGAnimateColorElement :: Maybe SVGAnimateColorElement
noSVGAnimateColorElement = Nothing
{-# INLINE noSVGAnimateColorElement #-}

gTypeSVGAnimateColorElement :: JSM GType
gTypeSVGAnimateColorElement = GType . Object <$> jsg "SVGAnimateColorElement"

-- | Functions for this inteface are in "JSDOM.SVGAnimateElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGAnimationElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimateElement Mozilla SVGAnimateElement documentation>
newtype SVGAnimateElement = SVGAnimateElement { unSVGAnimateElement :: JSVal }

instance PToJSVal SVGAnimateElement where
  pToJSVal = unSVGAnimateElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimateElement where
  pFromJSVal = SVGAnimateElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimateElement where
  toJSVal = return . unSVGAnimateElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimateElement where
  fromJSVal v = fmap SVGAnimateElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimateElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimateElement where
  makeObject = makeObject . unSVGAnimateElement

instance IsSVGAnimationElement SVGAnimateElement
instance IsSVGElement SVGAnimateElement
instance IsElement SVGAnimateElement
instance IsNode SVGAnimateElement
instance IsEventTarget SVGAnimateElement
instance IsSlotable SVGAnimateElement
instance IsParentNode SVGAnimateElement
instance IsNonDocumentTypeChildNode SVGAnimateElement
instance IsDocumentAndElementEventHandlers SVGAnimateElement
instance IsChildNode SVGAnimateElement
instance IsAnimatable SVGAnimateElement
instance IsGlobalEventHandlers SVGAnimateElement
instance IsElementCSSInlineStyle SVGAnimateElement
instance IsSVGTests SVGAnimateElement
instance IsSVGExternalResourcesRequired SVGAnimateElement
instance IsGObject SVGAnimateElement where
  typeGType _ = gTypeSVGAnimateElement
  {-# INLINE typeGType #-}

noSVGAnimateElement :: Maybe SVGAnimateElement
noSVGAnimateElement = Nothing
{-# INLINE noSVGAnimateElement #-}

gTypeSVGAnimateElement :: JSM GType
gTypeSVGAnimateElement = GType . Object <$> jsg "SVGAnimateElement"

-- | Functions for this inteface are in "JSDOM.SVGAnimateMotionElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGAnimationElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimateMotionElement Mozilla SVGAnimateMotionElement documentation>
newtype SVGAnimateMotionElement = SVGAnimateMotionElement { unSVGAnimateMotionElement :: JSVal }

instance PToJSVal SVGAnimateMotionElement where
  pToJSVal = unSVGAnimateMotionElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimateMotionElement where
  pFromJSVal = SVGAnimateMotionElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimateMotionElement where
  toJSVal = return . unSVGAnimateMotionElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimateMotionElement where
  fromJSVal v = fmap SVGAnimateMotionElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimateMotionElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimateMotionElement where
  makeObject = makeObject . unSVGAnimateMotionElement

instance IsSVGAnimationElement SVGAnimateMotionElement
instance IsSVGElement SVGAnimateMotionElement
instance IsElement SVGAnimateMotionElement
instance IsNode SVGAnimateMotionElement
instance IsEventTarget SVGAnimateMotionElement
instance IsSlotable SVGAnimateMotionElement
instance IsParentNode SVGAnimateMotionElement
instance IsNonDocumentTypeChildNode SVGAnimateMotionElement
instance IsDocumentAndElementEventHandlers SVGAnimateMotionElement
instance IsChildNode SVGAnimateMotionElement
instance IsAnimatable SVGAnimateMotionElement
instance IsGlobalEventHandlers SVGAnimateMotionElement
instance IsElementCSSInlineStyle SVGAnimateMotionElement
instance IsSVGTests SVGAnimateMotionElement
instance IsSVGExternalResourcesRequired SVGAnimateMotionElement
instance IsGObject SVGAnimateMotionElement where
  typeGType _ = gTypeSVGAnimateMotionElement
  {-# INLINE typeGType #-}

noSVGAnimateMotionElement :: Maybe SVGAnimateMotionElement
noSVGAnimateMotionElement = Nothing
{-# INLINE noSVGAnimateMotionElement #-}

gTypeSVGAnimateMotionElement :: JSM GType
gTypeSVGAnimateMotionElement = GType . Object <$> jsg "SVGAnimateMotionElement"

-- | Functions for this inteface are in "JSDOM.SVGAnimateTransformElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGAnimationElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimateTransformElement Mozilla SVGAnimateTransformElement documentation>
newtype SVGAnimateTransformElement = SVGAnimateTransformElement { unSVGAnimateTransformElement :: JSVal }

instance PToJSVal SVGAnimateTransformElement where
  pToJSVal = unSVGAnimateTransformElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimateTransformElement where
  pFromJSVal = SVGAnimateTransformElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimateTransformElement where
  toJSVal = return . unSVGAnimateTransformElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimateTransformElement where
  fromJSVal v = fmap SVGAnimateTransformElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimateTransformElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimateTransformElement where
  makeObject = makeObject . unSVGAnimateTransformElement

instance IsSVGAnimationElement SVGAnimateTransformElement
instance IsSVGElement SVGAnimateTransformElement
instance IsElement SVGAnimateTransformElement
instance IsNode SVGAnimateTransformElement
instance IsEventTarget SVGAnimateTransformElement
instance IsSlotable SVGAnimateTransformElement
instance IsParentNode SVGAnimateTransformElement
instance IsNonDocumentTypeChildNode SVGAnimateTransformElement
instance IsDocumentAndElementEventHandlers SVGAnimateTransformElement
instance IsChildNode SVGAnimateTransformElement
instance IsAnimatable SVGAnimateTransformElement
instance IsGlobalEventHandlers SVGAnimateTransformElement
instance IsElementCSSInlineStyle SVGAnimateTransformElement
instance IsSVGTests SVGAnimateTransformElement
instance IsSVGExternalResourcesRequired SVGAnimateTransformElement
instance IsGObject SVGAnimateTransformElement where
  typeGType _ = gTypeSVGAnimateTransformElement
  {-# INLINE typeGType #-}

noSVGAnimateTransformElement :: Maybe SVGAnimateTransformElement
noSVGAnimateTransformElement = Nothing
{-# INLINE noSVGAnimateTransformElement #-}

gTypeSVGAnimateTransformElement :: JSM GType
gTypeSVGAnimateTransformElement = GType . Object <$> jsg "SVGAnimateTransformElement"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedAngle".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedAngle Mozilla SVGAnimatedAngle documentation>
newtype SVGAnimatedAngle = SVGAnimatedAngle { unSVGAnimatedAngle :: JSVal }

instance PToJSVal SVGAnimatedAngle where
  pToJSVal = unSVGAnimatedAngle
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedAngle where
  pFromJSVal = SVGAnimatedAngle
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedAngle where
  toJSVal = return . unSVGAnimatedAngle
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedAngle where
  fromJSVal v = fmap SVGAnimatedAngle <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedAngle
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedAngle where
  makeObject = makeObject . unSVGAnimatedAngle

instance IsGObject SVGAnimatedAngle where
  typeGType _ = gTypeSVGAnimatedAngle
  {-# INLINE typeGType #-}

noSVGAnimatedAngle :: Maybe SVGAnimatedAngle
noSVGAnimatedAngle = Nothing
{-# INLINE noSVGAnimatedAngle #-}

gTypeSVGAnimatedAngle :: JSM GType
gTypeSVGAnimatedAngle = GType . Object <$> jsg "SVGAnimatedAngle"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedBoolean".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedBoolean Mozilla SVGAnimatedBoolean documentation>
newtype SVGAnimatedBoolean = SVGAnimatedBoolean { unSVGAnimatedBoolean :: JSVal }

instance PToJSVal SVGAnimatedBoolean where
  pToJSVal = unSVGAnimatedBoolean
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedBoolean where
  pFromJSVal = SVGAnimatedBoolean
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedBoolean where
  toJSVal = return . unSVGAnimatedBoolean
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedBoolean where
  fromJSVal v = fmap SVGAnimatedBoolean <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedBoolean
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedBoolean where
  makeObject = makeObject . unSVGAnimatedBoolean

instance IsGObject SVGAnimatedBoolean where
  typeGType _ = gTypeSVGAnimatedBoolean
  {-# INLINE typeGType #-}

noSVGAnimatedBoolean :: Maybe SVGAnimatedBoolean
noSVGAnimatedBoolean = Nothing
{-# INLINE noSVGAnimatedBoolean #-}

gTypeSVGAnimatedBoolean :: JSM GType
gTypeSVGAnimatedBoolean = GType . Object <$> jsg "SVGAnimatedBoolean"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedEnumeration".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedEnumeration Mozilla SVGAnimatedEnumeration documentation>
newtype SVGAnimatedEnumeration = SVGAnimatedEnumeration { unSVGAnimatedEnumeration :: JSVal }

instance PToJSVal SVGAnimatedEnumeration where
  pToJSVal = unSVGAnimatedEnumeration
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedEnumeration where
  pFromJSVal = SVGAnimatedEnumeration
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedEnumeration where
  toJSVal = return . unSVGAnimatedEnumeration
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedEnumeration where
  fromJSVal v = fmap SVGAnimatedEnumeration <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedEnumeration
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedEnumeration where
  makeObject = makeObject . unSVGAnimatedEnumeration

instance IsGObject SVGAnimatedEnumeration where
  typeGType _ = gTypeSVGAnimatedEnumeration
  {-# INLINE typeGType #-}

noSVGAnimatedEnumeration :: Maybe SVGAnimatedEnumeration
noSVGAnimatedEnumeration = Nothing
{-# INLINE noSVGAnimatedEnumeration #-}

gTypeSVGAnimatedEnumeration :: JSM GType
gTypeSVGAnimatedEnumeration = GType . Object <$> jsg "SVGAnimatedEnumeration"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedInteger".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedInteger Mozilla SVGAnimatedInteger documentation>
newtype SVGAnimatedInteger = SVGAnimatedInteger { unSVGAnimatedInteger :: JSVal }

instance PToJSVal SVGAnimatedInteger where
  pToJSVal = unSVGAnimatedInteger
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedInteger where
  pFromJSVal = SVGAnimatedInteger
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedInteger where
  toJSVal = return . unSVGAnimatedInteger
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedInteger where
  fromJSVal v = fmap SVGAnimatedInteger <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedInteger
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedInteger where
  makeObject = makeObject . unSVGAnimatedInteger

instance IsGObject SVGAnimatedInteger where
  typeGType _ = gTypeSVGAnimatedInteger
  {-# INLINE typeGType #-}

noSVGAnimatedInteger :: Maybe SVGAnimatedInteger
noSVGAnimatedInteger = Nothing
{-# INLINE noSVGAnimatedInteger #-}

gTypeSVGAnimatedInteger :: JSM GType
gTypeSVGAnimatedInteger = GType . Object <$> jsg "SVGAnimatedInteger"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedLength".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedLength Mozilla SVGAnimatedLength documentation>
newtype SVGAnimatedLength = SVGAnimatedLength { unSVGAnimatedLength :: JSVal }

instance PToJSVal SVGAnimatedLength where
  pToJSVal = unSVGAnimatedLength
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedLength where
  pFromJSVal = SVGAnimatedLength
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedLength where
  toJSVal = return . unSVGAnimatedLength
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedLength where
  fromJSVal v = fmap SVGAnimatedLength <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedLength
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedLength where
  makeObject = makeObject . unSVGAnimatedLength

instance IsGObject SVGAnimatedLength where
  typeGType _ = gTypeSVGAnimatedLength
  {-# INLINE typeGType #-}

noSVGAnimatedLength :: Maybe SVGAnimatedLength
noSVGAnimatedLength = Nothing
{-# INLINE noSVGAnimatedLength #-}

gTypeSVGAnimatedLength :: JSM GType
gTypeSVGAnimatedLength = GType . Object <$> jsg "SVGAnimatedLength"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedLengthList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedLengthList Mozilla SVGAnimatedLengthList documentation>
newtype SVGAnimatedLengthList = SVGAnimatedLengthList { unSVGAnimatedLengthList :: JSVal }

instance PToJSVal SVGAnimatedLengthList where
  pToJSVal = unSVGAnimatedLengthList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedLengthList where
  pFromJSVal = SVGAnimatedLengthList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedLengthList where
  toJSVal = return . unSVGAnimatedLengthList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedLengthList where
  fromJSVal v = fmap SVGAnimatedLengthList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedLengthList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedLengthList where
  makeObject = makeObject . unSVGAnimatedLengthList

instance IsGObject SVGAnimatedLengthList where
  typeGType _ = gTypeSVGAnimatedLengthList
  {-# INLINE typeGType #-}

noSVGAnimatedLengthList :: Maybe SVGAnimatedLengthList
noSVGAnimatedLengthList = Nothing
{-# INLINE noSVGAnimatedLengthList #-}

gTypeSVGAnimatedLengthList :: JSM GType
gTypeSVGAnimatedLengthList = GType . Object <$> jsg "SVGAnimatedLengthList"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedNumber".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedNumber Mozilla SVGAnimatedNumber documentation>
newtype SVGAnimatedNumber = SVGAnimatedNumber { unSVGAnimatedNumber :: JSVal }

instance PToJSVal SVGAnimatedNumber where
  pToJSVal = unSVGAnimatedNumber
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedNumber where
  pFromJSVal = SVGAnimatedNumber
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedNumber where
  toJSVal = return . unSVGAnimatedNumber
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedNumber where
  fromJSVal v = fmap SVGAnimatedNumber <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedNumber
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedNumber where
  makeObject = makeObject . unSVGAnimatedNumber

instance IsGObject SVGAnimatedNumber where
  typeGType _ = gTypeSVGAnimatedNumber
  {-# INLINE typeGType #-}

noSVGAnimatedNumber :: Maybe SVGAnimatedNumber
noSVGAnimatedNumber = Nothing
{-# INLINE noSVGAnimatedNumber #-}

gTypeSVGAnimatedNumber :: JSM GType
gTypeSVGAnimatedNumber = GType . Object <$> jsg "SVGAnimatedNumber"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedNumberList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedNumberList Mozilla SVGAnimatedNumberList documentation>
newtype SVGAnimatedNumberList = SVGAnimatedNumberList { unSVGAnimatedNumberList :: JSVal }

instance PToJSVal SVGAnimatedNumberList where
  pToJSVal = unSVGAnimatedNumberList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedNumberList where
  pFromJSVal = SVGAnimatedNumberList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedNumberList where
  toJSVal = return . unSVGAnimatedNumberList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedNumberList where
  fromJSVal v = fmap SVGAnimatedNumberList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedNumberList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedNumberList where
  makeObject = makeObject . unSVGAnimatedNumberList

instance IsGObject SVGAnimatedNumberList where
  typeGType _ = gTypeSVGAnimatedNumberList
  {-# INLINE typeGType #-}

noSVGAnimatedNumberList :: Maybe SVGAnimatedNumberList
noSVGAnimatedNumberList = Nothing
{-# INLINE noSVGAnimatedNumberList #-}

gTypeSVGAnimatedNumberList :: JSM GType
gTypeSVGAnimatedNumberList = GType . Object <$> jsg "SVGAnimatedNumberList"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedPreserveAspectRatio".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedPreserveAspectRatio Mozilla SVGAnimatedPreserveAspectRatio documentation>
newtype SVGAnimatedPreserveAspectRatio = SVGAnimatedPreserveAspectRatio { unSVGAnimatedPreserveAspectRatio :: JSVal }

instance PToJSVal SVGAnimatedPreserveAspectRatio where
  pToJSVal = unSVGAnimatedPreserveAspectRatio
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedPreserveAspectRatio where
  pFromJSVal = SVGAnimatedPreserveAspectRatio
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedPreserveAspectRatio where
  toJSVal = return . unSVGAnimatedPreserveAspectRatio
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedPreserveAspectRatio where
  fromJSVal v = fmap SVGAnimatedPreserveAspectRatio <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedPreserveAspectRatio
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedPreserveAspectRatio where
  makeObject = makeObject . unSVGAnimatedPreserveAspectRatio

instance IsGObject SVGAnimatedPreserveAspectRatio where
  typeGType _ = gTypeSVGAnimatedPreserveAspectRatio
  {-# INLINE typeGType #-}

noSVGAnimatedPreserveAspectRatio :: Maybe SVGAnimatedPreserveAspectRatio
noSVGAnimatedPreserveAspectRatio = Nothing
{-# INLINE noSVGAnimatedPreserveAspectRatio #-}

gTypeSVGAnimatedPreserveAspectRatio :: JSM GType
gTypeSVGAnimatedPreserveAspectRatio = GType . Object <$> jsg "SVGAnimatedPreserveAspectRatio"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedRect".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedRect Mozilla SVGAnimatedRect documentation>
newtype SVGAnimatedRect = SVGAnimatedRect { unSVGAnimatedRect :: JSVal }

instance PToJSVal SVGAnimatedRect where
  pToJSVal = unSVGAnimatedRect
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedRect where
  pFromJSVal = SVGAnimatedRect
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedRect where
  toJSVal = return . unSVGAnimatedRect
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedRect where
  fromJSVal v = fmap SVGAnimatedRect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedRect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedRect where
  makeObject = makeObject . unSVGAnimatedRect

instance IsGObject SVGAnimatedRect where
  typeGType _ = gTypeSVGAnimatedRect
  {-# INLINE typeGType #-}

noSVGAnimatedRect :: Maybe SVGAnimatedRect
noSVGAnimatedRect = Nothing
{-# INLINE noSVGAnimatedRect #-}

gTypeSVGAnimatedRect :: JSM GType
gTypeSVGAnimatedRect = GType . Object <$> jsg "SVGAnimatedRect"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedString".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedString Mozilla SVGAnimatedString documentation>
newtype SVGAnimatedString = SVGAnimatedString { unSVGAnimatedString :: JSVal }

instance PToJSVal SVGAnimatedString where
  pToJSVal = unSVGAnimatedString
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedString where
  pFromJSVal = SVGAnimatedString
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedString where
  toJSVal = return . unSVGAnimatedString
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedString where
  fromJSVal v = fmap SVGAnimatedString <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedString
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedString where
  makeObject = makeObject . unSVGAnimatedString

instance IsGObject SVGAnimatedString where
  typeGType _ = gTypeSVGAnimatedString
  {-# INLINE typeGType #-}

noSVGAnimatedString :: Maybe SVGAnimatedString
noSVGAnimatedString = Nothing
{-# INLINE noSVGAnimatedString #-}

gTypeSVGAnimatedString :: JSM GType
gTypeSVGAnimatedString = GType . Object <$> jsg "SVGAnimatedString"

-- | Functions for this inteface are in "JSDOM.SVGAnimatedTransformList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimatedTransformList Mozilla SVGAnimatedTransformList documentation>
newtype SVGAnimatedTransformList = SVGAnimatedTransformList { unSVGAnimatedTransformList :: JSVal }

instance PToJSVal SVGAnimatedTransformList where
  pToJSVal = unSVGAnimatedTransformList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimatedTransformList where
  pFromJSVal = SVGAnimatedTransformList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimatedTransformList where
  toJSVal = return . unSVGAnimatedTransformList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimatedTransformList where
  fromJSVal v = fmap SVGAnimatedTransformList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimatedTransformList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimatedTransformList where
  makeObject = makeObject . unSVGAnimatedTransformList

instance IsGObject SVGAnimatedTransformList where
  typeGType _ = gTypeSVGAnimatedTransformList
  {-# INLINE typeGType #-}

noSVGAnimatedTransformList :: Maybe SVGAnimatedTransformList
noSVGAnimatedTransformList = Nothing
{-# INLINE noSVGAnimatedTransformList #-}

gTypeSVGAnimatedTransformList :: JSM GType
gTypeSVGAnimatedTransformList = GType . Object <$> jsg "SVGAnimatedTransformList"

-- | Functions for this inteface are in "JSDOM.SVGAnimationElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGAnimationElement Mozilla SVGAnimationElement documentation>
newtype SVGAnimationElement = SVGAnimationElement { unSVGAnimationElement :: JSVal }

instance PToJSVal SVGAnimationElement where
  pToJSVal = unSVGAnimationElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGAnimationElement where
  pFromJSVal = SVGAnimationElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGAnimationElement where
  toJSVal = return . unSVGAnimationElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGAnimationElement where
  fromJSVal v = fmap SVGAnimationElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGAnimationElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGAnimationElement where
  makeObject = makeObject . unSVGAnimationElement

class (IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsSVGTests o, IsSVGExternalResourcesRequired o, IsGObject o) => IsSVGAnimationElement o
toSVGAnimationElement :: IsSVGAnimationElement o => o -> SVGAnimationElement
toSVGAnimationElement = SVGAnimationElement . coerce

instance IsSVGAnimationElement SVGAnimationElement
instance IsSVGElement SVGAnimationElement
instance IsElement SVGAnimationElement
instance IsNode SVGAnimationElement
instance IsEventTarget SVGAnimationElement
instance IsSlotable SVGAnimationElement
instance IsParentNode SVGAnimationElement
instance IsNonDocumentTypeChildNode SVGAnimationElement
instance IsDocumentAndElementEventHandlers SVGAnimationElement
instance IsChildNode SVGAnimationElement
instance IsAnimatable SVGAnimationElement
instance IsGlobalEventHandlers SVGAnimationElement
instance IsElementCSSInlineStyle SVGAnimationElement
instance IsSVGTests SVGAnimationElement
instance IsSVGExternalResourcesRequired SVGAnimationElement
instance IsGObject SVGAnimationElement where
  typeGType _ = gTypeSVGAnimationElement
  {-# INLINE typeGType #-}

noSVGAnimationElement :: Maybe SVGAnimationElement
noSVGAnimationElement = Nothing
{-# INLINE noSVGAnimationElement #-}

gTypeSVGAnimationElement :: JSM GType
gTypeSVGAnimationElement = GType . Object <$> jsg "SVGAnimationElement"

-- | Functions for this inteface are in "JSDOM.SVGCircleElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGCircleElement Mozilla SVGCircleElement documentation>
newtype SVGCircleElement = SVGCircleElement { unSVGCircleElement :: JSVal }

instance PToJSVal SVGCircleElement where
  pToJSVal = unSVGCircleElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGCircleElement where
  pFromJSVal = SVGCircleElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGCircleElement where
  toJSVal = return . unSVGCircleElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGCircleElement where
  fromJSVal v = fmap SVGCircleElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGCircleElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGCircleElement where
  makeObject = makeObject . unSVGCircleElement

instance IsSVGGraphicsElement SVGCircleElement
instance IsSVGElement SVGCircleElement
instance IsElement SVGCircleElement
instance IsNode SVGCircleElement
instance IsEventTarget SVGCircleElement
instance IsSlotable SVGCircleElement
instance IsParentNode SVGCircleElement
instance IsNonDocumentTypeChildNode SVGCircleElement
instance IsDocumentAndElementEventHandlers SVGCircleElement
instance IsChildNode SVGCircleElement
instance IsAnimatable SVGCircleElement
instance IsGlobalEventHandlers SVGCircleElement
instance IsElementCSSInlineStyle SVGCircleElement
instance IsSVGTests SVGCircleElement
instance IsSVGExternalResourcesRequired SVGCircleElement
instance IsGObject SVGCircleElement where
  typeGType _ = gTypeSVGCircleElement
  {-# INLINE typeGType #-}

noSVGCircleElement :: Maybe SVGCircleElement
noSVGCircleElement = Nothing
{-# INLINE noSVGCircleElement #-}

gTypeSVGCircleElement :: JSM GType
gTypeSVGCircleElement = GType . Object <$> jsg "SVGCircleElement"

-- | Functions for this inteface are in "JSDOM.SVGClipPathElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGClipPathElement Mozilla SVGClipPathElement documentation>
newtype SVGClipPathElement = SVGClipPathElement { unSVGClipPathElement :: JSVal }

instance PToJSVal SVGClipPathElement where
  pToJSVal = unSVGClipPathElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGClipPathElement where
  pFromJSVal = SVGClipPathElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGClipPathElement where
  toJSVal = return . unSVGClipPathElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGClipPathElement where
  fromJSVal v = fmap SVGClipPathElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGClipPathElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGClipPathElement where
  makeObject = makeObject . unSVGClipPathElement

instance IsSVGGraphicsElement SVGClipPathElement
instance IsSVGElement SVGClipPathElement
instance IsElement SVGClipPathElement
instance IsNode SVGClipPathElement
instance IsEventTarget SVGClipPathElement
instance IsSlotable SVGClipPathElement
instance IsParentNode SVGClipPathElement
instance IsNonDocumentTypeChildNode SVGClipPathElement
instance IsDocumentAndElementEventHandlers SVGClipPathElement
instance IsChildNode SVGClipPathElement
instance IsAnimatable SVGClipPathElement
instance IsGlobalEventHandlers SVGClipPathElement
instance IsElementCSSInlineStyle SVGClipPathElement
instance IsSVGTests SVGClipPathElement
instance IsSVGExternalResourcesRequired SVGClipPathElement
instance IsGObject SVGClipPathElement where
  typeGType _ = gTypeSVGClipPathElement
  {-# INLINE typeGType #-}

noSVGClipPathElement :: Maybe SVGClipPathElement
noSVGClipPathElement = Nothing
{-# INLINE noSVGClipPathElement #-}

gTypeSVGClipPathElement :: JSM GType
gTypeSVGClipPathElement = GType . Object <$> jsg "SVGClipPathElement"

-- | Functions for this inteface are in "JSDOM.SVGComponentTransferFunctionElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGComponentTransferFunctionElement Mozilla SVGComponentTransferFunctionElement documentation>
newtype SVGComponentTransferFunctionElement = SVGComponentTransferFunctionElement { unSVGComponentTransferFunctionElement :: JSVal }

instance PToJSVal SVGComponentTransferFunctionElement where
  pToJSVal = unSVGComponentTransferFunctionElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGComponentTransferFunctionElement where
  pFromJSVal = SVGComponentTransferFunctionElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGComponentTransferFunctionElement where
  toJSVal = return . unSVGComponentTransferFunctionElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGComponentTransferFunctionElement where
  fromJSVal v = fmap SVGComponentTransferFunctionElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGComponentTransferFunctionElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGComponentTransferFunctionElement where
  makeObject = makeObject . unSVGComponentTransferFunctionElement

class (IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsGObject o) => IsSVGComponentTransferFunctionElement o
toSVGComponentTransferFunctionElement :: IsSVGComponentTransferFunctionElement o => o -> SVGComponentTransferFunctionElement
toSVGComponentTransferFunctionElement = SVGComponentTransferFunctionElement . coerce

instance IsSVGComponentTransferFunctionElement SVGComponentTransferFunctionElement
instance IsSVGElement SVGComponentTransferFunctionElement
instance IsElement SVGComponentTransferFunctionElement
instance IsNode SVGComponentTransferFunctionElement
instance IsEventTarget SVGComponentTransferFunctionElement
instance IsSlotable SVGComponentTransferFunctionElement
instance IsParentNode SVGComponentTransferFunctionElement
instance IsNonDocumentTypeChildNode SVGComponentTransferFunctionElement
instance IsDocumentAndElementEventHandlers SVGComponentTransferFunctionElement
instance IsChildNode SVGComponentTransferFunctionElement
instance IsAnimatable SVGComponentTransferFunctionElement
instance IsGlobalEventHandlers SVGComponentTransferFunctionElement
instance IsElementCSSInlineStyle SVGComponentTransferFunctionElement
instance IsGObject SVGComponentTransferFunctionElement where
  typeGType _ = gTypeSVGComponentTransferFunctionElement
  {-# INLINE typeGType #-}

noSVGComponentTransferFunctionElement :: Maybe SVGComponentTransferFunctionElement
noSVGComponentTransferFunctionElement = Nothing
{-# INLINE noSVGComponentTransferFunctionElement #-}

gTypeSVGComponentTransferFunctionElement :: JSM GType
gTypeSVGComponentTransferFunctionElement = GType . Object <$> jsg "SVGComponentTransferFunctionElement"

-- | Functions for this inteface are in "JSDOM.SVGCursorElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGCursorElement Mozilla SVGCursorElement documentation>
newtype SVGCursorElement = SVGCursorElement { unSVGCursorElement :: JSVal }

instance PToJSVal SVGCursorElement where
  pToJSVal = unSVGCursorElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGCursorElement where
  pFromJSVal = SVGCursorElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGCursorElement where
  toJSVal = return . unSVGCursorElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGCursorElement where
  fromJSVal v = fmap SVGCursorElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGCursorElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGCursorElement where
  makeObject = makeObject . unSVGCursorElement

instance IsSVGElement SVGCursorElement
instance IsElement SVGCursorElement
instance IsNode SVGCursorElement
instance IsEventTarget SVGCursorElement
instance IsSlotable SVGCursorElement
instance IsParentNode SVGCursorElement
instance IsNonDocumentTypeChildNode SVGCursorElement
instance IsDocumentAndElementEventHandlers SVGCursorElement
instance IsChildNode SVGCursorElement
instance IsAnimatable SVGCursorElement
instance IsGlobalEventHandlers SVGCursorElement
instance IsElementCSSInlineStyle SVGCursorElement
instance IsSVGURIReference SVGCursorElement
instance IsSVGTests SVGCursorElement
instance IsSVGExternalResourcesRequired SVGCursorElement
instance IsGObject SVGCursorElement where
  typeGType _ = gTypeSVGCursorElement
  {-# INLINE typeGType #-}

noSVGCursorElement :: Maybe SVGCursorElement
noSVGCursorElement = Nothing
{-# INLINE noSVGCursorElement #-}

gTypeSVGCursorElement :: JSM GType
gTypeSVGCursorElement = GType . Object <$> jsg "SVGCursorElement"

-- | Functions for this inteface are in "JSDOM.SVGDefsElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGDefsElement Mozilla SVGDefsElement documentation>
newtype SVGDefsElement = SVGDefsElement { unSVGDefsElement :: JSVal }

instance PToJSVal SVGDefsElement where
  pToJSVal = unSVGDefsElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGDefsElement where
  pFromJSVal = SVGDefsElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGDefsElement where
  toJSVal = return . unSVGDefsElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGDefsElement where
  fromJSVal v = fmap SVGDefsElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGDefsElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGDefsElement where
  makeObject = makeObject . unSVGDefsElement

instance IsSVGGraphicsElement SVGDefsElement
instance IsSVGElement SVGDefsElement
instance IsElement SVGDefsElement
instance IsNode SVGDefsElement
instance IsEventTarget SVGDefsElement
instance IsSlotable SVGDefsElement
instance IsParentNode SVGDefsElement
instance IsNonDocumentTypeChildNode SVGDefsElement
instance IsDocumentAndElementEventHandlers SVGDefsElement
instance IsChildNode SVGDefsElement
instance IsAnimatable SVGDefsElement
instance IsGlobalEventHandlers SVGDefsElement
instance IsElementCSSInlineStyle SVGDefsElement
instance IsSVGTests SVGDefsElement
instance IsSVGExternalResourcesRequired SVGDefsElement
instance IsGObject SVGDefsElement where
  typeGType _ = gTypeSVGDefsElement
  {-# INLINE typeGType #-}

noSVGDefsElement :: Maybe SVGDefsElement
noSVGDefsElement = Nothing
{-# INLINE noSVGDefsElement #-}

gTypeSVGDefsElement :: JSM GType
gTypeSVGDefsElement = GType . Object <$> jsg "SVGDefsElement"

-- | Functions for this inteface are in "JSDOM.SVGDescElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGDescElement Mozilla SVGDescElement documentation>
newtype SVGDescElement = SVGDescElement { unSVGDescElement :: JSVal }

instance PToJSVal SVGDescElement where
  pToJSVal = unSVGDescElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGDescElement where
  pFromJSVal = SVGDescElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGDescElement where
  toJSVal = return . unSVGDescElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGDescElement where
  fromJSVal v = fmap SVGDescElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGDescElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGDescElement where
  makeObject = makeObject . unSVGDescElement

instance IsSVGElement SVGDescElement
instance IsElement SVGDescElement
instance IsNode SVGDescElement
instance IsEventTarget SVGDescElement
instance IsSlotable SVGDescElement
instance IsParentNode SVGDescElement
instance IsNonDocumentTypeChildNode SVGDescElement
instance IsDocumentAndElementEventHandlers SVGDescElement
instance IsChildNode SVGDescElement
instance IsAnimatable SVGDescElement
instance IsGlobalEventHandlers SVGDescElement
instance IsElementCSSInlineStyle SVGDescElement
instance IsGObject SVGDescElement where
  typeGType _ = gTypeSVGDescElement
  {-# INLINE typeGType #-}

noSVGDescElement :: Maybe SVGDescElement
noSVGDescElement = Nothing
{-# INLINE noSVGDescElement #-}

gTypeSVGDescElement :: JSM GType
gTypeSVGDescElement = GType . Object <$> jsg "SVGDescElement"

-- | Functions for this inteface are in "JSDOM.SVGElement".
-- Base interface functions are in:
--
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGElement Mozilla SVGElement documentation>
newtype SVGElement = SVGElement { unSVGElement :: JSVal }

instance PToJSVal SVGElement where
  pToJSVal = unSVGElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGElement where
  pFromJSVal = SVGElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGElement where
  toJSVal = return . unSVGElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGElement where
  fromJSVal v = fmap SVGElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGElement where
  makeObject = makeObject . unSVGElement

class (IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsGObject o) => IsSVGElement o
toSVGElement :: IsSVGElement o => o -> SVGElement
toSVGElement = SVGElement . coerce

instance IsSVGElement SVGElement
instance IsElement SVGElement
instance IsNode SVGElement
instance IsEventTarget SVGElement
instance IsSlotable SVGElement
instance IsParentNode SVGElement
instance IsNonDocumentTypeChildNode SVGElement
instance IsDocumentAndElementEventHandlers SVGElement
instance IsChildNode SVGElement
instance IsAnimatable SVGElement
instance IsGlobalEventHandlers SVGElement
instance IsElementCSSInlineStyle SVGElement
instance IsGObject SVGElement where
  typeGType _ = gTypeSVGElement
  {-# INLINE typeGType #-}

noSVGElement :: Maybe SVGElement
noSVGElement = Nothing
{-# INLINE noSVGElement #-}

gTypeSVGElement :: JSM GType
gTypeSVGElement = GType . Object <$> jsg "SVGElement"

-- | Functions for this inteface are in "JSDOM.SVGEllipseElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGEllipseElement Mozilla SVGEllipseElement documentation>
newtype SVGEllipseElement = SVGEllipseElement { unSVGEllipseElement :: JSVal }

instance PToJSVal SVGEllipseElement where
  pToJSVal = unSVGEllipseElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGEllipseElement where
  pFromJSVal = SVGEllipseElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGEllipseElement where
  toJSVal = return . unSVGEllipseElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGEllipseElement where
  fromJSVal v = fmap SVGEllipseElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGEllipseElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGEllipseElement where
  makeObject = makeObject . unSVGEllipseElement

instance IsSVGGraphicsElement SVGEllipseElement
instance IsSVGElement SVGEllipseElement
instance IsElement SVGEllipseElement
instance IsNode SVGEllipseElement
instance IsEventTarget SVGEllipseElement
instance IsSlotable SVGEllipseElement
instance IsParentNode SVGEllipseElement
instance IsNonDocumentTypeChildNode SVGEllipseElement
instance IsDocumentAndElementEventHandlers SVGEllipseElement
instance IsChildNode SVGEllipseElement
instance IsAnimatable SVGEllipseElement
instance IsGlobalEventHandlers SVGEllipseElement
instance IsElementCSSInlineStyle SVGEllipseElement
instance IsSVGTests SVGEllipseElement
instance IsSVGExternalResourcesRequired SVGEllipseElement
instance IsGObject SVGEllipseElement where
  typeGType _ = gTypeSVGEllipseElement
  {-# INLINE typeGType #-}

noSVGEllipseElement :: Maybe SVGEllipseElement
noSVGEllipseElement = Nothing
{-# INLINE noSVGEllipseElement #-}

gTypeSVGEllipseElement :: JSM GType
gTypeSVGEllipseElement = GType . Object <$> jsg "SVGEllipseElement"

-- | Functions for this inteface are in "JSDOM.SVGException".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGException Mozilla SVGException documentation>
newtype SVGException = SVGException { unSVGException :: JSVal }

instance PToJSVal SVGException where
  pToJSVal = unSVGException
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGException where
  pFromJSVal = SVGException
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGException where
  toJSVal = return . unSVGException
  {-# INLINE toJSVal #-}

instance FromJSVal SVGException where
  fromJSVal v = fmap SVGException <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGException
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGException where
  makeObject = makeObject . unSVGException

instance IsGObject SVGException where
  typeGType _ = gTypeSVGException
  {-# INLINE typeGType #-}

noSVGException :: Maybe SVGException
noSVGException = Nothing
{-# INLINE noSVGException #-}

gTypeSVGException :: JSM GType
gTypeSVGException = GType . Object <$> jsg "SVGException"

-- | Functions for this inteface are in "JSDOM.SVGExternalResourcesRequired".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGExternalResourcesRequired Mozilla SVGExternalResourcesRequired documentation>
newtype SVGExternalResourcesRequired = SVGExternalResourcesRequired { unSVGExternalResourcesRequired :: JSVal }

instance PToJSVal SVGExternalResourcesRequired where
  pToJSVal = unSVGExternalResourcesRequired
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGExternalResourcesRequired where
  pFromJSVal = SVGExternalResourcesRequired
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGExternalResourcesRequired where
  toJSVal = return . unSVGExternalResourcesRequired
  {-# INLINE toJSVal #-}

instance FromJSVal SVGExternalResourcesRequired where
  fromJSVal v = fmap SVGExternalResourcesRequired <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGExternalResourcesRequired
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGExternalResourcesRequired where
  makeObject = makeObject . unSVGExternalResourcesRequired

class (IsGObject o) => IsSVGExternalResourcesRequired o
toSVGExternalResourcesRequired :: IsSVGExternalResourcesRequired o => o -> SVGExternalResourcesRequired
toSVGExternalResourcesRequired = SVGExternalResourcesRequired . coerce

instance IsSVGExternalResourcesRequired SVGExternalResourcesRequired
instance IsGObject SVGExternalResourcesRequired where
  typeGType _ = gTypeSVGExternalResourcesRequired
  {-# INLINE typeGType #-}

noSVGExternalResourcesRequired :: Maybe SVGExternalResourcesRequired
noSVGExternalResourcesRequired = Nothing
{-# INLINE noSVGExternalResourcesRequired #-}

gTypeSVGExternalResourcesRequired :: JSM GType
gTypeSVGExternalResourcesRequired = GType . Object <$> jsg "SVGExternalResourcesRequired"

-- | Functions for this inteface are in "JSDOM.SVGFEBlendElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEBlendElement Mozilla SVGFEBlendElement documentation>
newtype SVGFEBlendElement = SVGFEBlendElement { unSVGFEBlendElement :: JSVal }

instance PToJSVal SVGFEBlendElement where
  pToJSVal = unSVGFEBlendElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEBlendElement where
  pFromJSVal = SVGFEBlendElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEBlendElement where
  toJSVal = return . unSVGFEBlendElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEBlendElement where
  fromJSVal v = fmap SVGFEBlendElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEBlendElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEBlendElement where
  makeObject = makeObject . unSVGFEBlendElement

instance IsSVGElement SVGFEBlendElement
instance IsElement SVGFEBlendElement
instance IsNode SVGFEBlendElement
instance IsEventTarget SVGFEBlendElement
instance IsSlotable SVGFEBlendElement
instance IsParentNode SVGFEBlendElement
instance IsNonDocumentTypeChildNode SVGFEBlendElement
instance IsDocumentAndElementEventHandlers SVGFEBlendElement
instance IsChildNode SVGFEBlendElement
instance IsAnimatable SVGFEBlendElement
instance IsGlobalEventHandlers SVGFEBlendElement
instance IsElementCSSInlineStyle SVGFEBlendElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEBlendElement
instance IsGObject SVGFEBlendElement where
  typeGType _ = gTypeSVGFEBlendElement
  {-# INLINE typeGType #-}

noSVGFEBlendElement :: Maybe SVGFEBlendElement
noSVGFEBlendElement = Nothing
{-# INLINE noSVGFEBlendElement #-}

gTypeSVGFEBlendElement :: JSM GType
gTypeSVGFEBlendElement = GType . Object <$> jsg "SVGFEBlendElement"

-- | Functions for this inteface are in "JSDOM.SVGFEColorMatrixElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEColorMatrixElement Mozilla SVGFEColorMatrixElement documentation>
newtype SVGFEColorMatrixElement = SVGFEColorMatrixElement { unSVGFEColorMatrixElement :: JSVal }

instance PToJSVal SVGFEColorMatrixElement where
  pToJSVal = unSVGFEColorMatrixElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEColorMatrixElement where
  pFromJSVal = SVGFEColorMatrixElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEColorMatrixElement where
  toJSVal = return . unSVGFEColorMatrixElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEColorMatrixElement where
  fromJSVal v = fmap SVGFEColorMatrixElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEColorMatrixElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEColorMatrixElement where
  makeObject = makeObject . unSVGFEColorMatrixElement

instance IsSVGElement SVGFEColorMatrixElement
instance IsElement SVGFEColorMatrixElement
instance IsNode SVGFEColorMatrixElement
instance IsEventTarget SVGFEColorMatrixElement
instance IsSlotable SVGFEColorMatrixElement
instance IsParentNode SVGFEColorMatrixElement
instance IsNonDocumentTypeChildNode SVGFEColorMatrixElement
instance IsDocumentAndElementEventHandlers SVGFEColorMatrixElement
instance IsChildNode SVGFEColorMatrixElement
instance IsAnimatable SVGFEColorMatrixElement
instance IsGlobalEventHandlers SVGFEColorMatrixElement
instance IsElementCSSInlineStyle SVGFEColorMatrixElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEColorMatrixElement
instance IsGObject SVGFEColorMatrixElement where
  typeGType _ = gTypeSVGFEColorMatrixElement
  {-# INLINE typeGType #-}

noSVGFEColorMatrixElement :: Maybe SVGFEColorMatrixElement
noSVGFEColorMatrixElement = Nothing
{-# INLINE noSVGFEColorMatrixElement #-}

gTypeSVGFEColorMatrixElement :: JSM GType
gTypeSVGFEColorMatrixElement = GType . Object <$> jsg "SVGFEColorMatrixElement"

-- | Functions for this inteface are in "JSDOM.SVGFEComponentTransferElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEComponentTransferElement Mozilla SVGFEComponentTransferElement documentation>
newtype SVGFEComponentTransferElement = SVGFEComponentTransferElement { unSVGFEComponentTransferElement :: JSVal }

instance PToJSVal SVGFEComponentTransferElement where
  pToJSVal = unSVGFEComponentTransferElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEComponentTransferElement where
  pFromJSVal = SVGFEComponentTransferElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEComponentTransferElement where
  toJSVal = return . unSVGFEComponentTransferElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEComponentTransferElement where
  fromJSVal v = fmap SVGFEComponentTransferElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEComponentTransferElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEComponentTransferElement where
  makeObject = makeObject . unSVGFEComponentTransferElement

instance IsSVGElement SVGFEComponentTransferElement
instance IsElement SVGFEComponentTransferElement
instance IsNode SVGFEComponentTransferElement
instance IsEventTarget SVGFEComponentTransferElement
instance IsSlotable SVGFEComponentTransferElement
instance IsParentNode SVGFEComponentTransferElement
instance IsNonDocumentTypeChildNode SVGFEComponentTransferElement
instance IsDocumentAndElementEventHandlers SVGFEComponentTransferElement
instance IsChildNode SVGFEComponentTransferElement
instance IsAnimatable SVGFEComponentTransferElement
instance IsGlobalEventHandlers SVGFEComponentTransferElement
instance IsElementCSSInlineStyle SVGFEComponentTransferElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEComponentTransferElement
instance IsGObject SVGFEComponentTransferElement where
  typeGType _ = gTypeSVGFEComponentTransferElement
  {-# INLINE typeGType #-}

noSVGFEComponentTransferElement :: Maybe SVGFEComponentTransferElement
noSVGFEComponentTransferElement = Nothing
{-# INLINE noSVGFEComponentTransferElement #-}

gTypeSVGFEComponentTransferElement :: JSM GType
gTypeSVGFEComponentTransferElement = GType . Object <$> jsg "SVGFEComponentTransferElement"

-- | Functions for this inteface are in "JSDOM.SVGFECompositeElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFECompositeElement Mozilla SVGFECompositeElement documentation>
newtype SVGFECompositeElement = SVGFECompositeElement { unSVGFECompositeElement :: JSVal }

instance PToJSVal SVGFECompositeElement where
  pToJSVal = unSVGFECompositeElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFECompositeElement where
  pFromJSVal = SVGFECompositeElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFECompositeElement where
  toJSVal = return . unSVGFECompositeElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFECompositeElement where
  fromJSVal v = fmap SVGFECompositeElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFECompositeElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFECompositeElement where
  makeObject = makeObject . unSVGFECompositeElement

instance IsSVGElement SVGFECompositeElement
instance IsElement SVGFECompositeElement
instance IsNode SVGFECompositeElement
instance IsEventTarget SVGFECompositeElement
instance IsSlotable SVGFECompositeElement
instance IsParentNode SVGFECompositeElement
instance IsNonDocumentTypeChildNode SVGFECompositeElement
instance IsDocumentAndElementEventHandlers SVGFECompositeElement
instance IsChildNode SVGFECompositeElement
instance IsAnimatable SVGFECompositeElement
instance IsGlobalEventHandlers SVGFECompositeElement
instance IsElementCSSInlineStyle SVGFECompositeElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFECompositeElement
instance IsGObject SVGFECompositeElement where
  typeGType _ = gTypeSVGFECompositeElement
  {-# INLINE typeGType #-}

noSVGFECompositeElement :: Maybe SVGFECompositeElement
noSVGFECompositeElement = Nothing
{-# INLINE noSVGFECompositeElement #-}

gTypeSVGFECompositeElement :: JSM GType
gTypeSVGFECompositeElement = GType . Object <$> jsg "SVGFECompositeElement"

-- | Functions for this inteface are in "JSDOM.SVGFEConvolveMatrixElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEConvolveMatrixElement Mozilla SVGFEConvolveMatrixElement documentation>
newtype SVGFEConvolveMatrixElement = SVGFEConvolveMatrixElement { unSVGFEConvolveMatrixElement :: JSVal }

instance PToJSVal SVGFEConvolveMatrixElement where
  pToJSVal = unSVGFEConvolveMatrixElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEConvolveMatrixElement where
  pFromJSVal = SVGFEConvolveMatrixElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEConvolveMatrixElement where
  toJSVal = return . unSVGFEConvolveMatrixElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEConvolveMatrixElement where
  fromJSVal v = fmap SVGFEConvolveMatrixElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEConvolveMatrixElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEConvolveMatrixElement where
  makeObject = makeObject . unSVGFEConvolveMatrixElement

instance IsSVGElement SVGFEConvolveMatrixElement
instance IsElement SVGFEConvolveMatrixElement
instance IsNode SVGFEConvolveMatrixElement
instance IsEventTarget SVGFEConvolveMatrixElement
instance IsSlotable SVGFEConvolveMatrixElement
instance IsParentNode SVGFEConvolveMatrixElement
instance IsNonDocumentTypeChildNode SVGFEConvolveMatrixElement
instance IsDocumentAndElementEventHandlers SVGFEConvolveMatrixElement
instance IsChildNode SVGFEConvolveMatrixElement
instance IsAnimatable SVGFEConvolveMatrixElement
instance IsGlobalEventHandlers SVGFEConvolveMatrixElement
instance IsElementCSSInlineStyle SVGFEConvolveMatrixElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEConvolveMatrixElement
instance IsGObject SVGFEConvolveMatrixElement where
  typeGType _ = gTypeSVGFEConvolveMatrixElement
  {-# INLINE typeGType #-}

noSVGFEConvolveMatrixElement :: Maybe SVGFEConvolveMatrixElement
noSVGFEConvolveMatrixElement = Nothing
{-# INLINE noSVGFEConvolveMatrixElement #-}

gTypeSVGFEConvolveMatrixElement :: JSM GType
gTypeSVGFEConvolveMatrixElement = GType . Object <$> jsg "SVGFEConvolveMatrixElement"

-- | Functions for this inteface are in "JSDOM.SVGFEDiffuseLightingElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDiffuseLightingElement Mozilla SVGFEDiffuseLightingElement documentation>
newtype SVGFEDiffuseLightingElement = SVGFEDiffuseLightingElement { unSVGFEDiffuseLightingElement :: JSVal }

instance PToJSVal SVGFEDiffuseLightingElement where
  pToJSVal = unSVGFEDiffuseLightingElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEDiffuseLightingElement where
  pFromJSVal = SVGFEDiffuseLightingElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEDiffuseLightingElement where
  toJSVal = return . unSVGFEDiffuseLightingElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEDiffuseLightingElement where
  fromJSVal v = fmap SVGFEDiffuseLightingElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEDiffuseLightingElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEDiffuseLightingElement where
  makeObject = makeObject . unSVGFEDiffuseLightingElement

instance IsSVGElement SVGFEDiffuseLightingElement
instance IsElement SVGFEDiffuseLightingElement
instance IsNode SVGFEDiffuseLightingElement
instance IsEventTarget SVGFEDiffuseLightingElement
instance IsSlotable SVGFEDiffuseLightingElement
instance IsParentNode SVGFEDiffuseLightingElement
instance IsNonDocumentTypeChildNode SVGFEDiffuseLightingElement
instance IsDocumentAndElementEventHandlers SVGFEDiffuseLightingElement
instance IsChildNode SVGFEDiffuseLightingElement
instance IsAnimatable SVGFEDiffuseLightingElement
instance IsGlobalEventHandlers SVGFEDiffuseLightingElement
instance IsElementCSSInlineStyle SVGFEDiffuseLightingElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEDiffuseLightingElement
instance IsGObject SVGFEDiffuseLightingElement where
  typeGType _ = gTypeSVGFEDiffuseLightingElement
  {-# INLINE typeGType #-}

noSVGFEDiffuseLightingElement :: Maybe SVGFEDiffuseLightingElement
noSVGFEDiffuseLightingElement = Nothing
{-# INLINE noSVGFEDiffuseLightingElement #-}

gTypeSVGFEDiffuseLightingElement :: JSM GType
gTypeSVGFEDiffuseLightingElement = GType . Object <$> jsg "SVGFEDiffuseLightingElement"

-- | Functions for this inteface are in "JSDOM.SVGFEDisplacementMapElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDisplacementMapElement Mozilla SVGFEDisplacementMapElement documentation>
newtype SVGFEDisplacementMapElement = SVGFEDisplacementMapElement { unSVGFEDisplacementMapElement :: JSVal }

instance PToJSVal SVGFEDisplacementMapElement where
  pToJSVal = unSVGFEDisplacementMapElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEDisplacementMapElement where
  pFromJSVal = SVGFEDisplacementMapElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEDisplacementMapElement where
  toJSVal = return . unSVGFEDisplacementMapElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEDisplacementMapElement where
  fromJSVal v = fmap SVGFEDisplacementMapElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEDisplacementMapElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEDisplacementMapElement where
  makeObject = makeObject . unSVGFEDisplacementMapElement

instance IsSVGElement SVGFEDisplacementMapElement
instance IsElement SVGFEDisplacementMapElement
instance IsNode SVGFEDisplacementMapElement
instance IsEventTarget SVGFEDisplacementMapElement
instance IsSlotable SVGFEDisplacementMapElement
instance IsParentNode SVGFEDisplacementMapElement
instance IsNonDocumentTypeChildNode SVGFEDisplacementMapElement
instance IsDocumentAndElementEventHandlers SVGFEDisplacementMapElement
instance IsChildNode SVGFEDisplacementMapElement
instance IsAnimatable SVGFEDisplacementMapElement
instance IsGlobalEventHandlers SVGFEDisplacementMapElement
instance IsElementCSSInlineStyle SVGFEDisplacementMapElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEDisplacementMapElement
instance IsGObject SVGFEDisplacementMapElement where
  typeGType _ = gTypeSVGFEDisplacementMapElement
  {-# INLINE typeGType #-}

noSVGFEDisplacementMapElement :: Maybe SVGFEDisplacementMapElement
noSVGFEDisplacementMapElement = Nothing
{-# INLINE noSVGFEDisplacementMapElement #-}

gTypeSVGFEDisplacementMapElement :: JSM GType
gTypeSVGFEDisplacementMapElement = GType . Object <$> jsg "SVGFEDisplacementMapElement"

-- | Functions for this inteface are in "JSDOM.SVGFEDistantLightElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDistantLightElement Mozilla SVGFEDistantLightElement documentation>
newtype SVGFEDistantLightElement = SVGFEDistantLightElement { unSVGFEDistantLightElement :: JSVal }

instance PToJSVal SVGFEDistantLightElement where
  pToJSVal = unSVGFEDistantLightElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEDistantLightElement where
  pFromJSVal = SVGFEDistantLightElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEDistantLightElement where
  toJSVal = return . unSVGFEDistantLightElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEDistantLightElement where
  fromJSVal v = fmap SVGFEDistantLightElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEDistantLightElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEDistantLightElement where
  makeObject = makeObject . unSVGFEDistantLightElement

instance IsSVGElement SVGFEDistantLightElement
instance IsElement SVGFEDistantLightElement
instance IsNode SVGFEDistantLightElement
instance IsEventTarget SVGFEDistantLightElement
instance IsSlotable SVGFEDistantLightElement
instance IsParentNode SVGFEDistantLightElement
instance IsNonDocumentTypeChildNode SVGFEDistantLightElement
instance IsDocumentAndElementEventHandlers SVGFEDistantLightElement
instance IsChildNode SVGFEDistantLightElement
instance IsAnimatable SVGFEDistantLightElement
instance IsGlobalEventHandlers SVGFEDistantLightElement
instance IsElementCSSInlineStyle SVGFEDistantLightElement
instance IsGObject SVGFEDistantLightElement where
  typeGType _ = gTypeSVGFEDistantLightElement
  {-# INLINE typeGType #-}

noSVGFEDistantLightElement :: Maybe SVGFEDistantLightElement
noSVGFEDistantLightElement = Nothing
{-# INLINE noSVGFEDistantLightElement #-}

gTypeSVGFEDistantLightElement :: JSM GType
gTypeSVGFEDistantLightElement = GType . Object <$> jsg "SVGFEDistantLightElement"

-- | Functions for this inteface are in "JSDOM.SVGFEDropShadowElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEDropShadowElement Mozilla SVGFEDropShadowElement documentation>
newtype SVGFEDropShadowElement = SVGFEDropShadowElement { unSVGFEDropShadowElement :: JSVal }

instance PToJSVal SVGFEDropShadowElement where
  pToJSVal = unSVGFEDropShadowElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEDropShadowElement where
  pFromJSVal = SVGFEDropShadowElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEDropShadowElement where
  toJSVal = return . unSVGFEDropShadowElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEDropShadowElement where
  fromJSVal v = fmap SVGFEDropShadowElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEDropShadowElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEDropShadowElement where
  makeObject = makeObject . unSVGFEDropShadowElement

instance IsSVGElement SVGFEDropShadowElement
instance IsElement SVGFEDropShadowElement
instance IsNode SVGFEDropShadowElement
instance IsEventTarget SVGFEDropShadowElement
instance IsSlotable SVGFEDropShadowElement
instance IsParentNode SVGFEDropShadowElement
instance IsNonDocumentTypeChildNode SVGFEDropShadowElement
instance IsDocumentAndElementEventHandlers SVGFEDropShadowElement
instance IsChildNode SVGFEDropShadowElement
instance IsAnimatable SVGFEDropShadowElement
instance IsGlobalEventHandlers SVGFEDropShadowElement
instance IsElementCSSInlineStyle SVGFEDropShadowElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEDropShadowElement
instance IsGObject SVGFEDropShadowElement where
  typeGType _ = gTypeSVGFEDropShadowElement
  {-# INLINE typeGType #-}

noSVGFEDropShadowElement :: Maybe SVGFEDropShadowElement
noSVGFEDropShadowElement = Nothing
{-# INLINE noSVGFEDropShadowElement #-}

gTypeSVGFEDropShadowElement :: JSM GType
gTypeSVGFEDropShadowElement = GType . Object <$> jsg "SVGFEDropShadowElement"

-- | Functions for this inteface are in "JSDOM.SVGFEFloodElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEFloodElement Mozilla SVGFEFloodElement documentation>
newtype SVGFEFloodElement = SVGFEFloodElement { unSVGFEFloodElement :: JSVal }

instance PToJSVal SVGFEFloodElement where
  pToJSVal = unSVGFEFloodElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEFloodElement where
  pFromJSVal = SVGFEFloodElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEFloodElement where
  toJSVal = return . unSVGFEFloodElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEFloodElement where
  fromJSVal v = fmap SVGFEFloodElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEFloodElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEFloodElement where
  makeObject = makeObject . unSVGFEFloodElement

instance IsSVGElement SVGFEFloodElement
instance IsElement SVGFEFloodElement
instance IsNode SVGFEFloodElement
instance IsEventTarget SVGFEFloodElement
instance IsSlotable SVGFEFloodElement
instance IsParentNode SVGFEFloodElement
instance IsNonDocumentTypeChildNode SVGFEFloodElement
instance IsDocumentAndElementEventHandlers SVGFEFloodElement
instance IsChildNode SVGFEFloodElement
instance IsAnimatable SVGFEFloodElement
instance IsGlobalEventHandlers SVGFEFloodElement
instance IsElementCSSInlineStyle SVGFEFloodElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEFloodElement
instance IsGObject SVGFEFloodElement where
  typeGType _ = gTypeSVGFEFloodElement
  {-# INLINE typeGType #-}

noSVGFEFloodElement :: Maybe SVGFEFloodElement
noSVGFEFloodElement = Nothing
{-# INLINE noSVGFEFloodElement #-}

gTypeSVGFEFloodElement :: JSM GType
gTypeSVGFEFloodElement = GType . Object <$> jsg "SVGFEFloodElement"

-- | Functions for this inteface are in "JSDOM.SVGFEFuncAElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGComponentTransferFunctionElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEFuncAElement Mozilla SVGFEFuncAElement documentation>
newtype SVGFEFuncAElement = SVGFEFuncAElement { unSVGFEFuncAElement :: JSVal }

instance PToJSVal SVGFEFuncAElement where
  pToJSVal = unSVGFEFuncAElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEFuncAElement where
  pFromJSVal = SVGFEFuncAElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEFuncAElement where
  toJSVal = return . unSVGFEFuncAElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEFuncAElement where
  fromJSVal v = fmap SVGFEFuncAElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEFuncAElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEFuncAElement where
  makeObject = makeObject . unSVGFEFuncAElement

instance IsSVGComponentTransferFunctionElement SVGFEFuncAElement
instance IsSVGElement SVGFEFuncAElement
instance IsElement SVGFEFuncAElement
instance IsNode SVGFEFuncAElement
instance IsEventTarget SVGFEFuncAElement
instance IsSlotable SVGFEFuncAElement
instance IsParentNode SVGFEFuncAElement
instance IsNonDocumentTypeChildNode SVGFEFuncAElement
instance IsDocumentAndElementEventHandlers SVGFEFuncAElement
instance IsChildNode SVGFEFuncAElement
instance IsAnimatable SVGFEFuncAElement
instance IsGlobalEventHandlers SVGFEFuncAElement
instance IsElementCSSInlineStyle SVGFEFuncAElement
instance IsGObject SVGFEFuncAElement where
  typeGType _ = gTypeSVGFEFuncAElement
  {-# INLINE typeGType #-}

noSVGFEFuncAElement :: Maybe SVGFEFuncAElement
noSVGFEFuncAElement = Nothing
{-# INLINE noSVGFEFuncAElement #-}

gTypeSVGFEFuncAElement :: JSM GType
gTypeSVGFEFuncAElement = GType . Object <$> jsg "SVGFEFuncAElement"

-- | Functions for this inteface are in "JSDOM.SVGFEFuncBElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGComponentTransferFunctionElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEFuncBElement Mozilla SVGFEFuncBElement documentation>
newtype SVGFEFuncBElement = SVGFEFuncBElement { unSVGFEFuncBElement :: JSVal }

instance PToJSVal SVGFEFuncBElement where
  pToJSVal = unSVGFEFuncBElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEFuncBElement where
  pFromJSVal = SVGFEFuncBElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEFuncBElement where
  toJSVal = return . unSVGFEFuncBElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEFuncBElement where
  fromJSVal v = fmap SVGFEFuncBElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEFuncBElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEFuncBElement where
  makeObject = makeObject . unSVGFEFuncBElement

instance IsSVGComponentTransferFunctionElement SVGFEFuncBElement
instance IsSVGElement SVGFEFuncBElement
instance IsElement SVGFEFuncBElement
instance IsNode SVGFEFuncBElement
instance IsEventTarget SVGFEFuncBElement
instance IsSlotable SVGFEFuncBElement
instance IsParentNode SVGFEFuncBElement
instance IsNonDocumentTypeChildNode SVGFEFuncBElement
instance IsDocumentAndElementEventHandlers SVGFEFuncBElement
instance IsChildNode SVGFEFuncBElement
instance IsAnimatable SVGFEFuncBElement
instance IsGlobalEventHandlers SVGFEFuncBElement
instance IsElementCSSInlineStyle SVGFEFuncBElement
instance IsGObject SVGFEFuncBElement where
  typeGType _ = gTypeSVGFEFuncBElement
  {-# INLINE typeGType #-}

noSVGFEFuncBElement :: Maybe SVGFEFuncBElement
noSVGFEFuncBElement = Nothing
{-# INLINE noSVGFEFuncBElement #-}

gTypeSVGFEFuncBElement :: JSM GType
gTypeSVGFEFuncBElement = GType . Object <$> jsg "SVGFEFuncBElement"

-- | Functions for this inteface are in "JSDOM.SVGFEFuncGElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGComponentTransferFunctionElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEFuncGElement Mozilla SVGFEFuncGElement documentation>
newtype SVGFEFuncGElement = SVGFEFuncGElement { unSVGFEFuncGElement :: JSVal }

instance PToJSVal SVGFEFuncGElement where
  pToJSVal = unSVGFEFuncGElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEFuncGElement where
  pFromJSVal = SVGFEFuncGElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEFuncGElement where
  toJSVal = return . unSVGFEFuncGElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEFuncGElement where
  fromJSVal v = fmap SVGFEFuncGElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEFuncGElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEFuncGElement where
  makeObject = makeObject . unSVGFEFuncGElement

instance IsSVGComponentTransferFunctionElement SVGFEFuncGElement
instance IsSVGElement SVGFEFuncGElement
instance IsElement SVGFEFuncGElement
instance IsNode SVGFEFuncGElement
instance IsEventTarget SVGFEFuncGElement
instance IsSlotable SVGFEFuncGElement
instance IsParentNode SVGFEFuncGElement
instance IsNonDocumentTypeChildNode SVGFEFuncGElement
instance IsDocumentAndElementEventHandlers SVGFEFuncGElement
instance IsChildNode SVGFEFuncGElement
instance IsAnimatable SVGFEFuncGElement
instance IsGlobalEventHandlers SVGFEFuncGElement
instance IsElementCSSInlineStyle SVGFEFuncGElement
instance IsGObject SVGFEFuncGElement where
  typeGType _ = gTypeSVGFEFuncGElement
  {-# INLINE typeGType #-}

noSVGFEFuncGElement :: Maybe SVGFEFuncGElement
noSVGFEFuncGElement = Nothing
{-# INLINE noSVGFEFuncGElement #-}

gTypeSVGFEFuncGElement :: JSM GType
gTypeSVGFEFuncGElement = GType . Object <$> jsg "SVGFEFuncGElement"

-- | Functions for this inteface are in "JSDOM.SVGFEFuncRElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGComponentTransferFunctionElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEFuncRElement Mozilla SVGFEFuncRElement documentation>
newtype SVGFEFuncRElement = SVGFEFuncRElement { unSVGFEFuncRElement :: JSVal }

instance PToJSVal SVGFEFuncRElement where
  pToJSVal = unSVGFEFuncRElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEFuncRElement where
  pFromJSVal = SVGFEFuncRElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEFuncRElement where
  toJSVal = return . unSVGFEFuncRElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEFuncRElement where
  fromJSVal v = fmap SVGFEFuncRElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEFuncRElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEFuncRElement where
  makeObject = makeObject . unSVGFEFuncRElement

instance IsSVGComponentTransferFunctionElement SVGFEFuncRElement
instance IsSVGElement SVGFEFuncRElement
instance IsElement SVGFEFuncRElement
instance IsNode SVGFEFuncRElement
instance IsEventTarget SVGFEFuncRElement
instance IsSlotable SVGFEFuncRElement
instance IsParentNode SVGFEFuncRElement
instance IsNonDocumentTypeChildNode SVGFEFuncRElement
instance IsDocumentAndElementEventHandlers SVGFEFuncRElement
instance IsChildNode SVGFEFuncRElement
instance IsAnimatable SVGFEFuncRElement
instance IsGlobalEventHandlers SVGFEFuncRElement
instance IsElementCSSInlineStyle SVGFEFuncRElement
instance IsGObject SVGFEFuncRElement where
  typeGType _ = gTypeSVGFEFuncRElement
  {-# INLINE typeGType #-}

noSVGFEFuncRElement :: Maybe SVGFEFuncRElement
noSVGFEFuncRElement = Nothing
{-# INLINE noSVGFEFuncRElement #-}

gTypeSVGFEFuncRElement :: JSM GType
gTypeSVGFEFuncRElement = GType . Object <$> jsg "SVGFEFuncRElement"

-- | Functions for this inteface are in "JSDOM.SVGFEGaussianBlurElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEGaussianBlurElement Mozilla SVGFEGaussianBlurElement documentation>
newtype SVGFEGaussianBlurElement = SVGFEGaussianBlurElement { unSVGFEGaussianBlurElement :: JSVal }

instance PToJSVal SVGFEGaussianBlurElement where
  pToJSVal = unSVGFEGaussianBlurElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEGaussianBlurElement where
  pFromJSVal = SVGFEGaussianBlurElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEGaussianBlurElement where
  toJSVal = return . unSVGFEGaussianBlurElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEGaussianBlurElement where
  fromJSVal v = fmap SVGFEGaussianBlurElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEGaussianBlurElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEGaussianBlurElement where
  makeObject = makeObject . unSVGFEGaussianBlurElement

instance IsSVGElement SVGFEGaussianBlurElement
instance IsElement SVGFEGaussianBlurElement
instance IsNode SVGFEGaussianBlurElement
instance IsEventTarget SVGFEGaussianBlurElement
instance IsSlotable SVGFEGaussianBlurElement
instance IsParentNode SVGFEGaussianBlurElement
instance IsNonDocumentTypeChildNode SVGFEGaussianBlurElement
instance IsDocumentAndElementEventHandlers SVGFEGaussianBlurElement
instance IsChildNode SVGFEGaussianBlurElement
instance IsAnimatable SVGFEGaussianBlurElement
instance IsGlobalEventHandlers SVGFEGaussianBlurElement
instance IsElementCSSInlineStyle SVGFEGaussianBlurElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEGaussianBlurElement
instance IsGObject SVGFEGaussianBlurElement where
  typeGType _ = gTypeSVGFEGaussianBlurElement
  {-# INLINE typeGType #-}

noSVGFEGaussianBlurElement :: Maybe SVGFEGaussianBlurElement
noSVGFEGaussianBlurElement = Nothing
{-# INLINE noSVGFEGaussianBlurElement #-}

gTypeSVGFEGaussianBlurElement :: JSM GType
gTypeSVGFEGaussianBlurElement = GType . Object <$> jsg "SVGFEGaussianBlurElement"

-- | Functions for this inteface are in "JSDOM.SVGFEImageElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEImageElement Mozilla SVGFEImageElement documentation>
newtype SVGFEImageElement = SVGFEImageElement { unSVGFEImageElement :: JSVal }

instance PToJSVal SVGFEImageElement where
  pToJSVal = unSVGFEImageElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEImageElement where
  pFromJSVal = SVGFEImageElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEImageElement where
  toJSVal = return . unSVGFEImageElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEImageElement where
  fromJSVal v = fmap SVGFEImageElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEImageElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEImageElement where
  makeObject = makeObject . unSVGFEImageElement

instance IsSVGElement SVGFEImageElement
instance IsElement SVGFEImageElement
instance IsNode SVGFEImageElement
instance IsEventTarget SVGFEImageElement
instance IsSlotable SVGFEImageElement
instance IsParentNode SVGFEImageElement
instance IsNonDocumentTypeChildNode SVGFEImageElement
instance IsDocumentAndElementEventHandlers SVGFEImageElement
instance IsChildNode SVGFEImageElement
instance IsAnimatable SVGFEImageElement
instance IsGlobalEventHandlers SVGFEImageElement
instance IsElementCSSInlineStyle SVGFEImageElement
instance IsSVGURIReference SVGFEImageElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEImageElement
instance IsSVGExternalResourcesRequired SVGFEImageElement
instance IsGObject SVGFEImageElement where
  typeGType _ = gTypeSVGFEImageElement
  {-# INLINE typeGType #-}

noSVGFEImageElement :: Maybe SVGFEImageElement
noSVGFEImageElement = Nothing
{-# INLINE noSVGFEImageElement #-}

gTypeSVGFEImageElement :: JSM GType
gTypeSVGFEImageElement = GType . Object <$> jsg "SVGFEImageElement"

-- | Functions for this inteface are in "JSDOM.SVGFEMergeElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMergeElement Mozilla SVGFEMergeElement documentation>
newtype SVGFEMergeElement = SVGFEMergeElement { unSVGFEMergeElement :: JSVal }

instance PToJSVal SVGFEMergeElement where
  pToJSVal = unSVGFEMergeElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEMergeElement where
  pFromJSVal = SVGFEMergeElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEMergeElement where
  toJSVal = return . unSVGFEMergeElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEMergeElement where
  fromJSVal v = fmap SVGFEMergeElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEMergeElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEMergeElement where
  makeObject = makeObject . unSVGFEMergeElement

instance IsSVGElement SVGFEMergeElement
instance IsElement SVGFEMergeElement
instance IsNode SVGFEMergeElement
instance IsEventTarget SVGFEMergeElement
instance IsSlotable SVGFEMergeElement
instance IsParentNode SVGFEMergeElement
instance IsNonDocumentTypeChildNode SVGFEMergeElement
instance IsDocumentAndElementEventHandlers SVGFEMergeElement
instance IsChildNode SVGFEMergeElement
instance IsAnimatable SVGFEMergeElement
instance IsGlobalEventHandlers SVGFEMergeElement
instance IsElementCSSInlineStyle SVGFEMergeElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEMergeElement
instance IsGObject SVGFEMergeElement where
  typeGType _ = gTypeSVGFEMergeElement
  {-# INLINE typeGType #-}

noSVGFEMergeElement :: Maybe SVGFEMergeElement
noSVGFEMergeElement = Nothing
{-# INLINE noSVGFEMergeElement #-}

gTypeSVGFEMergeElement :: JSM GType
gTypeSVGFEMergeElement = GType . Object <$> jsg "SVGFEMergeElement"

-- | Functions for this inteface are in "JSDOM.SVGFEMergeNodeElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMergeNodeElement Mozilla SVGFEMergeNodeElement documentation>
newtype SVGFEMergeNodeElement = SVGFEMergeNodeElement { unSVGFEMergeNodeElement :: JSVal }

instance PToJSVal SVGFEMergeNodeElement where
  pToJSVal = unSVGFEMergeNodeElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEMergeNodeElement where
  pFromJSVal = SVGFEMergeNodeElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEMergeNodeElement where
  toJSVal = return . unSVGFEMergeNodeElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEMergeNodeElement where
  fromJSVal v = fmap SVGFEMergeNodeElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEMergeNodeElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEMergeNodeElement where
  makeObject = makeObject . unSVGFEMergeNodeElement

instance IsSVGElement SVGFEMergeNodeElement
instance IsElement SVGFEMergeNodeElement
instance IsNode SVGFEMergeNodeElement
instance IsEventTarget SVGFEMergeNodeElement
instance IsSlotable SVGFEMergeNodeElement
instance IsParentNode SVGFEMergeNodeElement
instance IsNonDocumentTypeChildNode SVGFEMergeNodeElement
instance IsDocumentAndElementEventHandlers SVGFEMergeNodeElement
instance IsChildNode SVGFEMergeNodeElement
instance IsAnimatable SVGFEMergeNodeElement
instance IsGlobalEventHandlers SVGFEMergeNodeElement
instance IsElementCSSInlineStyle SVGFEMergeNodeElement
instance IsGObject SVGFEMergeNodeElement where
  typeGType _ = gTypeSVGFEMergeNodeElement
  {-# INLINE typeGType #-}

noSVGFEMergeNodeElement :: Maybe SVGFEMergeNodeElement
noSVGFEMergeNodeElement = Nothing
{-# INLINE noSVGFEMergeNodeElement #-}

gTypeSVGFEMergeNodeElement :: JSM GType
gTypeSVGFEMergeNodeElement = GType . Object <$> jsg "SVGFEMergeNodeElement"

-- | Functions for this inteface are in "JSDOM.SVGFEMorphologyElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEMorphologyElement Mozilla SVGFEMorphologyElement documentation>
newtype SVGFEMorphologyElement = SVGFEMorphologyElement { unSVGFEMorphologyElement :: JSVal }

instance PToJSVal SVGFEMorphologyElement where
  pToJSVal = unSVGFEMorphologyElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEMorphologyElement where
  pFromJSVal = SVGFEMorphologyElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEMorphologyElement where
  toJSVal = return . unSVGFEMorphologyElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEMorphologyElement where
  fromJSVal v = fmap SVGFEMorphologyElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEMorphologyElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEMorphologyElement where
  makeObject = makeObject . unSVGFEMorphologyElement

instance IsSVGElement SVGFEMorphologyElement
instance IsElement SVGFEMorphologyElement
instance IsNode SVGFEMorphologyElement
instance IsEventTarget SVGFEMorphologyElement
instance IsSlotable SVGFEMorphologyElement
instance IsParentNode SVGFEMorphologyElement
instance IsNonDocumentTypeChildNode SVGFEMorphologyElement
instance IsDocumentAndElementEventHandlers SVGFEMorphologyElement
instance IsChildNode SVGFEMorphologyElement
instance IsAnimatable SVGFEMorphologyElement
instance IsGlobalEventHandlers SVGFEMorphologyElement
instance IsElementCSSInlineStyle SVGFEMorphologyElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEMorphologyElement
instance IsGObject SVGFEMorphologyElement where
  typeGType _ = gTypeSVGFEMorphologyElement
  {-# INLINE typeGType #-}

noSVGFEMorphologyElement :: Maybe SVGFEMorphologyElement
noSVGFEMorphologyElement = Nothing
{-# INLINE noSVGFEMorphologyElement #-}

gTypeSVGFEMorphologyElement :: JSM GType
gTypeSVGFEMorphologyElement = GType . Object <$> jsg "SVGFEMorphologyElement"

-- | Functions for this inteface are in "JSDOM.SVGFEOffsetElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEOffsetElement Mozilla SVGFEOffsetElement documentation>
newtype SVGFEOffsetElement = SVGFEOffsetElement { unSVGFEOffsetElement :: JSVal }

instance PToJSVal SVGFEOffsetElement where
  pToJSVal = unSVGFEOffsetElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEOffsetElement where
  pFromJSVal = SVGFEOffsetElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEOffsetElement where
  toJSVal = return . unSVGFEOffsetElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEOffsetElement where
  fromJSVal v = fmap SVGFEOffsetElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEOffsetElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEOffsetElement where
  makeObject = makeObject . unSVGFEOffsetElement

instance IsSVGElement SVGFEOffsetElement
instance IsElement SVGFEOffsetElement
instance IsNode SVGFEOffsetElement
instance IsEventTarget SVGFEOffsetElement
instance IsSlotable SVGFEOffsetElement
instance IsParentNode SVGFEOffsetElement
instance IsNonDocumentTypeChildNode SVGFEOffsetElement
instance IsDocumentAndElementEventHandlers SVGFEOffsetElement
instance IsChildNode SVGFEOffsetElement
instance IsAnimatable SVGFEOffsetElement
instance IsGlobalEventHandlers SVGFEOffsetElement
instance IsElementCSSInlineStyle SVGFEOffsetElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFEOffsetElement
instance IsGObject SVGFEOffsetElement where
  typeGType _ = gTypeSVGFEOffsetElement
  {-# INLINE typeGType #-}

noSVGFEOffsetElement :: Maybe SVGFEOffsetElement
noSVGFEOffsetElement = Nothing
{-# INLINE noSVGFEOffsetElement #-}

gTypeSVGFEOffsetElement :: JSM GType
gTypeSVGFEOffsetElement = GType . Object <$> jsg "SVGFEOffsetElement"

-- | Functions for this inteface are in "JSDOM.SVGFEPointLightElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFEPointLightElement Mozilla SVGFEPointLightElement documentation>
newtype SVGFEPointLightElement = SVGFEPointLightElement { unSVGFEPointLightElement :: JSVal }

instance PToJSVal SVGFEPointLightElement where
  pToJSVal = unSVGFEPointLightElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFEPointLightElement where
  pFromJSVal = SVGFEPointLightElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFEPointLightElement where
  toJSVal = return . unSVGFEPointLightElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFEPointLightElement where
  fromJSVal v = fmap SVGFEPointLightElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFEPointLightElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFEPointLightElement where
  makeObject = makeObject . unSVGFEPointLightElement

instance IsSVGElement SVGFEPointLightElement
instance IsElement SVGFEPointLightElement
instance IsNode SVGFEPointLightElement
instance IsEventTarget SVGFEPointLightElement
instance IsSlotable SVGFEPointLightElement
instance IsParentNode SVGFEPointLightElement
instance IsNonDocumentTypeChildNode SVGFEPointLightElement
instance IsDocumentAndElementEventHandlers SVGFEPointLightElement
instance IsChildNode SVGFEPointLightElement
instance IsAnimatable SVGFEPointLightElement
instance IsGlobalEventHandlers SVGFEPointLightElement
instance IsElementCSSInlineStyle SVGFEPointLightElement
instance IsGObject SVGFEPointLightElement where
  typeGType _ = gTypeSVGFEPointLightElement
  {-# INLINE typeGType #-}

noSVGFEPointLightElement :: Maybe SVGFEPointLightElement
noSVGFEPointLightElement = Nothing
{-# INLINE noSVGFEPointLightElement #-}

gTypeSVGFEPointLightElement :: JSM GType
gTypeSVGFEPointLightElement = GType . Object <$> jsg "SVGFEPointLightElement"

-- | Functions for this inteface are in "JSDOM.SVGFESpecularLightingElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpecularLightingElement Mozilla SVGFESpecularLightingElement documentation>
newtype SVGFESpecularLightingElement = SVGFESpecularLightingElement { unSVGFESpecularLightingElement :: JSVal }

instance PToJSVal SVGFESpecularLightingElement where
  pToJSVal = unSVGFESpecularLightingElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFESpecularLightingElement where
  pFromJSVal = SVGFESpecularLightingElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFESpecularLightingElement where
  toJSVal = return . unSVGFESpecularLightingElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFESpecularLightingElement where
  fromJSVal v = fmap SVGFESpecularLightingElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFESpecularLightingElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFESpecularLightingElement where
  makeObject = makeObject . unSVGFESpecularLightingElement

instance IsSVGElement SVGFESpecularLightingElement
instance IsElement SVGFESpecularLightingElement
instance IsNode SVGFESpecularLightingElement
instance IsEventTarget SVGFESpecularLightingElement
instance IsSlotable SVGFESpecularLightingElement
instance IsParentNode SVGFESpecularLightingElement
instance IsNonDocumentTypeChildNode SVGFESpecularLightingElement
instance IsDocumentAndElementEventHandlers SVGFESpecularLightingElement
instance IsChildNode SVGFESpecularLightingElement
instance IsAnimatable SVGFESpecularLightingElement
instance IsGlobalEventHandlers SVGFESpecularLightingElement
instance IsElementCSSInlineStyle SVGFESpecularLightingElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFESpecularLightingElement
instance IsGObject SVGFESpecularLightingElement where
  typeGType _ = gTypeSVGFESpecularLightingElement
  {-# INLINE typeGType #-}

noSVGFESpecularLightingElement :: Maybe SVGFESpecularLightingElement
noSVGFESpecularLightingElement = Nothing
{-# INLINE noSVGFESpecularLightingElement #-}

gTypeSVGFESpecularLightingElement :: JSM GType
gTypeSVGFESpecularLightingElement = GType . Object <$> jsg "SVGFESpecularLightingElement"

-- | Functions for this inteface are in "JSDOM.SVGFESpotLightElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFESpotLightElement Mozilla SVGFESpotLightElement documentation>
newtype SVGFESpotLightElement = SVGFESpotLightElement { unSVGFESpotLightElement :: JSVal }

instance PToJSVal SVGFESpotLightElement where
  pToJSVal = unSVGFESpotLightElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFESpotLightElement where
  pFromJSVal = SVGFESpotLightElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFESpotLightElement where
  toJSVal = return . unSVGFESpotLightElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFESpotLightElement where
  fromJSVal v = fmap SVGFESpotLightElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFESpotLightElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFESpotLightElement where
  makeObject = makeObject . unSVGFESpotLightElement

instance IsSVGElement SVGFESpotLightElement
instance IsElement SVGFESpotLightElement
instance IsNode SVGFESpotLightElement
instance IsEventTarget SVGFESpotLightElement
instance IsSlotable SVGFESpotLightElement
instance IsParentNode SVGFESpotLightElement
instance IsNonDocumentTypeChildNode SVGFESpotLightElement
instance IsDocumentAndElementEventHandlers SVGFESpotLightElement
instance IsChildNode SVGFESpotLightElement
instance IsAnimatable SVGFESpotLightElement
instance IsGlobalEventHandlers SVGFESpotLightElement
instance IsElementCSSInlineStyle SVGFESpotLightElement
instance IsGObject SVGFESpotLightElement where
  typeGType _ = gTypeSVGFESpotLightElement
  {-# INLINE typeGType #-}

noSVGFESpotLightElement :: Maybe SVGFESpotLightElement
noSVGFESpotLightElement = Nothing
{-# INLINE noSVGFESpotLightElement #-}

gTypeSVGFESpotLightElement :: JSM GType
gTypeSVGFESpotLightElement = GType . Object <$> jsg "SVGFESpotLightElement"

-- | Functions for this inteface are in "JSDOM.SVGFETileElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETileElement Mozilla SVGFETileElement documentation>
newtype SVGFETileElement = SVGFETileElement { unSVGFETileElement :: JSVal }

instance PToJSVal SVGFETileElement where
  pToJSVal = unSVGFETileElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFETileElement where
  pFromJSVal = SVGFETileElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFETileElement where
  toJSVal = return . unSVGFETileElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFETileElement where
  fromJSVal v = fmap SVGFETileElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFETileElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFETileElement where
  makeObject = makeObject . unSVGFETileElement

instance IsSVGElement SVGFETileElement
instance IsElement SVGFETileElement
instance IsNode SVGFETileElement
instance IsEventTarget SVGFETileElement
instance IsSlotable SVGFETileElement
instance IsParentNode SVGFETileElement
instance IsNonDocumentTypeChildNode SVGFETileElement
instance IsDocumentAndElementEventHandlers SVGFETileElement
instance IsChildNode SVGFETileElement
instance IsAnimatable SVGFETileElement
instance IsGlobalEventHandlers SVGFETileElement
instance IsElementCSSInlineStyle SVGFETileElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFETileElement
instance IsGObject SVGFETileElement where
  typeGType _ = gTypeSVGFETileElement
  {-# INLINE typeGType #-}

noSVGFETileElement :: Maybe SVGFETileElement
noSVGFETileElement = Nothing
{-# INLINE noSVGFETileElement #-}

gTypeSVGFETileElement :: JSM GType
gTypeSVGFETileElement = GType . Object <$> jsg "SVGFETileElement"

-- | Functions for this inteface are in "JSDOM.SVGFETurbulenceElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFilterPrimitiveStandardAttributes"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFETurbulenceElement Mozilla SVGFETurbulenceElement documentation>
newtype SVGFETurbulenceElement = SVGFETurbulenceElement { unSVGFETurbulenceElement :: JSVal }

instance PToJSVal SVGFETurbulenceElement where
  pToJSVal = unSVGFETurbulenceElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFETurbulenceElement where
  pFromJSVal = SVGFETurbulenceElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFETurbulenceElement where
  toJSVal = return . unSVGFETurbulenceElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFETurbulenceElement where
  fromJSVal v = fmap SVGFETurbulenceElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFETurbulenceElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFETurbulenceElement where
  makeObject = makeObject . unSVGFETurbulenceElement

instance IsSVGElement SVGFETurbulenceElement
instance IsElement SVGFETurbulenceElement
instance IsNode SVGFETurbulenceElement
instance IsEventTarget SVGFETurbulenceElement
instance IsSlotable SVGFETurbulenceElement
instance IsParentNode SVGFETurbulenceElement
instance IsNonDocumentTypeChildNode SVGFETurbulenceElement
instance IsDocumentAndElementEventHandlers SVGFETurbulenceElement
instance IsChildNode SVGFETurbulenceElement
instance IsAnimatable SVGFETurbulenceElement
instance IsGlobalEventHandlers SVGFETurbulenceElement
instance IsElementCSSInlineStyle SVGFETurbulenceElement
instance IsSVGFilterPrimitiveStandardAttributes SVGFETurbulenceElement
instance IsGObject SVGFETurbulenceElement where
  typeGType _ = gTypeSVGFETurbulenceElement
  {-# INLINE typeGType #-}

noSVGFETurbulenceElement :: Maybe SVGFETurbulenceElement
noSVGFETurbulenceElement = Nothing
{-# INLINE noSVGFETurbulenceElement #-}

gTypeSVGFETurbulenceElement :: JSM GType
gTypeSVGFETurbulenceElement = GType . Object <$> jsg "SVGFETurbulenceElement"

-- | Functions for this inteface are in "JSDOM.SVGFilterElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterElement Mozilla SVGFilterElement documentation>
newtype SVGFilterElement = SVGFilterElement { unSVGFilterElement :: JSVal }

instance PToJSVal SVGFilterElement where
  pToJSVal = unSVGFilterElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFilterElement where
  pFromJSVal = SVGFilterElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFilterElement where
  toJSVal = return . unSVGFilterElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFilterElement where
  fromJSVal v = fmap SVGFilterElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFilterElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFilterElement where
  makeObject = makeObject . unSVGFilterElement

instance IsSVGElement SVGFilterElement
instance IsElement SVGFilterElement
instance IsNode SVGFilterElement
instance IsEventTarget SVGFilterElement
instance IsSlotable SVGFilterElement
instance IsParentNode SVGFilterElement
instance IsNonDocumentTypeChildNode SVGFilterElement
instance IsDocumentAndElementEventHandlers SVGFilterElement
instance IsChildNode SVGFilterElement
instance IsAnimatable SVGFilterElement
instance IsGlobalEventHandlers SVGFilterElement
instance IsElementCSSInlineStyle SVGFilterElement
instance IsSVGURIReference SVGFilterElement
instance IsSVGExternalResourcesRequired SVGFilterElement
instance IsGObject SVGFilterElement where
  typeGType _ = gTypeSVGFilterElement
  {-# INLINE typeGType #-}

noSVGFilterElement :: Maybe SVGFilterElement
noSVGFilterElement = Nothing
{-# INLINE noSVGFilterElement #-}

gTypeSVGFilterElement :: JSM GType
gTypeSVGFilterElement = GType . Object <$> jsg "SVGFilterElement"

-- | Functions for this inteface are in "JSDOM.SVGFilterPrimitiveStandardAttributes".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFilterPrimitiveStandardAttributes Mozilla SVGFilterPrimitiveStandardAttributes documentation>
newtype SVGFilterPrimitiveStandardAttributes = SVGFilterPrimitiveStandardAttributes { unSVGFilterPrimitiveStandardAttributes :: JSVal }

instance PToJSVal SVGFilterPrimitiveStandardAttributes where
  pToJSVal = unSVGFilterPrimitiveStandardAttributes
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFilterPrimitiveStandardAttributes where
  pFromJSVal = SVGFilterPrimitiveStandardAttributes
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFilterPrimitiveStandardAttributes where
  toJSVal = return . unSVGFilterPrimitiveStandardAttributes
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFilterPrimitiveStandardAttributes where
  fromJSVal v = fmap SVGFilterPrimitiveStandardAttributes <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFilterPrimitiveStandardAttributes
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFilterPrimitiveStandardAttributes where
  makeObject = makeObject . unSVGFilterPrimitiveStandardAttributes

class (IsGObject o) => IsSVGFilterPrimitiveStandardAttributes o
toSVGFilterPrimitiveStandardAttributes :: IsSVGFilterPrimitiveStandardAttributes o => o -> SVGFilterPrimitiveStandardAttributes
toSVGFilterPrimitiveStandardAttributes = SVGFilterPrimitiveStandardAttributes . coerce

instance IsSVGFilterPrimitiveStandardAttributes SVGFilterPrimitiveStandardAttributes
instance IsGObject SVGFilterPrimitiveStandardAttributes where
  typeGType _ = gTypeSVGFilterPrimitiveStandardAttributes
  {-# INLINE typeGType #-}

noSVGFilterPrimitiveStandardAttributes :: Maybe SVGFilterPrimitiveStandardAttributes
noSVGFilterPrimitiveStandardAttributes = Nothing
{-# INLINE noSVGFilterPrimitiveStandardAttributes #-}

gTypeSVGFilterPrimitiveStandardAttributes :: JSM GType
gTypeSVGFilterPrimitiveStandardAttributes = GType . Object <$> jsg "SVGFilterPrimitiveStandardAttributes"

-- | Functions for this inteface are in "JSDOM.SVGFitToViewBox".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFitToViewBox Mozilla SVGFitToViewBox documentation>
newtype SVGFitToViewBox = SVGFitToViewBox { unSVGFitToViewBox :: JSVal }

instance PToJSVal SVGFitToViewBox where
  pToJSVal = unSVGFitToViewBox
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFitToViewBox where
  pFromJSVal = SVGFitToViewBox
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFitToViewBox where
  toJSVal = return . unSVGFitToViewBox
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFitToViewBox where
  fromJSVal v = fmap SVGFitToViewBox <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFitToViewBox
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFitToViewBox where
  makeObject = makeObject . unSVGFitToViewBox

class (IsGObject o) => IsSVGFitToViewBox o
toSVGFitToViewBox :: IsSVGFitToViewBox o => o -> SVGFitToViewBox
toSVGFitToViewBox = SVGFitToViewBox . coerce

instance IsSVGFitToViewBox SVGFitToViewBox
instance IsGObject SVGFitToViewBox where
  typeGType _ = gTypeSVGFitToViewBox
  {-# INLINE typeGType #-}

noSVGFitToViewBox :: Maybe SVGFitToViewBox
noSVGFitToViewBox = Nothing
{-# INLINE noSVGFitToViewBox #-}

gTypeSVGFitToViewBox :: JSM GType
gTypeSVGFitToViewBox = GType . Object <$> jsg "SVGFitToViewBox"

-- | Functions for this inteface are in "JSDOM.SVGFontElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontElement Mozilla SVGFontElement documentation>
newtype SVGFontElement = SVGFontElement { unSVGFontElement :: JSVal }

instance PToJSVal SVGFontElement where
  pToJSVal = unSVGFontElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontElement where
  pFromJSVal = SVGFontElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontElement where
  toJSVal = return . unSVGFontElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontElement where
  fromJSVal v = fmap SVGFontElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontElement where
  makeObject = makeObject . unSVGFontElement

instance IsSVGElement SVGFontElement
instance IsElement SVGFontElement
instance IsNode SVGFontElement
instance IsEventTarget SVGFontElement
instance IsSlotable SVGFontElement
instance IsParentNode SVGFontElement
instance IsNonDocumentTypeChildNode SVGFontElement
instance IsDocumentAndElementEventHandlers SVGFontElement
instance IsChildNode SVGFontElement
instance IsAnimatable SVGFontElement
instance IsGlobalEventHandlers SVGFontElement
instance IsElementCSSInlineStyle SVGFontElement
instance IsGObject SVGFontElement where
  typeGType _ = gTypeSVGFontElement
  {-# INLINE typeGType #-}

noSVGFontElement :: Maybe SVGFontElement
noSVGFontElement = Nothing
{-# INLINE noSVGFontElement #-}

gTypeSVGFontElement :: JSM GType
gTypeSVGFontElement = GType . Object <$> jsg "SVGFontElement"

-- | Functions for this inteface are in "JSDOM.SVGFontFaceElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontFaceElement Mozilla SVGFontFaceElement documentation>
newtype SVGFontFaceElement = SVGFontFaceElement { unSVGFontFaceElement :: JSVal }

instance PToJSVal SVGFontFaceElement where
  pToJSVal = unSVGFontFaceElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontFaceElement where
  pFromJSVal = SVGFontFaceElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontFaceElement where
  toJSVal = return . unSVGFontFaceElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontFaceElement where
  fromJSVal v = fmap SVGFontFaceElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontFaceElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontFaceElement where
  makeObject = makeObject . unSVGFontFaceElement

instance IsSVGElement SVGFontFaceElement
instance IsElement SVGFontFaceElement
instance IsNode SVGFontFaceElement
instance IsEventTarget SVGFontFaceElement
instance IsSlotable SVGFontFaceElement
instance IsParentNode SVGFontFaceElement
instance IsNonDocumentTypeChildNode SVGFontFaceElement
instance IsDocumentAndElementEventHandlers SVGFontFaceElement
instance IsChildNode SVGFontFaceElement
instance IsAnimatable SVGFontFaceElement
instance IsGlobalEventHandlers SVGFontFaceElement
instance IsElementCSSInlineStyle SVGFontFaceElement
instance IsGObject SVGFontFaceElement where
  typeGType _ = gTypeSVGFontFaceElement
  {-# INLINE typeGType #-}

noSVGFontFaceElement :: Maybe SVGFontFaceElement
noSVGFontFaceElement = Nothing
{-# INLINE noSVGFontFaceElement #-}

gTypeSVGFontFaceElement :: JSM GType
gTypeSVGFontFaceElement = GType . Object <$> jsg "SVGFontFaceElement"

-- | Functions for this inteface are in "JSDOM.SVGFontFaceFormatElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontFaceFormatElement Mozilla SVGFontFaceFormatElement documentation>
newtype SVGFontFaceFormatElement = SVGFontFaceFormatElement { unSVGFontFaceFormatElement :: JSVal }

instance PToJSVal SVGFontFaceFormatElement where
  pToJSVal = unSVGFontFaceFormatElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontFaceFormatElement where
  pFromJSVal = SVGFontFaceFormatElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontFaceFormatElement where
  toJSVal = return . unSVGFontFaceFormatElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontFaceFormatElement where
  fromJSVal v = fmap SVGFontFaceFormatElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontFaceFormatElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontFaceFormatElement where
  makeObject = makeObject . unSVGFontFaceFormatElement

instance IsSVGElement SVGFontFaceFormatElement
instance IsElement SVGFontFaceFormatElement
instance IsNode SVGFontFaceFormatElement
instance IsEventTarget SVGFontFaceFormatElement
instance IsSlotable SVGFontFaceFormatElement
instance IsParentNode SVGFontFaceFormatElement
instance IsNonDocumentTypeChildNode SVGFontFaceFormatElement
instance IsDocumentAndElementEventHandlers SVGFontFaceFormatElement
instance IsChildNode SVGFontFaceFormatElement
instance IsAnimatable SVGFontFaceFormatElement
instance IsGlobalEventHandlers SVGFontFaceFormatElement
instance IsElementCSSInlineStyle SVGFontFaceFormatElement
instance IsGObject SVGFontFaceFormatElement where
  typeGType _ = gTypeSVGFontFaceFormatElement
  {-# INLINE typeGType #-}

noSVGFontFaceFormatElement :: Maybe SVGFontFaceFormatElement
noSVGFontFaceFormatElement = Nothing
{-# INLINE noSVGFontFaceFormatElement #-}

gTypeSVGFontFaceFormatElement :: JSM GType
gTypeSVGFontFaceFormatElement = GType . Object <$> jsg "SVGFontFaceFormatElement"

-- | Functions for this inteface are in "JSDOM.SVGFontFaceNameElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontFaceNameElement Mozilla SVGFontFaceNameElement documentation>
newtype SVGFontFaceNameElement = SVGFontFaceNameElement { unSVGFontFaceNameElement :: JSVal }

instance PToJSVal SVGFontFaceNameElement where
  pToJSVal = unSVGFontFaceNameElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontFaceNameElement where
  pFromJSVal = SVGFontFaceNameElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontFaceNameElement where
  toJSVal = return . unSVGFontFaceNameElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontFaceNameElement where
  fromJSVal v = fmap SVGFontFaceNameElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontFaceNameElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontFaceNameElement where
  makeObject = makeObject . unSVGFontFaceNameElement

instance IsSVGElement SVGFontFaceNameElement
instance IsElement SVGFontFaceNameElement
instance IsNode SVGFontFaceNameElement
instance IsEventTarget SVGFontFaceNameElement
instance IsSlotable SVGFontFaceNameElement
instance IsParentNode SVGFontFaceNameElement
instance IsNonDocumentTypeChildNode SVGFontFaceNameElement
instance IsDocumentAndElementEventHandlers SVGFontFaceNameElement
instance IsChildNode SVGFontFaceNameElement
instance IsAnimatable SVGFontFaceNameElement
instance IsGlobalEventHandlers SVGFontFaceNameElement
instance IsElementCSSInlineStyle SVGFontFaceNameElement
instance IsGObject SVGFontFaceNameElement where
  typeGType _ = gTypeSVGFontFaceNameElement
  {-# INLINE typeGType #-}

noSVGFontFaceNameElement :: Maybe SVGFontFaceNameElement
noSVGFontFaceNameElement = Nothing
{-# INLINE noSVGFontFaceNameElement #-}

gTypeSVGFontFaceNameElement :: JSM GType
gTypeSVGFontFaceNameElement = GType . Object <$> jsg "SVGFontFaceNameElement"

-- | Functions for this inteface are in "JSDOM.SVGFontFaceSrcElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontFaceSrcElement Mozilla SVGFontFaceSrcElement documentation>
newtype SVGFontFaceSrcElement = SVGFontFaceSrcElement { unSVGFontFaceSrcElement :: JSVal }

instance PToJSVal SVGFontFaceSrcElement where
  pToJSVal = unSVGFontFaceSrcElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontFaceSrcElement where
  pFromJSVal = SVGFontFaceSrcElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontFaceSrcElement where
  toJSVal = return . unSVGFontFaceSrcElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontFaceSrcElement where
  fromJSVal v = fmap SVGFontFaceSrcElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontFaceSrcElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontFaceSrcElement where
  makeObject = makeObject . unSVGFontFaceSrcElement

instance IsSVGElement SVGFontFaceSrcElement
instance IsElement SVGFontFaceSrcElement
instance IsNode SVGFontFaceSrcElement
instance IsEventTarget SVGFontFaceSrcElement
instance IsSlotable SVGFontFaceSrcElement
instance IsParentNode SVGFontFaceSrcElement
instance IsNonDocumentTypeChildNode SVGFontFaceSrcElement
instance IsDocumentAndElementEventHandlers SVGFontFaceSrcElement
instance IsChildNode SVGFontFaceSrcElement
instance IsAnimatable SVGFontFaceSrcElement
instance IsGlobalEventHandlers SVGFontFaceSrcElement
instance IsElementCSSInlineStyle SVGFontFaceSrcElement
instance IsGObject SVGFontFaceSrcElement where
  typeGType _ = gTypeSVGFontFaceSrcElement
  {-# INLINE typeGType #-}

noSVGFontFaceSrcElement :: Maybe SVGFontFaceSrcElement
noSVGFontFaceSrcElement = Nothing
{-# INLINE noSVGFontFaceSrcElement #-}

gTypeSVGFontFaceSrcElement :: JSM GType
gTypeSVGFontFaceSrcElement = GType . Object <$> jsg "SVGFontFaceSrcElement"

-- | Functions for this inteface are in "JSDOM.SVGFontFaceUriElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGFontFaceUriElement Mozilla SVGFontFaceUriElement documentation>
newtype SVGFontFaceUriElement = SVGFontFaceUriElement { unSVGFontFaceUriElement :: JSVal }

instance PToJSVal SVGFontFaceUriElement where
  pToJSVal = unSVGFontFaceUriElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGFontFaceUriElement where
  pFromJSVal = SVGFontFaceUriElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGFontFaceUriElement where
  toJSVal = return . unSVGFontFaceUriElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGFontFaceUriElement where
  fromJSVal v = fmap SVGFontFaceUriElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGFontFaceUriElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGFontFaceUriElement where
  makeObject = makeObject . unSVGFontFaceUriElement

instance IsSVGElement SVGFontFaceUriElement
instance IsElement SVGFontFaceUriElement
instance IsNode SVGFontFaceUriElement
instance IsEventTarget SVGFontFaceUriElement
instance IsSlotable SVGFontFaceUriElement
instance IsParentNode SVGFontFaceUriElement
instance IsNonDocumentTypeChildNode SVGFontFaceUriElement
instance IsDocumentAndElementEventHandlers SVGFontFaceUriElement
instance IsChildNode SVGFontFaceUriElement
instance IsAnimatable SVGFontFaceUriElement
instance IsGlobalEventHandlers SVGFontFaceUriElement
instance IsElementCSSInlineStyle SVGFontFaceUriElement
instance IsGObject SVGFontFaceUriElement where
  typeGType _ = gTypeSVGFontFaceUriElement
  {-# INLINE typeGType #-}

noSVGFontFaceUriElement :: Maybe SVGFontFaceUriElement
noSVGFontFaceUriElement = Nothing
{-# INLINE noSVGFontFaceUriElement #-}

gTypeSVGFontFaceUriElement :: JSM GType
gTypeSVGFontFaceUriElement = GType . Object <$> jsg "SVGFontFaceUriElement"

-- | Functions for this inteface are in "JSDOM.SVGForeignObjectElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGForeignObjectElement Mozilla SVGForeignObjectElement documentation>
newtype SVGForeignObjectElement = SVGForeignObjectElement { unSVGForeignObjectElement :: JSVal }

instance PToJSVal SVGForeignObjectElement where
  pToJSVal = unSVGForeignObjectElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGForeignObjectElement where
  pFromJSVal = SVGForeignObjectElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGForeignObjectElement where
  toJSVal = return . unSVGForeignObjectElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGForeignObjectElement where
  fromJSVal v = fmap SVGForeignObjectElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGForeignObjectElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGForeignObjectElement where
  makeObject = makeObject . unSVGForeignObjectElement

instance IsSVGGraphicsElement SVGForeignObjectElement
instance IsSVGElement SVGForeignObjectElement
instance IsElement SVGForeignObjectElement
instance IsNode SVGForeignObjectElement
instance IsEventTarget SVGForeignObjectElement
instance IsSlotable SVGForeignObjectElement
instance IsParentNode SVGForeignObjectElement
instance IsNonDocumentTypeChildNode SVGForeignObjectElement
instance IsDocumentAndElementEventHandlers SVGForeignObjectElement
instance IsChildNode SVGForeignObjectElement
instance IsAnimatable SVGForeignObjectElement
instance IsGlobalEventHandlers SVGForeignObjectElement
instance IsElementCSSInlineStyle SVGForeignObjectElement
instance IsSVGTests SVGForeignObjectElement
instance IsSVGExternalResourcesRequired SVGForeignObjectElement
instance IsGObject SVGForeignObjectElement where
  typeGType _ = gTypeSVGForeignObjectElement
  {-# INLINE typeGType #-}

noSVGForeignObjectElement :: Maybe SVGForeignObjectElement
noSVGForeignObjectElement = Nothing
{-# INLINE noSVGForeignObjectElement #-}

gTypeSVGForeignObjectElement :: JSM GType
gTypeSVGForeignObjectElement = GType . Object <$> jsg "SVGForeignObjectElement"

-- | Functions for this inteface are in "JSDOM.SVGGElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGGElement Mozilla SVGGElement documentation>
newtype SVGGElement = SVGGElement { unSVGGElement :: JSVal }

instance PToJSVal SVGGElement where
  pToJSVal = unSVGGElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGGElement where
  pFromJSVal = SVGGElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGGElement where
  toJSVal = return . unSVGGElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGGElement where
  fromJSVal v = fmap SVGGElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGGElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGGElement where
  makeObject = makeObject . unSVGGElement

instance IsSVGGraphicsElement SVGGElement
instance IsSVGElement SVGGElement
instance IsElement SVGGElement
instance IsNode SVGGElement
instance IsEventTarget SVGGElement
instance IsSlotable SVGGElement
instance IsParentNode SVGGElement
instance IsNonDocumentTypeChildNode SVGGElement
instance IsDocumentAndElementEventHandlers SVGGElement
instance IsChildNode SVGGElement
instance IsAnimatable SVGGElement
instance IsGlobalEventHandlers SVGGElement
instance IsElementCSSInlineStyle SVGGElement
instance IsSVGTests SVGGElement
instance IsSVGExternalResourcesRequired SVGGElement
instance IsGObject SVGGElement where
  typeGType _ = gTypeSVGGElement
  {-# INLINE typeGType #-}

noSVGGElement :: Maybe SVGGElement
noSVGGElement = Nothing
{-# INLINE noSVGGElement #-}

gTypeSVGGElement :: JSM GType
gTypeSVGGElement = GType . Object <$> jsg "SVGGElement"

-- | Functions for this inteface are in "JSDOM.SVGGlyphElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGGlyphElement Mozilla SVGGlyphElement documentation>
newtype SVGGlyphElement = SVGGlyphElement { unSVGGlyphElement :: JSVal }

instance PToJSVal SVGGlyphElement where
  pToJSVal = unSVGGlyphElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGGlyphElement where
  pFromJSVal = SVGGlyphElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGGlyphElement where
  toJSVal = return . unSVGGlyphElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGGlyphElement where
  fromJSVal v = fmap SVGGlyphElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGGlyphElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGGlyphElement where
  makeObject = makeObject . unSVGGlyphElement

instance IsSVGElement SVGGlyphElement
instance IsElement SVGGlyphElement
instance IsNode SVGGlyphElement
instance IsEventTarget SVGGlyphElement
instance IsSlotable SVGGlyphElement
instance IsParentNode SVGGlyphElement
instance IsNonDocumentTypeChildNode SVGGlyphElement
instance IsDocumentAndElementEventHandlers SVGGlyphElement
instance IsChildNode SVGGlyphElement
instance IsAnimatable SVGGlyphElement
instance IsGlobalEventHandlers SVGGlyphElement
instance IsElementCSSInlineStyle SVGGlyphElement
instance IsGObject SVGGlyphElement where
  typeGType _ = gTypeSVGGlyphElement
  {-# INLINE typeGType #-}

noSVGGlyphElement :: Maybe SVGGlyphElement
noSVGGlyphElement = Nothing
{-# INLINE noSVGGlyphElement #-}

gTypeSVGGlyphElement :: JSM GType
gTypeSVGGlyphElement = GType . Object <$> jsg "SVGGlyphElement"

-- | Functions for this inteface are in "JSDOM.SVGGlyphRefElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGGlyphRefElement Mozilla SVGGlyphRefElement documentation>
newtype SVGGlyphRefElement = SVGGlyphRefElement { unSVGGlyphRefElement :: JSVal }

instance PToJSVal SVGGlyphRefElement where
  pToJSVal = unSVGGlyphRefElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGGlyphRefElement where
  pFromJSVal = SVGGlyphRefElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGGlyphRefElement where
  toJSVal = return . unSVGGlyphRefElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGGlyphRefElement where
  fromJSVal v = fmap SVGGlyphRefElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGGlyphRefElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGGlyphRefElement where
  makeObject = makeObject . unSVGGlyphRefElement

instance IsSVGElement SVGGlyphRefElement
instance IsElement SVGGlyphRefElement
instance IsNode SVGGlyphRefElement
instance IsEventTarget SVGGlyphRefElement
instance IsSlotable SVGGlyphRefElement
instance IsParentNode SVGGlyphRefElement
instance IsNonDocumentTypeChildNode SVGGlyphRefElement
instance IsDocumentAndElementEventHandlers SVGGlyphRefElement
instance IsChildNode SVGGlyphRefElement
instance IsAnimatable SVGGlyphRefElement
instance IsGlobalEventHandlers SVGGlyphRefElement
instance IsElementCSSInlineStyle SVGGlyphRefElement
instance IsSVGURIReference SVGGlyphRefElement
instance IsGObject SVGGlyphRefElement where
  typeGType _ = gTypeSVGGlyphRefElement
  {-# INLINE typeGType #-}

noSVGGlyphRefElement :: Maybe SVGGlyphRefElement
noSVGGlyphRefElement = Nothing
{-# INLINE noSVGGlyphRefElement #-}

gTypeSVGGlyphRefElement :: JSM GType
gTypeSVGGlyphRefElement = GType . Object <$> jsg "SVGGlyphRefElement"

-- | Functions for this inteface are in "JSDOM.SVGGradientElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGGradientElement Mozilla SVGGradientElement documentation>
newtype SVGGradientElement = SVGGradientElement { unSVGGradientElement :: JSVal }

instance PToJSVal SVGGradientElement where
  pToJSVal = unSVGGradientElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGGradientElement where
  pFromJSVal = SVGGradientElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGGradientElement where
  toJSVal = return . unSVGGradientElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGGradientElement where
  fromJSVal v = fmap SVGGradientElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGGradientElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGGradientElement where
  makeObject = makeObject . unSVGGradientElement

class (IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsSVGURIReference o, IsSVGExternalResourcesRequired o, IsGObject o) => IsSVGGradientElement o
toSVGGradientElement :: IsSVGGradientElement o => o -> SVGGradientElement
toSVGGradientElement = SVGGradientElement . coerce

instance IsSVGGradientElement SVGGradientElement
instance IsSVGElement SVGGradientElement
instance IsElement SVGGradientElement
instance IsNode SVGGradientElement
instance IsEventTarget SVGGradientElement
instance IsSlotable SVGGradientElement
instance IsParentNode SVGGradientElement
instance IsNonDocumentTypeChildNode SVGGradientElement
instance IsDocumentAndElementEventHandlers SVGGradientElement
instance IsChildNode SVGGradientElement
instance IsAnimatable SVGGradientElement
instance IsGlobalEventHandlers SVGGradientElement
instance IsElementCSSInlineStyle SVGGradientElement
instance IsSVGURIReference SVGGradientElement
instance IsSVGExternalResourcesRequired SVGGradientElement
instance IsGObject SVGGradientElement where
  typeGType _ = gTypeSVGGradientElement
  {-# INLINE typeGType #-}

noSVGGradientElement :: Maybe SVGGradientElement
noSVGGradientElement = Nothing
{-# INLINE noSVGGradientElement #-}

gTypeSVGGradientElement :: JSM GType
gTypeSVGGradientElement = GType . Object <$> jsg "SVGGradientElement"

-- | Functions for this inteface are in "JSDOM.SVGGraphicsElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGGraphicsElement Mozilla SVGGraphicsElement documentation>
newtype SVGGraphicsElement = SVGGraphicsElement { unSVGGraphicsElement :: JSVal }

instance PToJSVal SVGGraphicsElement where
  pToJSVal = unSVGGraphicsElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGGraphicsElement where
  pFromJSVal = SVGGraphicsElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGGraphicsElement where
  toJSVal = return . unSVGGraphicsElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGGraphicsElement where
  fromJSVal v = fmap SVGGraphicsElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGGraphicsElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGGraphicsElement where
  makeObject = makeObject . unSVGGraphicsElement

class (IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsSVGTests o, IsGObject o) => IsSVGGraphicsElement o
toSVGGraphicsElement :: IsSVGGraphicsElement o => o -> SVGGraphicsElement
toSVGGraphicsElement = SVGGraphicsElement . coerce

instance IsSVGGraphicsElement SVGGraphicsElement
instance IsSVGElement SVGGraphicsElement
instance IsElement SVGGraphicsElement
instance IsNode SVGGraphicsElement
instance IsEventTarget SVGGraphicsElement
instance IsSlotable SVGGraphicsElement
instance IsParentNode SVGGraphicsElement
instance IsNonDocumentTypeChildNode SVGGraphicsElement
instance IsDocumentAndElementEventHandlers SVGGraphicsElement
instance IsChildNode SVGGraphicsElement
instance IsAnimatable SVGGraphicsElement
instance IsGlobalEventHandlers SVGGraphicsElement
instance IsElementCSSInlineStyle SVGGraphicsElement
instance IsSVGTests SVGGraphicsElement
instance IsGObject SVGGraphicsElement where
  typeGType _ = gTypeSVGGraphicsElement
  {-# INLINE typeGType #-}

noSVGGraphicsElement :: Maybe SVGGraphicsElement
noSVGGraphicsElement = Nothing
{-# INLINE noSVGGraphicsElement #-}

gTypeSVGGraphicsElement :: JSM GType
gTypeSVGGraphicsElement = GType . Object <$> jsg "SVGGraphicsElement"

-- | Functions for this inteface are in "JSDOM.SVGHKernElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGHKernElement Mozilla SVGHKernElement documentation>
newtype SVGHKernElement = SVGHKernElement { unSVGHKernElement :: JSVal }

instance PToJSVal SVGHKernElement where
  pToJSVal = unSVGHKernElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGHKernElement where
  pFromJSVal = SVGHKernElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGHKernElement where
  toJSVal = return . unSVGHKernElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGHKernElement where
  fromJSVal v = fmap SVGHKernElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGHKernElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGHKernElement where
  makeObject = makeObject . unSVGHKernElement

instance IsSVGElement SVGHKernElement
instance IsElement SVGHKernElement
instance IsNode SVGHKernElement
instance IsEventTarget SVGHKernElement
instance IsSlotable SVGHKernElement
instance IsParentNode SVGHKernElement
instance IsNonDocumentTypeChildNode SVGHKernElement
instance IsDocumentAndElementEventHandlers SVGHKernElement
instance IsChildNode SVGHKernElement
instance IsAnimatable SVGHKernElement
instance IsGlobalEventHandlers SVGHKernElement
instance IsElementCSSInlineStyle SVGHKernElement
instance IsGObject SVGHKernElement where
  typeGType _ = gTypeSVGHKernElement
  {-# INLINE typeGType #-}

noSVGHKernElement :: Maybe SVGHKernElement
noSVGHKernElement = Nothing
{-# INLINE noSVGHKernElement #-}

gTypeSVGHKernElement :: JSM GType
gTypeSVGHKernElement = GType . Object <$> jsg "SVGHKernElement"

-- | Functions for this inteface are in "JSDOM.SVGImageElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGImageElement Mozilla SVGImageElement documentation>
newtype SVGImageElement = SVGImageElement { unSVGImageElement :: JSVal }

instance PToJSVal SVGImageElement where
  pToJSVal = unSVGImageElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGImageElement where
  pFromJSVal = SVGImageElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGImageElement where
  toJSVal = return . unSVGImageElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGImageElement where
  fromJSVal v = fmap SVGImageElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGImageElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGImageElement where
  makeObject = makeObject . unSVGImageElement

instance IsSVGGraphicsElement SVGImageElement
instance IsSVGElement SVGImageElement
instance IsElement SVGImageElement
instance IsNode SVGImageElement
instance IsEventTarget SVGImageElement
instance IsSlotable SVGImageElement
instance IsParentNode SVGImageElement
instance IsNonDocumentTypeChildNode SVGImageElement
instance IsDocumentAndElementEventHandlers SVGImageElement
instance IsChildNode SVGImageElement
instance IsAnimatable SVGImageElement
instance IsGlobalEventHandlers SVGImageElement
instance IsElementCSSInlineStyle SVGImageElement
instance IsSVGTests SVGImageElement
instance IsSVGURIReference SVGImageElement
instance IsSVGExternalResourcesRequired SVGImageElement
instance IsGObject SVGImageElement where
  typeGType _ = gTypeSVGImageElement
  {-# INLINE typeGType #-}

noSVGImageElement :: Maybe SVGImageElement
noSVGImageElement = Nothing
{-# INLINE noSVGImageElement #-}

gTypeSVGImageElement :: JSM GType
gTypeSVGImageElement = GType . Object <$> jsg "SVGImageElement"

-- | Functions for this inteface are in "JSDOM.SVGLength".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGLength Mozilla SVGLength documentation>
newtype SVGLength = SVGLength { unSVGLength :: JSVal }

instance PToJSVal SVGLength where
  pToJSVal = unSVGLength
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGLength where
  pFromJSVal = SVGLength
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGLength where
  toJSVal = return . unSVGLength
  {-# INLINE toJSVal #-}

instance FromJSVal SVGLength where
  fromJSVal v = fmap SVGLength <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGLength
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGLength where
  makeObject = makeObject . unSVGLength

instance IsGObject SVGLength where
  typeGType _ = gTypeSVGLength
  {-# INLINE typeGType #-}

noSVGLength :: Maybe SVGLength
noSVGLength = Nothing
{-# INLINE noSVGLength #-}

gTypeSVGLength :: JSM GType
gTypeSVGLength = GType . Object <$> jsg "SVGLength"

-- | Functions for this inteface are in "JSDOM.SVGLengthList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGLengthList Mozilla SVGLengthList documentation>
newtype SVGLengthList = SVGLengthList { unSVGLengthList :: JSVal }

instance PToJSVal SVGLengthList where
  pToJSVal = unSVGLengthList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGLengthList where
  pFromJSVal = SVGLengthList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGLengthList where
  toJSVal = return . unSVGLengthList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGLengthList where
  fromJSVal v = fmap SVGLengthList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGLengthList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGLengthList where
  makeObject = makeObject . unSVGLengthList

instance IsGObject SVGLengthList where
  typeGType _ = gTypeSVGLengthList
  {-# INLINE typeGType #-}

noSVGLengthList :: Maybe SVGLengthList
noSVGLengthList = Nothing
{-# INLINE noSVGLengthList #-}

gTypeSVGLengthList :: JSM GType
gTypeSVGLengthList = GType . Object <$> jsg "SVGLengthList"

-- | Functions for this inteface are in "JSDOM.SVGLineElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGLineElement Mozilla SVGLineElement documentation>
newtype SVGLineElement = SVGLineElement { unSVGLineElement :: JSVal }

instance PToJSVal SVGLineElement where
  pToJSVal = unSVGLineElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGLineElement where
  pFromJSVal = SVGLineElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGLineElement where
  toJSVal = return . unSVGLineElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGLineElement where
  fromJSVal v = fmap SVGLineElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGLineElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGLineElement where
  makeObject = makeObject . unSVGLineElement

instance IsSVGGraphicsElement SVGLineElement
instance IsSVGElement SVGLineElement
instance IsElement SVGLineElement
instance IsNode SVGLineElement
instance IsEventTarget SVGLineElement
instance IsSlotable SVGLineElement
instance IsParentNode SVGLineElement
instance IsNonDocumentTypeChildNode SVGLineElement
instance IsDocumentAndElementEventHandlers SVGLineElement
instance IsChildNode SVGLineElement
instance IsAnimatable SVGLineElement
instance IsGlobalEventHandlers SVGLineElement
instance IsElementCSSInlineStyle SVGLineElement
instance IsSVGTests SVGLineElement
instance IsSVGExternalResourcesRequired SVGLineElement
instance IsGObject SVGLineElement where
  typeGType _ = gTypeSVGLineElement
  {-# INLINE typeGType #-}

noSVGLineElement :: Maybe SVGLineElement
noSVGLineElement = Nothing
{-# INLINE noSVGLineElement #-}

gTypeSVGLineElement :: JSM GType
gTypeSVGLineElement = GType . Object <$> jsg "SVGLineElement"

-- | Functions for this inteface are in "JSDOM.SVGLinearGradientElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGradientElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGLinearGradientElement Mozilla SVGLinearGradientElement documentation>
newtype SVGLinearGradientElement = SVGLinearGradientElement { unSVGLinearGradientElement :: JSVal }

instance PToJSVal SVGLinearGradientElement where
  pToJSVal = unSVGLinearGradientElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGLinearGradientElement where
  pFromJSVal = SVGLinearGradientElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGLinearGradientElement where
  toJSVal = return . unSVGLinearGradientElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGLinearGradientElement where
  fromJSVal v = fmap SVGLinearGradientElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGLinearGradientElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGLinearGradientElement where
  makeObject = makeObject . unSVGLinearGradientElement

instance IsSVGGradientElement SVGLinearGradientElement
instance IsSVGElement SVGLinearGradientElement
instance IsElement SVGLinearGradientElement
instance IsNode SVGLinearGradientElement
instance IsEventTarget SVGLinearGradientElement
instance IsSlotable SVGLinearGradientElement
instance IsParentNode SVGLinearGradientElement
instance IsNonDocumentTypeChildNode SVGLinearGradientElement
instance IsDocumentAndElementEventHandlers SVGLinearGradientElement
instance IsChildNode SVGLinearGradientElement
instance IsAnimatable SVGLinearGradientElement
instance IsGlobalEventHandlers SVGLinearGradientElement
instance IsElementCSSInlineStyle SVGLinearGradientElement
instance IsSVGURIReference SVGLinearGradientElement
instance IsSVGExternalResourcesRequired SVGLinearGradientElement
instance IsGObject SVGLinearGradientElement where
  typeGType _ = gTypeSVGLinearGradientElement
  {-# INLINE typeGType #-}

noSVGLinearGradientElement :: Maybe SVGLinearGradientElement
noSVGLinearGradientElement = Nothing
{-# INLINE noSVGLinearGradientElement #-}

gTypeSVGLinearGradientElement :: JSM GType
gTypeSVGLinearGradientElement = GType . Object <$> jsg "SVGLinearGradientElement"

-- | Functions for this inteface are in "JSDOM.SVGMPathElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMPathElement Mozilla SVGMPathElement documentation>
newtype SVGMPathElement = SVGMPathElement { unSVGMPathElement :: JSVal }

instance PToJSVal SVGMPathElement where
  pToJSVal = unSVGMPathElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMPathElement where
  pFromJSVal = SVGMPathElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMPathElement where
  toJSVal = return . unSVGMPathElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMPathElement where
  fromJSVal v = fmap SVGMPathElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMPathElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMPathElement where
  makeObject = makeObject . unSVGMPathElement

instance IsSVGElement SVGMPathElement
instance IsElement SVGMPathElement
instance IsNode SVGMPathElement
instance IsEventTarget SVGMPathElement
instance IsSlotable SVGMPathElement
instance IsParentNode SVGMPathElement
instance IsNonDocumentTypeChildNode SVGMPathElement
instance IsDocumentAndElementEventHandlers SVGMPathElement
instance IsChildNode SVGMPathElement
instance IsAnimatable SVGMPathElement
instance IsGlobalEventHandlers SVGMPathElement
instance IsElementCSSInlineStyle SVGMPathElement
instance IsSVGURIReference SVGMPathElement
instance IsSVGExternalResourcesRequired SVGMPathElement
instance IsGObject SVGMPathElement where
  typeGType _ = gTypeSVGMPathElement
  {-# INLINE typeGType #-}

noSVGMPathElement :: Maybe SVGMPathElement
noSVGMPathElement = Nothing
{-# INLINE noSVGMPathElement #-}

gTypeSVGMPathElement :: JSM GType
gTypeSVGMPathElement = GType . Object <$> jsg "SVGMPathElement"

-- | Functions for this inteface are in "JSDOM.SVGMarkerElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFitToViewBox"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMarkerElement Mozilla SVGMarkerElement documentation>
newtype SVGMarkerElement = SVGMarkerElement { unSVGMarkerElement :: JSVal }

instance PToJSVal SVGMarkerElement where
  pToJSVal = unSVGMarkerElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMarkerElement where
  pFromJSVal = SVGMarkerElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMarkerElement where
  toJSVal = return . unSVGMarkerElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMarkerElement where
  fromJSVal v = fmap SVGMarkerElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMarkerElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMarkerElement where
  makeObject = makeObject . unSVGMarkerElement

instance IsSVGElement SVGMarkerElement
instance IsElement SVGMarkerElement
instance IsNode SVGMarkerElement
instance IsEventTarget SVGMarkerElement
instance IsSlotable SVGMarkerElement
instance IsParentNode SVGMarkerElement
instance IsNonDocumentTypeChildNode SVGMarkerElement
instance IsDocumentAndElementEventHandlers SVGMarkerElement
instance IsChildNode SVGMarkerElement
instance IsAnimatable SVGMarkerElement
instance IsGlobalEventHandlers SVGMarkerElement
instance IsElementCSSInlineStyle SVGMarkerElement
instance IsSVGFitToViewBox SVGMarkerElement
instance IsSVGExternalResourcesRequired SVGMarkerElement
instance IsGObject SVGMarkerElement where
  typeGType _ = gTypeSVGMarkerElement
  {-# INLINE typeGType #-}

noSVGMarkerElement :: Maybe SVGMarkerElement
noSVGMarkerElement = Nothing
{-# INLINE noSVGMarkerElement #-}

gTypeSVGMarkerElement :: JSM GType
gTypeSVGMarkerElement = GType . Object <$> jsg "SVGMarkerElement"

-- | Functions for this inteface are in "JSDOM.SVGMaskElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMaskElement Mozilla SVGMaskElement documentation>
newtype SVGMaskElement = SVGMaskElement { unSVGMaskElement :: JSVal }

instance PToJSVal SVGMaskElement where
  pToJSVal = unSVGMaskElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMaskElement where
  pFromJSVal = SVGMaskElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMaskElement where
  toJSVal = return . unSVGMaskElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMaskElement where
  fromJSVal v = fmap SVGMaskElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMaskElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMaskElement where
  makeObject = makeObject . unSVGMaskElement

instance IsSVGElement SVGMaskElement
instance IsElement SVGMaskElement
instance IsNode SVGMaskElement
instance IsEventTarget SVGMaskElement
instance IsSlotable SVGMaskElement
instance IsParentNode SVGMaskElement
instance IsNonDocumentTypeChildNode SVGMaskElement
instance IsDocumentAndElementEventHandlers SVGMaskElement
instance IsChildNode SVGMaskElement
instance IsAnimatable SVGMaskElement
instance IsGlobalEventHandlers SVGMaskElement
instance IsElementCSSInlineStyle SVGMaskElement
instance IsSVGTests SVGMaskElement
instance IsSVGExternalResourcesRequired SVGMaskElement
instance IsGObject SVGMaskElement where
  typeGType _ = gTypeSVGMaskElement
  {-# INLINE typeGType #-}

noSVGMaskElement :: Maybe SVGMaskElement
noSVGMaskElement = Nothing
{-# INLINE noSVGMaskElement #-}

gTypeSVGMaskElement :: JSM GType
gTypeSVGMaskElement = GType . Object <$> jsg "SVGMaskElement"

-- | Functions for this inteface are in "JSDOM.SVGMatrix".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMatrix Mozilla SVGMatrix documentation>
newtype SVGMatrix = SVGMatrix { unSVGMatrix :: JSVal }

instance PToJSVal SVGMatrix where
  pToJSVal = unSVGMatrix
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMatrix where
  pFromJSVal = SVGMatrix
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMatrix where
  toJSVal = return . unSVGMatrix
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMatrix where
  fromJSVal v = fmap SVGMatrix <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMatrix
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMatrix where
  makeObject = makeObject . unSVGMatrix

instance IsGObject SVGMatrix where
  typeGType _ = gTypeSVGMatrix
  {-# INLINE typeGType #-}

noSVGMatrix :: Maybe SVGMatrix
noSVGMatrix = Nothing
{-# INLINE noSVGMatrix #-}

gTypeSVGMatrix :: JSM GType
gTypeSVGMatrix = GType . Object <$> jsg "SVGMatrix"

-- | Functions for this inteface are in "JSDOM.SVGMetadataElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMetadataElement Mozilla SVGMetadataElement documentation>
newtype SVGMetadataElement = SVGMetadataElement { unSVGMetadataElement :: JSVal }

instance PToJSVal SVGMetadataElement where
  pToJSVal = unSVGMetadataElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMetadataElement where
  pFromJSVal = SVGMetadataElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMetadataElement where
  toJSVal = return . unSVGMetadataElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMetadataElement where
  fromJSVal v = fmap SVGMetadataElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMetadataElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMetadataElement where
  makeObject = makeObject . unSVGMetadataElement

instance IsSVGElement SVGMetadataElement
instance IsElement SVGMetadataElement
instance IsNode SVGMetadataElement
instance IsEventTarget SVGMetadataElement
instance IsSlotable SVGMetadataElement
instance IsParentNode SVGMetadataElement
instance IsNonDocumentTypeChildNode SVGMetadataElement
instance IsDocumentAndElementEventHandlers SVGMetadataElement
instance IsChildNode SVGMetadataElement
instance IsAnimatable SVGMetadataElement
instance IsGlobalEventHandlers SVGMetadataElement
instance IsElementCSSInlineStyle SVGMetadataElement
instance IsGObject SVGMetadataElement where
  typeGType _ = gTypeSVGMetadataElement
  {-# INLINE typeGType #-}

noSVGMetadataElement :: Maybe SVGMetadataElement
noSVGMetadataElement = Nothing
{-# INLINE noSVGMetadataElement #-}

gTypeSVGMetadataElement :: JSM GType
gTypeSVGMetadataElement = GType . Object <$> jsg "SVGMetadataElement"

-- | Functions for this inteface are in "JSDOM.SVGMissingGlyphElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGMissingGlyphElement Mozilla SVGMissingGlyphElement documentation>
newtype SVGMissingGlyphElement = SVGMissingGlyphElement { unSVGMissingGlyphElement :: JSVal }

instance PToJSVal SVGMissingGlyphElement where
  pToJSVal = unSVGMissingGlyphElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGMissingGlyphElement where
  pFromJSVal = SVGMissingGlyphElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGMissingGlyphElement where
  toJSVal = return . unSVGMissingGlyphElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGMissingGlyphElement where
  fromJSVal v = fmap SVGMissingGlyphElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGMissingGlyphElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGMissingGlyphElement where
  makeObject = makeObject . unSVGMissingGlyphElement

instance IsSVGElement SVGMissingGlyphElement
instance IsElement SVGMissingGlyphElement
instance IsNode SVGMissingGlyphElement
instance IsEventTarget SVGMissingGlyphElement
instance IsSlotable SVGMissingGlyphElement
instance IsParentNode SVGMissingGlyphElement
instance IsNonDocumentTypeChildNode SVGMissingGlyphElement
instance IsDocumentAndElementEventHandlers SVGMissingGlyphElement
instance IsChildNode SVGMissingGlyphElement
instance IsAnimatable SVGMissingGlyphElement
instance IsGlobalEventHandlers SVGMissingGlyphElement
instance IsElementCSSInlineStyle SVGMissingGlyphElement
instance IsGObject SVGMissingGlyphElement where
  typeGType _ = gTypeSVGMissingGlyphElement
  {-# INLINE typeGType #-}

noSVGMissingGlyphElement :: Maybe SVGMissingGlyphElement
noSVGMissingGlyphElement = Nothing
{-# INLINE noSVGMissingGlyphElement #-}

gTypeSVGMissingGlyphElement :: JSM GType
gTypeSVGMissingGlyphElement = GType . Object <$> jsg "SVGMissingGlyphElement"

-- | Functions for this inteface are in "JSDOM.SVGNumber".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGNumber Mozilla SVGNumber documentation>
newtype SVGNumber = SVGNumber { unSVGNumber :: JSVal }

instance PToJSVal SVGNumber where
  pToJSVal = unSVGNumber
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGNumber where
  pFromJSVal = SVGNumber
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGNumber where
  toJSVal = return . unSVGNumber
  {-# INLINE toJSVal #-}

instance FromJSVal SVGNumber where
  fromJSVal v = fmap SVGNumber <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGNumber
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGNumber where
  makeObject = makeObject . unSVGNumber

instance IsGObject SVGNumber where
  typeGType _ = gTypeSVGNumber
  {-# INLINE typeGType #-}

noSVGNumber :: Maybe SVGNumber
noSVGNumber = Nothing
{-# INLINE noSVGNumber #-}

gTypeSVGNumber :: JSM GType
gTypeSVGNumber = GType . Object <$> jsg "SVGNumber"

-- | Functions for this inteface are in "JSDOM.SVGNumberList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGNumberList Mozilla SVGNumberList documentation>
newtype SVGNumberList = SVGNumberList { unSVGNumberList :: JSVal }

instance PToJSVal SVGNumberList where
  pToJSVal = unSVGNumberList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGNumberList where
  pFromJSVal = SVGNumberList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGNumberList where
  toJSVal = return . unSVGNumberList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGNumberList where
  fromJSVal v = fmap SVGNumberList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGNumberList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGNumberList where
  makeObject = makeObject . unSVGNumberList

instance IsGObject SVGNumberList where
  typeGType _ = gTypeSVGNumberList
  {-# INLINE typeGType #-}

noSVGNumberList :: Maybe SVGNumberList
noSVGNumberList = Nothing
{-# INLINE noSVGNumberList #-}

gTypeSVGNumberList :: JSM GType
gTypeSVGNumberList = GType . Object <$> jsg "SVGNumberList"

-- | Functions for this inteface are in "JSDOM.SVGPathElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathElement Mozilla SVGPathElement documentation>
newtype SVGPathElement = SVGPathElement { unSVGPathElement :: JSVal }

instance PToJSVal SVGPathElement where
  pToJSVal = unSVGPathElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathElement where
  pFromJSVal = SVGPathElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathElement where
  toJSVal = return . unSVGPathElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathElement where
  fromJSVal v = fmap SVGPathElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathElement where
  makeObject = makeObject . unSVGPathElement

instance IsSVGGraphicsElement SVGPathElement
instance IsSVGElement SVGPathElement
instance IsElement SVGPathElement
instance IsNode SVGPathElement
instance IsEventTarget SVGPathElement
instance IsSlotable SVGPathElement
instance IsParentNode SVGPathElement
instance IsNonDocumentTypeChildNode SVGPathElement
instance IsDocumentAndElementEventHandlers SVGPathElement
instance IsChildNode SVGPathElement
instance IsAnimatable SVGPathElement
instance IsGlobalEventHandlers SVGPathElement
instance IsElementCSSInlineStyle SVGPathElement
instance IsSVGTests SVGPathElement
instance IsSVGExternalResourcesRequired SVGPathElement
instance IsGObject SVGPathElement where
  typeGType _ = gTypeSVGPathElement
  {-# INLINE typeGType #-}

noSVGPathElement :: Maybe SVGPathElement
noSVGPathElement = Nothing
{-# INLINE noSVGPathElement #-}

gTypeSVGPathElement :: JSM GType
gTypeSVGPathElement = GType . Object <$> jsg "SVGPathElement"

-- | Functions for this inteface are in "JSDOM.SVGPathSeg".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSeg Mozilla SVGPathSeg documentation>
newtype SVGPathSeg = SVGPathSeg { unSVGPathSeg :: JSVal }

instance PToJSVal SVGPathSeg where
  pToJSVal = unSVGPathSeg
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSeg where
  pFromJSVal = SVGPathSeg
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSeg where
  toJSVal = return . unSVGPathSeg
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSeg where
  fromJSVal v = fmap SVGPathSeg <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSeg
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSeg where
  makeObject = makeObject . unSVGPathSeg

class (IsGObject o) => IsSVGPathSeg o
toSVGPathSeg :: IsSVGPathSeg o => o -> SVGPathSeg
toSVGPathSeg = SVGPathSeg . coerce

instance IsSVGPathSeg SVGPathSeg
instance IsGObject SVGPathSeg where
  typeGType _ = gTypeSVGPathSeg
  {-# INLINE typeGType #-}

noSVGPathSeg :: Maybe SVGPathSeg
noSVGPathSeg = Nothing
{-# INLINE noSVGPathSeg #-}

gTypeSVGPathSeg :: JSM GType
gTypeSVGPathSeg = GType . Object <$> jsg "SVGPathSeg"

-- | Functions for this inteface are in "JSDOM.SVGPathSegArcAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegArcAbs Mozilla SVGPathSegArcAbs documentation>
newtype SVGPathSegArcAbs = SVGPathSegArcAbs { unSVGPathSegArcAbs :: JSVal }

instance PToJSVal SVGPathSegArcAbs where
  pToJSVal = unSVGPathSegArcAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegArcAbs where
  pFromJSVal = SVGPathSegArcAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegArcAbs where
  toJSVal = return . unSVGPathSegArcAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegArcAbs where
  fromJSVal v = fmap SVGPathSegArcAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegArcAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegArcAbs where
  makeObject = makeObject . unSVGPathSegArcAbs

instance IsSVGPathSeg SVGPathSegArcAbs
instance IsGObject SVGPathSegArcAbs where
  typeGType _ = gTypeSVGPathSegArcAbs
  {-# INLINE typeGType #-}

noSVGPathSegArcAbs :: Maybe SVGPathSegArcAbs
noSVGPathSegArcAbs = Nothing
{-# INLINE noSVGPathSegArcAbs #-}

gTypeSVGPathSegArcAbs :: JSM GType
gTypeSVGPathSegArcAbs = GType . Object <$> jsg "SVGPathSegArcAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegArcRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegArcRel Mozilla SVGPathSegArcRel documentation>
newtype SVGPathSegArcRel = SVGPathSegArcRel { unSVGPathSegArcRel :: JSVal }

instance PToJSVal SVGPathSegArcRel where
  pToJSVal = unSVGPathSegArcRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegArcRel where
  pFromJSVal = SVGPathSegArcRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegArcRel where
  toJSVal = return . unSVGPathSegArcRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegArcRel where
  fromJSVal v = fmap SVGPathSegArcRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegArcRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegArcRel where
  makeObject = makeObject . unSVGPathSegArcRel

instance IsSVGPathSeg SVGPathSegArcRel
instance IsGObject SVGPathSegArcRel where
  typeGType _ = gTypeSVGPathSegArcRel
  {-# INLINE typeGType #-}

noSVGPathSegArcRel :: Maybe SVGPathSegArcRel
noSVGPathSegArcRel = Nothing
{-# INLINE noSVGPathSegArcRel #-}

gTypeSVGPathSegArcRel :: JSM GType
gTypeSVGPathSegArcRel = GType . Object <$> jsg "SVGPathSegArcRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegClosePath".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegClosePath Mozilla SVGPathSegClosePath documentation>
newtype SVGPathSegClosePath = SVGPathSegClosePath { unSVGPathSegClosePath :: JSVal }

instance PToJSVal SVGPathSegClosePath where
  pToJSVal = unSVGPathSegClosePath
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegClosePath where
  pFromJSVal = SVGPathSegClosePath
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegClosePath where
  toJSVal = return . unSVGPathSegClosePath
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegClosePath where
  fromJSVal v = fmap SVGPathSegClosePath <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegClosePath
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegClosePath where
  makeObject = makeObject . unSVGPathSegClosePath

instance IsSVGPathSeg SVGPathSegClosePath
instance IsGObject SVGPathSegClosePath where
  typeGType _ = gTypeSVGPathSegClosePath
  {-# INLINE typeGType #-}

noSVGPathSegClosePath :: Maybe SVGPathSegClosePath
noSVGPathSegClosePath = Nothing
{-# INLINE noSVGPathSegClosePath #-}

gTypeSVGPathSegClosePath :: JSM GType
gTypeSVGPathSegClosePath = GType . Object <$> jsg "SVGPathSegClosePath"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoCubicAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoCubicAbs Mozilla SVGPathSegCurvetoCubicAbs documentation>
newtype SVGPathSegCurvetoCubicAbs = SVGPathSegCurvetoCubicAbs { unSVGPathSegCurvetoCubicAbs :: JSVal }

instance PToJSVal SVGPathSegCurvetoCubicAbs where
  pToJSVal = unSVGPathSegCurvetoCubicAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoCubicAbs where
  pFromJSVal = SVGPathSegCurvetoCubicAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoCubicAbs where
  toJSVal = return . unSVGPathSegCurvetoCubicAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoCubicAbs where
  fromJSVal v = fmap SVGPathSegCurvetoCubicAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoCubicAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoCubicAbs where
  makeObject = makeObject . unSVGPathSegCurvetoCubicAbs

instance IsSVGPathSeg SVGPathSegCurvetoCubicAbs
instance IsGObject SVGPathSegCurvetoCubicAbs where
  typeGType _ = gTypeSVGPathSegCurvetoCubicAbs
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoCubicAbs :: Maybe SVGPathSegCurvetoCubicAbs
noSVGPathSegCurvetoCubicAbs = Nothing
{-# INLINE noSVGPathSegCurvetoCubicAbs #-}

gTypeSVGPathSegCurvetoCubicAbs :: JSM GType
gTypeSVGPathSegCurvetoCubicAbs = GType . Object <$> jsg "SVGPathSegCurvetoCubicAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoCubicRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoCubicRel Mozilla SVGPathSegCurvetoCubicRel documentation>
newtype SVGPathSegCurvetoCubicRel = SVGPathSegCurvetoCubicRel { unSVGPathSegCurvetoCubicRel :: JSVal }

instance PToJSVal SVGPathSegCurvetoCubicRel where
  pToJSVal = unSVGPathSegCurvetoCubicRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoCubicRel where
  pFromJSVal = SVGPathSegCurvetoCubicRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoCubicRel where
  toJSVal = return . unSVGPathSegCurvetoCubicRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoCubicRel where
  fromJSVal v = fmap SVGPathSegCurvetoCubicRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoCubicRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoCubicRel where
  makeObject = makeObject . unSVGPathSegCurvetoCubicRel

instance IsSVGPathSeg SVGPathSegCurvetoCubicRel
instance IsGObject SVGPathSegCurvetoCubicRel where
  typeGType _ = gTypeSVGPathSegCurvetoCubicRel
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoCubicRel :: Maybe SVGPathSegCurvetoCubicRel
noSVGPathSegCurvetoCubicRel = Nothing
{-# INLINE noSVGPathSegCurvetoCubicRel #-}

gTypeSVGPathSegCurvetoCubicRel :: JSM GType
gTypeSVGPathSegCurvetoCubicRel = GType . Object <$> jsg "SVGPathSegCurvetoCubicRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoCubicSmoothAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoCubicSmoothAbs Mozilla SVGPathSegCurvetoCubicSmoothAbs documentation>
newtype SVGPathSegCurvetoCubicSmoothAbs = SVGPathSegCurvetoCubicSmoothAbs { unSVGPathSegCurvetoCubicSmoothAbs :: JSVal }

instance PToJSVal SVGPathSegCurvetoCubicSmoothAbs where
  pToJSVal = unSVGPathSegCurvetoCubicSmoothAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoCubicSmoothAbs where
  pFromJSVal = SVGPathSegCurvetoCubicSmoothAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoCubicSmoothAbs where
  toJSVal = return . unSVGPathSegCurvetoCubicSmoothAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoCubicSmoothAbs where
  fromJSVal v = fmap SVGPathSegCurvetoCubicSmoothAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoCubicSmoothAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoCubicSmoothAbs where
  makeObject = makeObject . unSVGPathSegCurvetoCubicSmoothAbs

instance IsSVGPathSeg SVGPathSegCurvetoCubicSmoothAbs
instance IsGObject SVGPathSegCurvetoCubicSmoothAbs where
  typeGType _ = gTypeSVGPathSegCurvetoCubicSmoothAbs
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoCubicSmoothAbs :: Maybe SVGPathSegCurvetoCubicSmoothAbs
noSVGPathSegCurvetoCubicSmoothAbs = Nothing
{-# INLINE noSVGPathSegCurvetoCubicSmoothAbs #-}

gTypeSVGPathSegCurvetoCubicSmoothAbs :: JSM GType
gTypeSVGPathSegCurvetoCubicSmoothAbs = GType . Object <$> jsg "SVGPathSegCurvetoCubicSmoothAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoCubicSmoothRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoCubicSmoothRel Mozilla SVGPathSegCurvetoCubicSmoothRel documentation>
newtype SVGPathSegCurvetoCubicSmoothRel = SVGPathSegCurvetoCubicSmoothRel { unSVGPathSegCurvetoCubicSmoothRel :: JSVal }

instance PToJSVal SVGPathSegCurvetoCubicSmoothRel where
  pToJSVal = unSVGPathSegCurvetoCubicSmoothRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoCubicSmoothRel where
  pFromJSVal = SVGPathSegCurvetoCubicSmoothRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoCubicSmoothRel where
  toJSVal = return . unSVGPathSegCurvetoCubicSmoothRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoCubicSmoothRel where
  fromJSVal v = fmap SVGPathSegCurvetoCubicSmoothRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoCubicSmoothRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoCubicSmoothRel where
  makeObject = makeObject . unSVGPathSegCurvetoCubicSmoothRel

instance IsSVGPathSeg SVGPathSegCurvetoCubicSmoothRel
instance IsGObject SVGPathSegCurvetoCubicSmoothRel where
  typeGType _ = gTypeSVGPathSegCurvetoCubicSmoothRel
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoCubicSmoothRel :: Maybe SVGPathSegCurvetoCubicSmoothRel
noSVGPathSegCurvetoCubicSmoothRel = Nothing
{-# INLINE noSVGPathSegCurvetoCubicSmoothRel #-}

gTypeSVGPathSegCurvetoCubicSmoothRel :: JSM GType
gTypeSVGPathSegCurvetoCubicSmoothRel = GType . Object <$> jsg "SVGPathSegCurvetoCubicSmoothRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoQuadraticAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoQuadraticAbs Mozilla SVGPathSegCurvetoQuadraticAbs documentation>
newtype SVGPathSegCurvetoQuadraticAbs = SVGPathSegCurvetoQuadraticAbs { unSVGPathSegCurvetoQuadraticAbs :: JSVal }

instance PToJSVal SVGPathSegCurvetoQuadraticAbs where
  pToJSVal = unSVGPathSegCurvetoQuadraticAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoQuadraticAbs where
  pFromJSVal = SVGPathSegCurvetoQuadraticAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoQuadraticAbs where
  toJSVal = return . unSVGPathSegCurvetoQuadraticAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoQuadraticAbs where
  fromJSVal v = fmap SVGPathSegCurvetoQuadraticAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoQuadraticAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoQuadraticAbs where
  makeObject = makeObject . unSVGPathSegCurvetoQuadraticAbs

instance IsSVGPathSeg SVGPathSegCurvetoQuadraticAbs
instance IsGObject SVGPathSegCurvetoQuadraticAbs where
  typeGType _ = gTypeSVGPathSegCurvetoQuadraticAbs
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoQuadraticAbs :: Maybe SVGPathSegCurvetoQuadraticAbs
noSVGPathSegCurvetoQuadraticAbs = Nothing
{-# INLINE noSVGPathSegCurvetoQuadraticAbs #-}

gTypeSVGPathSegCurvetoQuadraticAbs :: JSM GType
gTypeSVGPathSegCurvetoQuadraticAbs = GType . Object <$> jsg "SVGPathSegCurvetoQuadraticAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoQuadraticRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoQuadraticRel Mozilla SVGPathSegCurvetoQuadraticRel documentation>
newtype SVGPathSegCurvetoQuadraticRel = SVGPathSegCurvetoQuadraticRel { unSVGPathSegCurvetoQuadraticRel :: JSVal }

instance PToJSVal SVGPathSegCurvetoQuadraticRel where
  pToJSVal = unSVGPathSegCurvetoQuadraticRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoQuadraticRel where
  pFromJSVal = SVGPathSegCurvetoQuadraticRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoQuadraticRel where
  toJSVal = return . unSVGPathSegCurvetoQuadraticRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoQuadraticRel where
  fromJSVal v = fmap SVGPathSegCurvetoQuadraticRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoQuadraticRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoQuadraticRel where
  makeObject = makeObject . unSVGPathSegCurvetoQuadraticRel

instance IsSVGPathSeg SVGPathSegCurvetoQuadraticRel
instance IsGObject SVGPathSegCurvetoQuadraticRel where
  typeGType _ = gTypeSVGPathSegCurvetoQuadraticRel
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoQuadraticRel :: Maybe SVGPathSegCurvetoQuadraticRel
noSVGPathSegCurvetoQuadraticRel = Nothing
{-# INLINE noSVGPathSegCurvetoQuadraticRel #-}

gTypeSVGPathSegCurvetoQuadraticRel :: JSM GType
gTypeSVGPathSegCurvetoQuadraticRel = GType . Object <$> jsg "SVGPathSegCurvetoQuadraticRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoQuadraticSmoothAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoQuadraticSmoothAbs Mozilla SVGPathSegCurvetoQuadraticSmoothAbs documentation>
newtype SVGPathSegCurvetoQuadraticSmoothAbs = SVGPathSegCurvetoQuadraticSmoothAbs { unSVGPathSegCurvetoQuadraticSmoothAbs :: JSVal }

instance PToJSVal SVGPathSegCurvetoQuadraticSmoothAbs where
  pToJSVal = unSVGPathSegCurvetoQuadraticSmoothAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoQuadraticSmoothAbs where
  pFromJSVal = SVGPathSegCurvetoQuadraticSmoothAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoQuadraticSmoothAbs where
  toJSVal = return . unSVGPathSegCurvetoQuadraticSmoothAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoQuadraticSmoothAbs where
  fromJSVal v = fmap SVGPathSegCurvetoQuadraticSmoothAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoQuadraticSmoothAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoQuadraticSmoothAbs where
  makeObject = makeObject . unSVGPathSegCurvetoQuadraticSmoothAbs

instance IsSVGPathSeg SVGPathSegCurvetoQuadraticSmoothAbs
instance IsGObject SVGPathSegCurvetoQuadraticSmoothAbs where
  typeGType _ = gTypeSVGPathSegCurvetoQuadraticSmoothAbs
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoQuadraticSmoothAbs :: Maybe SVGPathSegCurvetoQuadraticSmoothAbs
noSVGPathSegCurvetoQuadraticSmoothAbs = Nothing
{-# INLINE noSVGPathSegCurvetoQuadraticSmoothAbs #-}

gTypeSVGPathSegCurvetoQuadraticSmoothAbs :: JSM GType
gTypeSVGPathSegCurvetoQuadraticSmoothAbs = GType . Object <$> jsg "SVGPathSegCurvetoQuadraticSmoothAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegCurvetoQuadraticSmoothRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegCurvetoQuadraticSmoothRel Mozilla SVGPathSegCurvetoQuadraticSmoothRel documentation>
newtype SVGPathSegCurvetoQuadraticSmoothRel = SVGPathSegCurvetoQuadraticSmoothRel { unSVGPathSegCurvetoQuadraticSmoothRel :: JSVal }

instance PToJSVal SVGPathSegCurvetoQuadraticSmoothRel where
  pToJSVal = unSVGPathSegCurvetoQuadraticSmoothRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegCurvetoQuadraticSmoothRel where
  pFromJSVal = SVGPathSegCurvetoQuadraticSmoothRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegCurvetoQuadraticSmoothRel where
  toJSVal = return . unSVGPathSegCurvetoQuadraticSmoothRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegCurvetoQuadraticSmoothRel where
  fromJSVal v = fmap SVGPathSegCurvetoQuadraticSmoothRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegCurvetoQuadraticSmoothRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegCurvetoQuadraticSmoothRel where
  makeObject = makeObject . unSVGPathSegCurvetoQuadraticSmoothRel

instance IsSVGPathSeg SVGPathSegCurvetoQuadraticSmoothRel
instance IsGObject SVGPathSegCurvetoQuadraticSmoothRel where
  typeGType _ = gTypeSVGPathSegCurvetoQuadraticSmoothRel
  {-# INLINE typeGType #-}

noSVGPathSegCurvetoQuadraticSmoothRel :: Maybe SVGPathSegCurvetoQuadraticSmoothRel
noSVGPathSegCurvetoQuadraticSmoothRel = Nothing
{-# INLINE noSVGPathSegCurvetoQuadraticSmoothRel #-}

gTypeSVGPathSegCurvetoQuadraticSmoothRel :: JSM GType
gTypeSVGPathSegCurvetoQuadraticSmoothRel = GType . Object <$> jsg "SVGPathSegCurvetoQuadraticSmoothRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoAbs Mozilla SVGPathSegLinetoAbs documentation>
newtype SVGPathSegLinetoAbs = SVGPathSegLinetoAbs { unSVGPathSegLinetoAbs :: JSVal }

instance PToJSVal SVGPathSegLinetoAbs where
  pToJSVal = unSVGPathSegLinetoAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoAbs where
  pFromJSVal = SVGPathSegLinetoAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoAbs where
  toJSVal = return . unSVGPathSegLinetoAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoAbs where
  fromJSVal v = fmap SVGPathSegLinetoAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoAbs where
  makeObject = makeObject . unSVGPathSegLinetoAbs

instance IsSVGPathSeg SVGPathSegLinetoAbs
instance IsGObject SVGPathSegLinetoAbs where
  typeGType _ = gTypeSVGPathSegLinetoAbs
  {-# INLINE typeGType #-}

noSVGPathSegLinetoAbs :: Maybe SVGPathSegLinetoAbs
noSVGPathSegLinetoAbs = Nothing
{-# INLINE noSVGPathSegLinetoAbs #-}

gTypeSVGPathSegLinetoAbs :: JSM GType
gTypeSVGPathSegLinetoAbs = GType . Object <$> jsg "SVGPathSegLinetoAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoHorizontalAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoHorizontalAbs Mozilla SVGPathSegLinetoHorizontalAbs documentation>
newtype SVGPathSegLinetoHorizontalAbs = SVGPathSegLinetoHorizontalAbs { unSVGPathSegLinetoHorizontalAbs :: JSVal }

instance PToJSVal SVGPathSegLinetoHorizontalAbs where
  pToJSVal = unSVGPathSegLinetoHorizontalAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoHorizontalAbs where
  pFromJSVal = SVGPathSegLinetoHorizontalAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoHorizontalAbs where
  toJSVal = return . unSVGPathSegLinetoHorizontalAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoHorizontalAbs where
  fromJSVal v = fmap SVGPathSegLinetoHorizontalAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoHorizontalAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoHorizontalAbs where
  makeObject = makeObject . unSVGPathSegLinetoHorizontalAbs

instance IsSVGPathSeg SVGPathSegLinetoHorizontalAbs
instance IsGObject SVGPathSegLinetoHorizontalAbs where
  typeGType _ = gTypeSVGPathSegLinetoHorizontalAbs
  {-# INLINE typeGType #-}

noSVGPathSegLinetoHorizontalAbs :: Maybe SVGPathSegLinetoHorizontalAbs
noSVGPathSegLinetoHorizontalAbs = Nothing
{-# INLINE noSVGPathSegLinetoHorizontalAbs #-}

gTypeSVGPathSegLinetoHorizontalAbs :: JSM GType
gTypeSVGPathSegLinetoHorizontalAbs = GType . Object <$> jsg "SVGPathSegLinetoHorizontalAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoHorizontalRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoHorizontalRel Mozilla SVGPathSegLinetoHorizontalRel documentation>
newtype SVGPathSegLinetoHorizontalRel = SVGPathSegLinetoHorizontalRel { unSVGPathSegLinetoHorizontalRel :: JSVal }

instance PToJSVal SVGPathSegLinetoHorizontalRel where
  pToJSVal = unSVGPathSegLinetoHorizontalRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoHorizontalRel where
  pFromJSVal = SVGPathSegLinetoHorizontalRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoHorizontalRel where
  toJSVal = return . unSVGPathSegLinetoHorizontalRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoHorizontalRel where
  fromJSVal v = fmap SVGPathSegLinetoHorizontalRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoHorizontalRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoHorizontalRel where
  makeObject = makeObject . unSVGPathSegLinetoHorizontalRel

instance IsSVGPathSeg SVGPathSegLinetoHorizontalRel
instance IsGObject SVGPathSegLinetoHorizontalRel where
  typeGType _ = gTypeSVGPathSegLinetoHorizontalRel
  {-# INLINE typeGType #-}

noSVGPathSegLinetoHorizontalRel :: Maybe SVGPathSegLinetoHorizontalRel
noSVGPathSegLinetoHorizontalRel = Nothing
{-# INLINE noSVGPathSegLinetoHorizontalRel #-}

gTypeSVGPathSegLinetoHorizontalRel :: JSM GType
gTypeSVGPathSegLinetoHorizontalRel = GType . Object <$> jsg "SVGPathSegLinetoHorizontalRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoRel Mozilla SVGPathSegLinetoRel documentation>
newtype SVGPathSegLinetoRel = SVGPathSegLinetoRel { unSVGPathSegLinetoRel :: JSVal }

instance PToJSVal SVGPathSegLinetoRel where
  pToJSVal = unSVGPathSegLinetoRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoRel where
  pFromJSVal = SVGPathSegLinetoRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoRel where
  toJSVal = return . unSVGPathSegLinetoRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoRel where
  fromJSVal v = fmap SVGPathSegLinetoRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoRel where
  makeObject = makeObject . unSVGPathSegLinetoRel

instance IsSVGPathSeg SVGPathSegLinetoRel
instance IsGObject SVGPathSegLinetoRel where
  typeGType _ = gTypeSVGPathSegLinetoRel
  {-# INLINE typeGType #-}

noSVGPathSegLinetoRel :: Maybe SVGPathSegLinetoRel
noSVGPathSegLinetoRel = Nothing
{-# INLINE noSVGPathSegLinetoRel #-}

gTypeSVGPathSegLinetoRel :: JSM GType
gTypeSVGPathSegLinetoRel = GType . Object <$> jsg "SVGPathSegLinetoRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoVerticalAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoVerticalAbs Mozilla SVGPathSegLinetoVerticalAbs documentation>
newtype SVGPathSegLinetoVerticalAbs = SVGPathSegLinetoVerticalAbs { unSVGPathSegLinetoVerticalAbs :: JSVal }

instance PToJSVal SVGPathSegLinetoVerticalAbs where
  pToJSVal = unSVGPathSegLinetoVerticalAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoVerticalAbs where
  pFromJSVal = SVGPathSegLinetoVerticalAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoVerticalAbs where
  toJSVal = return . unSVGPathSegLinetoVerticalAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoVerticalAbs where
  fromJSVal v = fmap SVGPathSegLinetoVerticalAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoVerticalAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoVerticalAbs where
  makeObject = makeObject . unSVGPathSegLinetoVerticalAbs

instance IsSVGPathSeg SVGPathSegLinetoVerticalAbs
instance IsGObject SVGPathSegLinetoVerticalAbs where
  typeGType _ = gTypeSVGPathSegLinetoVerticalAbs
  {-# INLINE typeGType #-}

noSVGPathSegLinetoVerticalAbs :: Maybe SVGPathSegLinetoVerticalAbs
noSVGPathSegLinetoVerticalAbs = Nothing
{-# INLINE noSVGPathSegLinetoVerticalAbs #-}

gTypeSVGPathSegLinetoVerticalAbs :: JSM GType
gTypeSVGPathSegLinetoVerticalAbs = GType . Object <$> jsg "SVGPathSegLinetoVerticalAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegLinetoVerticalRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegLinetoVerticalRel Mozilla SVGPathSegLinetoVerticalRel documentation>
newtype SVGPathSegLinetoVerticalRel = SVGPathSegLinetoVerticalRel { unSVGPathSegLinetoVerticalRel :: JSVal }

instance PToJSVal SVGPathSegLinetoVerticalRel where
  pToJSVal = unSVGPathSegLinetoVerticalRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegLinetoVerticalRel where
  pFromJSVal = SVGPathSegLinetoVerticalRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegLinetoVerticalRel where
  toJSVal = return . unSVGPathSegLinetoVerticalRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegLinetoVerticalRel where
  fromJSVal v = fmap SVGPathSegLinetoVerticalRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegLinetoVerticalRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegLinetoVerticalRel where
  makeObject = makeObject . unSVGPathSegLinetoVerticalRel

instance IsSVGPathSeg SVGPathSegLinetoVerticalRel
instance IsGObject SVGPathSegLinetoVerticalRel where
  typeGType _ = gTypeSVGPathSegLinetoVerticalRel
  {-# INLINE typeGType #-}

noSVGPathSegLinetoVerticalRel :: Maybe SVGPathSegLinetoVerticalRel
noSVGPathSegLinetoVerticalRel = Nothing
{-# INLINE noSVGPathSegLinetoVerticalRel #-}

gTypeSVGPathSegLinetoVerticalRel :: JSM GType
gTypeSVGPathSegLinetoVerticalRel = GType . Object <$> jsg "SVGPathSegLinetoVerticalRel"

-- | Functions for this inteface are in "JSDOM.SVGPathSegList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegList Mozilla SVGPathSegList documentation>
newtype SVGPathSegList = SVGPathSegList { unSVGPathSegList :: JSVal }

instance PToJSVal SVGPathSegList where
  pToJSVal = unSVGPathSegList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegList where
  pFromJSVal = SVGPathSegList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegList where
  toJSVal = return . unSVGPathSegList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegList where
  fromJSVal v = fmap SVGPathSegList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegList where
  makeObject = makeObject . unSVGPathSegList

instance IsGObject SVGPathSegList where
  typeGType _ = gTypeSVGPathSegList
  {-# INLINE typeGType #-}

noSVGPathSegList :: Maybe SVGPathSegList
noSVGPathSegList = Nothing
{-# INLINE noSVGPathSegList #-}

gTypeSVGPathSegList :: JSM GType
gTypeSVGPathSegList = GType . Object <$> jsg "SVGPathSegList"

-- | Functions for this inteface are in "JSDOM.SVGPathSegMovetoAbs".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegMovetoAbs Mozilla SVGPathSegMovetoAbs documentation>
newtype SVGPathSegMovetoAbs = SVGPathSegMovetoAbs { unSVGPathSegMovetoAbs :: JSVal }

instance PToJSVal SVGPathSegMovetoAbs where
  pToJSVal = unSVGPathSegMovetoAbs
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegMovetoAbs where
  pFromJSVal = SVGPathSegMovetoAbs
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegMovetoAbs where
  toJSVal = return . unSVGPathSegMovetoAbs
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegMovetoAbs where
  fromJSVal v = fmap SVGPathSegMovetoAbs <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegMovetoAbs
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegMovetoAbs where
  makeObject = makeObject . unSVGPathSegMovetoAbs

instance IsSVGPathSeg SVGPathSegMovetoAbs
instance IsGObject SVGPathSegMovetoAbs where
  typeGType _ = gTypeSVGPathSegMovetoAbs
  {-# INLINE typeGType #-}

noSVGPathSegMovetoAbs :: Maybe SVGPathSegMovetoAbs
noSVGPathSegMovetoAbs = Nothing
{-# INLINE noSVGPathSegMovetoAbs #-}

gTypeSVGPathSegMovetoAbs :: JSM GType
gTypeSVGPathSegMovetoAbs = GType . Object <$> jsg "SVGPathSegMovetoAbs"

-- | Functions for this inteface are in "JSDOM.SVGPathSegMovetoRel".
-- Base interface functions are in:
--
--     * "JSDOM.SVGPathSeg"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPathSegMovetoRel Mozilla SVGPathSegMovetoRel documentation>
newtype SVGPathSegMovetoRel = SVGPathSegMovetoRel { unSVGPathSegMovetoRel :: JSVal }

instance PToJSVal SVGPathSegMovetoRel where
  pToJSVal = unSVGPathSegMovetoRel
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPathSegMovetoRel where
  pFromJSVal = SVGPathSegMovetoRel
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPathSegMovetoRel where
  toJSVal = return . unSVGPathSegMovetoRel
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPathSegMovetoRel where
  fromJSVal v = fmap SVGPathSegMovetoRel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPathSegMovetoRel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPathSegMovetoRel where
  makeObject = makeObject . unSVGPathSegMovetoRel

instance IsSVGPathSeg SVGPathSegMovetoRel
instance IsGObject SVGPathSegMovetoRel where
  typeGType _ = gTypeSVGPathSegMovetoRel
  {-# INLINE typeGType #-}

noSVGPathSegMovetoRel :: Maybe SVGPathSegMovetoRel
noSVGPathSegMovetoRel = Nothing
{-# INLINE noSVGPathSegMovetoRel #-}

gTypeSVGPathSegMovetoRel :: JSM GType
gTypeSVGPathSegMovetoRel = GType . Object <$> jsg "SVGPathSegMovetoRel"

-- | Functions for this inteface are in "JSDOM.SVGPatternElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGFitToViewBox"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPatternElement Mozilla SVGPatternElement documentation>
newtype SVGPatternElement = SVGPatternElement { unSVGPatternElement :: JSVal }

instance PToJSVal SVGPatternElement where
  pToJSVal = unSVGPatternElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPatternElement where
  pFromJSVal = SVGPatternElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPatternElement where
  toJSVal = return . unSVGPatternElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPatternElement where
  fromJSVal v = fmap SVGPatternElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPatternElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPatternElement where
  makeObject = makeObject . unSVGPatternElement

instance IsSVGElement SVGPatternElement
instance IsElement SVGPatternElement
instance IsNode SVGPatternElement
instance IsEventTarget SVGPatternElement
instance IsSlotable SVGPatternElement
instance IsParentNode SVGPatternElement
instance IsNonDocumentTypeChildNode SVGPatternElement
instance IsDocumentAndElementEventHandlers SVGPatternElement
instance IsChildNode SVGPatternElement
instance IsAnimatable SVGPatternElement
instance IsGlobalEventHandlers SVGPatternElement
instance IsElementCSSInlineStyle SVGPatternElement
instance IsSVGURIReference SVGPatternElement
instance IsSVGTests SVGPatternElement
instance IsSVGFitToViewBox SVGPatternElement
instance IsSVGExternalResourcesRequired SVGPatternElement
instance IsGObject SVGPatternElement where
  typeGType _ = gTypeSVGPatternElement
  {-# INLINE typeGType #-}

noSVGPatternElement :: Maybe SVGPatternElement
noSVGPatternElement = Nothing
{-# INLINE noSVGPatternElement #-}

gTypeSVGPatternElement :: JSM GType
gTypeSVGPatternElement = GType . Object <$> jsg "SVGPatternElement"

-- | Functions for this inteface are in "JSDOM.SVGPoint".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPoint Mozilla SVGPoint documentation>
newtype SVGPoint = SVGPoint { unSVGPoint :: JSVal }

instance PToJSVal SVGPoint where
  pToJSVal = unSVGPoint
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPoint where
  pFromJSVal = SVGPoint
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPoint where
  toJSVal = return . unSVGPoint
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPoint where
  fromJSVal v = fmap SVGPoint <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPoint
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPoint where
  makeObject = makeObject . unSVGPoint

instance IsGObject SVGPoint where
  typeGType _ = gTypeSVGPoint
  {-# INLINE typeGType #-}

noSVGPoint :: Maybe SVGPoint
noSVGPoint = Nothing
{-# INLINE noSVGPoint #-}

gTypeSVGPoint :: JSM GType
gTypeSVGPoint = GType . Object <$> jsg "SVGPoint"

-- | Functions for this inteface are in "JSDOM.SVGPointList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPointList Mozilla SVGPointList documentation>
newtype SVGPointList = SVGPointList { unSVGPointList :: JSVal }

instance PToJSVal SVGPointList where
  pToJSVal = unSVGPointList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPointList where
  pFromJSVal = SVGPointList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPointList where
  toJSVal = return . unSVGPointList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPointList where
  fromJSVal v = fmap SVGPointList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPointList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPointList where
  makeObject = makeObject . unSVGPointList

instance IsGObject SVGPointList where
  typeGType _ = gTypeSVGPointList
  {-# INLINE typeGType #-}

noSVGPointList :: Maybe SVGPointList
noSVGPointList = Nothing
{-# INLINE noSVGPointList #-}

gTypeSVGPointList :: JSM GType
gTypeSVGPointList = GType . Object <$> jsg "SVGPointList"

-- | Functions for this inteface are in "JSDOM.SVGPolygonElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPolygonElement Mozilla SVGPolygonElement documentation>
newtype SVGPolygonElement = SVGPolygonElement { unSVGPolygonElement :: JSVal }

instance PToJSVal SVGPolygonElement where
  pToJSVal = unSVGPolygonElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPolygonElement where
  pFromJSVal = SVGPolygonElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPolygonElement where
  toJSVal = return . unSVGPolygonElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPolygonElement where
  fromJSVal v = fmap SVGPolygonElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPolygonElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPolygonElement where
  makeObject = makeObject . unSVGPolygonElement

instance IsSVGGraphicsElement SVGPolygonElement
instance IsSVGElement SVGPolygonElement
instance IsElement SVGPolygonElement
instance IsNode SVGPolygonElement
instance IsEventTarget SVGPolygonElement
instance IsSlotable SVGPolygonElement
instance IsParentNode SVGPolygonElement
instance IsNonDocumentTypeChildNode SVGPolygonElement
instance IsDocumentAndElementEventHandlers SVGPolygonElement
instance IsChildNode SVGPolygonElement
instance IsAnimatable SVGPolygonElement
instance IsGlobalEventHandlers SVGPolygonElement
instance IsElementCSSInlineStyle SVGPolygonElement
instance IsSVGTests SVGPolygonElement
instance IsSVGExternalResourcesRequired SVGPolygonElement
instance IsGObject SVGPolygonElement where
  typeGType _ = gTypeSVGPolygonElement
  {-# INLINE typeGType #-}

noSVGPolygonElement :: Maybe SVGPolygonElement
noSVGPolygonElement = Nothing
{-# INLINE noSVGPolygonElement #-}

gTypeSVGPolygonElement :: JSM GType
gTypeSVGPolygonElement = GType . Object <$> jsg "SVGPolygonElement"

-- | Functions for this inteface are in "JSDOM.SVGPolylineElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPolylineElement Mozilla SVGPolylineElement documentation>
newtype SVGPolylineElement = SVGPolylineElement { unSVGPolylineElement :: JSVal }

instance PToJSVal SVGPolylineElement where
  pToJSVal = unSVGPolylineElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPolylineElement where
  pFromJSVal = SVGPolylineElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPolylineElement where
  toJSVal = return . unSVGPolylineElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPolylineElement where
  fromJSVal v = fmap SVGPolylineElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPolylineElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPolylineElement where
  makeObject = makeObject . unSVGPolylineElement

instance IsSVGGraphicsElement SVGPolylineElement
instance IsSVGElement SVGPolylineElement
instance IsElement SVGPolylineElement
instance IsNode SVGPolylineElement
instance IsEventTarget SVGPolylineElement
instance IsSlotable SVGPolylineElement
instance IsParentNode SVGPolylineElement
instance IsNonDocumentTypeChildNode SVGPolylineElement
instance IsDocumentAndElementEventHandlers SVGPolylineElement
instance IsChildNode SVGPolylineElement
instance IsAnimatable SVGPolylineElement
instance IsGlobalEventHandlers SVGPolylineElement
instance IsElementCSSInlineStyle SVGPolylineElement
instance IsSVGTests SVGPolylineElement
instance IsSVGExternalResourcesRequired SVGPolylineElement
instance IsGObject SVGPolylineElement where
  typeGType _ = gTypeSVGPolylineElement
  {-# INLINE typeGType #-}

noSVGPolylineElement :: Maybe SVGPolylineElement
noSVGPolylineElement = Nothing
{-# INLINE noSVGPolylineElement #-}

gTypeSVGPolylineElement :: JSM GType
gTypeSVGPolylineElement = GType . Object <$> jsg "SVGPolylineElement"

-- | Functions for this inteface are in "JSDOM.SVGPreserveAspectRatio".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGPreserveAspectRatio Mozilla SVGPreserveAspectRatio documentation>
newtype SVGPreserveAspectRatio = SVGPreserveAspectRatio { unSVGPreserveAspectRatio :: JSVal }

instance PToJSVal SVGPreserveAspectRatio where
  pToJSVal = unSVGPreserveAspectRatio
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGPreserveAspectRatio where
  pFromJSVal = SVGPreserveAspectRatio
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGPreserveAspectRatio where
  toJSVal = return . unSVGPreserveAspectRatio
  {-# INLINE toJSVal #-}

instance FromJSVal SVGPreserveAspectRatio where
  fromJSVal v = fmap SVGPreserveAspectRatio <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGPreserveAspectRatio
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGPreserveAspectRatio where
  makeObject = makeObject . unSVGPreserveAspectRatio

instance IsGObject SVGPreserveAspectRatio where
  typeGType _ = gTypeSVGPreserveAspectRatio
  {-# INLINE typeGType #-}

noSVGPreserveAspectRatio :: Maybe SVGPreserveAspectRatio
noSVGPreserveAspectRatio = Nothing
{-# INLINE noSVGPreserveAspectRatio #-}

gTypeSVGPreserveAspectRatio :: JSM GType
gTypeSVGPreserveAspectRatio = GType . Object <$> jsg "SVGPreserveAspectRatio"

-- | Functions for this inteface are in "JSDOM.SVGRadialGradientElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGradientElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGRadialGradientElement Mozilla SVGRadialGradientElement documentation>
newtype SVGRadialGradientElement = SVGRadialGradientElement { unSVGRadialGradientElement :: JSVal }

instance PToJSVal SVGRadialGradientElement where
  pToJSVal = unSVGRadialGradientElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGRadialGradientElement where
  pFromJSVal = SVGRadialGradientElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGRadialGradientElement where
  toJSVal = return . unSVGRadialGradientElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGRadialGradientElement where
  fromJSVal v = fmap SVGRadialGradientElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGRadialGradientElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGRadialGradientElement where
  makeObject = makeObject . unSVGRadialGradientElement

instance IsSVGGradientElement SVGRadialGradientElement
instance IsSVGElement SVGRadialGradientElement
instance IsElement SVGRadialGradientElement
instance IsNode SVGRadialGradientElement
instance IsEventTarget SVGRadialGradientElement
instance IsSlotable SVGRadialGradientElement
instance IsParentNode SVGRadialGradientElement
instance IsNonDocumentTypeChildNode SVGRadialGradientElement
instance IsDocumentAndElementEventHandlers SVGRadialGradientElement
instance IsChildNode SVGRadialGradientElement
instance IsAnimatable SVGRadialGradientElement
instance IsGlobalEventHandlers SVGRadialGradientElement
instance IsElementCSSInlineStyle SVGRadialGradientElement
instance IsSVGURIReference SVGRadialGradientElement
instance IsSVGExternalResourcesRequired SVGRadialGradientElement
instance IsGObject SVGRadialGradientElement where
  typeGType _ = gTypeSVGRadialGradientElement
  {-# INLINE typeGType #-}

noSVGRadialGradientElement :: Maybe SVGRadialGradientElement
noSVGRadialGradientElement = Nothing
{-# INLINE noSVGRadialGradientElement #-}

gTypeSVGRadialGradientElement :: JSM GType
gTypeSVGRadialGradientElement = GType . Object <$> jsg "SVGRadialGradientElement"

-- | Functions for this inteface are in "JSDOM.SVGRect".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGRect Mozilla SVGRect documentation>
newtype SVGRect = SVGRect { unSVGRect :: JSVal }

instance PToJSVal SVGRect where
  pToJSVal = unSVGRect
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGRect where
  pFromJSVal = SVGRect
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGRect where
  toJSVal = return . unSVGRect
  {-# INLINE toJSVal #-}

instance FromJSVal SVGRect where
  fromJSVal v = fmap SVGRect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGRect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGRect where
  makeObject = makeObject . unSVGRect

instance IsGObject SVGRect where
  typeGType _ = gTypeSVGRect
  {-# INLINE typeGType #-}

noSVGRect :: Maybe SVGRect
noSVGRect = Nothing
{-# INLINE noSVGRect #-}

gTypeSVGRect :: JSM GType
gTypeSVGRect = GType . Object <$> jsg "SVGRect"

-- | Functions for this inteface are in "JSDOM.SVGRectElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGRectElement Mozilla SVGRectElement documentation>
newtype SVGRectElement = SVGRectElement { unSVGRectElement :: JSVal }

instance PToJSVal SVGRectElement where
  pToJSVal = unSVGRectElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGRectElement where
  pFromJSVal = SVGRectElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGRectElement where
  toJSVal = return . unSVGRectElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGRectElement where
  fromJSVal v = fmap SVGRectElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGRectElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGRectElement where
  makeObject = makeObject . unSVGRectElement

instance IsSVGGraphicsElement SVGRectElement
instance IsSVGElement SVGRectElement
instance IsElement SVGRectElement
instance IsNode SVGRectElement
instance IsEventTarget SVGRectElement
instance IsSlotable SVGRectElement
instance IsParentNode SVGRectElement
instance IsNonDocumentTypeChildNode SVGRectElement
instance IsDocumentAndElementEventHandlers SVGRectElement
instance IsChildNode SVGRectElement
instance IsAnimatable SVGRectElement
instance IsGlobalEventHandlers SVGRectElement
instance IsElementCSSInlineStyle SVGRectElement
instance IsSVGTests SVGRectElement
instance IsSVGExternalResourcesRequired SVGRectElement
instance IsGObject SVGRectElement where
  typeGType _ = gTypeSVGRectElement
  {-# INLINE typeGType #-}

noSVGRectElement :: Maybe SVGRectElement
noSVGRectElement = Nothing
{-# INLINE noSVGRectElement #-}

gTypeSVGRectElement :: JSM GType
gTypeSVGRectElement = GType . Object <$> jsg "SVGRectElement"

-- | Functions for this inteface are in "JSDOM.SVGRenderingIntent".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGRenderingIntent Mozilla SVGRenderingIntent documentation>
newtype SVGRenderingIntent = SVGRenderingIntent { unSVGRenderingIntent :: JSVal }

instance PToJSVal SVGRenderingIntent where
  pToJSVal = unSVGRenderingIntent
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGRenderingIntent where
  pFromJSVal = SVGRenderingIntent
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGRenderingIntent where
  toJSVal = return . unSVGRenderingIntent
  {-# INLINE toJSVal #-}

instance FromJSVal SVGRenderingIntent where
  fromJSVal v = fmap SVGRenderingIntent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGRenderingIntent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGRenderingIntent where
  makeObject = makeObject . unSVGRenderingIntent

instance IsGObject SVGRenderingIntent where
  typeGType _ = gTypeSVGRenderingIntent
  {-# INLINE typeGType #-}

noSVGRenderingIntent :: Maybe SVGRenderingIntent
noSVGRenderingIntent = Nothing
{-# INLINE noSVGRenderingIntent #-}

gTypeSVGRenderingIntent :: JSM GType
gTypeSVGRenderingIntent = GType . Object <$> jsg "SVGRenderingIntent"

-- | Functions for this inteface are in "JSDOM.SVGSVGElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGZoomAndPan"
--     * "JSDOM.SVGFitToViewBox"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGSVGElement Mozilla SVGSVGElement documentation>
newtype SVGSVGElement = SVGSVGElement { unSVGSVGElement :: JSVal }

instance PToJSVal SVGSVGElement where
  pToJSVal = unSVGSVGElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGSVGElement where
  pFromJSVal = SVGSVGElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGSVGElement where
  toJSVal = return . unSVGSVGElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGSVGElement where
  fromJSVal v = fmap SVGSVGElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGSVGElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGSVGElement where
  makeObject = makeObject . unSVGSVGElement

instance IsSVGGraphicsElement SVGSVGElement
instance IsSVGElement SVGSVGElement
instance IsElement SVGSVGElement
instance IsNode SVGSVGElement
instance IsEventTarget SVGSVGElement
instance IsSlotable SVGSVGElement
instance IsParentNode SVGSVGElement
instance IsNonDocumentTypeChildNode SVGSVGElement
instance IsDocumentAndElementEventHandlers SVGSVGElement
instance IsChildNode SVGSVGElement
instance IsAnimatable SVGSVGElement
instance IsGlobalEventHandlers SVGSVGElement
instance IsElementCSSInlineStyle SVGSVGElement
instance IsSVGTests SVGSVGElement
instance IsSVGZoomAndPan SVGSVGElement
instance IsSVGFitToViewBox SVGSVGElement
instance IsSVGExternalResourcesRequired SVGSVGElement
instance IsGObject SVGSVGElement where
  typeGType _ = gTypeSVGSVGElement
  {-# INLINE typeGType #-}

noSVGSVGElement :: Maybe SVGSVGElement
noSVGSVGElement = Nothing
{-# INLINE noSVGSVGElement #-}

gTypeSVGSVGElement :: JSM GType
gTypeSVGSVGElement = GType . Object <$> jsg "SVGSVGElement"

-- | Functions for this inteface are in "JSDOM.SVGScriptElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGScriptElement Mozilla SVGScriptElement documentation>
newtype SVGScriptElement = SVGScriptElement { unSVGScriptElement :: JSVal }

instance PToJSVal SVGScriptElement where
  pToJSVal = unSVGScriptElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGScriptElement where
  pFromJSVal = SVGScriptElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGScriptElement where
  toJSVal = return . unSVGScriptElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGScriptElement where
  fromJSVal v = fmap SVGScriptElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGScriptElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGScriptElement where
  makeObject = makeObject . unSVGScriptElement

instance IsSVGElement SVGScriptElement
instance IsElement SVGScriptElement
instance IsNode SVGScriptElement
instance IsEventTarget SVGScriptElement
instance IsSlotable SVGScriptElement
instance IsParentNode SVGScriptElement
instance IsNonDocumentTypeChildNode SVGScriptElement
instance IsDocumentAndElementEventHandlers SVGScriptElement
instance IsChildNode SVGScriptElement
instance IsAnimatable SVGScriptElement
instance IsGlobalEventHandlers SVGScriptElement
instance IsElementCSSInlineStyle SVGScriptElement
instance IsSVGURIReference SVGScriptElement
instance IsSVGExternalResourcesRequired SVGScriptElement
instance IsGObject SVGScriptElement where
  typeGType _ = gTypeSVGScriptElement
  {-# INLINE typeGType #-}

noSVGScriptElement :: Maybe SVGScriptElement
noSVGScriptElement = Nothing
{-# INLINE noSVGScriptElement #-}

gTypeSVGScriptElement :: JSM GType
gTypeSVGScriptElement = GType . Object <$> jsg "SVGScriptElement"

-- | Functions for this inteface are in "JSDOM.SVGSetElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGAnimationElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGSetElement Mozilla SVGSetElement documentation>
newtype SVGSetElement = SVGSetElement { unSVGSetElement :: JSVal }

instance PToJSVal SVGSetElement where
  pToJSVal = unSVGSetElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGSetElement where
  pFromJSVal = SVGSetElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGSetElement where
  toJSVal = return . unSVGSetElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGSetElement where
  fromJSVal v = fmap SVGSetElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGSetElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGSetElement where
  makeObject = makeObject . unSVGSetElement

instance IsSVGAnimationElement SVGSetElement
instance IsSVGElement SVGSetElement
instance IsElement SVGSetElement
instance IsNode SVGSetElement
instance IsEventTarget SVGSetElement
instance IsSlotable SVGSetElement
instance IsParentNode SVGSetElement
instance IsNonDocumentTypeChildNode SVGSetElement
instance IsDocumentAndElementEventHandlers SVGSetElement
instance IsChildNode SVGSetElement
instance IsAnimatable SVGSetElement
instance IsGlobalEventHandlers SVGSetElement
instance IsElementCSSInlineStyle SVGSetElement
instance IsSVGTests SVGSetElement
instance IsSVGExternalResourcesRequired SVGSetElement
instance IsGObject SVGSetElement where
  typeGType _ = gTypeSVGSetElement
  {-# INLINE typeGType #-}

noSVGSetElement :: Maybe SVGSetElement
noSVGSetElement = Nothing
{-# INLINE noSVGSetElement #-}

gTypeSVGSetElement :: JSM GType
gTypeSVGSetElement = GType . Object <$> jsg "SVGSetElement"

-- | Functions for this inteface are in "JSDOM.SVGStopElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGStopElement Mozilla SVGStopElement documentation>
newtype SVGStopElement = SVGStopElement { unSVGStopElement :: JSVal }

instance PToJSVal SVGStopElement where
  pToJSVal = unSVGStopElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGStopElement where
  pFromJSVal = SVGStopElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGStopElement where
  toJSVal = return . unSVGStopElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGStopElement where
  fromJSVal v = fmap SVGStopElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGStopElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGStopElement where
  makeObject = makeObject . unSVGStopElement

instance IsSVGElement SVGStopElement
instance IsElement SVGStopElement
instance IsNode SVGStopElement
instance IsEventTarget SVGStopElement
instance IsSlotable SVGStopElement
instance IsParentNode SVGStopElement
instance IsNonDocumentTypeChildNode SVGStopElement
instance IsDocumentAndElementEventHandlers SVGStopElement
instance IsChildNode SVGStopElement
instance IsAnimatable SVGStopElement
instance IsGlobalEventHandlers SVGStopElement
instance IsElementCSSInlineStyle SVGStopElement
instance IsGObject SVGStopElement where
  typeGType _ = gTypeSVGStopElement
  {-# INLINE typeGType #-}

noSVGStopElement :: Maybe SVGStopElement
noSVGStopElement = Nothing
{-# INLINE noSVGStopElement #-}

gTypeSVGStopElement :: JSM GType
gTypeSVGStopElement = GType . Object <$> jsg "SVGStopElement"

-- | Functions for this inteface are in "JSDOM.SVGStringList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGStringList Mozilla SVGStringList documentation>
newtype SVGStringList = SVGStringList { unSVGStringList :: JSVal }

instance PToJSVal SVGStringList where
  pToJSVal = unSVGStringList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGStringList where
  pFromJSVal = SVGStringList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGStringList where
  toJSVal = return . unSVGStringList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGStringList where
  fromJSVal v = fmap SVGStringList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGStringList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGStringList where
  makeObject = makeObject . unSVGStringList

instance IsGObject SVGStringList where
  typeGType _ = gTypeSVGStringList
  {-# INLINE typeGType #-}

noSVGStringList :: Maybe SVGStringList
noSVGStringList = Nothing
{-# INLINE noSVGStringList #-}

gTypeSVGStringList :: JSM GType
gTypeSVGStringList = GType . Object <$> jsg "SVGStringList"

-- | Functions for this inteface are in "JSDOM.SVGStyleElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGStyleElement Mozilla SVGStyleElement documentation>
newtype SVGStyleElement = SVGStyleElement { unSVGStyleElement :: JSVal }

instance PToJSVal SVGStyleElement where
  pToJSVal = unSVGStyleElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGStyleElement where
  pFromJSVal = SVGStyleElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGStyleElement where
  toJSVal = return . unSVGStyleElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGStyleElement where
  fromJSVal v = fmap SVGStyleElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGStyleElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGStyleElement where
  makeObject = makeObject . unSVGStyleElement

instance IsSVGElement SVGStyleElement
instance IsElement SVGStyleElement
instance IsNode SVGStyleElement
instance IsEventTarget SVGStyleElement
instance IsSlotable SVGStyleElement
instance IsParentNode SVGStyleElement
instance IsNonDocumentTypeChildNode SVGStyleElement
instance IsDocumentAndElementEventHandlers SVGStyleElement
instance IsChildNode SVGStyleElement
instance IsAnimatable SVGStyleElement
instance IsGlobalEventHandlers SVGStyleElement
instance IsElementCSSInlineStyle SVGStyleElement
instance IsGObject SVGStyleElement where
  typeGType _ = gTypeSVGStyleElement
  {-# INLINE typeGType #-}

noSVGStyleElement :: Maybe SVGStyleElement
noSVGStyleElement = Nothing
{-# INLINE noSVGStyleElement #-}

gTypeSVGStyleElement :: JSM GType
gTypeSVGStyleElement = GType . Object <$> jsg "SVGStyleElement"

-- | Functions for this inteface are in "JSDOM.SVGSwitchElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGSwitchElement Mozilla SVGSwitchElement documentation>
newtype SVGSwitchElement = SVGSwitchElement { unSVGSwitchElement :: JSVal }

instance PToJSVal SVGSwitchElement where
  pToJSVal = unSVGSwitchElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGSwitchElement where
  pFromJSVal = SVGSwitchElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGSwitchElement where
  toJSVal = return . unSVGSwitchElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGSwitchElement where
  fromJSVal v = fmap SVGSwitchElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGSwitchElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGSwitchElement where
  makeObject = makeObject . unSVGSwitchElement

instance IsSVGGraphicsElement SVGSwitchElement
instance IsSVGElement SVGSwitchElement
instance IsElement SVGSwitchElement
instance IsNode SVGSwitchElement
instance IsEventTarget SVGSwitchElement
instance IsSlotable SVGSwitchElement
instance IsParentNode SVGSwitchElement
instance IsNonDocumentTypeChildNode SVGSwitchElement
instance IsDocumentAndElementEventHandlers SVGSwitchElement
instance IsChildNode SVGSwitchElement
instance IsAnimatable SVGSwitchElement
instance IsGlobalEventHandlers SVGSwitchElement
instance IsElementCSSInlineStyle SVGSwitchElement
instance IsSVGTests SVGSwitchElement
instance IsSVGExternalResourcesRequired SVGSwitchElement
instance IsGObject SVGSwitchElement where
  typeGType _ = gTypeSVGSwitchElement
  {-# INLINE typeGType #-}

noSVGSwitchElement :: Maybe SVGSwitchElement
noSVGSwitchElement = Nothing
{-# INLINE noSVGSwitchElement #-}

gTypeSVGSwitchElement :: JSM GType
gTypeSVGSwitchElement = GType . Object <$> jsg "SVGSwitchElement"

-- | Functions for this inteface are in "JSDOM.SVGSymbolElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGFitToViewBox"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGSymbolElement Mozilla SVGSymbolElement documentation>
newtype SVGSymbolElement = SVGSymbolElement { unSVGSymbolElement :: JSVal }

instance PToJSVal SVGSymbolElement where
  pToJSVal = unSVGSymbolElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGSymbolElement where
  pFromJSVal = SVGSymbolElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGSymbolElement where
  toJSVal = return . unSVGSymbolElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGSymbolElement where
  fromJSVal v = fmap SVGSymbolElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGSymbolElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGSymbolElement where
  makeObject = makeObject . unSVGSymbolElement

instance IsSVGElement SVGSymbolElement
instance IsElement SVGSymbolElement
instance IsNode SVGSymbolElement
instance IsEventTarget SVGSymbolElement
instance IsSlotable SVGSymbolElement
instance IsParentNode SVGSymbolElement
instance IsNonDocumentTypeChildNode SVGSymbolElement
instance IsDocumentAndElementEventHandlers SVGSymbolElement
instance IsChildNode SVGSymbolElement
instance IsAnimatable SVGSymbolElement
instance IsGlobalEventHandlers SVGSymbolElement
instance IsElementCSSInlineStyle SVGSymbolElement
instance IsSVGFitToViewBox SVGSymbolElement
instance IsSVGExternalResourcesRequired SVGSymbolElement
instance IsGObject SVGSymbolElement where
  typeGType _ = gTypeSVGSymbolElement
  {-# INLINE typeGType #-}

noSVGSymbolElement :: Maybe SVGSymbolElement
noSVGSymbolElement = Nothing
{-# INLINE noSVGSymbolElement #-}

gTypeSVGSymbolElement :: JSM GType
gTypeSVGSymbolElement = GType . Object <$> jsg "SVGSymbolElement"

-- | Functions for this inteface are in "JSDOM.SVGTRefElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextPositioningElement"
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--     * "JSDOM.SVGURIReference"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTRefElement Mozilla SVGTRefElement documentation>
newtype SVGTRefElement = SVGTRefElement { unSVGTRefElement :: JSVal }

instance PToJSVal SVGTRefElement where
  pToJSVal = unSVGTRefElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTRefElement where
  pFromJSVal = SVGTRefElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTRefElement where
  toJSVal = return . unSVGTRefElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTRefElement where
  fromJSVal v = fmap SVGTRefElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTRefElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTRefElement where
  makeObject = makeObject . unSVGTRefElement

instance IsSVGTextPositioningElement SVGTRefElement
instance IsSVGTextContentElement SVGTRefElement
instance IsSVGGraphicsElement SVGTRefElement
instance IsSVGElement SVGTRefElement
instance IsElement SVGTRefElement
instance IsNode SVGTRefElement
instance IsEventTarget SVGTRefElement
instance IsSlotable SVGTRefElement
instance IsParentNode SVGTRefElement
instance IsNonDocumentTypeChildNode SVGTRefElement
instance IsDocumentAndElementEventHandlers SVGTRefElement
instance IsChildNode SVGTRefElement
instance IsAnimatable SVGTRefElement
instance IsGlobalEventHandlers SVGTRefElement
instance IsElementCSSInlineStyle SVGTRefElement
instance IsSVGTests SVGTRefElement
instance IsSVGExternalResourcesRequired SVGTRefElement
instance IsSVGURIReference SVGTRefElement
instance IsGObject SVGTRefElement where
  typeGType _ = gTypeSVGTRefElement
  {-# INLINE typeGType #-}

noSVGTRefElement :: Maybe SVGTRefElement
noSVGTRefElement = Nothing
{-# INLINE noSVGTRefElement #-}

gTypeSVGTRefElement :: JSM GType
gTypeSVGTRefElement = GType . Object <$> jsg "SVGTRefElement"

-- | Functions for this inteface are in "JSDOM.SVGTSpanElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextPositioningElement"
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTSpanElement Mozilla SVGTSpanElement documentation>
newtype SVGTSpanElement = SVGTSpanElement { unSVGTSpanElement :: JSVal }

instance PToJSVal SVGTSpanElement where
  pToJSVal = unSVGTSpanElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTSpanElement where
  pFromJSVal = SVGTSpanElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTSpanElement where
  toJSVal = return . unSVGTSpanElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTSpanElement where
  fromJSVal v = fmap SVGTSpanElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTSpanElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTSpanElement where
  makeObject = makeObject . unSVGTSpanElement

instance IsSVGTextPositioningElement SVGTSpanElement
instance IsSVGTextContentElement SVGTSpanElement
instance IsSVGGraphicsElement SVGTSpanElement
instance IsSVGElement SVGTSpanElement
instance IsElement SVGTSpanElement
instance IsNode SVGTSpanElement
instance IsEventTarget SVGTSpanElement
instance IsSlotable SVGTSpanElement
instance IsParentNode SVGTSpanElement
instance IsNonDocumentTypeChildNode SVGTSpanElement
instance IsDocumentAndElementEventHandlers SVGTSpanElement
instance IsChildNode SVGTSpanElement
instance IsAnimatable SVGTSpanElement
instance IsGlobalEventHandlers SVGTSpanElement
instance IsElementCSSInlineStyle SVGTSpanElement
instance IsSVGTests SVGTSpanElement
instance IsSVGExternalResourcesRequired SVGTSpanElement
instance IsGObject SVGTSpanElement where
  typeGType _ = gTypeSVGTSpanElement
  {-# INLINE typeGType #-}

noSVGTSpanElement :: Maybe SVGTSpanElement
noSVGTSpanElement = Nothing
{-# INLINE noSVGTSpanElement #-}

gTypeSVGTSpanElement :: JSM GType
gTypeSVGTSpanElement = GType . Object <$> jsg "SVGTSpanElement"

-- | Functions for this inteface are in "JSDOM.SVGTests".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTests Mozilla SVGTests documentation>
newtype SVGTests = SVGTests { unSVGTests :: JSVal }

instance PToJSVal SVGTests where
  pToJSVal = unSVGTests
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTests where
  pFromJSVal = SVGTests
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTests where
  toJSVal = return . unSVGTests
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTests where
  fromJSVal v = fmap SVGTests <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTests
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTests where
  makeObject = makeObject . unSVGTests

class (IsGObject o) => IsSVGTests o
toSVGTests :: IsSVGTests o => o -> SVGTests
toSVGTests = SVGTests . coerce

instance IsSVGTests SVGTests
instance IsGObject SVGTests where
  typeGType _ = gTypeSVGTests
  {-# INLINE typeGType #-}

noSVGTests :: Maybe SVGTests
noSVGTests = Nothing
{-# INLINE noSVGTests #-}

gTypeSVGTests :: JSM GType
gTypeSVGTests = GType . Object <$> jsg "SVGTests"

-- | Functions for this inteface are in "JSDOM.SVGTextContentElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextContentElement Mozilla SVGTextContentElement documentation>
newtype SVGTextContentElement = SVGTextContentElement { unSVGTextContentElement :: JSVal }

instance PToJSVal SVGTextContentElement where
  pToJSVal = unSVGTextContentElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTextContentElement where
  pFromJSVal = SVGTextContentElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTextContentElement where
  toJSVal = return . unSVGTextContentElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTextContentElement where
  fromJSVal v = fmap SVGTextContentElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTextContentElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTextContentElement where
  makeObject = makeObject . unSVGTextContentElement

class (IsSVGGraphicsElement o, IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsSVGTests o, IsSVGExternalResourcesRequired o, IsGObject o) => IsSVGTextContentElement o
toSVGTextContentElement :: IsSVGTextContentElement o => o -> SVGTextContentElement
toSVGTextContentElement = SVGTextContentElement . coerce

instance IsSVGTextContentElement SVGTextContentElement
instance IsSVGGraphicsElement SVGTextContentElement
instance IsSVGElement SVGTextContentElement
instance IsElement SVGTextContentElement
instance IsNode SVGTextContentElement
instance IsEventTarget SVGTextContentElement
instance IsSlotable SVGTextContentElement
instance IsParentNode SVGTextContentElement
instance IsNonDocumentTypeChildNode SVGTextContentElement
instance IsDocumentAndElementEventHandlers SVGTextContentElement
instance IsChildNode SVGTextContentElement
instance IsAnimatable SVGTextContentElement
instance IsGlobalEventHandlers SVGTextContentElement
instance IsElementCSSInlineStyle SVGTextContentElement
instance IsSVGTests SVGTextContentElement
instance IsSVGExternalResourcesRequired SVGTextContentElement
instance IsGObject SVGTextContentElement where
  typeGType _ = gTypeSVGTextContentElement
  {-# INLINE typeGType #-}

noSVGTextContentElement :: Maybe SVGTextContentElement
noSVGTextContentElement = Nothing
{-# INLINE noSVGTextContentElement #-}

gTypeSVGTextContentElement :: JSM GType
gTypeSVGTextContentElement = GType . Object <$> jsg "SVGTextContentElement"

-- | Functions for this inteface are in "JSDOM.SVGTextElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextPositioningElement"
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextElement Mozilla SVGTextElement documentation>
newtype SVGTextElement = SVGTextElement { unSVGTextElement :: JSVal }

instance PToJSVal SVGTextElement where
  pToJSVal = unSVGTextElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTextElement where
  pFromJSVal = SVGTextElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTextElement where
  toJSVal = return . unSVGTextElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTextElement where
  fromJSVal v = fmap SVGTextElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTextElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTextElement where
  makeObject = makeObject . unSVGTextElement

instance IsSVGTextPositioningElement SVGTextElement
instance IsSVGTextContentElement SVGTextElement
instance IsSVGGraphicsElement SVGTextElement
instance IsSVGElement SVGTextElement
instance IsElement SVGTextElement
instance IsNode SVGTextElement
instance IsEventTarget SVGTextElement
instance IsSlotable SVGTextElement
instance IsParentNode SVGTextElement
instance IsNonDocumentTypeChildNode SVGTextElement
instance IsDocumentAndElementEventHandlers SVGTextElement
instance IsChildNode SVGTextElement
instance IsAnimatable SVGTextElement
instance IsGlobalEventHandlers SVGTextElement
instance IsElementCSSInlineStyle SVGTextElement
instance IsSVGTests SVGTextElement
instance IsSVGExternalResourcesRequired SVGTextElement
instance IsGObject SVGTextElement where
  typeGType _ = gTypeSVGTextElement
  {-# INLINE typeGType #-}

noSVGTextElement :: Maybe SVGTextElement
noSVGTextElement = Nothing
{-# INLINE noSVGTextElement #-}

gTypeSVGTextElement :: JSM GType
gTypeSVGTextElement = GType . Object <$> jsg "SVGTextElement"

-- | Functions for this inteface are in "JSDOM.SVGTextPathElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--     * "JSDOM.SVGURIReference"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPathElement Mozilla SVGTextPathElement documentation>
newtype SVGTextPathElement = SVGTextPathElement { unSVGTextPathElement :: JSVal }

instance PToJSVal SVGTextPathElement where
  pToJSVal = unSVGTextPathElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTextPathElement where
  pFromJSVal = SVGTextPathElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTextPathElement where
  toJSVal = return . unSVGTextPathElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTextPathElement where
  fromJSVal v = fmap SVGTextPathElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTextPathElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTextPathElement where
  makeObject = makeObject . unSVGTextPathElement

instance IsSVGTextContentElement SVGTextPathElement
instance IsSVGGraphicsElement SVGTextPathElement
instance IsSVGElement SVGTextPathElement
instance IsElement SVGTextPathElement
instance IsNode SVGTextPathElement
instance IsEventTarget SVGTextPathElement
instance IsSlotable SVGTextPathElement
instance IsParentNode SVGTextPathElement
instance IsNonDocumentTypeChildNode SVGTextPathElement
instance IsDocumentAndElementEventHandlers SVGTextPathElement
instance IsChildNode SVGTextPathElement
instance IsAnimatable SVGTextPathElement
instance IsGlobalEventHandlers SVGTextPathElement
instance IsElementCSSInlineStyle SVGTextPathElement
instance IsSVGTests SVGTextPathElement
instance IsSVGExternalResourcesRequired SVGTextPathElement
instance IsSVGURIReference SVGTextPathElement
instance IsGObject SVGTextPathElement where
  typeGType _ = gTypeSVGTextPathElement
  {-# INLINE typeGType #-}

noSVGTextPathElement :: Maybe SVGTextPathElement
noSVGTextPathElement = Nothing
{-# INLINE noSVGTextPathElement #-}

gTypeSVGTextPathElement :: JSM GType
gTypeSVGTextPathElement = GType . Object <$> jsg "SVGTextPathElement"

-- | Functions for this inteface are in "JSDOM.SVGTextPositioningElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGTextContentElement"
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTextPositioningElement Mozilla SVGTextPositioningElement documentation>
newtype SVGTextPositioningElement = SVGTextPositioningElement { unSVGTextPositioningElement :: JSVal }

instance PToJSVal SVGTextPositioningElement where
  pToJSVal = unSVGTextPositioningElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTextPositioningElement where
  pFromJSVal = SVGTextPositioningElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTextPositioningElement where
  toJSVal = return . unSVGTextPositioningElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTextPositioningElement where
  fromJSVal v = fmap SVGTextPositioningElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTextPositioningElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTextPositioningElement where
  makeObject = makeObject . unSVGTextPositioningElement

class (IsSVGTextContentElement o, IsSVGGraphicsElement o, IsSVGElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsSVGTests o, IsSVGExternalResourcesRequired o, IsGObject o) => IsSVGTextPositioningElement o
toSVGTextPositioningElement :: IsSVGTextPositioningElement o => o -> SVGTextPositioningElement
toSVGTextPositioningElement = SVGTextPositioningElement . coerce

instance IsSVGTextPositioningElement SVGTextPositioningElement
instance IsSVGTextContentElement SVGTextPositioningElement
instance IsSVGGraphicsElement SVGTextPositioningElement
instance IsSVGElement SVGTextPositioningElement
instance IsElement SVGTextPositioningElement
instance IsNode SVGTextPositioningElement
instance IsEventTarget SVGTextPositioningElement
instance IsSlotable SVGTextPositioningElement
instance IsParentNode SVGTextPositioningElement
instance IsNonDocumentTypeChildNode SVGTextPositioningElement
instance IsDocumentAndElementEventHandlers SVGTextPositioningElement
instance IsChildNode SVGTextPositioningElement
instance IsAnimatable SVGTextPositioningElement
instance IsGlobalEventHandlers SVGTextPositioningElement
instance IsElementCSSInlineStyle SVGTextPositioningElement
instance IsSVGTests SVGTextPositioningElement
instance IsSVGExternalResourcesRequired SVGTextPositioningElement
instance IsGObject SVGTextPositioningElement where
  typeGType _ = gTypeSVGTextPositioningElement
  {-# INLINE typeGType #-}

noSVGTextPositioningElement :: Maybe SVGTextPositioningElement
noSVGTextPositioningElement = Nothing
{-# INLINE noSVGTextPositioningElement #-}

gTypeSVGTextPositioningElement :: JSM GType
gTypeSVGTextPositioningElement = GType . Object <$> jsg "SVGTextPositioningElement"

-- | Functions for this inteface are in "JSDOM.SVGTitleElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTitleElement Mozilla SVGTitleElement documentation>
newtype SVGTitleElement = SVGTitleElement { unSVGTitleElement :: JSVal }

instance PToJSVal SVGTitleElement where
  pToJSVal = unSVGTitleElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTitleElement where
  pFromJSVal = SVGTitleElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTitleElement where
  toJSVal = return . unSVGTitleElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTitleElement where
  fromJSVal v = fmap SVGTitleElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTitleElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTitleElement where
  makeObject = makeObject . unSVGTitleElement

instance IsSVGElement SVGTitleElement
instance IsElement SVGTitleElement
instance IsNode SVGTitleElement
instance IsEventTarget SVGTitleElement
instance IsSlotable SVGTitleElement
instance IsParentNode SVGTitleElement
instance IsNonDocumentTypeChildNode SVGTitleElement
instance IsDocumentAndElementEventHandlers SVGTitleElement
instance IsChildNode SVGTitleElement
instance IsAnimatable SVGTitleElement
instance IsGlobalEventHandlers SVGTitleElement
instance IsElementCSSInlineStyle SVGTitleElement
instance IsGObject SVGTitleElement where
  typeGType _ = gTypeSVGTitleElement
  {-# INLINE typeGType #-}

noSVGTitleElement :: Maybe SVGTitleElement
noSVGTitleElement = Nothing
{-# INLINE noSVGTitleElement #-}

gTypeSVGTitleElement :: JSM GType
gTypeSVGTitleElement = GType . Object <$> jsg "SVGTitleElement"

-- | Functions for this inteface are in "JSDOM.SVGTransform".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTransform Mozilla SVGTransform documentation>
newtype SVGTransform = SVGTransform { unSVGTransform :: JSVal }

instance PToJSVal SVGTransform where
  pToJSVal = unSVGTransform
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTransform where
  pFromJSVal = SVGTransform
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTransform where
  toJSVal = return . unSVGTransform
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTransform where
  fromJSVal v = fmap SVGTransform <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTransform
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTransform where
  makeObject = makeObject . unSVGTransform

instance IsGObject SVGTransform where
  typeGType _ = gTypeSVGTransform
  {-# INLINE typeGType #-}

noSVGTransform :: Maybe SVGTransform
noSVGTransform = Nothing
{-# INLINE noSVGTransform #-}

gTypeSVGTransform :: JSM GType
gTypeSVGTransform = GType . Object <$> jsg "SVGTransform"

-- | Functions for this inteface are in "JSDOM.SVGTransformList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGTransformList Mozilla SVGTransformList documentation>
newtype SVGTransformList = SVGTransformList { unSVGTransformList :: JSVal }

instance PToJSVal SVGTransformList where
  pToJSVal = unSVGTransformList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGTransformList where
  pFromJSVal = SVGTransformList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGTransformList where
  toJSVal = return . unSVGTransformList
  {-# INLINE toJSVal #-}

instance FromJSVal SVGTransformList where
  fromJSVal v = fmap SVGTransformList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGTransformList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGTransformList where
  makeObject = makeObject . unSVGTransformList

instance IsGObject SVGTransformList where
  typeGType _ = gTypeSVGTransformList
  {-# INLINE typeGType #-}

noSVGTransformList :: Maybe SVGTransformList
noSVGTransformList = Nothing
{-# INLINE noSVGTransformList #-}

gTypeSVGTransformList :: JSM GType
gTypeSVGTransformList = GType . Object <$> jsg "SVGTransformList"

-- | Functions for this inteface are in "JSDOM.SVGURIReference".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGURIReference Mozilla SVGURIReference documentation>
newtype SVGURIReference = SVGURIReference { unSVGURIReference :: JSVal }

instance PToJSVal SVGURIReference where
  pToJSVal = unSVGURIReference
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGURIReference where
  pFromJSVal = SVGURIReference
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGURIReference where
  toJSVal = return . unSVGURIReference
  {-# INLINE toJSVal #-}

instance FromJSVal SVGURIReference where
  fromJSVal v = fmap SVGURIReference <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGURIReference
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGURIReference where
  makeObject = makeObject . unSVGURIReference

class (IsGObject o) => IsSVGURIReference o
toSVGURIReference :: IsSVGURIReference o => o -> SVGURIReference
toSVGURIReference = SVGURIReference . coerce

instance IsSVGURIReference SVGURIReference
instance IsGObject SVGURIReference where
  typeGType _ = gTypeSVGURIReference
  {-# INLINE typeGType #-}

noSVGURIReference :: Maybe SVGURIReference
noSVGURIReference = Nothing
{-# INLINE noSVGURIReference #-}

gTypeSVGURIReference :: JSM GType
gTypeSVGURIReference = GType . Object <$> jsg "SVGURIReference"

-- | Functions for this inteface are in "JSDOM.SVGUnitTypes".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGUnitTypes Mozilla SVGUnitTypes documentation>
newtype SVGUnitTypes = SVGUnitTypes { unSVGUnitTypes :: JSVal }

instance PToJSVal SVGUnitTypes where
  pToJSVal = unSVGUnitTypes
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGUnitTypes where
  pFromJSVal = SVGUnitTypes
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGUnitTypes where
  toJSVal = return . unSVGUnitTypes
  {-# INLINE toJSVal #-}

instance FromJSVal SVGUnitTypes where
  fromJSVal v = fmap SVGUnitTypes <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGUnitTypes
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGUnitTypes where
  makeObject = makeObject . unSVGUnitTypes

instance IsGObject SVGUnitTypes where
  typeGType _ = gTypeSVGUnitTypes
  {-# INLINE typeGType #-}

noSVGUnitTypes :: Maybe SVGUnitTypes
noSVGUnitTypes = Nothing
{-# INLINE noSVGUnitTypes #-}

gTypeSVGUnitTypes :: JSM GType
gTypeSVGUnitTypes = GType . Object <$> jsg "SVGUnitTypes"

-- | Functions for this inteface are in "JSDOM.SVGUseElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGGraphicsElement"
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGTests"
--     * "JSDOM.SVGURIReference"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGUseElement Mozilla SVGUseElement documentation>
newtype SVGUseElement = SVGUseElement { unSVGUseElement :: JSVal }

instance PToJSVal SVGUseElement where
  pToJSVal = unSVGUseElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGUseElement where
  pFromJSVal = SVGUseElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGUseElement where
  toJSVal = return . unSVGUseElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGUseElement where
  fromJSVal v = fmap SVGUseElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGUseElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGUseElement where
  makeObject = makeObject . unSVGUseElement

instance IsSVGGraphicsElement SVGUseElement
instance IsSVGElement SVGUseElement
instance IsElement SVGUseElement
instance IsNode SVGUseElement
instance IsEventTarget SVGUseElement
instance IsSlotable SVGUseElement
instance IsParentNode SVGUseElement
instance IsNonDocumentTypeChildNode SVGUseElement
instance IsDocumentAndElementEventHandlers SVGUseElement
instance IsChildNode SVGUseElement
instance IsAnimatable SVGUseElement
instance IsGlobalEventHandlers SVGUseElement
instance IsElementCSSInlineStyle SVGUseElement
instance IsSVGTests SVGUseElement
instance IsSVGURIReference SVGUseElement
instance IsSVGExternalResourcesRequired SVGUseElement
instance IsGObject SVGUseElement where
  typeGType _ = gTypeSVGUseElement
  {-# INLINE typeGType #-}

noSVGUseElement :: Maybe SVGUseElement
noSVGUseElement = Nothing
{-# INLINE noSVGUseElement #-}

gTypeSVGUseElement :: JSM GType
gTypeSVGUseElement = GType . Object <$> jsg "SVGUseElement"

-- | Functions for this inteface are in "JSDOM.SVGVKernElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGVKernElement Mozilla SVGVKernElement documentation>
newtype SVGVKernElement = SVGVKernElement { unSVGVKernElement :: JSVal }

instance PToJSVal SVGVKernElement where
  pToJSVal = unSVGVKernElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGVKernElement where
  pFromJSVal = SVGVKernElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGVKernElement where
  toJSVal = return . unSVGVKernElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGVKernElement where
  fromJSVal v = fmap SVGVKernElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGVKernElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGVKernElement where
  makeObject = makeObject . unSVGVKernElement

instance IsSVGElement SVGVKernElement
instance IsElement SVGVKernElement
instance IsNode SVGVKernElement
instance IsEventTarget SVGVKernElement
instance IsSlotable SVGVKernElement
instance IsParentNode SVGVKernElement
instance IsNonDocumentTypeChildNode SVGVKernElement
instance IsDocumentAndElementEventHandlers SVGVKernElement
instance IsChildNode SVGVKernElement
instance IsAnimatable SVGVKernElement
instance IsGlobalEventHandlers SVGVKernElement
instance IsElementCSSInlineStyle SVGVKernElement
instance IsGObject SVGVKernElement where
  typeGType _ = gTypeSVGVKernElement
  {-# INLINE typeGType #-}

noSVGVKernElement :: Maybe SVGVKernElement
noSVGVKernElement = Nothing
{-# INLINE noSVGVKernElement #-}

gTypeSVGVKernElement :: JSM GType
gTypeSVGVKernElement = GType . Object <$> jsg "SVGVKernElement"

-- | Functions for this inteface are in "JSDOM.SVGViewElement".
-- Base interface functions are in:
--
--     * "JSDOM.SVGElement"
--     * "JSDOM.Element"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.ElementCSSInlineStyle"
--     * "JSDOM.SVGZoomAndPan"
--     * "JSDOM.SVGFitToViewBox"
--     * "JSDOM.SVGExternalResourcesRequired"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGViewElement Mozilla SVGViewElement documentation>
newtype SVGViewElement = SVGViewElement { unSVGViewElement :: JSVal }

instance PToJSVal SVGViewElement where
  pToJSVal = unSVGViewElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGViewElement where
  pFromJSVal = SVGViewElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGViewElement where
  toJSVal = return . unSVGViewElement
  {-# INLINE toJSVal #-}

instance FromJSVal SVGViewElement where
  fromJSVal v = fmap SVGViewElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGViewElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGViewElement where
  makeObject = makeObject . unSVGViewElement

instance IsSVGElement SVGViewElement
instance IsElement SVGViewElement
instance IsNode SVGViewElement
instance IsEventTarget SVGViewElement
instance IsSlotable SVGViewElement
instance IsParentNode SVGViewElement
instance IsNonDocumentTypeChildNode SVGViewElement
instance IsDocumentAndElementEventHandlers SVGViewElement
instance IsChildNode SVGViewElement
instance IsAnimatable SVGViewElement
instance IsGlobalEventHandlers SVGViewElement
instance IsElementCSSInlineStyle SVGViewElement
instance IsSVGZoomAndPan SVGViewElement
instance IsSVGFitToViewBox SVGViewElement
instance IsSVGExternalResourcesRequired SVGViewElement
instance IsGObject SVGViewElement where
  typeGType _ = gTypeSVGViewElement
  {-# INLINE typeGType #-}

noSVGViewElement :: Maybe SVGViewElement
noSVGViewElement = Nothing
{-# INLINE noSVGViewElement #-}

gTypeSVGViewElement :: JSM GType
gTypeSVGViewElement = GType . Object <$> jsg "SVGViewElement"

-- | Functions for this inteface are in "JSDOM.SVGViewSpec".
-- Base interface functions are in:
--
--     * "JSDOM.SVGFitToViewBox"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGViewSpec Mozilla SVGViewSpec documentation>
newtype SVGViewSpec = SVGViewSpec { unSVGViewSpec :: JSVal }

instance PToJSVal SVGViewSpec where
  pToJSVal = unSVGViewSpec
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGViewSpec where
  pFromJSVal = SVGViewSpec
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGViewSpec where
  toJSVal = return . unSVGViewSpec
  {-# INLINE toJSVal #-}

instance FromJSVal SVGViewSpec where
  fromJSVal v = fmap SVGViewSpec <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGViewSpec
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGViewSpec where
  makeObject = makeObject . unSVGViewSpec

instance IsSVGFitToViewBox SVGViewSpec
instance IsGObject SVGViewSpec where
  typeGType _ = gTypeSVGViewSpec
  {-# INLINE typeGType #-}

noSVGViewSpec :: Maybe SVGViewSpec
noSVGViewSpec = Nothing
{-# INLINE noSVGViewSpec #-}

gTypeSVGViewSpec :: JSM GType
gTypeSVGViewSpec = GType . Object <$> jsg "SVGViewSpec"

-- | Functions for this inteface are in "JSDOM.SVGZoomAndPan".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGZoomAndPan Mozilla SVGZoomAndPan documentation>
newtype SVGZoomAndPan = SVGZoomAndPan { unSVGZoomAndPan :: JSVal }

instance PToJSVal SVGZoomAndPan where
  pToJSVal = unSVGZoomAndPan
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGZoomAndPan where
  pFromJSVal = SVGZoomAndPan
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGZoomAndPan where
  toJSVal = return . unSVGZoomAndPan
  {-# INLINE toJSVal #-}

instance FromJSVal SVGZoomAndPan where
  fromJSVal v = fmap SVGZoomAndPan <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGZoomAndPan
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGZoomAndPan where
  makeObject = makeObject . unSVGZoomAndPan

class (IsGObject o) => IsSVGZoomAndPan o
toSVGZoomAndPan :: IsSVGZoomAndPan o => o -> SVGZoomAndPan
toSVGZoomAndPan = SVGZoomAndPan . coerce

instance IsSVGZoomAndPan SVGZoomAndPan
instance IsGObject SVGZoomAndPan where
  typeGType _ = gTypeSVGZoomAndPan
  {-# INLINE typeGType #-}

noSVGZoomAndPan :: Maybe SVGZoomAndPan
noSVGZoomAndPan = Nothing
{-# INLINE noSVGZoomAndPan #-}

gTypeSVGZoomAndPan :: JSM GType
gTypeSVGZoomAndPan = GType . Object <$> jsg "SVGZoomAndPan"

-- | Functions for this inteface are in "JSDOM.SVGZoomEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SVGZoomEvent Mozilla SVGZoomEvent documentation>
newtype SVGZoomEvent = SVGZoomEvent { unSVGZoomEvent :: JSVal }

instance PToJSVal SVGZoomEvent where
  pToJSVal = unSVGZoomEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal SVGZoomEvent where
  pFromJSVal = SVGZoomEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal SVGZoomEvent where
  toJSVal = return . unSVGZoomEvent
  {-# INLINE toJSVal #-}

instance FromJSVal SVGZoomEvent where
  fromJSVal v = fmap SVGZoomEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SVGZoomEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SVGZoomEvent where
  makeObject = makeObject . unSVGZoomEvent

instance IsUIEvent SVGZoomEvent
instance IsEvent SVGZoomEvent
instance IsGObject SVGZoomEvent where
  typeGType _ = gTypeSVGZoomEvent
  {-# INLINE typeGType #-}

noSVGZoomEvent :: Maybe SVGZoomEvent
noSVGZoomEvent = Nothing
{-# INLINE noSVGZoomEvent #-}

gTypeSVGZoomEvent :: JSM GType
gTypeSVGZoomEvent = GType . Object <$> jsg "SVGZoomEvent"
