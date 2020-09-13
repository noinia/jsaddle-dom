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
module JSDOM.Types (
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

  -- * Interface types from IDL files

-- AUTO GENERATION STARTS HERE
  , AddEventListenerOptionsOrBool(AddEventListenerOptionsOrBool), unAddEventListenerOptionsOrBool, IsAddEventListenerOptionsOrBool, toAddEventListenerOptionsOrBool
  , BinaryData(BinaryData), unBinaryData, IsBinaryData, toBinaryData
  , BlobPart(BlobPart), unBlobPart, IsBlobPart
  , BodyInit(BodyInit), unBodyInit, IsBodyInit
  , BufferDataSource(BufferDataSource), unBufferDataSource, IsBufferDataSource, toBufferDataSource
  , BufferSource(BufferSource), unBufferSource, IsBufferSource, toBufferSource
  , CanvasImageSource(CanvasImageSource), unCanvasImageSource, IsCanvasImageSource, toCanvasImageSource
  , CanvasStyle(CanvasStyle), unCanvasStyle, IsCanvasStyle
  , CredentialBodyType(CredentialBodyType), unCredentialBodyType, IsCredentialBodyType, toCredentialBodyType
  , CryptoKeyOrKeyPair(CryptoKeyOrKeyPair), unCryptoKeyOrKeyPair, IsCryptoKeyOrKeyPair, toCryptoKeyOrKeyPair
  , EventListenerOptionsOrBool(EventListenerOptionsOrBool), unEventListenerOptionsOrBool, IsEventListenerOptionsOrBool, toEventListenerOptionsOrBool
  , Float32List(Float32List), unFloat32List, IsFloat32List
  , HTMLCollectionOrElement(HTMLCollectionOrElement), unHTMLCollectionOrElement, IsHTMLCollectionOrElement, toHTMLCollectionOrElement
  , HTMLElementOrLong(HTMLElementOrLong), unHTMLElementOrLong, IsHTMLElementOrLong
  , HTMLOptionElementOrGroup(HTMLOptionElementOrGroup), unHTMLOptionElementOrGroup, IsHTMLOptionElementOrGroup, toHTMLOptionElementOrGroup
  , IDBCursorSource(IDBCursorSource), unIDBCursorSource, IsIDBCursorSource, toIDBCursorSource
  , IDBKeyPath(IDBKeyPath), unIDBKeyPath, IsIDBKeyPath
  , IDBRequestResult(IDBRequestResult), unIDBRequestResult, IsIDBRequestResult, toIDBRequestResult
  , IDBRequestSource(IDBRequestSource), unIDBRequestSource, IsIDBRequestSource, toIDBRequestSource
  , Int32List(Int32List), unInt32List, IsInt32List
  , KeyData(KeyData), unKeyData, IsKeyData, toKeyData
  , MediaProvider(MediaProvider), unMediaProvider, IsMediaProvider, toMediaProvider
  , MediaStreamTrackOrKind(MediaStreamTrackOrKind), unMediaStreamTrackOrKind, IsMediaStreamTrackOrKind
  , MessageEventSource(MessageEventSource), unMessageEventSource, IsMessageEventSource, toMessageEventSource
  , NodeOrString(NodeOrString), unNodeOrString, IsNodeOrString
  , RTCIceCandidateOrInit(RTCIceCandidateOrInit), unRTCIceCandidateOrInit, IsRTCIceCandidateOrInit, toRTCIceCandidateOrInit
  , RadioNodeListOrElement(RadioNodeListOrElement), unRadioNodeListOrElement, IsRadioNodeListOrElement, toRadioNodeListOrElement
  , RenderingContext(RenderingContext), unRenderingContext, IsRenderingContext, toRenderingContext
  , SQLValue(SQLValue), unSQLValue, IsSQLValue
  , StringOrArrayBuffer(StringOrArrayBuffer), unStringOrArrayBuffer, IsStringOrArrayBuffer
  , StringOrBinaryData(StringOrBinaryData), unStringOrBinaryData, IsStringOrBinaryData
  , StringOrStrings(StringOrStrings), unStringOrStrings, IsStringOrStrings
  , TexImageSource(TexImageSource), unTexImageSource, IsTexImageSource, toTexImageSource
  , Track(Track), unTrack, IsTrack, toTrack
  , URLSearchParamsInit(URLSearchParamsInit), unURLSearchParamsInit, IsURLSearchParamsInit
  , XMLHttpRequestBody(XMLHttpRequestBody), unXMLHttpRequestBody, IsXMLHttpRequestBody

  , ANGLEInstancedArrays(ANGLEInstancedArrays), unANGLEInstancedArrays, noANGLEInstancedArrays, gTypeANGLEInstancedArrays
  , AbstractWorker(AbstractWorker), unAbstractWorker, IsAbstractWorker, toAbstractWorker, noAbstractWorker, gTypeAbstractWorker
  , Acceleration(Acceleration), unAcceleration, noAcceleration, gTypeAcceleration
  , AddEventListenerOptions(AddEventListenerOptions), unAddEventListenerOptions, noAddEventListenerOptions, gTypeAddEventListenerOptions
  , AesCbcCfbParams(AesCbcCfbParams), unAesCbcCfbParams, noAesCbcCfbParams, gTypeAesCbcCfbParams
  , AesCtrParams(AesCtrParams), unAesCtrParams, noAesCtrParams, gTypeAesCtrParams
  , AesGcmParams(AesGcmParams), unAesGcmParams, noAesGcmParams, gTypeAesGcmParams
  , AesKeyParams(AesKeyParams), unAesKeyParams, noAesKeyParams, gTypeAesKeyParams
  , AnalyserNode(AnalyserNode), unAnalyserNode, noAnalyserNode, gTypeAnalyserNode
  , Animatable(Animatable), unAnimatable, IsAnimatable, toAnimatable, noAnimatable, gTypeAnimatable
  , Animation(Animation), unAnimation, noAnimation, gTypeAnimation
  , AnimationEffect(AnimationEffect), unAnimationEffect, IsAnimationEffect, toAnimationEffect, noAnimationEffect, gTypeAnimationEffect
  , AnimationEvent(AnimationEvent), unAnimationEvent, noAnimationEvent, gTypeAnimationEvent
  , AnimationEventInit(AnimationEventInit), unAnimationEventInit, noAnimationEventInit, gTypeAnimationEventInit
  , AnimationTimeline(AnimationTimeline), unAnimationTimeline, IsAnimationTimeline, toAnimationTimeline, noAnimationTimeline, gTypeAnimationTimeline
  , ApplePayError(ApplePayError), unApplePayError, noApplePayError, gTypeApplePayError
  , ApplePayLineItem(ApplePayLineItem), unApplePayLineItem, noApplePayLineItem, gTypeApplePayLineItem
  , ApplePayPayment(ApplePayPayment), unApplePayPayment, noApplePayPayment, gTypeApplePayPayment
  , ApplePayPaymentAuthorizationResult(ApplePayPaymentAuthorizationResult), unApplePayPaymentAuthorizationResult, noApplePayPaymentAuthorizationResult, gTypeApplePayPaymentAuthorizationResult
  , ApplePayPaymentAuthorizedEvent(ApplePayPaymentAuthorizedEvent), unApplePayPaymentAuthorizedEvent, noApplePayPaymentAuthorizedEvent, gTypeApplePayPaymentAuthorizedEvent
  , ApplePayPaymentContact(ApplePayPaymentContact), unApplePayPaymentContact, noApplePayPaymentContact, gTypeApplePayPaymentContact
  , ApplePayPaymentMethod(ApplePayPaymentMethod), unApplePayPaymentMethod, noApplePayPaymentMethod, gTypeApplePayPaymentMethod
  , ApplePayPaymentMethodSelectedEvent(ApplePayPaymentMethodSelectedEvent), unApplePayPaymentMethodSelectedEvent, noApplePayPaymentMethodSelectedEvent, gTypeApplePayPaymentMethodSelectedEvent
  , ApplePayPaymentMethodUpdate(ApplePayPaymentMethodUpdate), unApplePayPaymentMethodUpdate, noApplePayPaymentMethodUpdate, gTypeApplePayPaymentMethodUpdate
  , ApplePayPaymentPass(ApplePayPaymentPass), unApplePayPaymentPass, noApplePayPaymentPass, gTypeApplePayPaymentPass
  , ApplePayPaymentRequest(ApplePayPaymentRequest), unApplePayPaymentRequest, noApplePayPaymentRequest, gTypeApplePayPaymentRequest
  , ApplePayPaymentToken(ApplePayPaymentToken), unApplePayPaymentToken, noApplePayPaymentToken, gTypeApplePayPaymentToken
  , ApplePaySession(ApplePaySession), unApplePaySession, noApplePaySession, gTypeApplePaySession
  , ApplePayShippingContactSelectedEvent(ApplePayShippingContactSelectedEvent), unApplePayShippingContactSelectedEvent, noApplePayShippingContactSelectedEvent, gTypeApplePayShippingContactSelectedEvent
  , ApplePayShippingContactUpdate(ApplePayShippingContactUpdate), unApplePayShippingContactUpdate, noApplePayShippingContactUpdate, gTypeApplePayShippingContactUpdate
  , ApplePayShippingMethod(ApplePayShippingMethod), unApplePayShippingMethod, noApplePayShippingMethod, gTypeApplePayShippingMethod
  , ApplePayShippingMethodSelectedEvent(ApplePayShippingMethodSelectedEvent), unApplePayShippingMethodSelectedEvent, noApplePayShippingMethodSelectedEvent, gTypeApplePayShippingMethodSelectedEvent
  , ApplePayShippingMethodUpdate(ApplePayShippingMethodUpdate), unApplePayShippingMethodUpdate, noApplePayShippingMethodUpdate, gTypeApplePayShippingMethodUpdate
  , ApplePayValidateMerchantEvent(ApplePayValidateMerchantEvent), unApplePayValidateMerchantEvent, noApplePayValidateMerchantEvent, gTypeApplePayValidateMerchantEvent
  , ApplicationCache(ApplicationCache), unApplicationCache, noApplicationCache, gTypeApplicationCache
  , AssignedNodesOptions(AssignedNodesOptions), unAssignedNodesOptions, noAssignedNodesOptions, gTypeAssignedNodesOptions
  , Attr(Attr), unAttr, noAttr, gTypeAttr
  , AudioBuffer(AudioBuffer), unAudioBuffer, noAudioBuffer, gTypeAudioBuffer
  , AudioBufferSourceNode(AudioBufferSourceNode), unAudioBufferSourceNode, noAudioBufferSourceNode, gTypeAudioBufferSourceNode
  , AudioContext(AudioContext), unAudioContext, IsAudioContext, toAudioContext, noAudioContext, gTypeAudioContext
  , AudioDestinationNode(AudioDestinationNode), unAudioDestinationNode, noAudioDestinationNode, gTypeAudioDestinationNode
  , AudioListener(AudioListener), unAudioListener, noAudioListener, gTypeAudioListener
  , AudioNode(AudioNode), unAudioNode, IsAudioNode, toAudioNode, noAudioNode, gTypeAudioNode
  , AudioParam(AudioParam), unAudioParam, noAudioParam, gTypeAudioParam
  , AudioProcessingEvent(AudioProcessingEvent), unAudioProcessingEvent, noAudioProcessingEvent, gTypeAudioProcessingEvent
  , AudioTrack(AudioTrack), unAudioTrack, noAudioTrack, gTypeAudioTrack
  , AudioTrackList(AudioTrackList), unAudioTrackList, noAudioTrackList, gTypeAudioTrackList
  , AutocompleteErrorEvent(AutocompleteErrorEvent), unAutocompleteErrorEvent, noAutocompleteErrorEvent, gTypeAutocompleteErrorEvent
  , AutocompleteErrorEventInit(AutocompleteErrorEventInit), unAutocompleteErrorEventInit, noAutocompleteErrorEventInit, gTypeAutocompleteErrorEventInit
  , BarProp(BarProp), unBarProp, noBarProp, gTypeBarProp
  , BasicCredential(BasicCredential), unBasicCredential, IsBasicCredential, toBasicCredential, noBasicCredential, gTypeBasicCredential
  , BeforeLoadEvent(BeforeLoadEvent), unBeforeLoadEvent, noBeforeLoadEvent, gTypeBeforeLoadEvent
  , BeforeLoadEventInit(BeforeLoadEventInit), unBeforeLoadEventInit, noBeforeLoadEventInit, gTypeBeforeLoadEventInit
  , BeforeUnloadEvent(BeforeUnloadEvent), unBeforeUnloadEvent, noBeforeUnloadEvent, gTypeBeforeUnloadEvent
  , BiquadFilterNode(BiquadFilterNode), unBiquadFilterNode, noBiquadFilterNode, gTypeBiquadFilterNode
  , Blob(Blob), unBlob, IsBlob, toBlob, noBlob, gTypeBlob
  , BlobPropertyBag(BlobPropertyBag), unBlobPropertyBag, IsBlobPropertyBag, toBlobPropertyBag, noBlobPropertyBag, gTypeBlobPropertyBag
  , Body(Body), unBody, IsBody, toBody, noBody, gTypeBody
  , ByteLengthQueuingStrategy(ByteLengthQueuingStrategy), unByteLengthQueuingStrategy, noByteLengthQueuingStrategy, gTypeByteLengthQueuingStrategy
  , CDATASection(CDATASection), unCDATASection, noCDATASection, gTypeCDATASection
  , CSS(CSS), unCSS, noCSS, gTypeCSS
  , CSSFontFaceLoadEvent(CSSFontFaceLoadEvent), unCSSFontFaceLoadEvent, noCSSFontFaceLoadEvent, gTypeCSSFontFaceLoadEvent
  , CSSFontFaceLoadEventInit(CSSFontFaceLoadEventInit), unCSSFontFaceLoadEventInit, noCSSFontFaceLoadEventInit, gTypeCSSFontFaceLoadEventInit
  , CSSFontFaceRule(CSSFontFaceRule), unCSSFontFaceRule, noCSSFontFaceRule, gTypeCSSFontFaceRule
  , CSSImportRule(CSSImportRule), unCSSImportRule, noCSSImportRule, gTypeCSSImportRule
  , CSSKeyframeRule(CSSKeyframeRule), unCSSKeyframeRule, noCSSKeyframeRule, gTypeCSSKeyframeRule
  , CSSKeyframesRule(CSSKeyframesRule), unCSSKeyframesRule, noCSSKeyframesRule, gTypeCSSKeyframesRule
  , CSSMediaRule(CSSMediaRule), unCSSMediaRule, noCSSMediaRule, gTypeCSSMediaRule
  , CSSNamespaceRule(CSSNamespaceRule), unCSSNamespaceRule, noCSSNamespaceRule, gTypeCSSNamespaceRule
  , CSSPageRule(CSSPageRule), unCSSPageRule, noCSSPageRule, gTypeCSSPageRule
  , CSSPrimitiveValue(CSSPrimitiveValue), unCSSPrimitiveValue, noCSSPrimitiveValue, gTypeCSSPrimitiveValue
  , CSSRule(CSSRule), unCSSRule, IsCSSRule, toCSSRule, noCSSRule, gTypeCSSRule
  , CSSRuleList(CSSRuleList), unCSSRuleList, noCSSRuleList, gTypeCSSRuleList
  , CSSStyleDeclaration(CSSStyleDeclaration), unCSSStyleDeclaration, noCSSStyleDeclaration, gTypeCSSStyleDeclaration
  , CSSStyleRule(CSSStyleRule), unCSSStyleRule, noCSSStyleRule, gTypeCSSStyleRule
  , CSSStyleSheet(CSSStyleSheet), unCSSStyleSheet, noCSSStyleSheet, gTypeCSSStyleSheet
  , CSSSupportsRule(CSSSupportsRule), unCSSSupportsRule, noCSSSupportsRule, gTypeCSSSupportsRule
  , CSSUnknownRule(CSSUnknownRule), unCSSUnknownRule, noCSSUnknownRule, gTypeCSSUnknownRule
  , CSSValue(CSSValue), unCSSValue, IsCSSValue, toCSSValue, noCSSValue, gTypeCSSValue
  , CSSValueList(CSSValueList), unCSSValueList, noCSSValueList, gTypeCSSValueList
  , CanvasCaptureMediaStreamTrack(CanvasCaptureMediaStreamTrack), unCanvasCaptureMediaStreamTrack, noCanvasCaptureMediaStreamTrack, gTypeCanvasCaptureMediaStreamTrack
  , CanvasGradient(CanvasGradient), unCanvasGradient, noCanvasGradient, gTypeCanvasGradient
  , CanvasPath(CanvasPath), unCanvasPath, IsCanvasPath, toCanvasPath, noCanvasPath, gTypeCanvasPath
  , CanvasPattern(CanvasPattern), unCanvasPattern, noCanvasPattern, gTypeCanvasPattern
  , CanvasProxy(CanvasProxy), unCanvasProxy, noCanvasProxy, gTypeCanvasProxy
  , CanvasRenderingContext2D(CanvasRenderingContext2D), unCanvasRenderingContext2D, noCanvasRenderingContext2D, gTypeCanvasRenderingContext2D
  , ChannelMergerNode(ChannelMergerNode), unChannelMergerNode, noChannelMergerNode, gTypeChannelMergerNode
  , ChannelSplitterNode(ChannelSplitterNode), unChannelSplitterNode, noChannelSplitterNode, gTypeChannelSplitterNode
  , CharacterData(CharacterData), unCharacterData, IsCharacterData, toCharacterData, noCharacterData, gTypeCharacterData
  , ChildNode(ChildNode), unChildNode, IsChildNode, toChildNode, noChildNode, gTypeChildNode
  , ClipboardEvent(ClipboardEvent), unClipboardEvent, noClipboardEvent, gTypeClipboardEvent
  , ClipboardEventInit(ClipboardEventInit), unClipboardEventInit, noClipboardEventInit, gTypeClipboardEventInit
  , CloseEvent(CloseEvent), unCloseEvent, noCloseEvent, gTypeCloseEvent
  , CloseEventInit(CloseEventInit), unCloseEventInit, noCloseEventInit, gTypeCloseEventInit
  , CommandLineAPIHost(CommandLineAPIHost), unCommandLineAPIHost, noCommandLineAPIHost, gTypeCommandLineAPIHost
  , Comment(Comment), unComment, noComment, gTypeComment
  , CompositionEvent(CompositionEvent), unCompositionEvent, noCompositionEvent, gTypeCompositionEvent
  , CompositionEventInit(CompositionEventInit), unCompositionEventInit, noCompositionEventInit, gTypeCompositionEventInit
  , ConstrainBooleanParameters(ConstrainBooleanParameters), unConstrainBooleanParameters, noConstrainBooleanParameters, gTypeConstrainBooleanParameters
  , ConstrainDOMStringParameters(ConstrainDOMStringParameters), unConstrainDOMStringParameters, noConstrainDOMStringParameters, gTypeConstrainDOMStringParameters
  , ConstrainDoubleRange(ConstrainDoubleRange), unConstrainDoubleRange, noConstrainDoubleRange, gTypeConstrainDoubleRange
  , ConstrainLongRange(ConstrainLongRange), unConstrainLongRange, noConstrainLongRange, gTypeConstrainLongRange
  , ConvolverNode(ConvolverNode), unConvolverNode, noConvolverNode, gTypeConvolverNode
  , Coordinates(Coordinates), unCoordinates, noCoordinates, gTypeCoordinates
  , CountQueuingStrategy(CountQueuingStrategy), unCountQueuingStrategy, noCountQueuingStrategy, gTypeCountQueuingStrategy
  , Counter(Counter), unCounter, noCounter, gTypeCounter
  , CredentialData(CredentialData), unCredentialData, IsCredentialData, toCredentialData, noCredentialData, gTypeCredentialData
  , Crypto(Crypto), unCrypto, noCrypto, gTypeCrypto
  , CryptoAlgorithmParameters(CryptoAlgorithmParameters), unCryptoAlgorithmParameters, IsCryptoAlgorithmParameters, toCryptoAlgorithmParameters, noCryptoAlgorithmParameters, gTypeCryptoAlgorithmParameters
  , CryptoKey(CryptoKey), unCryptoKey, noCryptoKey, gTypeCryptoKey
  , CryptoKeyPair(CryptoKeyPair), unCryptoKeyPair, noCryptoKeyPair, gTypeCryptoKeyPair
  , CustomElementRegistry(CustomElementRegistry), unCustomElementRegistry, noCustomElementRegistry, gTypeCustomElementRegistry
  , CustomEvent(CustomEvent), unCustomEvent, noCustomEvent, gTypeCustomEvent
  , CustomEventInit(CustomEventInit), unCustomEventInit, noCustomEventInit, gTypeCustomEventInit
  , DOMError(DOMError), unDOMError, IsDOMError, toDOMError, noDOMError, gTypeDOMError
  , DOMException(DOMException), unDOMException, noDOMException, gTypeDOMException
  , DOMImplementation(DOMImplementation), unDOMImplementation, noDOMImplementation, gTypeDOMImplementation
  , DOMNamedFlowCollection(DOMNamedFlowCollection), unDOMNamedFlowCollection, noDOMNamedFlowCollection, gTypeDOMNamedFlowCollection
  , DOMParser(DOMParser), unDOMParser, noDOMParser, gTypeDOMParser
  , DOMPoint(DOMPoint), unDOMPoint, noDOMPoint, gTypeDOMPoint
  , DOMPointInit(DOMPointInit), unDOMPointInit, noDOMPointInit, gTypeDOMPointInit
  , DOMPointReadOnly(DOMPointReadOnly), unDOMPointReadOnly, IsDOMPointReadOnly, toDOMPointReadOnly, noDOMPointReadOnly, gTypeDOMPointReadOnly
  , DOMRect(DOMRect), unDOMRect, noDOMRect, gTypeDOMRect
  , DOMRectInit(DOMRectInit), unDOMRectInit, noDOMRectInit, gTypeDOMRectInit
  , DOMRectReadOnly(DOMRectReadOnly), unDOMRectReadOnly, IsDOMRectReadOnly, toDOMRectReadOnly, noDOMRectReadOnly, gTypeDOMRectReadOnly
  , DOMStringList(DOMStringList), unDOMStringList, noDOMStringList, gTypeDOMStringList
  , DOMStringMap(DOMStringMap), unDOMStringMap, noDOMStringMap, gTypeDOMStringMap
  , DOMTokenList(DOMTokenList), unDOMTokenList, noDOMTokenList, gTypeDOMTokenList
  , DataCue(DataCue), unDataCue, noDataCue, gTypeDataCue
  , DataTransfer(DataTransfer), unDataTransfer, noDataTransfer, gTypeDataTransfer
  , DataTransferItem(DataTransferItem), unDataTransferItem, noDataTransferItem, gTypeDataTransferItem
  , DataTransferItemList(DataTransferItemList), unDataTransferItemList, noDataTransferItemList, gTypeDataTransferItemList
  , Database(Database), unDatabase, noDatabase, gTypeDatabase
  , DedicatedWorkerGlobalScope(DedicatedWorkerGlobalScope), unDedicatedWorkerGlobalScope, noDedicatedWorkerGlobalScope, gTypeDedicatedWorkerGlobalScope
  , DelayNode(DelayNode), unDelayNode, noDelayNode, gTypeDelayNode
  , DeviceMotionEvent(DeviceMotionEvent), unDeviceMotionEvent, noDeviceMotionEvent, gTypeDeviceMotionEvent
  , DeviceOrientationEvent(DeviceOrientationEvent), unDeviceOrientationEvent, noDeviceOrientationEvent, gTypeDeviceOrientationEvent
  , DeviceProximityEvent(DeviceProximityEvent), unDeviceProximityEvent, noDeviceProximityEvent, gTypeDeviceProximityEvent
  , DeviceProximityEventInit(DeviceProximityEventInit), unDeviceProximityEventInit, noDeviceProximityEventInit, gTypeDeviceProximityEventInit
  , Document(Document), unDocument, IsDocument, toDocument, noDocument, gTypeDocument
  , DocumentAndElementEventHandlers(DocumentAndElementEventHandlers), unDocumentAndElementEventHandlers, IsDocumentAndElementEventHandlers, toDocumentAndElementEventHandlers, noDocumentAndElementEventHandlers, gTypeDocumentAndElementEventHandlers
  , DocumentFragment(DocumentFragment), unDocumentFragment, IsDocumentFragment, toDocumentFragment, noDocumentFragment, gTypeDocumentFragment
  , DocumentOrShadowRoot(DocumentOrShadowRoot), unDocumentOrShadowRoot, IsDocumentOrShadowRoot, toDocumentOrShadowRoot, noDocumentOrShadowRoot, gTypeDocumentOrShadowRoot
  , DocumentTimeline(DocumentTimeline), unDocumentTimeline, noDocumentTimeline, gTypeDocumentTimeline
  , DocumentType(DocumentType), unDocumentType, noDocumentType, gTypeDocumentType
  , DoubleRange(DoubleRange), unDoubleRange, IsDoubleRange, toDoubleRange, noDoubleRange, gTypeDoubleRange
  , DynamicsCompressorNode(DynamicsCompressorNode), unDynamicsCompressorNode, noDynamicsCompressorNode, gTypeDynamicsCompressorNode
  , EXTBlendMinMax(EXTBlendMinMax), unEXTBlendMinMax, noEXTBlendMinMax, gTypeEXTBlendMinMax
  , EXTFragDepth(EXTFragDepth), unEXTFragDepth, noEXTFragDepth, gTypeEXTFragDepth
  , EXTShaderTextureLOD(EXTShaderTextureLOD), unEXTShaderTextureLOD, noEXTShaderTextureLOD, gTypeEXTShaderTextureLOD
  , EXTTextureFilterAnisotropic(EXTTextureFilterAnisotropic), unEXTTextureFilterAnisotropic, noEXTTextureFilterAnisotropic, gTypeEXTTextureFilterAnisotropic
  , EXTsRGB(EXTsRGB), unEXTsRGB, noEXTsRGB, gTypeEXTsRGB
  , EcKeyParams(EcKeyParams), unEcKeyParams, noEcKeyParams, gTypeEcKeyParams
  , EcdhKeyDeriveParams(EcdhKeyDeriveParams), unEcdhKeyDeriveParams, noEcdhKeyDeriveParams, gTypeEcdhKeyDeriveParams
  , EcdsaParams(EcdsaParams), unEcdsaParams, noEcdsaParams, gTypeEcdsaParams
  , Element(Element), unElement, IsElement, toElement, noElement, gTypeElement
  , ElementCSSInlineStyle(ElementCSSInlineStyle), unElementCSSInlineStyle, IsElementCSSInlineStyle, toElementCSSInlineStyle, noElementCSSInlineStyle, gTypeElementCSSInlineStyle
  , ErrorEvent(ErrorEvent), unErrorEvent, noErrorEvent, gTypeErrorEvent
  , ErrorEventInit(ErrorEventInit), unErrorEventInit, noErrorEventInit, gTypeErrorEventInit
  , Event(Event), unEvent, IsEvent, toEvent, noEvent, gTypeEvent
  , EventInit(EventInit), unEventInit, IsEventInit, toEventInit, noEventInit, gTypeEventInit
  , EventListener(EventListener), unEventListener, noEventListener, gTypeEventListener
  , EventListenerOptions(EventListenerOptions), unEventListenerOptions, IsEventListenerOptions, toEventListenerOptions, noEventListenerOptions, gTypeEventListenerOptions
  , EventModifierInit(EventModifierInit), unEventModifierInit, IsEventModifierInit, toEventModifierInit, noEventModifierInit, gTypeEventModifierInit
  , EventSource(EventSource), unEventSource, noEventSource, gTypeEventSource
  , EventSourceInit(EventSourceInit), unEventSourceInit, noEventSourceInit, gTypeEventSourceInit
  , EventTarget(EventTarget), unEventTarget, IsEventTarget, toEventTarget, noEventTarget, gTypeEventTarget
  , File(File), unFile, noFile, gTypeFile
  , FileError(FileError), unFileError, noFileError, gTypeFileError
  , FileException(FileException), unFileException, noFileException, gTypeFileException
  , FileList(FileList), unFileList, noFileList, gTypeFileList
  , FilePropertyBag(FilePropertyBag), unFilePropertyBag, noFilePropertyBag, gTypeFilePropertyBag
  , FileReader(FileReader), unFileReader, noFileReader, gTypeFileReader
  , FileReaderSync(FileReaderSync), unFileReaderSync, noFileReaderSync, gTypeFileReaderSync
  , FocusEvent(FocusEvent), unFocusEvent, noFocusEvent, gTypeFocusEvent
  , FocusEventInit(FocusEventInit), unFocusEventInit, noFocusEventInit, gTypeFocusEventInit
  , FontFace(FontFace), unFontFace, noFontFace, gTypeFontFace
  , FontFaceDescriptors(FontFaceDescriptors), unFontFaceDescriptors, noFontFaceDescriptors, gTypeFontFaceDescriptors
  , FontFaceSet(FontFaceSet), unFontFaceSet, noFontFaceSet, gTypeFontFaceSet
  , FormData(FormData), unFormData, noFormData, gTypeFormData
  , GainNode(GainNode), unGainNode, noGainNode, gTypeGainNode
  , Gamepad(Gamepad), unGamepad, noGamepad, gTypeGamepad
  , GamepadButton(GamepadButton), unGamepadButton, noGamepadButton, gTypeGamepadButton
  , GamepadEvent(GamepadEvent), unGamepadEvent, noGamepadEvent, gTypeGamepadEvent
  , GamepadEventInit(GamepadEventInit), unGamepadEventInit, noGamepadEventInit, gTypeGamepadEventInit
  , Geolocation(Geolocation), unGeolocation, noGeolocation, gTypeGeolocation
  , Geoposition(Geoposition), unGeoposition, noGeoposition, gTypeGeoposition
  , GetRootNodeOptions(GetRootNodeOptions), unGetRootNodeOptions, noGetRootNodeOptions, gTypeGetRootNodeOptions
  , GlobalCrypto(GlobalCrypto), unGlobalCrypto, IsGlobalCrypto, toGlobalCrypto, noGlobalCrypto, gTypeGlobalCrypto
  , GlobalEventHandlers(GlobalEventHandlers), unGlobalEventHandlers, IsGlobalEventHandlers, toGlobalEventHandlers, noGlobalEventHandlers, gTypeGlobalEventHandlers
  , GlobalPerformance(GlobalPerformance), unGlobalPerformance, IsGlobalPerformance, toGlobalPerformance, noGlobalPerformance, gTypeGlobalPerformance
  , HTMLAllCollection(HTMLAllCollection), unHTMLAllCollection, noHTMLAllCollection, gTypeHTMLAllCollection
  , HTMLAnchorElement(HTMLAnchorElement), unHTMLAnchorElement, noHTMLAnchorElement, gTypeHTMLAnchorElement
  , HTMLAppletElement(HTMLAppletElement), unHTMLAppletElement, noHTMLAppletElement, gTypeHTMLAppletElement
  , HTMLAreaElement(HTMLAreaElement), unHTMLAreaElement, noHTMLAreaElement, gTypeHTMLAreaElement
  , HTMLAttachmentElement(HTMLAttachmentElement), unHTMLAttachmentElement, noHTMLAttachmentElement, gTypeHTMLAttachmentElement
  , HTMLAudioElement(HTMLAudioElement), unHTMLAudioElement, noHTMLAudioElement, gTypeHTMLAudioElement
  , HTMLBRElement(HTMLBRElement), unHTMLBRElement, noHTMLBRElement, gTypeHTMLBRElement
  , HTMLBaseElement(HTMLBaseElement), unHTMLBaseElement, noHTMLBaseElement, gTypeHTMLBaseElement
  , HTMLBodyElement(HTMLBodyElement), unHTMLBodyElement, noHTMLBodyElement, gTypeHTMLBodyElement
  , HTMLButtonElement(HTMLButtonElement), unHTMLButtonElement, noHTMLButtonElement, gTypeHTMLButtonElement
  , HTMLCanvasElement(HTMLCanvasElement), unHTMLCanvasElement, noHTMLCanvasElement, gTypeHTMLCanvasElement
  , HTMLCollection(HTMLCollection), unHTMLCollection, IsHTMLCollection, toHTMLCollection, noHTMLCollection, gTypeHTMLCollection
  , HTMLDListElement(HTMLDListElement), unHTMLDListElement, noHTMLDListElement, gTypeHTMLDListElement
  , HTMLDataElement(HTMLDataElement), unHTMLDataElement, noHTMLDataElement, gTypeHTMLDataElement
  , HTMLDataListElement(HTMLDataListElement), unHTMLDataListElement, noHTMLDataListElement, gTypeHTMLDataListElement
  , HTMLDetailsElement(HTMLDetailsElement), unHTMLDetailsElement, noHTMLDetailsElement, gTypeHTMLDetailsElement
  , HTMLDirectoryElement(HTMLDirectoryElement), unHTMLDirectoryElement, noHTMLDirectoryElement, gTypeHTMLDirectoryElement
  , HTMLDivElement(HTMLDivElement), unHTMLDivElement, noHTMLDivElement, gTypeHTMLDivElement
  , HTMLDocument(HTMLDocument), unHTMLDocument, noHTMLDocument, gTypeHTMLDocument
  , HTMLElement(HTMLElement), unHTMLElement, IsHTMLElement, toHTMLElement, noHTMLElement, gTypeHTMLElement
  , HTMLEmbedElement(HTMLEmbedElement), unHTMLEmbedElement, noHTMLEmbedElement, gTypeHTMLEmbedElement
  , HTMLFieldSetElement(HTMLFieldSetElement), unHTMLFieldSetElement, noHTMLFieldSetElement, gTypeHTMLFieldSetElement
  , HTMLFontElement(HTMLFontElement), unHTMLFontElement, noHTMLFontElement, gTypeHTMLFontElement
  , HTMLFormControlsCollection(HTMLFormControlsCollection), unHTMLFormControlsCollection, noHTMLFormControlsCollection, gTypeHTMLFormControlsCollection
  , HTMLFormElement(HTMLFormElement), unHTMLFormElement, noHTMLFormElement, gTypeHTMLFormElement
  , HTMLFrameElement(HTMLFrameElement), unHTMLFrameElement, noHTMLFrameElement, gTypeHTMLFrameElement
  , HTMLFrameSetElement(HTMLFrameSetElement), unHTMLFrameSetElement, noHTMLFrameSetElement, gTypeHTMLFrameSetElement
  , HTMLHRElement(HTMLHRElement), unHTMLHRElement, noHTMLHRElement, gTypeHTMLHRElement
  , HTMLHeadElement(HTMLHeadElement), unHTMLHeadElement, noHTMLHeadElement, gTypeHTMLHeadElement
  , HTMLHeadingElement(HTMLHeadingElement), unHTMLHeadingElement, noHTMLHeadingElement, gTypeHTMLHeadingElement
  , HTMLHtmlElement(HTMLHtmlElement), unHTMLHtmlElement, noHTMLHtmlElement, gTypeHTMLHtmlElement
  , HTMLHyperlinkElementUtils(HTMLHyperlinkElementUtils), unHTMLHyperlinkElementUtils, IsHTMLHyperlinkElementUtils, toHTMLHyperlinkElementUtils, noHTMLHyperlinkElementUtils, gTypeHTMLHyperlinkElementUtils
  , HTMLIFrameElement(HTMLIFrameElement), unHTMLIFrameElement, noHTMLIFrameElement, gTypeHTMLIFrameElement
  , HTMLImageElement(HTMLImageElement), unHTMLImageElement, noHTMLImageElement, gTypeHTMLImageElement
  , HTMLInputElement(HTMLInputElement), unHTMLInputElement, noHTMLInputElement, gTypeHTMLInputElement
  , HTMLKeygenElement(HTMLKeygenElement), unHTMLKeygenElement, noHTMLKeygenElement, gTypeHTMLKeygenElement
  , HTMLLIElement(HTMLLIElement), unHTMLLIElement, noHTMLLIElement, gTypeHTMLLIElement
  , HTMLLabelElement(HTMLLabelElement), unHTMLLabelElement, noHTMLLabelElement, gTypeHTMLLabelElement
  , HTMLLegendElement(HTMLLegendElement), unHTMLLegendElement, noHTMLLegendElement, gTypeHTMLLegendElement
  , HTMLLinkElement(HTMLLinkElement), unHTMLLinkElement, noHTMLLinkElement, gTypeHTMLLinkElement
  , HTMLMapElement(HTMLMapElement), unHTMLMapElement, noHTMLMapElement, gTypeHTMLMapElement
  , HTMLMarqueeElement(HTMLMarqueeElement), unHTMLMarqueeElement, noHTMLMarqueeElement, gTypeHTMLMarqueeElement
  , HTMLMediaElement(HTMLMediaElement), unHTMLMediaElement, IsHTMLMediaElement, toHTMLMediaElement, noHTMLMediaElement, gTypeHTMLMediaElement
  , HTMLMenuElement(HTMLMenuElement), unHTMLMenuElement, noHTMLMenuElement, gTypeHTMLMenuElement
  , HTMLMetaElement(HTMLMetaElement), unHTMLMetaElement, noHTMLMetaElement, gTypeHTMLMetaElement
  , HTMLMeterElement(HTMLMeterElement), unHTMLMeterElement, noHTMLMeterElement, gTypeHTMLMeterElement
  , HTMLModElement(HTMLModElement), unHTMLModElement, noHTMLModElement, gTypeHTMLModElement
  , HTMLOListElement(HTMLOListElement), unHTMLOListElement, noHTMLOListElement, gTypeHTMLOListElement
  , HTMLObjectElement(HTMLObjectElement), unHTMLObjectElement, noHTMLObjectElement, gTypeHTMLObjectElement
  , HTMLOptGroupElement(HTMLOptGroupElement), unHTMLOptGroupElement, noHTMLOptGroupElement, gTypeHTMLOptGroupElement
  , HTMLOptionElement(HTMLOptionElement), unHTMLOptionElement, noHTMLOptionElement, gTypeHTMLOptionElement
  , HTMLOptionsCollection(HTMLOptionsCollection), unHTMLOptionsCollection, noHTMLOptionsCollection, gTypeHTMLOptionsCollection
  , HTMLOutputElement(HTMLOutputElement), unHTMLOutputElement, noHTMLOutputElement, gTypeHTMLOutputElement
  , HTMLParagraphElement(HTMLParagraphElement), unHTMLParagraphElement, noHTMLParagraphElement, gTypeHTMLParagraphElement
  , HTMLParamElement(HTMLParamElement), unHTMLParamElement, noHTMLParamElement, gTypeHTMLParamElement
  , HTMLPictureElement(HTMLPictureElement), unHTMLPictureElement, noHTMLPictureElement, gTypeHTMLPictureElement
  , HTMLPreElement(HTMLPreElement), unHTMLPreElement, noHTMLPreElement, gTypeHTMLPreElement
  , HTMLProgressElement(HTMLProgressElement), unHTMLProgressElement, noHTMLProgressElement, gTypeHTMLProgressElement
  , HTMLQuoteElement(HTMLQuoteElement), unHTMLQuoteElement, noHTMLQuoteElement, gTypeHTMLQuoteElement
  , HTMLScriptElement(HTMLScriptElement), unHTMLScriptElement, noHTMLScriptElement, gTypeHTMLScriptElement
  , HTMLSelectElement(HTMLSelectElement), unHTMLSelectElement, noHTMLSelectElement, gTypeHTMLSelectElement
  , HTMLSlotElement(HTMLSlotElement), unHTMLSlotElement, noHTMLSlotElement, gTypeHTMLSlotElement
  , HTMLSourceElement(HTMLSourceElement), unHTMLSourceElement, noHTMLSourceElement, gTypeHTMLSourceElement
  , HTMLSpanElement(HTMLSpanElement), unHTMLSpanElement, noHTMLSpanElement, gTypeHTMLSpanElement
  , HTMLStyleElement(HTMLStyleElement), unHTMLStyleElement, noHTMLStyleElement, gTypeHTMLStyleElement
  , HTMLTableCaptionElement(HTMLTableCaptionElement), unHTMLTableCaptionElement, noHTMLTableCaptionElement, gTypeHTMLTableCaptionElement
  , HTMLTableCellElement(HTMLTableCellElement), unHTMLTableCellElement, noHTMLTableCellElement, gTypeHTMLTableCellElement
  , HTMLTableColElement(HTMLTableColElement), unHTMLTableColElement, noHTMLTableColElement, gTypeHTMLTableColElement
  , HTMLTableElement(HTMLTableElement), unHTMLTableElement, noHTMLTableElement, gTypeHTMLTableElement
  , HTMLTableRowElement(HTMLTableRowElement), unHTMLTableRowElement, noHTMLTableRowElement, gTypeHTMLTableRowElement
  , HTMLTableSectionElement(HTMLTableSectionElement), unHTMLTableSectionElement, noHTMLTableSectionElement, gTypeHTMLTableSectionElement
  , HTMLTemplateElement(HTMLTemplateElement), unHTMLTemplateElement, noHTMLTemplateElement, gTypeHTMLTemplateElement
  , HTMLTextAreaElement(HTMLTextAreaElement), unHTMLTextAreaElement, noHTMLTextAreaElement, gTypeHTMLTextAreaElement
  , HTMLTimeElement(HTMLTimeElement), unHTMLTimeElement, noHTMLTimeElement, gTypeHTMLTimeElement
  , HTMLTitleElement(HTMLTitleElement), unHTMLTitleElement, noHTMLTitleElement, gTypeHTMLTitleElement
  , HTMLTrackElement(HTMLTrackElement), unHTMLTrackElement, noHTMLTrackElement, gTypeHTMLTrackElement
  , HTMLUListElement(HTMLUListElement), unHTMLUListElement, noHTMLUListElement, gTypeHTMLUListElement
  , HTMLUnknownElement(HTMLUnknownElement), unHTMLUnknownElement, noHTMLUnknownElement, gTypeHTMLUnknownElement
  , HTMLVideoElement(HTMLVideoElement), unHTMLVideoElement, noHTMLVideoElement, gTypeHTMLVideoElement
  , HashChangeEvent(HashChangeEvent), unHashChangeEvent, noHashChangeEvent, gTypeHashChangeEvent
  , HashChangeEventInit(HashChangeEventInit), unHashChangeEventInit, noHashChangeEventInit, gTypeHashChangeEventInit
  , Headers(Headers), unHeaders, noHeaders, gTypeHeaders
  , History(History), unHistory, noHistory, gTypeHistory
  , HkdfParams(HkdfParams), unHkdfParams, noHkdfParams, gTypeHkdfParams
  , HmacKeyParams(HmacKeyParams), unHmacKeyParams, noHmacKeyParams, gTypeHmacKeyParams
  , IDBCursor(IDBCursor), unIDBCursor, IsIDBCursor, toIDBCursor, noIDBCursor, gTypeIDBCursor
  , IDBCursorWithValue(IDBCursorWithValue), unIDBCursorWithValue, noIDBCursorWithValue, gTypeIDBCursorWithValue
  , IDBDatabase(IDBDatabase), unIDBDatabase, noIDBDatabase, gTypeIDBDatabase
  , IDBFactory(IDBFactory), unIDBFactory, noIDBFactory, gTypeIDBFactory
  , IDBIndex(IDBIndex), unIDBIndex, noIDBIndex, gTypeIDBIndex
  , IDBIndexParameters(IDBIndexParameters), unIDBIndexParameters, noIDBIndexParameters, gTypeIDBIndexParameters
  , IDBKeyRange(IDBKeyRange), unIDBKeyRange, noIDBKeyRange, gTypeIDBKeyRange
  , IDBObjectStore(IDBObjectStore), unIDBObjectStore, noIDBObjectStore, gTypeIDBObjectStore
  , IDBObjectStoreParameters(IDBObjectStoreParameters), unIDBObjectStoreParameters, noIDBObjectStoreParameters, gTypeIDBObjectStoreParameters
  , IDBOpenDBRequest(IDBOpenDBRequest), unIDBOpenDBRequest, noIDBOpenDBRequest, gTypeIDBOpenDBRequest
  , IDBRequest(IDBRequest), unIDBRequest, IsIDBRequest, toIDBRequest, noIDBRequest, gTypeIDBRequest
  , IDBTransaction(IDBTransaction), unIDBTransaction, noIDBTransaction, gTypeIDBTransaction
  , IDBVersionChangeEvent(IDBVersionChangeEvent), unIDBVersionChangeEvent, noIDBVersionChangeEvent, gTypeIDBVersionChangeEvent
  , IDBVersionChangeEventInit(IDBVersionChangeEventInit), unIDBVersionChangeEventInit, noIDBVersionChangeEventInit, gTypeIDBVersionChangeEventInit
  , ImageData(ImageData), unImageData, noImageData, gTypeImageData
  , InputEvent(InputEvent), unInputEvent, noInputEvent, gTypeInputEvent
  , InputEventInit(InputEventInit), unInputEventInit, noInputEventInit, gTypeInputEventInit
  , InspectorFrontendHost(InspectorFrontendHost), unInspectorFrontendHost, noInspectorFrontendHost, gTypeInspectorFrontendHost
  , IntersectionObserver(IntersectionObserver), unIntersectionObserver, noIntersectionObserver, gTypeIntersectionObserver
  , IntersectionObserverEntry(IntersectionObserverEntry), unIntersectionObserverEntry, noIntersectionObserverEntry, gTypeIntersectionObserverEntry
  , IntersectionObserverEntryInit(IntersectionObserverEntryInit), unIntersectionObserverEntryInit, noIntersectionObserverEntryInit, gTypeIntersectionObserverEntryInit
  , IntersectionObserverInit(IntersectionObserverInit), unIntersectionObserverInit, noIntersectionObserverInit, gTypeIntersectionObserverInit
  , JsonWebKey(JsonWebKey), unJsonWebKey, noJsonWebKey, gTypeJsonWebKey
  , KeyboardEvent(KeyboardEvent), unKeyboardEvent, noKeyboardEvent, gTypeKeyboardEvent
  , KeyboardEventInit(KeyboardEventInit), unKeyboardEventInit, noKeyboardEventInit, gTypeKeyboardEventInit
  , KeyframeEffect(KeyframeEffect), unKeyframeEffect, noKeyframeEffect, gTypeKeyframeEffect
  , Location(Location), unLocation, noLocation, gTypeLocation
  , LongRange(LongRange), unLongRange, IsLongRange, toLongRange, noLongRange, gTypeLongRange
  , MediaController(MediaController), unMediaController, noMediaController, gTypeMediaController
  , MediaControlsHost(MediaControlsHost), unMediaControlsHost, noMediaControlsHost, gTypeMediaControlsHost
  , MediaDeviceInfo(MediaDeviceInfo), unMediaDeviceInfo, noMediaDeviceInfo, gTypeMediaDeviceInfo
  , MediaDevices(MediaDevices), unMediaDevices, noMediaDevices, gTypeMediaDevices
  , MediaElementAudioSourceNode(MediaElementAudioSourceNode), unMediaElementAudioSourceNode, noMediaElementAudioSourceNode, gTypeMediaElementAudioSourceNode
  , MediaEncryptedEvent(MediaEncryptedEvent), unMediaEncryptedEvent, noMediaEncryptedEvent, gTypeMediaEncryptedEvent
  , MediaEncryptedEventInit(MediaEncryptedEventInit), unMediaEncryptedEventInit, noMediaEncryptedEventInit, gTypeMediaEncryptedEventInit
  , MediaError(MediaError), unMediaError, noMediaError, gTypeMediaError
  , MediaKeyMessageEvent(MediaKeyMessageEvent), unMediaKeyMessageEvent, noMediaKeyMessageEvent, gTypeMediaKeyMessageEvent
  , MediaKeyMessageEventInit(MediaKeyMessageEventInit), unMediaKeyMessageEventInit, noMediaKeyMessageEventInit, gTypeMediaKeyMessageEventInit
  , MediaKeySession(MediaKeySession), unMediaKeySession, noMediaKeySession, gTypeMediaKeySession
  , MediaKeyStatusMap(MediaKeyStatusMap), unMediaKeyStatusMap, noMediaKeyStatusMap, gTypeMediaKeyStatusMap
  , MediaKeySystemAccess(MediaKeySystemAccess), unMediaKeySystemAccess, noMediaKeySystemAccess, gTypeMediaKeySystemAccess
  , MediaKeySystemConfiguration(MediaKeySystemConfiguration), unMediaKeySystemConfiguration, noMediaKeySystemConfiguration, gTypeMediaKeySystemConfiguration
  , MediaKeySystemMediaCapability(MediaKeySystemMediaCapability), unMediaKeySystemMediaCapability, noMediaKeySystemMediaCapability, gTypeMediaKeySystemMediaCapability
  , MediaKeys(MediaKeys), unMediaKeys, noMediaKeys, gTypeMediaKeys
  , MediaList(MediaList), unMediaList, noMediaList, gTypeMediaList
  , MediaMetadata(MediaMetadata), unMediaMetadata, noMediaMetadata, gTypeMediaMetadata
  , MediaQueryList(MediaQueryList), unMediaQueryList, noMediaQueryList, gTypeMediaQueryList
  , MediaRemoteControls(MediaRemoteControls), unMediaRemoteControls, noMediaRemoteControls, gTypeMediaRemoteControls
  , MediaSession(MediaSession), unMediaSession, noMediaSession, gTypeMediaSession
  , MediaSource(MediaSource), unMediaSource, noMediaSource, gTypeMediaSource
  , MediaStream(MediaStream), unMediaStream, noMediaStream, gTypeMediaStream
  , MediaStreamAudioDestinationNode(MediaStreamAudioDestinationNode), unMediaStreamAudioDestinationNode, noMediaStreamAudioDestinationNode, gTypeMediaStreamAudioDestinationNode
  , MediaStreamAudioSourceNode(MediaStreamAudioSourceNode), unMediaStreamAudioSourceNode, noMediaStreamAudioSourceNode, gTypeMediaStreamAudioSourceNode
  , MediaStreamConstraints(MediaStreamConstraints), unMediaStreamConstraints, noMediaStreamConstraints, gTypeMediaStreamConstraints
  , MediaStreamEvent(MediaStreamEvent), unMediaStreamEvent, noMediaStreamEvent, gTypeMediaStreamEvent
  , MediaStreamEventInit(MediaStreamEventInit), unMediaStreamEventInit, noMediaStreamEventInit, gTypeMediaStreamEventInit
  , MediaStreamTrack(MediaStreamTrack), unMediaStreamTrack, IsMediaStreamTrack, toMediaStreamTrack, noMediaStreamTrack, gTypeMediaStreamTrack
  , MediaStreamTrackEvent(MediaStreamTrackEvent), unMediaStreamTrackEvent, noMediaStreamTrackEvent, gTypeMediaStreamTrackEvent
  , MediaStreamTrackEventInit(MediaStreamTrackEventInit), unMediaStreamTrackEventInit, noMediaStreamTrackEventInit, gTypeMediaStreamTrackEventInit
  , MediaTrackCapabilities(MediaTrackCapabilities), unMediaTrackCapabilities, noMediaTrackCapabilities, gTypeMediaTrackCapabilities
  , MediaTrackConstraintSet(MediaTrackConstraintSet), unMediaTrackConstraintSet, IsMediaTrackConstraintSet, toMediaTrackConstraintSet, noMediaTrackConstraintSet, gTypeMediaTrackConstraintSet
  , MediaTrackConstraints(MediaTrackConstraints), unMediaTrackConstraints, noMediaTrackConstraints, gTypeMediaTrackConstraints
  , MediaTrackSettings(MediaTrackSettings), unMediaTrackSettings, noMediaTrackSettings, gTypeMediaTrackSettings
  , MediaTrackSupportedConstraints(MediaTrackSupportedConstraints), unMediaTrackSupportedConstraints, noMediaTrackSupportedConstraints, gTypeMediaTrackSupportedConstraints
  , MessageChannel(MessageChannel), unMessageChannel, noMessageChannel, gTypeMessageChannel
  , MessageEvent(MessageEvent), unMessageEvent, noMessageEvent, gTypeMessageEvent
  , MessageEventInit(MessageEventInit), unMessageEventInit, noMessageEventInit, gTypeMessageEventInit
  , MessagePort(MessagePort), unMessagePort, noMessagePort, gTypeMessagePort
  , MimeType(MimeType), unMimeType, noMimeType, gTypeMimeType
  , MimeTypeArray(MimeTypeArray), unMimeTypeArray, noMimeTypeArray, gTypeMimeTypeArray
  , MouseEvent(MouseEvent), unMouseEvent, IsMouseEvent, toMouseEvent, noMouseEvent, gTypeMouseEvent
  , MouseEventInit(MouseEventInit), unMouseEventInit, IsMouseEventInit, toMouseEventInit, noMouseEventInit, gTypeMouseEventInit
  , MutationEvent(MutationEvent), unMutationEvent, noMutationEvent, gTypeMutationEvent
  , MutationObserver(MutationObserver), unMutationObserver, noMutationObserver, gTypeMutationObserver
  , MutationObserverInit(MutationObserverInit), unMutationObserverInit, noMutationObserverInit, gTypeMutationObserverInit
  , MutationRecord(MutationRecord), unMutationRecord, noMutationRecord, gTypeMutationRecord
  , NamedNodeMap(NamedNodeMap), unNamedNodeMap, noNamedNodeMap, gTypeNamedNodeMap
  , Navigator(Navigator), unNavigator, noNavigator, gTypeNavigator
  , NavigatorConcurrentHardware(NavigatorConcurrentHardware), unNavigatorConcurrentHardware, IsNavigatorConcurrentHardware, toNavigatorConcurrentHardware, noNavigatorConcurrentHardware, gTypeNavigatorConcurrentHardware
  , NavigatorID(NavigatorID), unNavigatorID, IsNavigatorID, toNavigatorID, noNavigatorID, gTypeNavigatorID
  , NavigatorLanguage(NavigatorLanguage), unNavigatorLanguage, IsNavigatorLanguage, toNavigatorLanguage, noNavigatorLanguage, gTypeNavigatorLanguage
  , NavigatorOnLine(NavigatorOnLine), unNavigatorOnLine, IsNavigatorOnLine, toNavigatorOnLine, noNavigatorOnLine, gTypeNavigatorOnLine
  , NavigatorUserMediaError(NavigatorUserMediaError), unNavigatorUserMediaError, noNavigatorUserMediaError, gTypeNavigatorUserMediaError
  , Node(Node), unNode, IsNode, toNode, noNode, gTypeNode
  , NodeIterator(NodeIterator), unNodeIterator, noNodeIterator, gTypeNodeIterator
  , NodeList(NodeList), unNodeList, IsNodeList, toNodeList, noNodeList, gTypeNodeList
  , NonDocumentTypeChildNode(NonDocumentTypeChildNode), unNonDocumentTypeChildNode, IsNonDocumentTypeChildNode, toNonDocumentTypeChildNode, noNonDocumentTypeChildNode, gTypeNonDocumentTypeChildNode
  , NonElementParentNode(NonElementParentNode), unNonElementParentNode, IsNonElementParentNode, toNonElementParentNode, noNonElementParentNode, gTypeNonElementParentNode
  , Notification(Notification), unNotification, noNotification, gTypeNotification
  , NotificationOptions(NotificationOptions), unNotificationOptions, noNotificationOptions, gTypeNotificationOptions
  , OESElementIndexUint(OESElementIndexUint), unOESElementIndexUint, noOESElementIndexUint, gTypeOESElementIndexUint
  , OESStandardDerivatives(OESStandardDerivatives), unOESStandardDerivatives, noOESStandardDerivatives, gTypeOESStandardDerivatives
  , OESTextureFloat(OESTextureFloat), unOESTextureFloat, noOESTextureFloat, gTypeOESTextureFloat
  , OESTextureFloatLinear(OESTextureFloatLinear), unOESTextureFloatLinear, noOESTextureFloatLinear, gTypeOESTextureFloatLinear
  , OESTextureHalfFloat(OESTextureHalfFloat), unOESTextureHalfFloat, noOESTextureHalfFloat, gTypeOESTextureHalfFloat
  , OESTextureHalfFloatLinear(OESTextureHalfFloatLinear), unOESTextureHalfFloatLinear, noOESTextureHalfFloatLinear, gTypeOESTextureHalfFloatLinear
  , OESVertexArrayObject(OESVertexArrayObject), unOESVertexArrayObject, noOESVertexArrayObject, gTypeOESVertexArrayObject
  , OfflineAudioCompletionEvent(OfflineAudioCompletionEvent), unOfflineAudioCompletionEvent, noOfflineAudioCompletionEvent, gTypeOfflineAudioCompletionEvent
  , OfflineAudioContext(OfflineAudioContext), unOfflineAudioContext, noOfflineAudioContext, gTypeOfflineAudioContext
  , OscillatorNode(OscillatorNode), unOscillatorNode, noOscillatorNode, gTypeOscillatorNode
  , OverconstrainedError(OverconstrainedError), unOverconstrainedError, noOverconstrainedError, gTypeOverconstrainedError
  , OverconstrainedErrorEvent(OverconstrainedErrorEvent), unOverconstrainedErrorEvent, noOverconstrainedErrorEvent, gTypeOverconstrainedErrorEvent
  , OverconstrainedErrorEventInit(OverconstrainedErrorEventInit), unOverconstrainedErrorEventInit, noOverconstrainedErrorEventInit, gTypeOverconstrainedErrorEventInit
  , OverflowEvent(OverflowEvent), unOverflowEvent, noOverflowEvent, gTypeOverflowEvent
  , OverflowEventInit(OverflowEventInit), unOverflowEventInit, noOverflowEventInit, gTypeOverflowEventInit
  , PageTransitionEvent(PageTransitionEvent), unPageTransitionEvent, noPageTransitionEvent, gTypePageTransitionEvent
  , PageTransitionEventInit(PageTransitionEventInit), unPageTransitionEventInit, noPageTransitionEventInit, gTypePageTransitionEventInit
  , PannerNode(PannerNode), unPannerNode, noPannerNode, gTypePannerNode
  , ParentNode(ParentNode), unParentNode, IsParentNode, toParentNode, noParentNode, gTypeParentNode
  , PasswordCredential(PasswordCredential), unPasswordCredential, noPasswordCredential, gTypePasswordCredential
  , PasswordCredentialData(PasswordCredentialData), unPasswordCredentialData, noPasswordCredentialData, gTypePasswordCredentialData
  , Path2D(Path2D), unPath2D, noPath2D, gTypePath2D
  , Pbkdf2Params(Pbkdf2Params), unPbkdf2Params, noPbkdf2Params, gTypePbkdf2Params
  , Performance(Performance), unPerformance, noPerformance, gTypePerformance
  , PerformanceEntry(PerformanceEntry), unPerformanceEntry, IsPerformanceEntry, toPerformanceEntry, noPerformanceEntry, gTypePerformanceEntry
  , PerformanceMark(PerformanceMark), unPerformanceMark, noPerformanceMark, gTypePerformanceMark
  , PerformanceMeasure(PerformanceMeasure), unPerformanceMeasure, noPerformanceMeasure, gTypePerformanceMeasure
  , PerformanceNavigation(PerformanceNavigation), unPerformanceNavigation, noPerformanceNavigation, gTypePerformanceNavigation
  , PerformanceObserver(PerformanceObserver), unPerformanceObserver, noPerformanceObserver, gTypePerformanceObserver
  , PerformanceObserverEntryList(PerformanceObserverEntryList), unPerformanceObserverEntryList, noPerformanceObserverEntryList, gTypePerformanceObserverEntryList
  , PerformanceObserverInit(PerformanceObserverInit), unPerformanceObserverInit, noPerformanceObserverInit, gTypePerformanceObserverInit
  , PerformanceResourceTiming(PerformanceResourceTiming), unPerformanceResourceTiming, noPerformanceResourceTiming, gTypePerformanceResourceTiming
  , PerformanceTiming(PerformanceTiming), unPerformanceTiming, noPerformanceTiming, gTypePerformanceTiming
  , PeriodicWave(PeriodicWave), unPeriodicWave, noPeriodicWave, gTypePeriodicWave
  , Plugin(Plugin), unPlugin, noPlugin, gTypePlugin
  , PluginArray(PluginArray), unPluginArray, noPluginArray, gTypePluginArray
  , PopStateEvent(PopStateEvent), unPopStateEvent, noPopStateEvent, gTypePopStateEvent
  , PopStateEventInit(PopStateEventInit), unPopStateEventInit, noPopStateEventInit, gTypePopStateEventInit
  , PositionError(PositionError), unPositionError, noPositionError, gTypePositionError
  , PositionOptions(PositionOptions), unPositionOptions, noPositionOptions, gTypePositionOptions
  , ProcessingInstruction(ProcessingInstruction), unProcessingInstruction, noProcessingInstruction, gTypeProcessingInstruction
  , ProgressEvent(ProgressEvent), unProgressEvent, IsProgressEvent, toProgressEvent, noProgressEvent, gTypeProgressEvent
  , ProgressEventInit(ProgressEventInit), unProgressEventInit, noProgressEventInit, gTypeProgressEventInit
  , PromiseRejectionEvent(PromiseRejectionEvent), unPromiseRejectionEvent, noPromiseRejectionEvent, gTypePromiseRejectionEvent
  , PromiseRejectionEventInit(PromiseRejectionEventInit), unPromiseRejectionEventInit, noPromiseRejectionEventInit, gTypePromiseRejectionEventInit
  , QuickTimePluginReplacement(QuickTimePluginReplacement), unQuickTimePluginReplacement, noQuickTimePluginReplacement, gTypeQuickTimePluginReplacement
  , RGBColor(RGBColor), unRGBColor, noRGBColor, gTypeRGBColor
  , RTCAnswerOptions(RTCAnswerOptions), unRTCAnswerOptions, noRTCAnswerOptions, gTypeRTCAnswerOptions
  , RTCConfiguration(RTCConfiguration), unRTCConfiguration, noRTCConfiguration, gTypeRTCConfiguration
  , RTCDTMFSender(RTCDTMFSender), unRTCDTMFSender, noRTCDTMFSender, gTypeRTCDTMFSender
  , RTCDTMFToneChangeEvent(RTCDTMFToneChangeEvent), unRTCDTMFToneChangeEvent, noRTCDTMFToneChangeEvent, gTypeRTCDTMFToneChangeEvent
  , RTCDTMFToneChangeEventInit(RTCDTMFToneChangeEventInit), unRTCDTMFToneChangeEventInit, noRTCDTMFToneChangeEventInit, gTypeRTCDTMFToneChangeEventInit
  , RTCDataChannel(RTCDataChannel), unRTCDataChannel, noRTCDataChannel, gTypeRTCDataChannel
  , RTCDataChannelEvent(RTCDataChannelEvent), unRTCDataChannelEvent, noRTCDataChannelEvent, gTypeRTCDataChannelEvent
  , RTCDataChannelEventInit(RTCDataChannelEventInit), unRTCDataChannelEventInit, noRTCDataChannelEventInit, gTypeRTCDataChannelEventInit
  , RTCDataChannelInit(RTCDataChannelInit), unRTCDataChannelInit, noRTCDataChannelInit, gTypeRTCDataChannelInit
  , RTCDataChannelStats(RTCDataChannelStats), unRTCDataChannelStats, noRTCDataChannelStats, gTypeRTCDataChannelStats
  , RTCIceCandidate(RTCIceCandidate), unRTCIceCandidate, noRTCIceCandidate, gTypeRTCIceCandidate
  , RTCIceCandidateEvent(RTCIceCandidateEvent), unRTCIceCandidateEvent, noRTCIceCandidateEvent, gTypeRTCIceCandidateEvent
  , RTCIceCandidateInit(RTCIceCandidateInit), unRTCIceCandidateInit, noRTCIceCandidateInit, gTypeRTCIceCandidateInit
  , RTCIceServer(RTCIceServer), unRTCIceServer, noRTCIceServer, gTypeRTCIceServer
  , RTCIceTransport(RTCIceTransport), unRTCIceTransport, noRTCIceTransport, gTypeRTCIceTransport
  , RTCInboundRTPStreamStats(RTCInboundRTPStreamStats), unRTCInboundRTPStreamStats, noRTCInboundRTPStreamStats, gTypeRTCInboundRTPStreamStats
  , RTCMediaStreamTrackStats(RTCMediaStreamTrackStats), unRTCMediaStreamTrackStats, noRTCMediaStreamTrackStats, gTypeRTCMediaStreamTrackStats
  , RTCOfferAnswerOptions(RTCOfferAnswerOptions), unRTCOfferAnswerOptions, IsRTCOfferAnswerOptions, toRTCOfferAnswerOptions, noRTCOfferAnswerOptions, gTypeRTCOfferAnswerOptions
  , RTCOfferOptions(RTCOfferOptions), unRTCOfferOptions, noRTCOfferOptions, gTypeRTCOfferOptions
  , RTCOutboundRTPStreamStats(RTCOutboundRTPStreamStats), unRTCOutboundRTPStreamStats, noRTCOutboundRTPStreamStats, gTypeRTCOutboundRTPStreamStats
  , RTCPeerConnection(RTCPeerConnection), unRTCPeerConnection, noRTCPeerConnection, gTypeRTCPeerConnection
  , RTCPeerConnectionIceEvent(RTCPeerConnectionIceEvent), unRTCPeerConnectionIceEvent, noRTCPeerConnectionIceEvent, gTypeRTCPeerConnectionIceEvent
  , RTCRTPStreamStats(RTCRTPStreamStats), unRTCRTPStreamStats, IsRTCRTPStreamStats, toRTCRTPStreamStats, noRTCRTPStreamStats, gTypeRTCRTPStreamStats
  , RTCRtpCodecParameters(RTCRtpCodecParameters), unRTCRtpCodecParameters, noRTCRtpCodecParameters, gTypeRTCRtpCodecParameters
  , RTCRtpEncodingParameters(RTCRtpEncodingParameters), unRTCRtpEncodingParameters, noRTCRtpEncodingParameters, gTypeRTCRtpEncodingParameters
  , RTCRtpFecParameters(RTCRtpFecParameters), unRTCRtpFecParameters, noRTCRtpFecParameters, gTypeRTCRtpFecParameters
  , RTCRtpHeaderExtensionParameters(RTCRtpHeaderExtensionParameters), unRTCRtpHeaderExtensionParameters, noRTCRtpHeaderExtensionParameters, gTypeRTCRtpHeaderExtensionParameters
  , RTCRtpParameters(RTCRtpParameters), unRTCRtpParameters, noRTCRtpParameters, gTypeRTCRtpParameters
  , RTCRtpReceiver(RTCRtpReceiver), unRTCRtpReceiver, noRTCRtpReceiver, gTypeRTCRtpReceiver
  , RTCRtpRtxParameters(RTCRtpRtxParameters), unRTCRtpRtxParameters, noRTCRtpRtxParameters, gTypeRTCRtpRtxParameters
  , RTCRtpSender(RTCRtpSender), unRTCRtpSender, noRTCRtpSender, gTypeRTCRtpSender
  , RTCRtpTransceiver(RTCRtpTransceiver), unRTCRtpTransceiver, noRTCRtpTransceiver, gTypeRTCRtpTransceiver
  , RTCRtpTransceiverInit(RTCRtpTransceiverInit), unRTCRtpTransceiverInit, noRTCRtpTransceiverInit, gTypeRTCRtpTransceiverInit
  , RTCSessionDescription(RTCSessionDescription), unRTCSessionDescription, noRTCSessionDescription, gTypeRTCSessionDescription
  , RTCSessionDescriptionInit(RTCSessionDescriptionInit), unRTCSessionDescriptionInit, noRTCSessionDescriptionInit, gTypeRTCSessionDescriptionInit
  , RTCStats(RTCStats), unRTCStats, IsRTCStats, toRTCStats, noRTCStats, gTypeRTCStats
  , RTCStatsReport(RTCStatsReport), unRTCStatsReport, noRTCStatsReport, gTypeRTCStatsReport
  , RTCTrackEvent(RTCTrackEvent), unRTCTrackEvent, noRTCTrackEvent, gTypeRTCTrackEvent
  , RTCTrackEventInit(RTCTrackEventInit), unRTCTrackEventInit, noRTCTrackEventInit, gTypeRTCTrackEventInit
  , RadioNodeList(RadioNodeList), unRadioNodeList, noRadioNodeList, gTypeRadioNodeList
  , Range(Range), unRange, noRange, gTypeRange
  , ReadableByteStreamController(ReadableByteStreamController), unReadableByteStreamController, noReadableByteStreamController, gTypeReadableByteStreamController
  , ReadableStream(ReadableStream), unReadableStream, noReadableStream, gTypeReadableStream
  , ReadableStreamBYOBReader(ReadableStreamBYOBReader), unReadableStreamBYOBReader, noReadableStreamBYOBReader, gTypeReadableStreamBYOBReader
  , ReadableStreamBYOBRequest(ReadableStreamBYOBRequest), unReadableStreamBYOBRequest, noReadableStreamBYOBRequest, gTypeReadableStreamBYOBRequest
  , ReadableStreamDefaultController(ReadableStreamDefaultController), unReadableStreamDefaultController, noReadableStreamDefaultController, gTypeReadableStreamDefaultController
  , ReadableStreamDefaultReader(ReadableStreamDefaultReader), unReadableStreamDefaultReader, noReadableStreamDefaultReader, gTypeReadableStreamDefaultReader
  , ReadableStreamSource(ReadableStreamSource), unReadableStreamSource, noReadableStreamSource, gTypeReadableStreamSource
  , Rect(Rect), unRect, noRect, gTypeRect
  , Request(Request), unRequest, noRequest, gTypeRequest
  , RequestInit(RequestInit), unRequestInit, noRequestInit, gTypeRequestInit
  , Response(Response), unResponse, noResponse, gTypeResponse
  , RotationRate(RotationRate), unRotationRate, noRotationRate, gTypeRotationRate
  , RsaHashedImportParams(RsaHashedImportParams), unRsaHashedImportParams, noRsaHashedImportParams, gTypeRsaHashedImportParams
  , RsaHashedKeyGenParams(RsaHashedKeyGenParams), unRsaHashedKeyGenParams, noRsaHashedKeyGenParams, gTypeRsaHashedKeyGenParams
  , RsaKeyGenParams(RsaKeyGenParams), unRsaKeyGenParams, IsRsaKeyGenParams, toRsaKeyGenParams, noRsaKeyGenParams, gTypeRsaKeyGenParams
  , RsaOaepParams(RsaOaepParams), unRsaOaepParams, noRsaOaepParams, gTypeRsaOaepParams
  , RsaOtherPrimesInfo(RsaOtherPrimesInfo), unRsaOtherPrimesInfo, noRsaOtherPrimesInfo, gTypeRsaOtherPrimesInfo
  , SQLError(SQLError), unSQLError, noSQLError, gTypeSQLError
  , SQLException(SQLException), unSQLException, noSQLException, gTypeSQLException
  , SQLResultSet(SQLResultSet), unSQLResultSet, noSQLResultSet, gTypeSQLResultSet
  , SQLResultSetRowList(SQLResultSetRowList), unSQLResultSetRowList, noSQLResultSetRowList, gTypeSQLResultSetRowList
  , SQLTransaction(SQLTransaction), unSQLTransaction, noSQLTransaction, gTypeSQLTransaction
  , SVGAElement(SVGAElement), unSVGAElement, noSVGAElement, gTypeSVGAElement
  , SVGAltGlyphDefElement(SVGAltGlyphDefElement), unSVGAltGlyphDefElement, noSVGAltGlyphDefElement, gTypeSVGAltGlyphDefElement
  , SVGAltGlyphElement(SVGAltGlyphElement), unSVGAltGlyphElement, noSVGAltGlyphElement, gTypeSVGAltGlyphElement
  , SVGAltGlyphItemElement(SVGAltGlyphItemElement), unSVGAltGlyphItemElement, noSVGAltGlyphItemElement, gTypeSVGAltGlyphItemElement
  , SVGAngle(SVGAngle), unSVGAngle, noSVGAngle, gTypeSVGAngle
  , SVGAnimateColorElement(SVGAnimateColorElement), unSVGAnimateColorElement, noSVGAnimateColorElement, gTypeSVGAnimateColorElement
  , SVGAnimateElement(SVGAnimateElement), unSVGAnimateElement, noSVGAnimateElement, gTypeSVGAnimateElement
  , SVGAnimateMotionElement(SVGAnimateMotionElement), unSVGAnimateMotionElement, noSVGAnimateMotionElement, gTypeSVGAnimateMotionElement
  , SVGAnimateTransformElement(SVGAnimateTransformElement), unSVGAnimateTransformElement, noSVGAnimateTransformElement, gTypeSVGAnimateTransformElement
  , SVGAnimatedAngle(SVGAnimatedAngle), unSVGAnimatedAngle, noSVGAnimatedAngle, gTypeSVGAnimatedAngle
  , SVGAnimatedBoolean(SVGAnimatedBoolean), unSVGAnimatedBoolean, noSVGAnimatedBoolean, gTypeSVGAnimatedBoolean
  , SVGAnimatedEnumeration(SVGAnimatedEnumeration), unSVGAnimatedEnumeration, noSVGAnimatedEnumeration, gTypeSVGAnimatedEnumeration
  , SVGAnimatedInteger(SVGAnimatedInteger), unSVGAnimatedInteger, noSVGAnimatedInteger, gTypeSVGAnimatedInteger
  , SVGAnimatedLength(SVGAnimatedLength), unSVGAnimatedLength, noSVGAnimatedLength, gTypeSVGAnimatedLength
  , SVGAnimatedLengthList(SVGAnimatedLengthList), unSVGAnimatedLengthList, noSVGAnimatedLengthList, gTypeSVGAnimatedLengthList
  , SVGAnimatedNumber(SVGAnimatedNumber), unSVGAnimatedNumber, noSVGAnimatedNumber, gTypeSVGAnimatedNumber
  , SVGAnimatedNumberList(SVGAnimatedNumberList), unSVGAnimatedNumberList, noSVGAnimatedNumberList, gTypeSVGAnimatedNumberList
  , SVGAnimatedPreserveAspectRatio(SVGAnimatedPreserveAspectRatio), unSVGAnimatedPreserveAspectRatio, noSVGAnimatedPreserveAspectRatio, gTypeSVGAnimatedPreserveAspectRatio
  , SVGAnimatedRect(SVGAnimatedRect), unSVGAnimatedRect, noSVGAnimatedRect, gTypeSVGAnimatedRect
  , SVGAnimatedString(SVGAnimatedString), unSVGAnimatedString, noSVGAnimatedString, gTypeSVGAnimatedString
  , SVGAnimatedTransformList(SVGAnimatedTransformList), unSVGAnimatedTransformList, noSVGAnimatedTransformList, gTypeSVGAnimatedTransformList
  , SVGAnimationElement(SVGAnimationElement), unSVGAnimationElement, IsSVGAnimationElement, toSVGAnimationElement, noSVGAnimationElement, gTypeSVGAnimationElement
  , SVGCircleElement(SVGCircleElement), unSVGCircleElement, noSVGCircleElement, gTypeSVGCircleElement
  , SVGClipPathElement(SVGClipPathElement), unSVGClipPathElement, noSVGClipPathElement, gTypeSVGClipPathElement
  , SVGComponentTransferFunctionElement(SVGComponentTransferFunctionElement), unSVGComponentTransferFunctionElement, IsSVGComponentTransferFunctionElement, toSVGComponentTransferFunctionElement, noSVGComponentTransferFunctionElement, gTypeSVGComponentTransferFunctionElement
  , SVGCursorElement(SVGCursorElement), unSVGCursorElement, noSVGCursorElement, gTypeSVGCursorElement
  , SVGDefsElement(SVGDefsElement), unSVGDefsElement, noSVGDefsElement, gTypeSVGDefsElement
  , SVGDescElement(SVGDescElement), unSVGDescElement, noSVGDescElement, gTypeSVGDescElement
  , SVGElement(SVGElement), unSVGElement, IsSVGElement, toSVGElement, noSVGElement, gTypeSVGElement
  , SVGEllipseElement(SVGEllipseElement), unSVGEllipseElement, noSVGEllipseElement, gTypeSVGEllipseElement
  , SVGException(SVGException), unSVGException, noSVGException, gTypeSVGException
  , SVGExternalResourcesRequired(SVGExternalResourcesRequired), unSVGExternalResourcesRequired, IsSVGExternalResourcesRequired, toSVGExternalResourcesRequired, noSVGExternalResourcesRequired, gTypeSVGExternalResourcesRequired
  , SVGFEBlendElement(SVGFEBlendElement), unSVGFEBlendElement, noSVGFEBlendElement, gTypeSVGFEBlendElement
  , SVGFEColorMatrixElement(SVGFEColorMatrixElement), unSVGFEColorMatrixElement, noSVGFEColorMatrixElement, gTypeSVGFEColorMatrixElement
  , SVGFEComponentTransferElement(SVGFEComponentTransferElement), unSVGFEComponentTransferElement, noSVGFEComponentTransferElement, gTypeSVGFEComponentTransferElement
  , SVGFECompositeElement(SVGFECompositeElement), unSVGFECompositeElement, noSVGFECompositeElement, gTypeSVGFECompositeElement
  , SVGFEConvolveMatrixElement(SVGFEConvolveMatrixElement), unSVGFEConvolveMatrixElement, noSVGFEConvolveMatrixElement, gTypeSVGFEConvolveMatrixElement
  , SVGFEDiffuseLightingElement(SVGFEDiffuseLightingElement), unSVGFEDiffuseLightingElement, noSVGFEDiffuseLightingElement, gTypeSVGFEDiffuseLightingElement
  , SVGFEDisplacementMapElement(SVGFEDisplacementMapElement), unSVGFEDisplacementMapElement, noSVGFEDisplacementMapElement, gTypeSVGFEDisplacementMapElement
  , SVGFEDistantLightElement(SVGFEDistantLightElement), unSVGFEDistantLightElement, noSVGFEDistantLightElement, gTypeSVGFEDistantLightElement
  , SVGFEDropShadowElement(SVGFEDropShadowElement), unSVGFEDropShadowElement, noSVGFEDropShadowElement, gTypeSVGFEDropShadowElement
  , SVGFEFloodElement(SVGFEFloodElement), unSVGFEFloodElement, noSVGFEFloodElement, gTypeSVGFEFloodElement
  , SVGFEFuncAElement(SVGFEFuncAElement), unSVGFEFuncAElement, noSVGFEFuncAElement, gTypeSVGFEFuncAElement
  , SVGFEFuncBElement(SVGFEFuncBElement), unSVGFEFuncBElement, noSVGFEFuncBElement, gTypeSVGFEFuncBElement
  , SVGFEFuncGElement(SVGFEFuncGElement), unSVGFEFuncGElement, noSVGFEFuncGElement, gTypeSVGFEFuncGElement
  , SVGFEFuncRElement(SVGFEFuncRElement), unSVGFEFuncRElement, noSVGFEFuncRElement, gTypeSVGFEFuncRElement
  , SVGFEGaussianBlurElement(SVGFEGaussianBlurElement), unSVGFEGaussianBlurElement, noSVGFEGaussianBlurElement, gTypeSVGFEGaussianBlurElement
  , SVGFEImageElement(SVGFEImageElement), unSVGFEImageElement, noSVGFEImageElement, gTypeSVGFEImageElement
  , SVGFEMergeElement(SVGFEMergeElement), unSVGFEMergeElement, noSVGFEMergeElement, gTypeSVGFEMergeElement
  , SVGFEMergeNodeElement(SVGFEMergeNodeElement), unSVGFEMergeNodeElement, noSVGFEMergeNodeElement, gTypeSVGFEMergeNodeElement
  , SVGFEMorphologyElement(SVGFEMorphologyElement), unSVGFEMorphologyElement, noSVGFEMorphologyElement, gTypeSVGFEMorphologyElement
  , SVGFEOffsetElement(SVGFEOffsetElement), unSVGFEOffsetElement, noSVGFEOffsetElement, gTypeSVGFEOffsetElement
  , SVGFEPointLightElement(SVGFEPointLightElement), unSVGFEPointLightElement, noSVGFEPointLightElement, gTypeSVGFEPointLightElement
  , SVGFESpecularLightingElement(SVGFESpecularLightingElement), unSVGFESpecularLightingElement, noSVGFESpecularLightingElement, gTypeSVGFESpecularLightingElement
  , SVGFESpotLightElement(SVGFESpotLightElement), unSVGFESpotLightElement, noSVGFESpotLightElement, gTypeSVGFESpotLightElement
  , SVGFETileElement(SVGFETileElement), unSVGFETileElement, noSVGFETileElement, gTypeSVGFETileElement
  , SVGFETurbulenceElement(SVGFETurbulenceElement), unSVGFETurbulenceElement, noSVGFETurbulenceElement, gTypeSVGFETurbulenceElement
  , SVGFilterElement(SVGFilterElement), unSVGFilterElement, noSVGFilterElement, gTypeSVGFilterElement
  , SVGFilterPrimitiveStandardAttributes(SVGFilterPrimitiveStandardAttributes), unSVGFilterPrimitiveStandardAttributes, IsSVGFilterPrimitiveStandardAttributes, toSVGFilterPrimitiveStandardAttributes, noSVGFilterPrimitiveStandardAttributes, gTypeSVGFilterPrimitiveStandardAttributes
  , SVGFitToViewBox(SVGFitToViewBox), unSVGFitToViewBox, IsSVGFitToViewBox, toSVGFitToViewBox, noSVGFitToViewBox, gTypeSVGFitToViewBox
  , SVGFontElement(SVGFontElement), unSVGFontElement, noSVGFontElement, gTypeSVGFontElement
  , SVGFontFaceElement(SVGFontFaceElement), unSVGFontFaceElement, noSVGFontFaceElement, gTypeSVGFontFaceElement
  , SVGFontFaceFormatElement(SVGFontFaceFormatElement), unSVGFontFaceFormatElement, noSVGFontFaceFormatElement, gTypeSVGFontFaceFormatElement
  , SVGFontFaceNameElement(SVGFontFaceNameElement), unSVGFontFaceNameElement, noSVGFontFaceNameElement, gTypeSVGFontFaceNameElement
  , SVGFontFaceSrcElement(SVGFontFaceSrcElement), unSVGFontFaceSrcElement, noSVGFontFaceSrcElement, gTypeSVGFontFaceSrcElement
  , SVGFontFaceUriElement(SVGFontFaceUriElement), unSVGFontFaceUriElement, noSVGFontFaceUriElement, gTypeSVGFontFaceUriElement
  , SVGForeignObjectElement(SVGForeignObjectElement), unSVGForeignObjectElement, noSVGForeignObjectElement, gTypeSVGForeignObjectElement
  , SVGGElement(SVGGElement), unSVGGElement, noSVGGElement, gTypeSVGGElement
  , SVGGlyphElement(SVGGlyphElement), unSVGGlyphElement, noSVGGlyphElement, gTypeSVGGlyphElement
  , SVGGlyphRefElement(SVGGlyphRefElement), unSVGGlyphRefElement, noSVGGlyphRefElement, gTypeSVGGlyphRefElement
  , SVGGradientElement(SVGGradientElement), unSVGGradientElement, IsSVGGradientElement, toSVGGradientElement, noSVGGradientElement, gTypeSVGGradientElement
  , SVGGraphicsElement(SVGGraphicsElement), unSVGGraphicsElement, IsSVGGraphicsElement, toSVGGraphicsElement, noSVGGraphicsElement, gTypeSVGGraphicsElement
  , SVGHKernElement(SVGHKernElement), unSVGHKernElement, noSVGHKernElement, gTypeSVGHKernElement
  , SVGImageElement(SVGImageElement), unSVGImageElement, noSVGImageElement, gTypeSVGImageElement
  , SVGLength(SVGLength), unSVGLength, noSVGLength, gTypeSVGLength
  , SVGLengthList(SVGLengthList), unSVGLengthList, noSVGLengthList, gTypeSVGLengthList
  , SVGLineElement(SVGLineElement), unSVGLineElement, noSVGLineElement, gTypeSVGLineElement
  , SVGLinearGradientElement(SVGLinearGradientElement), unSVGLinearGradientElement, noSVGLinearGradientElement, gTypeSVGLinearGradientElement
  , SVGMPathElement(SVGMPathElement), unSVGMPathElement, noSVGMPathElement, gTypeSVGMPathElement
  , SVGMarkerElement(SVGMarkerElement), unSVGMarkerElement, noSVGMarkerElement, gTypeSVGMarkerElement
  , SVGMaskElement(SVGMaskElement), unSVGMaskElement, noSVGMaskElement, gTypeSVGMaskElement
  , SVGMatrix(SVGMatrix), unSVGMatrix, noSVGMatrix, gTypeSVGMatrix
  , SVGMetadataElement(SVGMetadataElement), unSVGMetadataElement, noSVGMetadataElement, gTypeSVGMetadataElement
  , SVGMissingGlyphElement(SVGMissingGlyphElement), unSVGMissingGlyphElement, noSVGMissingGlyphElement, gTypeSVGMissingGlyphElement
  , SVGNumber(SVGNumber), unSVGNumber, noSVGNumber, gTypeSVGNumber
  , SVGNumberList(SVGNumberList), unSVGNumberList, noSVGNumberList, gTypeSVGNumberList
  , SVGPathElement(SVGPathElement), unSVGPathElement, noSVGPathElement, gTypeSVGPathElement
  , SVGPathSeg(SVGPathSeg), unSVGPathSeg, IsSVGPathSeg, toSVGPathSeg, noSVGPathSeg, gTypeSVGPathSeg
  , SVGPathSegArcAbs(SVGPathSegArcAbs), unSVGPathSegArcAbs, noSVGPathSegArcAbs, gTypeSVGPathSegArcAbs
  , SVGPathSegArcRel(SVGPathSegArcRel), unSVGPathSegArcRel, noSVGPathSegArcRel, gTypeSVGPathSegArcRel
  , SVGPathSegClosePath(SVGPathSegClosePath), unSVGPathSegClosePath, noSVGPathSegClosePath, gTypeSVGPathSegClosePath
  , SVGPathSegCurvetoCubicAbs(SVGPathSegCurvetoCubicAbs), unSVGPathSegCurvetoCubicAbs, noSVGPathSegCurvetoCubicAbs, gTypeSVGPathSegCurvetoCubicAbs
  , SVGPathSegCurvetoCubicRel(SVGPathSegCurvetoCubicRel), unSVGPathSegCurvetoCubicRel, noSVGPathSegCurvetoCubicRel, gTypeSVGPathSegCurvetoCubicRel
  , SVGPathSegCurvetoCubicSmoothAbs(SVGPathSegCurvetoCubicSmoothAbs), unSVGPathSegCurvetoCubicSmoothAbs, noSVGPathSegCurvetoCubicSmoothAbs, gTypeSVGPathSegCurvetoCubicSmoothAbs
  , SVGPathSegCurvetoCubicSmoothRel(SVGPathSegCurvetoCubicSmoothRel), unSVGPathSegCurvetoCubicSmoothRel, noSVGPathSegCurvetoCubicSmoothRel, gTypeSVGPathSegCurvetoCubicSmoothRel
  , SVGPathSegCurvetoQuadraticAbs(SVGPathSegCurvetoQuadraticAbs), unSVGPathSegCurvetoQuadraticAbs, noSVGPathSegCurvetoQuadraticAbs, gTypeSVGPathSegCurvetoQuadraticAbs
  , SVGPathSegCurvetoQuadraticRel(SVGPathSegCurvetoQuadraticRel), unSVGPathSegCurvetoQuadraticRel, noSVGPathSegCurvetoQuadraticRel, gTypeSVGPathSegCurvetoQuadraticRel
  , SVGPathSegCurvetoQuadraticSmoothAbs(SVGPathSegCurvetoQuadraticSmoothAbs), unSVGPathSegCurvetoQuadraticSmoothAbs, noSVGPathSegCurvetoQuadraticSmoothAbs, gTypeSVGPathSegCurvetoQuadraticSmoothAbs
  , SVGPathSegCurvetoQuadraticSmoothRel(SVGPathSegCurvetoQuadraticSmoothRel), unSVGPathSegCurvetoQuadraticSmoothRel, noSVGPathSegCurvetoQuadraticSmoothRel, gTypeSVGPathSegCurvetoQuadraticSmoothRel
  , SVGPathSegLinetoAbs(SVGPathSegLinetoAbs), unSVGPathSegLinetoAbs, noSVGPathSegLinetoAbs, gTypeSVGPathSegLinetoAbs
  , SVGPathSegLinetoHorizontalAbs(SVGPathSegLinetoHorizontalAbs), unSVGPathSegLinetoHorizontalAbs, noSVGPathSegLinetoHorizontalAbs, gTypeSVGPathSegLinetoHorizontalAbs
  , SVGPathSegLinetoHorizontalRel(SVGPathSegLinetoHorizontalRel), unSVGPathSegLinetoHorizontalRel, noSVGPathSegLinetoHorizontalRel, gTypeSVGPathSegLinetoHorizontalRel
  , SVGPathSegLinetoRel(SVGPathSegLinetoRel), unSVGPathSegLinetoRel, noSVGPathSegLinetoRel, gTypeSVGPathSegLinetoRel
  , SVGPathSegLinetoVerticalAbs(SVGPathSegLinetoVerticalAbs), unSVGPathSegLinetoVerticalAbs, noSVGPathSegLinetoVerticalAbs, gTypeSVGPathSegLinetoVerticalAbs
  , SVGPathSegLinetoVerticalRel(SVGPathSegLinetoVerticalRel), unSVGPathSegLinetoVerticalRel, noSVGPathSegLinetoVerticalRel, gTypeSVGPathSegLinetoVerticalRel
  , SVGPathSegList(SVGPathSegList), unSVGPathSegList, noSVGPathSegList, gTypeSVGPathSegList
  , SVGPathSegMovetoAbs(SVGPathSegMovetoAbs), unSVGPathSegMovetoAbs, noSVGPathSegMovetoAbs, gTypeSVGPathSegMovetoAbs
  , SVGPathSegMovetoRel(SVGPathSegMovetoRel), unSVGPathSegMovetoRel, noSVGPathSegMovetoRel, gTypeSVGPathSegMovetoRel
  , SVGPatternElement(SVGPatternElement), unSVGPatternElement, noSVGPatternElement, gTypeSVGPatternElement
  , SVGPoint(SVGPoint), unSVGPoint, noSVGPoint, gTypeSVGPoint
  , SVGPointList(SVGPointList), unSVGPointList, noSVGPointList, gTypeSVGPointList
  , SVGPolygonElement(SVGPolygonElement), unSVGPolygonElement, noSVGPolygonElement, gTypeSVGPolygonElement
  , SVGPolylineElement(SVGPolylineElement), unSVGPolylineElement, noSVGPolylineElement, gTypeSVGPolylineElement
  , SVGPreserveAspectRatio(SVGPreserveAspectRatio), unSVGPreserveAspectRatio, noSVGPreserveAspectRatio, gTypeSVGPreserveAspectRatio
  , SVGRadialGradientElement(SVGRadialGradientElement), unSVGRadialGradientElement, noSVGRadialGradientElement, gTypeSVGRadialGradientElement
  , SVGRect(SVGRect), unSVGRect, noSVGRect, gTypeSVGRect
  , SVGRectElement(SVGRectElement), unSVGRectElement, noSVGRectElement, gTypeSVGRectElement
  , SVGRenderingIntent(SVGRenderingIntent), unSVGRenderingIntent, noSVGRenderingIntent, gTypeSVGRenderingIntent
  , SVGSVGElement(SVGSVGElement), unSVGSVGElement, noSVGSVGElement, gTypeSVGSVGElement
  , SVGScriptElement(SVGScriptElement), unSVGScriptElement, noSVGScriptElement, gTypeSVGScriptElement
  , SVGSetElement(SVGSetElement), unSVGSetElement, noSVGSetElement, gTypeSVGSetElement
  , SVGStopElement(SVGStopElement), unSVGStopElement, noSVGStopElement, gTypeSVGStopElement
  , SVGStringList(SVGStringList), unSVGStringList, noSVGStringList, gTypeSVGStringList
  , SVGStyleElement(SVGStyleElement), unSVGStyleElement, noSVGStyleElement, gTypeSVGStyleElement
  , SVGSwitchElement(SVGSwitchElement), unSVGSwitchElement, noSVGSwitchElement, gTypeSVGSwitchElement
  , SVGSymbolElement(SVGSymbolElement), unSVGSymbolElement, noSVGSymbolElement, gTypeSVGSymbolElement
  , SVGTRefElement(SVGTRefElement), unSVGTRefElement, noSVGTRefElement, gTypeSVGTRefElement
  , SVGTSpanElement(SVGTSpanElement), unSVGTSpanElement, noSVGTSpanElement, gTypeSVGTSpanElement
  , SVGTests(SVGTests), unSVGTests, IsSVGTests, toSVGTests, noSVGTests, gTypeSVGTests
  , SVGTextContentElement(SVGTextContentElement), unSVGTextContentElement, IsSVGTextContentElement, toSVGTextContentElement, noSVGTextContentElement, gTypeSVGTextContentElement
  , SVGTextElement(SVGTextElement), unSVGTextElement, noSVGTextElement, gTypeSVGTextElement
  , SVGTextPathElement(SVGTextPathElement), unSVGTextPathElement, noSVGTextPathElement, gTypeSVGTextPathElement
  , SVGTextPositioningElement(SVGTextPositioningElement), unSVGTextPositioningElement, IsSVGTextPositioningElement, toSVGTextPositioningElement, noSVGTextPositioningElement, gTypeSVGTextPositioningElement
  , SVGTitleElement(SVGTitleElement), unSVGTitleElement, noSVGTitleElement, gTypeSVGTitleElement
  , SVGTransform(SVGTransform), unSVGTransform, noSVGTransform, gTypeSVGTransform
  , SVGTransformList(SVGTransformList), unSVGTransformList, noSVGTransformList, gTypeSVGTransformList
  , SVGURIReference(SVGURIReference), unSVGURIReference, IsSVGURIReference, toSVGURIReference, noSVGURIReference, gTypeSVGURIReference
  , SVGUnitTypes(SVGUnitTypes), unSVGUnitTypes, noSVGUnitTypes, gTypeSVGUnitTypes
  , SVGUseElement(SVGUseElement), unSVGUseElement, noSVGUseElement, gTypeSVGUseElement
  , SVGVKernElement(SVGVKernElement), unSVGVKernElement, noSVGVKernElement, gTypeSVGVKernElement
  , SVGViewElement(SVGViewElement), unSVGViewElement, noSVGViewElement, gTypeSVGViewElement
  , SVGViewSpec(SVGViewSpec), unSVGViewSpec, noSVGViewSpec, gTypeSVGViewSpec
  , SVGZoomAndPan(SVGZoomAndPan), unSVGZoomAndPan, IsSVGZoomAndPan, toSVGZoomAndPan, noSVGZoomAndPan, gTypeSVGZoomAndPan
  , SVGZoomEvent(SVGZoomEvent), unSVGZoomEvent, noSVGZoomEvent, gTypeSVGZoomEvent
  , Screen(Screen), unScreen, noScreen, gTypeScreen
  , ScriptProcessorNode(ScriptProcessorNode), unScriptProcessorNode, noScriptProcessorNode, gTypeScriptProcessorNode
  , ScrollToOptions(ScrollToOptions), unScrollToOptions, noScrollToOptions, gTypeScrollToOptions
  , SecurityPolicyViolationEvent(SecurityPolicyViolationEvent), unSecurityPolicyViolationEvent, noSecurityPolicyViolationEvent, gTypeSecurityPolicyViolationEvent
  , SecurityPolicyViolationEventInit(SecurityPolicyViolationEventInit), unSecurityPolicyViolationEventInit, noSecurityPolicyViolationEventInit, gTypeSecurityPolicyViolationEventInit
  , Selection(Selection), unSelection, noSelection, gTypeSelection
  , ShadowRoot(ShadowRoot), unShadowRoot, noShadowRoot, gTypeShadowRoot
  , ShadowRootInit(ShadowRootInit), unShadowRootInit, noShadowRootInit, gTypeShadowRootInit
  , SiteBoundCredential(SiteBoundCredential), unSiteBoundCredential, IsSiteBoundCredential, toSiteBoundCredential, noSiteBoundCredential, gTypeSiteBoundCredential
  , SiteBoundCredentialData(SiteBoundCredentialData), unSiteBoundCredentialData, IsSiteBoundCredentialData, toSiteBoundCredentialData, noSiteBoundCredentialData, gTypeSiteBoundCredentialData
  , Slotable(Slotable), unSlotable, IsSlotable, toSlotable, noSlotable, gTypeSlotable
  , SourceBuffer(SourceBuffer), unSourceBuffer, noSourceBuffer, gTypeSourceBuffer
  , SourceBufferList(SourceBufferList), unSourceBufferList, noSourceBufferList, gTypeSourceBufferList
  , SpeechSynthesis(SpeechSynthesis), unSpeechSynthesis, noSpeechSynthesis, gTypeSpeechSynthesis
  , SpeechSynthesisEvent(SpeechSynthesisEvent), unSpeechSynthesisEvent, noSpeechSynthesisEvent, gTypeSpeechSynthesisEvent
  , SpeechSynthesisUtterance(SpeechSynthesisUtterance), unSpeechSynthesisUtterance, noSpeechSynthesisUtterance, gTypeSpeechSynthesisUtterance
  , SpeechSynthesisVoice(SpeechSynthesisVoice), unSpeechSynthesisVoice, noSpeechSynthesisVoice, gTypeSpeechSynthesisVoice
  , StaticRange(StaticRange), unStaticRange, noStaticRange, gTypeStaticRange
  , Storage(Storage), unStorage, noStorage, gTypeStorage
  , StorageEvent(StorageEvent), unStorageEvent, noStorageEvent, gTypeStorageEvent
  , StorageEventInit(StorageEventInit), unStorageEventInit, noStorageEventInit, gTypeStorageEventInit
  , StorageInfo(StorageInfo), unStorageInfo, noStorageInfo, gTypeStorageInfo
  , StorageQuota(StorageQuota), unStorageQuota, noStorageQuota, gTypeStorageQuota
  , StyleMedia(StyleMedia), unStyleMedia, noStyleMedia, gTypeStyleMedia
  , StyleSheet(StyleSheet), unStyleSheet, IsStyleSheet, toStyleSheet, noStyleSheet, gTypeStyleSheet
  , StyleSheetList(StyleSheetList), unStyleSheetList, noStyleSheetList, gTypeStyleSheetList
  , SubtleCrypto(SubtleCrypto), unSubtleCrypto, noSubtleCrypto, gTypeSubtleCrypto
  , Text(Text), unText, IsText, toText, noText, gTypeText
  , TextDecodeOptions(TextDecodeOptions), unTextDecodeOptions, noTextDecodeOptions, gTypeTextDecodeOptions
  , TextDecoder(TextDecoder), unTextDecoder, noTextDecoder, gTypeTextDecoder
  , TextDecoderOptions(TextDecoderOptions), unTextDecoderOptions, noTextDecoderOptions, gTypeTextDecoderOptions
  , TextEncoder(TextEncoder), unTextEncoder, noTextEncoder, gTypeTextEncoder
  , TextEvent(TextEvent), unTextEvent, noTextEvent, gTypeTextEvent
  , TextMetrics(TextMetrics), unTextMetrics, noTextMetrics, gTypeTextMetrics
  , TextTrack(TextTrack), unTextTrack, noTextTrack, gTypeTextTrack
  , TextTrackCue(TextTrackCue), unTextTrackCue, IsTextTrackCue, toTextTrackCue, noTextTrackCue, gTypeTextTrackCue
  , TextTrackCueList(TextTrackCueList), unTextTrackCueList, noTextTrackCueList, gTypeTextTrackCueList
  , TextTrackList(TextTrackList), unTextTrackList, noTextTrackList, gTypeTextTrackList
  , TimeRanges(TimeRanges), unTimeRanges, noTimeRanges, gTypeTimeRanges
  , Touch(Touch), unTouch, noTouch, gTypeTouch
  , TouchEvent(TouchEvent), unTouchEvent, noTouchEvent, gTypeTouchEvent
  , TouchEventInit(TouchEventInit), unTouchEventInit, noTouchEventInit, gTypeTouchEventInit
  , TouchList(TouchList), unTouchList, noTouchList, gTypeTouchList
  , TrackEvent(TrackEvent), unTrackEvent, noTrackEvent, gTypeTrackEvent
  , TrackEventInit(TrackEventInit), unTrackEventInit, noTrackEventInit, gTypeTrackEventInit
  , TransitionEvent(TransitionEvent), unTransitionEvent, noTransitionEvent, gTypeTransitionEvent
  , TransitionEventInit(TransitionEventInit), unTransitionEventInit, noTransitionEventInit, gTypeTransitionEventInit
  , TreeWalker(TreeWalker), unTreeWalker, noTreeWalker, gTypeTreeWalker
  , UIEvent(UIEvent), unUIEvent, IsUIEvent, toUIEvent, noUIEvent, gTypeUIEvent
  , UIEventInit(UIEventInit), unUIEventInit, IsUIEventInit, toUIEventInit, noUIEventInit, gTypeUIEventInit
  , URL(URL), unURL, noURL, gTypeURL
  , URLSearchParams(URLSearchParams), unURLSearchParams, noURLSearchParams, gTypeURLSearchParams
  , UserMessageHandler(UserMessageHandler), unUserMessageHandler, noUserMessageHandler, gTypeUserMessageHandler
  , UserMessageHandlersNamespace(UserMessageHandlersNamespace), unUserMessageHandlersNamespace, noUserMessageHandlersNamespace, gTypeUserMessageHandlersNamespace
  , VTTCue(VTTCue), unVTTCue, noVTTCue, gTypeVTTCue
  , VTTRegion(VTTRegion), unVTTRegion, noVTTRegion, gTypeVTTRegion
  , VTTRegionList(VTTRegionList), unVTTRegionList, noVTTRegionList, gTypeVTTRegionList
  , ValidityState(ValidityState), unValidityState, noValidityState, gTypeValidityState
  , VideoPlaybackQuality(VideoPlaybackQuality), unVideoPlaybackQuality, noVideoPlaybackQuality, gTypeVideoPlaybackQuality
  , VideoTrack(VideoTrack), unVideoTrack, noVideoTrack, gTypeVideoTrack
  , VideoTrackList(VideoTrackList), unVideoTrackList, noVideoTrackList, gTypeVideoTrackList
  , WaveShaperNode(WaveShaperNode), unWaveShaperNode, noWaveShaperNode, gTypeWaveShaperNode
  , WebGL2RenderingContext(WebGL2RenderingContext), unWebGL2RenderingContext, noWebGL2RenderingContext, gTypeWebGL2RenderingContext
  , WebGLActiveInfo(WebGLActiveInfo), unWebGLActiveInfo, noWebGLActiveInfo, gTypeWebGLActiveInfo
  , WebGLBuffer(WebGLBuffer), unWebGLBuffer, noWebGLBuffer, gTypeWebGLBuffer
  , WebGLCompressedTextureATC(WebGLCompressedTextureATC), unWebGLCompressedTextureATC, noWebGLCompressedTextureATC, gTypeWebGLCompressedTextureATC
  , WebGLCompressedTexturePVRTC(WebGLCompressedTexturePVRTC), unWebGLCompressedTexturePVRTC, noWebGLCompressedTexturePVRTC, gTypeWebGLCompressedTexturePVRTC
  , WebGLCompressedTextureS3TC(WebGLCompressedTextureS3TC), unWebGLCompressedTextureS3TC, noWebGLCompressedTextureS3TC, gTypeWebGLCompressedTextureS3TC
  , WebGLContextAttributes(WebGLContextAttributes), unWebGLContextAttributes, noWebGLContextAttributes, gTypeWebGLContextAttributes
  , WebGLContextEvent(WebGLContextEvent), unWebGLContextEvent, noWebGLContextEvent, gTypeWebGLContextEvent
  , WebGLContextEventInit(WebGLContextEventInit), unWebGLContextEventInit, noWebGLContextEventInit, gTypeWebGLContextEventInit
  , WebGLDebugRendererInfo(WebGLDebugRendererInfo), unWebGLDebugRendererInfo, noWebGLDebugRendererInfo, gTypeWebGLDebugRendererInfo
  , WebGLDebugShaders(WebGLDebugShaders), unWebGLDebugShaders, noWebGLDebugShaders, gTypeWebGLDebugShaders
  , WebGLDepthTexture(WebGLDepthTexture), unWebGLDepthTexture, noWebGLDepthTexture, gTypeWebGLDepthTexture
  , WebGLDrawBuffers(WebGLDrawBuffers), unWebGLDrawBuffers, noWebGLDrawBuffers, gTypeWebGLDrawBuffers
  , WebGLFramebuffer(WebGLFramebuffer), unWebGLFramebuffer, noWebGLFramebuffer, gTypeWebGLFramebuffer
  , WebGLLoseContext(WebGLLoseContext), unWebGLLoseContext, noWebGLLoseContext, gTypeWebGLLoseContext
  , WebGLProgram(WebGLProgram), unWebGLProgram, noWebGLProgram, gTypeWebGLProgram
  , WebGLQuery(WebGLQuery), unWebGLQuery, noWebGLQuery, gTypeWebGLQuery
  , WebGLRenderbuffer(WebGLRenderbuffer), unWebGLRenderbuffer, noWebGLRenderbuffer, gTypeWebGLRenderbuffer
  , WebGLRenderingContext(WebGLRenderingContext), unWebGLRenderingContext, noWebGLRenderingContext, gTypeWebGLRenderingContext
  , WebGLRenderingContextBase(WebGLRenderingContextBase), unWebGLRenderingContextBase, IsWebGLRenderingContextBase, toWebGLRenderingContextBase, noWebGLRenderingContextBase, gTypeWebGLRenderingContextBase
  , WebGLSampler(WebGLSampler), unWebGLSampler, noWebGLSampler, gTypeWebGLSampler
  , WebGLShader(WebGLShader), unWebGLShader, noWebGLShader, gTypeWebGLShader
  , WebGLShaderPrecisionFormat(WebGLShaderPrecisionFormat), unWebGLShaderPrecisionFormat, noWebGLShaderPrecisionFormat, gTypeWebGLShaderPrecisionFormat
  , WebGLSync(WebGLSync), unWebGLSync, noWebGLSync, gTypeWebGLSync
  , WebGLTexture(WebGLTexture), unWebGLTexture, noWebGLTexture, gTypeWebGLTexture
  , WebGLTransformFeedback(WebGLTransformFeedback), unWebGLTransformFeedback, noWebGLTransformFeedback, gTypeWebGLTransformFeedback
  , WebGLUniformLocation(WebGLUniformLocation), unWebGLUniformLocation, noWebGLUniformLocation, gTypeWebGLUniformLocation
  , WebGLVertexArrayObject(WebGLVertexArrayObject), unWebGLVertexArrayObject, noWebGLVertexArrayObject, gTypeWebGLVertexArrayObject
  , WebGLVertexArrayObjectOES(WebGLVertexArrayObjectOES), unWebGLVertexArrayObjectOES, noWebGLVertexArrayObjectOES, gTypeWebGLVertexArrayObjectOES
  , WebGPUBuffer(WebGPUBuffer), unWebGPUBuffer, noWebGPUBuffer, gTypeWebGPUBuffer
  , WebGPUCommandBuffer(WebGPUCommandBuffer), unWebGPUCommandBuffer, noWebGPUCommandBuffer, gTypeWebGPUCommandBuffer
  , WebGPUCommandQueue(WebGPUCommandQueue), unWebGPUCommandQueue, noWebGPUCommandQueue, gTypeWebGPUCommandQueue
  , WebGPUComputeCommandEncoder(WebGPUComputeCommandEncoder), unWebGPUComputeCommandEncoder, noWebGPUComputeCommandEncoder, gTypeWebGPUComputeCommandEncoder
  , WebGPUComputePipelineState(WebGPUComputePipelineState), unWebGPUComputePipelineState, noWebGPUComputePipelineState, gTypeWebGPUComputePipelineState
  , WebGPUDepthStencilDescriptor(WebGPUDepthStencilDescriptor), unWebGPUDepthStencilDescriptor, noWebGPUDepthStencilDescriptor, gTypeWebGPUDepthStencilDescriptor
  , WebGPUDepthStencilState(WebGPUDepthStencilState), unWebGPUDepthStencilState, noWebGPUDepthStencilState, gTypeWebGPUDepthStencilState
  , WebGPUDrawable(WebGPUDrawable), unWebGPUDrawable, noWebGPUDrawable, gTypeWebGPUDrawable
  , WebGPUFunction(WebGPUFunction), unWebGPUFunction, noWebGPUFunction, gTypeWebGPUFunction
  , WebGPULibrary(WebGPULibrary), unWebGPULibrary, noWebGPULibrary, gTypeWebGPULibrary
  , WebGPURenderCommandEncoder(WebGPURenderCommandEncoder), unWebGPURenderCommandEncoder, noWebGPURenderCommandEncoder, gTypeWebGPURenderCommandEncoder
  , WebGPURenderPassAttachmentDescriptor(WebGPURenderPassAttachmentDescriptor), unWebGPURenderPassAttachmentDescriptor, IsWebGPURenderPassAttachmentDescriptor, toWebGPURenderPassAttachmentDescriptor, noWebGPURenderPassAttachmentDescriptor, gTypeWebGPURenderPassAttachmentDescriptor
  , WebGPURenderPassColorAttachmentDescriptor(WebGPURenderPassColorAttachmentDescriptor), unWebGPURenderPassColorAttachmentDescriptor, noWebGPURenderPassColorAttachmentDescriptor, gTypeWebGPURenderPassColorAttachmentDescriptor
  , WebGPURenderPassDepthAttachmentDescriptor(WebGPURenderPassDepthAttachmentDescriptor), unWebGPURenderPassDepthAttachmentDescriptor, noWebGPURenderPassDepthAttachmentDescriptor, gTypeWebGPURenderPassDepthAttachmentDescriptor
  , WebGPURenderPassDescriptor(WebGPURenderPassDescriptor), unWebGPURenderPassDescriptor, noWebGPURenderPassDescriptor, gTypeWebGPURenderPassDescriptor
  , WebGPURenderPipelineColorAttachmentDescriptor(WebGPURenderPipelineColorAttachmentDescriptor), unWebGPURenderPipelineColorAttachmentDescriptor, noWebGPURenderPipelineColorAttachmentDescriptor, gTypeWebGPURenderPipelineColorAttachmentDescriptor
  , WebGPURenderPipelineDescriptor(WebGPURenderPipelineDescriptor), unWebGPURenderPipelineDescriptor, noWebGPURenderPipelineDescriptor, gTypeWebGPURenderPipelineDescriptor
  , WebGPURenderPipelineState(WebGPURenderPipelineState), unWebGPURenderPipelineState, noWebGPURenderPipelineState, gTypeWebGPURenderPipelineState
  , WebGPURenderingContext(WebGPURenderingContext), unWebGPURenderingContext, noWebGPURenderingContext, gTypeWebGPURenderingContext
  , WebGPUSize(WebGPUSize), unWebGPUSize, noWebGPUSize, gTypeWebGPUSize
  , WebGPUTexture(WebGPUTexture), unWebGPUTexture, noWebGPUTexture, gTypeWebGPUTexture
  , WebGPUTextureDescriptor(WebGPUTextureDescriptor), unWebGPUTextureDescriptor, noWebGPUTextureDescriptor, gTypeWebGPUTextureDescriptor
  , WebKitAnimationEvent(WebKitAnimationEvent), unWebKitAnimationEvent, noWebKitAnimationEvent, gTypeWebKitAnimationEvent
  , WebKitAnimationEventInit(WebKitAnimationEventInit), unWebKitAnimationEventInit, noWebKitAnimationEventInit, gTypeWebKitAnimationEventInit
  , WebKitCSSMatrix(WebKitCSSMatrix), unWebKitCSSMatrix, noWebKitCSSMatrix, gTypeWebKitCSSMatrix
  , WebKitCSSRegionRule(WebKitCSSRegionRule), unWebKitCSSRegionRule, noWebKitCSSRegionRule, gTypeWebKitCSSRegionRule
  , WebKitCSSViewportRule(WebKitCSSViewportRule), unWebKitCSSViewportRule, noWebKitCSSViewportRule, gTypeWebKitCSSViewportRule
  , WebKitMediaKeyError(WebKitMediaKeyError), unWebKitMediaKeyError, noWebKitMediaKeyError, gTypeWebKitMediaKeyError
  , WebKitMediaKeyMessageEvent(WebKitMediaKeyMessageEvent), unWebKitMediaKeyMessageEvent, noWebKitMediaKeyMessageEvent, gTypeWebKitMediaKeyMessageEvent
  , WebKitMediaKeyMessageEventInit(WebKitMediaKeyMessageEventInit), unWebKitMediaKeyMessageEventInit, noWebKitMediaKeyMessageEventInit, gTypeWebKitMediaKeyMessageEventInit
  , WebKitMediaKeyNeededEvent(WebKitMediaKeyNeededEvent), unWebKitMediaKeyNeededEvent, noWebKitMediaKeyNeededEvent, gTypeWebKitMediaKeyNeededEvent
  , WebKitMediaKeyNeededEventInit(WebKitMediaKeyNeededEventInit), unWebKitMediaKeyNeededEventInit, noWebKitMediaKeyNeededEventInit, gTypeWebKitMediaKeyNeededEventInit
  , WebKitMediaKeySession(WebKitMediaKeySession), unWebKitMediaKeySession, noWebKitMediaKeySession, gTypeWebKitMediaKeySession
  , WebKitMediaKeys(WebKitMediaKeys), unWebKitMediaKeys, noWebKitMediaKeys, gTypeWebKitMediaKeys
  , WebKitNamedFlow(WebKitNamedFlow), unWebKitNamedFlow, noWebKitNamedFlow, gTypeWebKitNamedFlow
  , WebKitNamespace(WebKitNamespace), unWebKitNamespace, noWebKitNamespace, gTypeWebKitNamespace
  , WebKitPlaybackTargetAvailabilityEvent(WebKitPlaybackTargetAvailabilityEvent), unWebKitPlaybackTargetAvailabilityEvent, noWebKitPlaybackTargetAvailabilityEvent, gTypeWebKitPlaybackTargetAvailabilityEvent
  , WebKitPlaybackTargetAvailabilityEventInit(WebKitPlaybackTargetAvailabilityEventInit), unWebKitPlaybackTargetAvailabilityEventInit, noWebKitPlaybackTargetAvailabilityEventInit, gTypeWebKitPlaybackTargetAvailabilityEventInit
  , WebKitPoint(WebKitPoint), unWebKitPoint, noWebKitPoint, gTypeWebKitPoint
  , WebKitSubtleCrypto(WebKitSubtleCrypto), unWebKitSubtleCrypto, noWebKitSubtleCrypto, gTypeWebKitSubtleCrypto
  , WebKitTransitionEvent(WebKitTransitionEvent), unWebKitTransitionEvent, noWebKitTransitionEvent, gTypeWebKitTransitionEvent
  , WebKitTransitionEventInit(WebKitTransitionEventInit), unWebKitTransitionEventInit, noWebKitTransitionEventInit, gTypeWebKitTransitionEventInit
  , WebSocket(WebSocket), unWebSocket, noWebSocket, gTypeWebSocket
  , WheelEvent(WheelEvent), unWheelEvent, noWheelEvent, gTypeWheelEvent
  , WheelEventInit(WheelEventInit), unWheelEventInit, noWheelEventInit, gTypeWheelEventInit
  , Window(Window), unWindow, noWindow, gTypeWindow
  , WindowEventHandlers(WindowEventHandlers), unWindowEventHandlers, IsWindowEventHandlers, toWindowEventHandlers, noWindowEventHandlers, gTypeWindowEventHandlers
  , WindowOrWorkerGlobalScope(WindowOrWorkerGlobalScope), unWindowOrWorkerGlobalScope, IsWindowOrWorkerGlobalScope, toWindowOrWorkerGlobalScope, noWindowOrWorkerGlobalScope, gTypeWindowOrWorkerGlobalScope
  , Worker(Worker), unWorker, noWorker, gTypeWorker
  , WorkerGlobalScope(WorkerGlobalScope), unWorkerGlobalScope, IsWorkerGlobalScope, toWorkerGlobalScope, noWorkerGlobalScope, gTypeWorkerGlobalScope
  , WorkerLocation(WorkerLocation), unWorkerLocation, noWorkerLocation, gTypeWorkerLocation
  , WorkerNavigator(WorkerNavigator), unWorkerNavigator, noWorkerNavigator, gTypeWorkerNavigator
  , WritableStream(WritableStream), unWritableStream, noWritableStream, gTypeWritableStream
  , XMLDocument(XMLDocument), unXMLDocument, noXMLDocument, gTypeXMLDocument
  , XMLHttpRequest(XMLHttpRequest), unXMLHttpRequest, noXMLHttpRequest, gTypeXMLHttpRequest
  , XMLHttpRequestEventTarget(XMLHttpRequestEventTarget), unXMLHttpRequestEventTarget, IsXMLHttpRequestEventTarget, toXMLHttpRequestEventTarget, noXMLHttpRequestEventTarget, gTypeXMLHttpRequestEventTarget
  , XMLHttpRequestProgressEvent(XMLHttpRequestProgressEvent), unXMLHttpRequestProgressEvent, noXMLHttpRequestProgressEvent, gTypeXMLHttpRequestProgressEvent
  , XMLHttpRequestUpload(XMLHttpRequestUpload), unXMLHttpRequestUpload, noXMLHttpRequestUpload, gTypeXMLHttpRequestUpload
  , XMLSerializer(XMLSerializer), unXMLSerializer, noXMLSerializer, gTypeXMLSerializer
  , XPathEvaluator(XPathEvaluator), unXPathEvaluator, noXPathEvaluator, gTypeXPathEvaluator
  , XPathException(XPathException), unXPathException, noXPathException, gTypeXPathException
  , XPathExpression(XPathExpression), unXPathExpression, noXPathExpression, gTypeXPathExpression
  , XPathNSResolver(XPathNSResolver), unXPathNSResolver, noXPathNSResolver, gTypeXPathNSResolver
  , XPathResult(XPathResult), unXPathResult, noXPathResult, gTypeXPathResult
  , XSLTProcessor(XSLTProcessor), unXSLTProcessor, noXSLTProcessor, gTypeXSLTProcessor
-- AUTO GENERATION ENDS HERE
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

import JSDOM.Types.Core
import JSDOM.Types.TypesCore
import JSDOM.Types.TypesSVG
import JSDOM.Types.TypesHTML
import JSDOM.Types.TypesWebGL
import JSDOM.Types.Types00
import JSDOM.Types.Types0
import JSDOM.Types.TypesAB

-- AUTO GENERATION STARTS HERE

-- | Functions for this inteface are in "JSDOM.KeyboardEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent Mozilla KeyboardEvent documentation>
newtype KeyboardEvent = KeyboardEvent { unKeyboardEvent :: JSVal }

instance PToJSVal KeyboardEvent where
  pToJSVal = unKeyboardEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal KeyboardEvent where
  pFromJSVal = KeyboardEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal KeyboardEvent where
  toJSVal = return . unKeyboardEvent
  {-# INLINE toJSVal #-}

instance FromJSVal KeyboardEvent where
  fromJSVal v = fmap KeyboardEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . KeyboardEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject KeyboardEvent where
  makeObject = makeObject . unKeyboardEvent

instance IsUIEvent KeyboardEvent
instance IsEvent KeyboardEvent
instance IsGObject KeyboardEvent where
  typeGType _ = gTypeKeyboardEvent
  {-# INLINE typeGType #-}

noKeyboardEvent :: Maybe KeyboardEvent
noKeyboardEvent = Nothing
{-# INLINE noKeyboardEvent #-}

gTypeKeyboardEvent :: JSM GType
gTypeKeyboardEvent = GType . Object <$> jsg "KeyboardEvent"

-- | Functions for this inteface are in "JSDOM.KeyboardEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventModifierInit"
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEventInit Mozilla KeyboardEventInit documentation>
newtype KeyboardEventInit = KeyboardEventInit { unKeyboardEventInit :: JSVal }

instance PToJSVal KeyboardEventInit where
  pToJSVal = unKeyboardEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal KeyboardEventInit where
  pFromJSVal = KeyboardEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal KeyboardEventInit where
  toJSVal = return . unKeyboardEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal KeyboardEventInit where
  fromJSVal v = fmap KeyboardEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . KeyboardEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject KeyboardEventInit where
  makeObject = makeObject . unKeyboardEventInit

instance IsEventModifierInit KeyboardEventInit
instance IsUIEventInit KeyboardEventInit
instance IsEventInit KeyboardEventInit
instance IsGObject KeyboardEventInit where
  typeGType _ = gTypeKeyboardEventInit
  {-# INLINE typeGType #-}

noKeyboardEventInit :: Maybe KeyboardEventInit
noKeyboardEventInit = Nothing
{-# INLINE noKeyboardEventInit #-}

gTypeKeyboardEventInit :: JSM GType
gTypeKeyboardEventInit = GType . Object <$> jsg "KeyboardEventInit"

-- | Functions for this inteface are in "JSDOM.KeyframeEffect".
-- Base interface functions are in:
--
--     * "JSDOM.AnimationEffect"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/KeyframeEffect Mozilla KeyframeEffect documentation>
newtype KeyframeEffect = KeyframeEffect { unKeyframeEffect :: JSVal }

instance PToJSVal KeyframeEffect where
  pToJSVal = unKeyframeEffect
  {-# INLINE pToJSVal #-}

instance PFromJSVal KeyframeEffect where
  pFromJSVal = KeyframeEffect
  {-# INLINE pFromJSVal #-}

instance ToJSVal KeyframeEffect where
  toJSVal = return . unKeyframeEffect
  {-# INLINE toJSVal #-}

instance FromJSVal KeyframeEffect where
  fromJSVal v = fmap KeyframeEffect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . KeyframeEffect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject KeyframeEffect where
  makeObject = makeObject . unKeyframeEffect

instance IsAnimationEffect KeyframeEffect
instance IsGObject KeyframeEffect where
  typeGType _ = gTypeKeyframeEffect
  {-# INLINE typeGType #-}

noKeyframeEffect :: Maybe KeyframeEffect
noKeyframeEffect = Nothing
{-# INLINE noKeyframeEffect #-}

gTypeKeyframeEffect :: JSM GType
gTypeKeyframeEffect = GType . Object <$> jsg "KeyframeEffect"

-- | Functions for this inteface are in "JSDOM.Location".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Location Mozilla Location documentation>
newtype Location = Location { unLocation :: JSVal }

instance PToJSVal Location where
  pToJSVal = unLocation
  {-# INLINE pToJSVal #-}

instance PFromJSVal Location where
  pFromJSVal = Location
  {-# INLINE pFromJSVal #-}

instance ToJSVal Location where
  toJSVal = return . unLocation
  {-# INLINE toJSVal #-}

instance FromJSVal Location where
  fromJSVal v = fmap Location <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Location
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Location where
  makeObject = makeObject . unLocation

instance IsGObject Location where
  typeGType _ = gTypeLocation
  {-# INLINE typeGType #-}

noLocation :: Maybe Location
noLocation = Nothing
{-# INLINE noLocation #-}

gTypeLocation :: JSM GType
gTypeLocation = GType . Object <$> jsg "Location"

-- | Functions for this inteface are in "JSDOM.MediaController".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaController Mozilla MediaController documentation>
newtype MediaController = MediaController { unMediaController :: JSVal }

instance PToJSVal MediaController where
  pToJSVal = unMediaController
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaController where
  pFromJSVal = MediaController
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaController where
  toJSVal = return . unMediaController
  {-# INLINE toJSVal #-}

instance FromJSVal MediaController where
  fromJSVal v = fmap MediaController <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaController
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaController where
  makeObject = makeObject . unMediaController

instance IsEventTarget MediaController
instance IsGObject MediaController where
  typeGType _ = gTypeMediaController
  {-# INLINE typeGType #-}

noMediaController :: Maybe MediaController
noMediaController = Nothing
{-# INLINE noMediaController #-}

gTypeMediaController :: JSM GType
gTypeMediaController = GType . Object <$> jsg "MediaController"

-- | Functions for this inteface are in "JSDOM.MediaControlsHost".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaControlsHost Mozilla MediaControlsHost documentation>
newtype MediaControlsHost = MediaControlsHost { unMediaControlsHost :: JSVal }

instance PToJSVal MediaControlsHost where
  pToJSVal = unMediaControlsHost
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaControlsHost where
  pFromJSVal = MediaControlsHost
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaControlsHost where
  toJSVal = return . unMediaControlsHost
  {-# INLINE toJSVal #-}

instance FromJSVal MediaControlsHost where
  fromJSVal v = fmap MediaControlsHost <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaControlsHost
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaControlsHost where
  makeObject = makeObject . unMediaControlsHost

instance IsGObject MediaControlsHost where
  typeGType _ = gTypeMediaControlsHost
  {-# INLINE typeGType #-}

noMediaControlsHost :: Maybe MediaControlsHost
noMediaControlsHost = Nothing
{-# INLINE noMediaControlsHost #-}

gTypeMediaControlsHost :: JSM GType
gTypeMediaControlsHost = GType . Object <$> jsg "MediaControlsHost"

-- | Functions for this inteface are in "JSDOM.MediaDeviceInfo".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaDeviceInfo Mozilla MediaDeviceInfo documentation>
newtype MediaDeviceInfo = MediaDeviceInfo { unMediaDeviceInfo :: JSVal }

instance PToJSVal MediaDeviceInfo where
  pToJSVal = unMediaDeviceInfo
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaDeviceInfo where
  pFromJSVal = MediaDeviceInfo
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaDeviceInfo where
  toJSVal = return . unMediaDeviceInfo
  {-# INLINE toJSVal #-}

instance FromJSVal MediaDeviceInfo where
  fromJSVal v = fmap MediaDeviceInfo <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaDeviceInfo
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaDeviceInfo where
  makeObject = makeObject . unMediaDeviceInfo

instance IsGObject MediaDeviceInfo where
  typeGType _ = gTypeMediaDeviceInfo
  {-# INLINE typeGType #-}

noMediaDeviceInfo :: Maybe MediaDeviceInfo
noMediaDeviceInfo = Nothing
{-# INLINE noMediaDeviceInfo #-}

gTypeMediaDeviceInfo :: JSM GType
gTypeMediaDeviceInfo = GType . Object <$> jsg "MediaDeviceInfo"

-- | Functions for this inteface are in "JSDOM.MediaDevices".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaDevices Mozilla MediaDevices documentation>
newtype MediaDevices = MediaDevices { unMediaDevices :: JSVal }

instance PToJSVal MediaDevices where
  pToJSVal = unMediaDevices
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaDevices where
  pFromJSVal = MediaDevices
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaDevices where
  toJSVal = return . unMediaDevices
  {-# INLINE toJSVal #-}

instance FromJSVal MediaDevices where
  fromJSVal v = fmap MediaDevices <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaDevices
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaDevices where
  makeObject = makeObject . unMediaDevices

instance IsGObject MediaDevices where
  typeGType _ = gTypeMediaDevices
  {-# INLINE typeGType #-}

noMediaDevices :: Maybe MediaDevices
noMediaDevices = Nothing
{-# INLINE noMediaDevices #-}

gTypeMediaDevices :: JSM GType
gTypeMediaDevices = GType . Object <$> jsg "MediaDevices"

-- | Functions for this inteface are in "JSDOM.MediaElementAudioSourceNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaElementAudioSourceNode Mozilla MediaElementAudioSourceNode documentation>
newtype MediaElementAudioSourceNode = MediaElementAudioSourceNode { unMediaElementAudioSourceNode :: JSVal }

instance PToJSVal MediaElementAudioSourceNode where
  pToJSVal = unMediaElementAudioSourceNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaElementAudioSourceNode where
  pFromJSVal = MediaElementAudioSourceNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaElementAudioSourceNode where
  toJSVal = return . unMediaElementAudioSourceNode
  {-# INLINE toJSVal #-}

instance FromJSVal MediaElementAudioSourceNode where
  fromJSVal v = fmap MediaElementAudioSourceNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaElementAudioSourceNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaElementAudioSourceNode where
  makeObject = makeObject . unMediaElementAudioSourceNode

instance IsAudioNode MediaElementAudioSourceNode
instance IsEventTarget MediaElementAudioSourceNode
instance IsGObject MediaElementAudioSourceNode where
  typeGType _ = gTypeMediaElementAudioSourceNode
  {-# INLINE typeGType #-}

noMediaElementAudioSourceNode :: Maybe MediaElementAudioSourceNode
noMediaElementAudioSourceNode = Nothing
{-# INLINE noMediaElementAudioSourceNode #-}

gTypeMediaElementAudioSourceNode :: JSM GType
gTypeMediaElementAudioSourceNode = GType . Object <$> jsg "MediaElementAudioSourceNode"

-- | Functions for this inteface are in "JSDOM.MediaEncryptedEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaEncryptedEvent Mozilla MediaEncryptedEvent documentation>
newtype MediaEncryptedEvent = MediaEncryptedEvent { unMediaEncryptedEvent :: JSVal }

instance PToJSVal MediaEncryptedEvent where
  pToJSVal = unMediaEncryptedEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaEncryptedEvent where
  pFromJSVal = MediaEncryptedEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaEncryptedEvent where
  toJSVal = return . unMediaEncryptedEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MediaEncryptedEvent where
  fromJSVal v = fmap MediaEncryptedEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaEncryptedEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaEncryptedEvent where
  makeObject = makeObject . unMediaEncryptedEvent

instance IsEvent MediaEncryptedEvent
instance IsGObject MediaEncryptedEvent where
  typeGType _ = gTypeMediaEncryptedEvent
  {-# INLINE typeGType #-}

noMediaEncryptedEvent :: Maybe MediaEncryptedEvent
noMediaEncryptedEvent = Nothing
{-# INLINE noMediaEncryptedEvent #-}

gTypeMediaEncryptedEvent :: JSM GType
gTypeMediaEncryptedEvent = GType . Object <$> jsg "MediaEncryptedEvent"

-- | Functions for this inteface are in "JSDOM.MediaEncryptedEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaEncryptedEventInit Mozilla MediaEncryptedEventInit documentation>
newtype MediaEncryptedEventInit = MediaEncryptedEventInit { unMediaEncryptedEventInit :: JSVal }

instance PToJSVal MediaEncryptedEventInit where
  pToJSVal = unMediaEncryptedEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaEncryptedEventInit where
  pFromJSVal = MediaEncryptedEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaEncryptedEventInit where
  toJSVal = return . unMediaEncryptedEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MediaEncryptedEventInit where
  fromJSVal v = fmap MediaEncryptedEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaEncryptedEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaEncryptedEventInit where
  makeObject = makeObject . unMediaEncryptedEventInit

instance IsEventInit MediaEncryptedEventInit
instance IsGObject MediaEncryptedEventInit where
  typeGType _ = gTypeMediaEncryptedEventInit
  {-# INLINE typeGType #-}

noMediaEncryptedEventInit :: Maybe MediaEncryptedEventInit
noMediaEncryptedEventInit = Nothing
{-# INLINE noMediaEncryptedEventInit #-}

gTypeMediaEncryptedEventInit :: JSM GType
gTypeMediaEncryptedEventInit = GType . Object <$> jsg "MediaEncryptedEventInit"

-- | Functions for this inteface are in "JSDOM.MediaError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaError Mozilla MediaError documentation>
newtype MediaError = MediaError { unMediaError :: JSVal }

instance PToJSVal MediaError where
  pToJSVal = unMediaError
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaError where
  pFromJSVal = MediaError
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaError where
  toJSVal = return . unMediaError
  {-# INLINE toJSVal #-}

instance FromJSVal MediaError where
  fromJSVal v = fmap MediaError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaError where
  makeObject = makeObject . unMediaError

instance IsGObject MediaError where
  typeGType _ = gTypeMediaError
  {-# INLINE typeGType #-}

noMediaError :: Maybe MediaError
noMediaError = Nothing
{-# INLINE noMediaError #-}

gTypeMediaError :: JSM GType
gTypeMediaError = GType . Object <$> jsg "MediaError"

-- | Functions for this inteface are in "JSDOM.MediaKeyMessageEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyMessageEvent Mozilla WebKitMediaKeyMessageEvent documentation>
newtype MediaKeyMessageEvent = MediaKeyMessageEvent { unMediaKeyMessageEvent :: JSVal }

instance PToJSVal MediaKeyMessageEvent where
  pToJSVal = unMediaKeyMessageEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeyMessageEvent where
  pFromJSVal = MediaKeyMessageEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeyMessageEvent where
  toJSVal = return . unMediaKeyMessageEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeyMessageEvent where
  fromJSVal v = fmap MediaKeyMessageEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeyMessageEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeyMessageEvent where
  makeObject = makeObject . unMediaKeyMessageEvent

instance IsEvent MediaKeyMessageEvent
instance IsGObject MediaKeyMessageEvent where
  typeGType _ = gTypeMediaKeyMessageEvent
  {-# INLINE typeGType #-}

noMediaKeyMessageEvent :: Maybe MediaKeyMessageEvent
noMediaKeyMessageEvent = Nothing
{-# INLINE noMediaKeyMessageEvent #-}

gTypeMediaKeyMessageEvent :: JSM GType
gTypeMediaKeyMessageEvent = GType . Object <$> jsg "WebKitMediaKeyMessageEvent"

-- | Functions for this inteface are in "JSDOM.MediaKeyMessageEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeyMessageEventInit Mozilla MediaKeyMessageEventInit documentation>
newtype MediaKeyMessageEventInit = MediaKeyMessageEventInit { unMediaKeyMessageEventInit :: JSVal }

instance PToJSVal MediaKeyMessageEventInit where
  pToJSVal = unMediaKeyMessageEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeyMessageEventInit where
  pFromJSVal = MediaKeyMessageEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeyMessageEventInit where
  toJSVal = return . unMediaKeyMessageEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeyMessageEventInit where
  fromJSVal v = fmap MediaKeyMessageEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeyMessageEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeyMessageEventInit where
  makeObject = makeObject . unMediaKeyMessageEventInit

instance IsEventInit MediaKeyMessageEventInit
instance IsGObject MediaKeyMessageEventInit where
  typeGType _ = gTypeMediaKeyMessageEventInit
  {-# INLINE typeGType #-}

noMediaKeyMessageEventInit :: Maybe MediaKeyMessageEventInit
noMediaKeyMessageEventInit = Nothing
{-# INLINE noMediaKeyMessageEventInit #-}

gTypeMediaKeyMessageEventInit :: JSM GType
gTypeMediaKeyMessageEventInit = GType . Object <$> jsg "MediaKeyMessageEventInit"

-- | Functions for this inteface are in "JSDOM.MediaKeySession".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeySession Mozilla WebKitMediaKeySession documentation>
newtype MediaKeySession = MediaKeySession { unMediaKeySession :: JSVal }

instance PToJSVal MediaKeySession where
  pToJSVal = unMediaKeySession
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeySession where
  pFromJSVal = MediaKeySession
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeySession where
  toJSVal = return . unMediaKeySession
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeySession where
  fromJSVal v = fmap MediaKeySession <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeySession
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeySession where
  makeObject = makeObject . unMediaKeySession

instance IsEventTarget MediaKeySession
instance IsGObject MediaKeySession where
  typeGType _ = gTypeMediaKeySession
  {-# INLINE typeGType #-}

noMediaKeySession :: Maybe MediaKeySession
noMediaKeySession = Nothing
{-# INLINE noMediaKeySession #-}

gTypeMediaKeySession :: JSM GType
gTypeMediaKeySession = GType . Object <$> jsg "WebKitMediaKeySession"

-- | Functions for this inteface are in "JSDOM.MediaKeyStatusMap".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeyStatusMap Mozilla MediaKeyStatusMap documentation>
newtype MediaKeyStatusMap = MediaKeyStatusMap { unMediaKeyStatusMap :: JSVal }

instance PToJSVal MediaKeyStatusMap where
  pToJSVal = unMediaKeyStatusMap
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeyStatusMap where
  pFromJSVal = MediaKeyStatusMap
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeyStatusMap where
  toJSVal = return . unMediaKeyStatusMap
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeyStatusMap where
  fromJSVal v = fmap MediaKeyStatusMap <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeyStatusMap
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeyStatusMap where
  makeObject = makeObject . unMediaKeyStatusMap

instance IsGObject MediaKeyStatusMap where
  typeGType _ = gTypeMediaKeyStatusMap
  {-# INLINE typeGType #-}

noMediaKeyStatusMap :: Maybe MediaKeyStatusMap
noMediaKeyStatusMap = Nothing
{-# INLINE noMediaKeyStatusMap #-}

gTypeMediaKeyStatusMap :: JSM GType
gTypeMediaKeyStatusMap = GType . Object <$> jsg "MediaKeyStatusMap"

-- | Functions for this inteface are in "JSDOM.MediaKeySystemAccess".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeySystemAccess Mozilla MediaKeySystemAccess documentation>
newtype MediaKeySystemAccess = MediaKeySystemAccess { unMediaKeySystemAccess :: JSVal }

instance PToJSVal MediaKeySystemAccess where
  pToJSVal = unMediaKeySystemAccess
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeySystemAccess where
  pFromJSVal = MediaKeySystemAccess
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeySystemAccess where
  toJSVal = return . unMediaKeySystemAccess
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeySystemAccess where
  fromJSVal v = fmap MediaKeySystemAccess <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeySystemAccess
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeySystemAccess where
  makeObject = makeObject . unMediaKeySystemAccess

instance IsGObject MediaKeySystemAccess where
  typeGType _ = gTypeMediaKeySystemAccess
  {-# INLINE typeGType #-}

noMediaKeySystemAccess :: Maybe MediaKeySystemAccess
noMediaKeySystemAccess = Nothing
{-# INLINE noMediaKeySystemAccess #-}

gTypeMediaKeySystemAccess :: JSM GType
gTypeMediaKeySystemAccess = GType . Object <$> jsg "MediaKeySystemAccess"

-- | Functions for this inteface are in "JSDOM.MediaKeySystemConfiguration".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeySystemConfiguration Mozilla MediaKeySystemConfiguration documentation>
newtype MediaKeySystemConfiguration = MediaKeySystemConfiguration { unMediaKeySystemConfiguration :: JSVal }

instance PToJSVal MediaKeySystemConfiguration where
  pToJSVal = unMediaKeySystemConfiguration
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeySystemConfiguration where
  pFromJSVal = MediaKeySystemConfiguration
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeySystemConfiguration where
  toJSVal = return . unMediaKeySystemConfiguration
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeySystemConfiguration where
  fromJSVal v = fmap MediaKeySystemConfiguration <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeySystemConfiguration
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeySystemConfiguration where
  makeObject = makeObject . unMediaKeySystemConfiguration

instance IsGObject MediaKeySystemConfiguration where
  typeGType _ = gTypeMediaKeySystemConfiguration
  {-# INLINE typeGType #-}

noMediaKeySystemConfiguration :: Maybe MediaKeySystemConfiguration
noMediaKeySystemConfiguration = Nothing
{-# INLINE noMediaKeySystemConfiguration #-}

gTypeMediaKeySystemConfiguration :: JSM GType
gTypeMediaKeySystemConfiguration = GType . Object <$> jsg "MediaKeySystemConfiguration"

-- | Functions for this inteface are in "JSDOM.MediaKeySystemMediaCapability".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaKeySystemMediaCapability Mozilla MediaKeySystemMediaCapability documentation>
newtype MediaKeySystemMediaCapability = MediaKeySystemMediaCapability { unMediaKeySystemMediaCapability :: JSVal }

instance PToJSVal MediaKeySystemMediaCapability where
  pToJSVal = unMediaKeySystemMediaCapability
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeySystemMediaCapability where
  pFromJSVal = MediaKeySystemMediaCapability
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeySystemMediaCapability where
  toJSVal = return . unMediaKeySystemMediaCapability
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeySystemMediaCapability where
  fromJSVal v = fmap MediaKeySystemMediaCapability <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeySystemMediaCapability
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeySystemMediaCapability where
  makeObject = makeObject . unMediaKeySystemMediaCapability

instance IsGObject MediaKeySystemMediaCapability where
  typeGType _ = gTypeMediaKeySystemMediaCapability
  {-# INLINE typeGType #-}

noMediaKeySystemMediaCapability :: Maybe MediaKeySystemMediaCapability
noMediaKeySystemMediaCapability = Nothing
{-# INLINE noMediaKeySystemMediaCapability #-}

gTypeMediaKeySystemMediaCapability :: JSM GType
gTypeMediaKeySystemMediaCapability = GType . Object <$> jsg "MediaKeySystemMediaCapability"

-- | Functions for this inteface are in "JSDOM.MediaKeys".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeys Mozilla WebKitMediaKeys documentation>
newtype MediaKeys = MediaKeys { unMediaKeys :: JSVal }

instance PToJSVal MediaKeys where
  pToJSVal = unMediaKeys
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaKeys where
  pFromJSVal = MediaKeys
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaKeys where
  toJSVal = return . unMediaKeys
  {-# INLINE toJSVal #-}

instance FromJSVal MediaKeys where
  fromJSVal v = fmap MediaKeys <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaKeys
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaKeys where
  makeObject = makeObject . unMediaKeys

instance IsGObject MediaKeys where
  typeGType _ = gTypeMediaKeys
  {-# INLINE typeGType #-}

noMediaKeys :: Maybe MediaKeys
noMediaKeys = Nothing
{-# INLINE noMediaKeys #-}

gTypeMediaKeys :: JSM GType
gTypeMediaKeys = GType . Object <$> jsg "WebKitMediaKeys"

-- | Functions for this inteface are in "JSDOM.MediaList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaList Mozilla MediaList documentation>
newtype MediaList = MediaList { unMediaList :: JSVal }

instance PToJSVal MediaList where
  pToJSVal = unMediaList
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaList where
  pFromJSVal = MediaList
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaList where
  toJSVal = return . unMediaList
  {-# INLINE toJSVal #-}

instance FromJSVal MediaList where
  fromJSVal v = fmap MediaList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaList where
  makeObject = makeObject . unMediaList

instance IsGObject MediaList where
  typeGType _ = gTypeMediaList
  {-# INLINE typeGType #-}

noMediaList :: Maybe MediaList
noMediaList = Nothing
{-# INLINE noMediaList #-}

gTypeMediaList :: JSM GType
gTypeMediaList = GType . Object <$> jsg "MediaList"

-- | Functions for this inteface are in "JSDOM.MediaMetadata".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaMetadata Mozilla MediaMetadata documentation>
newtype MediaMetadata = MediaMetadata { unMediaMetadata :: JSVal }

instance PToJSVal MediaMetadata where
  pToJSVal = unMediaMetadata
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaMetadata where
  pFromJSVal = MediaMetadata
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaMetadata where
  toJSVal = return . unMediaMetadata
  {-# INLINE toJSVal #-}

instance FromJSVal MediaMetadata where
  fromJSVal v = fmap MediaMetadata <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaMetadata
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaMetadata where
  makeObject = makeObject . unMediaMetadata

instance IsGObject MediaMetadata where
  typeGType _ = gTypeMediaMetadata
  {-# INLINE typeGType #-}

noMediaMetadata :: Maybe MediaMetadata
noMediaMetadata = Nothing
{-# INLINE noMediaMetadata #-}

gTypeMediaMetadata :: JSM GType
gTypeMediaMetadata = GType . Object <$> jsg "MediaMetadata"

-- | Functions for this inteface are in "JSDOM.MediaQueryList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaQueryList Mozilla MediaQueryList documentation>
newtype MediaQueryList = MediaQueryList { unMediaQueryList :: JSVal }

instance PToJSVal MediaQueryList where
  pToJSVal = unMediaQueryList
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaQueryList where
  pFromJSVal = MediaQueryList
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaQueryList where
  toJSVal = return . unMediaQueryList
  {-# INLINE toJSVal #-}

instance FromJSVal MediaQueryList where
  fromJSVal v = fmap MediaQueryList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaQueryList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaQueryList where
  makeObject = makeObject . unMediaQueryList

instance IsGObject MediaQueryList where
  typeGType _ = gTypeMediaQueryList
  {-# INLINE typeGType #-}

noMediaQueryList :: Maybe MediaQueryList
noMediaQueryList = Nothing
{-# INLINE noMediaQueryList #-}

gTypeMediaQueryList :: JSM GType
gTypeMediaQueryList = GType . Object <$> jsg "MediaQueryList"

-- | Functions for this inteface are in "JSDOM.MediaRemoteControls".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaRemoteControls Mozilla MediaRemoteControls documentation>
newtype MediaRemoteControls = MediaRemoteControls { unMediaRemoteControls :: JSVal }

instance PToJSVal MediaRemoteControls where
  pToJSVal = unMediaRemoteControls
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaRemoteControls where
  pFromJSVal = MediaRemoteControls
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaRemoteControls where
  toJSVal = return . unMediaRemoteControls
  {-# INLINE toJSVal #-}

instance FromJSVal MediaRemoteControls where
  fromJSVal v = fmap MediaRemoteControls <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaRemoteControls
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaRemoteControls where
  makeObject = makeObject . unMediaRemoteControls

instance IsEventTarget MediaRemoteControls
instance IsGObject MediaRemoteControls where
  typeGType _ = gTypeMediaRemoteControls
  {-# INLINE typeGType #-}

noMediaRemoteControls :: Maybe MediaRemoteControls
noMediaRemoteControls = Nothing
{-# INLINE noMediaRemoteControls #-}

gTypeMediaRemoteControls :: JSM GType
gTypeMediaRemoteControls = GType . Object <$> jsg "MediaRemoteControls"

-- | Functions for this inteface are in "JSDOM.MediaSession".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaSession Mozilla MediaSession documentation>
newtype MediaSession = MediaSession { unMediaSession :: JSVal }

instance PToJSVal MediaSession where
  pToJSVal = unMediaSession
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaSession where
  pFromJSVal = MediaSession
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaSession where
  toJSVal = return . unMediaSession
  {-# INLINE toJSVal #-}

instance FromJSVal MediaSession where
  fromJSVal v = fmap MediaSession <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaSession
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaSession where
  makeObject = makeObject . unMediaSession

instance IsGObject MediaSession where
  typeGType _ = gTypeMediaSession
  {-# INLINE typeGType #-}

noMediaSession :: Maybe MediaSession
noMediaSession = Nothing
{-# INLINE noMediaSession #-}

gTypeMediaSession :: JSM GType
gTypeMediaSession = GType . Object <$> jsg "MediaSession"


-- | Functions for this inteface are in "JSDOM.MediaStreamAudioDestinationNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioDestinationNode Mozilla MediaStreamAudioDestinationNode documentation>
newtype MediaStreamAudioDestinationNode = MediaStreamAudioDestinationNode { unMediaStreamAudioDestinationNode :: JSVal }

instance PToJSVal MediaStreamAudioDestinationNode where
  pToJSVal = unMediaStreamAudioDestinationNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamAudioDestinationNode where
  pFromJSVal = MediaStreamAudioDestinationNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamAudioDestinationNode where
  toJSVal = return . unMediaStreamAudioDestinationNode
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamAudioDestinationNode where
  fromJSVal v = fmap MediaStreamAudioDestinationNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamAudioDestinationNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamAudioDestinationNode where
  makeObject = makeObject . unMediaStreamAudioDestinationNode

instance IsAudioNode MediaStreamAudioDestinationNode
instance IsEventTarget MediaStreamAudioDestinationNode
instance IsGObject MediaStreamAudioDestinationNode where
  typeGType _ = gTypeMediaStreamAudioDestinationNode
  {-# INLINE typeGType #-}

noMediaStreamAudioDestinationNode :: Maybe MediaStreamAudioDestinationNode
noMediaStreamAudioDestinationNode = Nothing
{-# INLINE noMediaStreamAudioDestinationNode #-}

gTypeMediaStreamAudioDestinationNode :: JSM GType
gTypeMediaStreamAudioDestinationNode = GType . Object <$> jsg "MediaStreamAudioDestinationNode"

-- | Functions for this inteface are in "JSDOM.MediaStreamAudioSourceNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamAudioSourceNode Mozilla MediaStreamAudioSourceNode documentation>
newtype MediaStreamAudioSourceNode = MediaStreamAudioSourceNode { unMediaStreamAudioSourceNode :: JSVal }

instance PToJSVal MediaStreamAudioSourceNode where
  pToJSVal = unMediaStreamAudioSourceNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamAudioSourceNode where
  pFromJSVal = MediaStreamAudioSourceNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamAudioSourceNode where
  toJSVal = return . unMediaStreamAudioSourceNode
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamAudioSourceNode where
  fromJSVal v = fmap MediaStreamAudioSourceNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamAudioSourceNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamAudioSourceNode where
  makeObject = makeObject . unMediaStreamAudioSourceNode

instance IsAudioNode MediaStreamAudioSourceNode
instance IsEventTarget MediaStreamAudioSourceNode
instance IsGObject MediaStreamAudioSourceNode where
  typeGType _ = gTypeMediaStreamAudioSourceNode
  {-# INLINE typeGType #-}

noMediaStreamAudioSourceNode :: Maybe MediaStreamAudioSourceNode
noMediaStreamAudioSourceNode = Nothing
{-# INLINE noMediaStreamAudioSourceNode #-}

gTypeMediaStreamAudioSourceNode :: JSM GType
gTypeMediaStreamAudioSourceNode = GType . Object <$> jsg "MediaStreamAudioSourceNode"

-- | Functions for this inteface are in "JSDOM.MediaStreamConstraints".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamConstraints Mozilla MediaStreamConstraints documentation>
newtype MediaStreamConstraints = MediaStreamConstraints { unMediaStreamConstraints :: JSVal }

instance PToJSVal MediaStreamConstraints where
  pToJSVal = unMediaStreamConstraints
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamConstraints where
  pFromJSVal = MediaStreamConstraints
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamConstraints where
  toJSVal = return . unMediaStreamConstraints
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamConstraints where
  fromJSVal v = fmap MediaStreamConstraints <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamConstraints
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamConstraints where
  makeObject = makeObject . unMediaStreamConstraints

instance IsGObject MediaStreamConstraints where
  typeGType _ = gTypeMediaStreamConstraints
  {-# INLINE typeGType #-}

noMediaStreamConstraints :: Maybe MediaStreamConstraints
noMediaStreamConstraints = Nothing
{-# INLINE noMediaStreamConstraints #-}

gTypeMediaStreamConstraints :: JSM GType
gTypeMediaStreamConstraints = GType . Object <$> jsg "MediaStreamConstraints"

-- | Functions for this inteface are in "JSDOM.MediaStreamEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamEvent Mozilla MediaStreamEvent documentation>
newtype MediaStreamEvent = MediaStreamEvent { unMediaStreamEvent :: JSVal }

instance PToJSVal MediaStreamEvent where
  pToJSVal = unMediaStreamEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamEvent where
  pFromJSVal = MediaStreamEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamEvent where
  toJSVal = return . unMediaStreamEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamEvent where
  fromJSVal v = fmap MediaStreamEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamEvent where
  makeObject = makeObject . unMediaStreamEvent

instance IsEvent MediaStreamEvent
instance IsGObject MediaStreamEvent where
  typeGType _ = gTypeMediaStreamEvent
  {-# INLINE typeGType #-}

noMediaStreamEvent :: Maybe MediaStreamEvent
noMediaStreamEvent = Nothing
{-# INLINE noMediaStreamEvent #-}

gTypeMediaStreamEvent :: JSM GType
gTypeMediaStreamEvent = GType . Object <$> jsg "MediaStreamEvent"

-- | Functions for this inteface are in "JSDOM.MediaStreamEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamEventInit Mozilla MediaStreamEventInit documentation>
newtype MediaStreamEventInit = MediaStreamEventInit { unMediaStreamEventInit :: JSVal }

instance PToJSVal MediaStreamEventInit where
  pToJSVal = unMediaStreamEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamEventInit where
  pFromJSVal = MediaStreamEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamEventInit where
  toJSVal = return . unMediaStreamEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamEventInit where
  fromJSVal v = fmap MediaStreamEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamEventInit where
  makeObject = makeObject . unMediaStreamEventInit

instance IsEventInit MediaStreamEventInit
instance IsGObject MediaStreamEventInit where
  typeGType _ = gTypeMediaStreamEventInit
  {-# INLINE typeGType #-}

noMediaStreamEventInit :: Maybe MediaStreamEventInit
noMediaStreamEventInit = Nothing
{-# INLINE noMediaStreamEventInit #-}

gTypeMediaStreamEventInit :: JSM GType
gTypeMediaStreamEventInit = GType . Object <$> jsg "MediaStreamEventInit"

-- | Functions for this inteface are in "JSDOM.MediaStreamTrackEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrackEvent Mozilla MediaStreamTrackEvent documentation>
newtype MediaStreamTrackEvent = MediaStreamTrackEvent { unMediaStreamTrackEvent :: JSVal }

instance PToJSVal MediaStreamTrackEvent where
  pToJSVal = unMediaStreamTrackEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamTrackEvent where
  pFromJSVal = MediaStreamTrackEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamTrackEvent where
  toJSVal = return . unMediaStreamTrackEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamTrackEvent where
  fromJSVal v = fmap MediaStreamTrackEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamTrackEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamTrackEvent where
  makeObject = makeObject . unMediaStreamTrackEvent

instance IsEvent MediaStreamTrackEvent
instance IsGObject MediaStreamTrackEvent where
  typeGType _ = gTypeMediaStreamTrackEvent
  {-# INLINE typeGType #-}

noMediaStreamTrackEvent :: Maybe MediaStreamTrackEvent
noMediaStreamTrackEvent = Nothing
{-# INLINE noMediaStreamTrackEvent #-}

gTypeMediaStreamTrackEvent :: JSM GType
gTypeMediaStreamTrackEvent = GType . Object <$> jsg "MediaStreamTrackEvent"

-- | Functions for this inteface are in "JSDOM.MediaStreamTrackEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaStreamTrackEventInit Mozilla MediaStreamTrackEventInit documentation>
newtype MediaStreamTrackEventInit = MediaStreamTrackEventInit { unMediaStreamTrackEventInit :: JSVal }

instance PToJSVal MediaStreamTrackEventInit where
  pToJSVal = unMediaStreamTrackEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaStreamTrackEventInit where
  pFromJSVal = MediaStreamTrackEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaStreamTrackEventInit where
  toJSVal = return . unMediaStreamTrackEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MediaStreamTrackEventInit where
  fromJSVal v = fmap MediaStreamTrackEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaStreamTrackEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaStreamTrackEventInit where
  makeObject = makeObject . unMediaStreamTrackEventInit

instance IsEventInit MediaStreamTrackEventInit
instance IsGObject MediaStreamTrackEventInit where
  typeGType _ = gTypeMediaStreamTrackEventInit
  {-# INLINE typeGType #-}

noMediaStreamTrackEventInit :: Maybe MediaStreamTrackEventInit
noMediaStreamTrackEventInit = Nothing
{-# INLINE noMediaStreamTrackEventInit #-}

gTypeMediaStreamTrackEventInit :: JSM GType
gTypeMediaStreamTrackEventInit = GType . Object <$> jsg "MediaStreamTrackEventInit"

-- | Functions for this inteface are in "JSDOM.MediaTrackCapabilities".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackCapabilities Mozilla MediaTrackCapabilities documentation>
newtype MediaTrackCapabilities = MediaTrackCapabilities { unMediaTrackCapabilities :: JSVal }

instance PToJSVal MediaTrackCapabilities where
  pToJSVal = unMediaTrackCapabilities
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaTrackCapabilities where
  pFromJSVal = MediaTrackCapabilities
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaTrackCapabilities where
  toJSVal = return . unMediaTrackCapabilities
  {-# INLINE toJSVal #-}

instance FromJSVal MediaTrackCapabilities where
  fromJSVal v = fmap MediaTrackCapabilities <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaTrackCapabilities
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaTrackCapabilities where
  makeObject = makeObject . unMediaTrackCapabilities

instance IsGObject MediaTrackCapabilities where
  typeGType _ = gTypeMediaTrackCapabilities
  {-# INLINE typeGType #-}

noMediaTrackCapabilities :: Maybe MediaTrackCapabilities
noMediaTrackCapabilities = Nothing
{-# INLINE noMediaTrackCapabilities #-}

gTypeMediaTrackCapabilities :: JSM GType
gTypeMediaTrackCapabilities = GType . Object <$> jsg "MediaTrackCapabilities"

-- | Functions for this inteface are in "JSDOM.MediaTrackConstraintSet".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackConstraintSet Mozilla MediaTrackConstraintSet documentation>
newtype MediaTrackConstraintSet = MediaTrackConstraintSet { unMediaTrackConstraintSet :: JSVal }

instance PToJSVal MediaTrackConstraintSet where
  pToJSVal = unMediaTrackConstraintSet
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaTrackConstraintSet where
  pFromJSVal = MediaTrackConstraintSet
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaTrackConstraintSet where
  toJSVal = return . unMediaTrackConstraintSet
  {-# INLINE toJSVal #-}

instance FromJSVal MediaTrackConstraintSet where
  fromJSVal v = fmap MediaTrackConstraintSet <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaTrackConstraintSet
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaTrackConstraintSet where
  makeObject = makeObject . unMediaTrackConstraintSet

class (IsGObject o) => IsMediaTrackConstraintSet o
toMediaTrackConstraintSet :: IsMediaTrackConstraintSet o => o -> MediaTrackConstraintSet
toMediaTrackConstraintSet = MediaTrackConstraintSet . coerce

instance IsMediaTrackConstraintSet MediaTrackConstraintSet
instance IsGObject MediaTrackConstraintSet where
  typeGType _ = gTypeMediaTrackConstraintSet
  {-# INLINE typeGType #-}

noMediaTrackConstraintSet :: Maybe MediaTrackConstraintSet
noMediaTrackConstraintSet = Nothing
{-# INLINE noMediaTrackConstraintSet #-}

gTypeMediaTrackConstraintSet :: JSM GType
gTypeMediaTrackConstraintSet = GType . Object <$> jsg "MediaTrackConstraintSet"

-- | Functions for this inteface are in "JSDOM.MediaTrackConstraints".
-- Base interface functions are in:
--
--     * "JSDOM.MediaTrackConstraintSet"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackConstraints Mozilla MediaTrackConstraints documentation>
newtype MediaTrackConstraints = MediaTrackConstraints { unMediaTrackConstraints :: JSVal }

instance PToJSVal MediaTrackConstraints where
  pToJSVal = unMediaTrackConstraints
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaTrackConstraints where
  pFromJSVal = MediaTrackConstraints
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaTrackConstraints where
  toJSVal = return . unMediaTrackConstraints
  {-# INLINE toJSVal #-}

instance FromJSVal MediaTrackConstraints where
  fromJSVal v = fmap MediaTrackConstraints <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaTrackConstraints
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaTrackConstraints where
  makeObject = makeObject . unMediaTrackConstraints

instance IsMediaTrackConstraintSet MediaTrackConstraints
instance IsGObject MediaTrackConstraints where
  typeGType _ = gTypeMediaTrackConstraints
  {-# INLINE typeGType #-}

noMediaTrackConstraints :: Maybe MediaTrackConstraints
noMediaTrackConstraints = Nothing
{-# INLINE noMediaTrackConstraints #-}

gTypeMediaTrackConstraints :: JSM GType
gTypeMediaTrackConstraints = GType . Object <$> jsg "MediaTrackConstraints"

-- | Functions for this inteface are in "JSDOM.MediaTrackSettings".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSettings Mozilla MediaTrackSettings documentation>
newtype MediaTrackSettings = MediaTrackSettings { unMediaTrackSettings :: JSVal }

instance PToJSVal MediaTrackSettings where
  pToJSVal = unMediaTrackSettings
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaTrackSettings where
  pFromJSVal = MediaTrackSettings
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaTrackSettings where
  toJSVal = return . unMediaTrackSettings
  {-# INLINE toJSVal #-}

instance FromJSVal MediaTrackSettings where
  fromJSVal v = fmap MediaTrackSettings <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaTrackSettings
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaTrackSettings where
  makeObject = makeObject . unMediaTrackSettings

instance IsGObject MediaTrackSettings where
  typeGType _ = gTypeMediaTrackSettings
  {-# INLINE typeGType #-}

noMediaTrackSettings :: Maybe MediaTrackSettings
noMediaTrackSettings = Nothing
{-# INLINE noMediaTrackSettings #-}

gTypeMediaTrackSettings :: JSM GType
gTypeMediaTrackSettings = GType . Object <$> jsg "MediaTrackSettings"

-- | Functions for this inteface are in "JSDOM.MediaTrackSupportedConstraints".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MediaTrackSupportedConstraints Mozilla MediaTrackSupportedConstraints documentation>
newtype MediaTrackSupportedConstraints = MediaTrackSupportedConstraints { unMediaTrackSupportedConstraints :: JSVal }

instance PToJSVal MediaTrackSupportedConstraints where
  pToJSVal = unMediaTrackSupportedConstraints
  {-# INLINE pToJSVal #-}

instance PFromJSVal MediaTrackSupportedConstraints where
  pFromJSVal = MediaTrackSupportedConstraints
  {-# INLINE pFromJSVal #-}

instance ToJSVal MediaTrackSupportedConstraints where
  toJSVal = return . unMediaTrackSupportedConstraints
  {-# INLINE toJSVal #-}

instance FromJSVal MediaTrackSupportedConstraints where
  fromJSVal v = fmap MediaTrackSupportedConstraints <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MediaTrackSupportedConstraints
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MediaTrackSupportedConstraints where
  makeObject = makeObject . unMediaTrackSupportedConstraints

instance IsGObject MediaTrackSupportedConstraints where
  typeGType _ = gTypeMediaTrackSupportedConstraints
  {-# INLINE typeGType #-}

noMediaTrackSupportedConstraints :: Maybe MediaTrackSupportedConstraints
noMediaTrackSupportedConstraints = Nothing
{-# INLINE noMediaTrackSupportedConstraints #-}

gTypeMediaTrackSupportedConstraints :: JSM GType
gTypeMediaTrackSupportedConstraints = GType . Object <$> jsg "MediaTrackSupportedConstraints"

-- | Functions for this inteface are in "JSDOM.MessageChannel".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MessageChannel Mozilla MessageChannel documentation>
newtype MessageChannel = MessageChannel { unMessageChannel :: JSVal }

instance PToJSVal MessageChannel where
  pToJSVal = unMessageChannel
  {-# INLINE pToJSVal #-}

instance PFromJSVal MessageChannel where
  pFromJSVal = MessageChannel
  {-# INLINE pFromJSVal #-}

instance ToJSVal MessageChannel where
  toJSVal = return . unMessageChannel
  {-# INLINE toJSVal #-}

instance FromJSVal MessageChannel where
  fromJSVal v = fmap MessageChannel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MessageChannel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MessageChannel where
  makeObject = makeObject . unMessageChannel

instance IsGObject MessageChannel where
  typeGType _ = gTypeMessageChannel
  {-# INLINE typeGType #-}

noMessageChannel :: Maybe MessageChannel
noMessageChannel = Nothing
{-# INLINE noMessageChannel #-}

gTypeMessageChannel :: JSM GType
gTypeMessageChannel = GType . Object <$> jsg "MessageChannel"

-- | Functions for this inteface are in "JSDOM.MessageEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MessageEvent Mozilla MessageEvent documentation>
newtype MessageEvent = MessageEvent { unMessageEvent :: JSVal }

instance PToJSVal MessageEvent where
  pToJSVal = unMessageEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MessageEvent where
  pFromJSVal = MessageEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MessageEvent where
  toJSVal = return . unMessageEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MessageEvent where
  fromJSVal v = fmap MessageEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MessageEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MessageEvent where
  makeObject = makeObject . unMessageEvent

instance IsEvent MessageEvent
instance IsGObject MessageEvent where
  typeGType _ = gTypeMessageEvent
  {-# INLINE typeGType #-}

noMessageEvent :: Maybe MessageEvent
noMessageEvent = Nothing
{-# INLINE noMessageEvent #-}

gTypeMessageEvent :: JSM GType
gTypeMessageEvent = GType . Object <$> jsg "MessageEvent"

-- | Functions for this inteface are in "JSDOM.MessageEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MessageEventInit Mozilla MessageEventInit documentation>
newtype MessageEventInit = MessageEventInit { unMessageEventInit :: JSVal }

instance PToJSVal MessageEventInit where
  pToJSVal = unMessageEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MessageEventInit where
  pFromJSVal = MessageEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MessageEventInit where
  toJSVal = return . unMessageEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MessageEventInit where
  fromJSVal v = fmap MessageEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MessageEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MessageEventInit where
  makeObject = makeObject . unMessageEventInit

instance IsEventInit MessageEventInit
instance IsGObject MessageEventInit where
  typeGType _ = gTypeMessageEventInit
  {-# INLINE typeGType #-}

noMessageEventInit :: Maybe MessageEventInit
noMessageEventInit = Nothing
{-# INLINE noMessageEventInit #-}

gTypeMessageEventInit :: JSM GType
gTypeMessageEventInit = GType . Object <$> jsg "MessageEventInit"

-- | Functions for this inteface are in "JSDOM.MimeType".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MimeType Mozilla MimeType documentation>
newtype MimeType = MimeType { unMimeType :: JSVal }

instance PToJSVal MimeType where
  pToJSVal = unMimeType
  {-# INLINE pToJSVal #-}

instance PFromJSVal MimeType where
  pFromJSVal = MimeType
  {-# INLINE pFromJSVal #-}

instance ToJSVal MimeType where
  toJSVal = return . unMimeType
  {-# INLINE toJSVal #-}

instance FromJSVal MimeType where
  fromJSVal v = fmap MimeType <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MimeType
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MimeType where
  makeObject = makeObject . unMimeType

instance IsGObject MimeType where
  typeGType _ = gTypeMimeType
  {-# INLINE typeGType #-}

noMimeType :: Maybe MimeType
noMimeType = Nothing
{-# INLINE noMimeType #-}

gTypeMimeType :: JSM GType
gTypeMimeType = GType . Object <$> jsg "MimeType"

-- | Functions for this inteface are in "JSDOM.MimeTypeArray".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MimeTypeArray Mozilla MimeTypeArray documentation>
newtype MimeTypeArray = MimeTypeArray { unMimeTypeArray :: JSVal }

instance PToJSVal MimeTypeArray where
  pToJSVal = unMimeTypeArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal MimeTypeArray where
  pFromJSVal = MimeTypeArray
  {-# INLINE pFromJSVal #-}

instance ToJSVal MimeTypeArray where
  toJSVal = return . unMimeTypeArray
  {-# INLINE toJSVal #-}

instance FromJSVal MimeTypeArray where
  fromJSVal v = fmap MimeTypeArray <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MimeTypeArray
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MimeTypeArray where
  makeObject = makeObject . unMimeTypeArray

instance IsGObject MimeTypeArray where
  typeGType _ = gTypeMimeTypeArray
  {-# INLINE typeGType #-}

noMimeTypeArray :: Maybe MimeTypeArray
noMimeTypeArray = Nothing
{-# INLINE noMimeTypeArray #-}

gTypeMimeTypeArray :: JSM GType
gTypeMimeTypeArray = GType . Object <$> jsg "MimeTypeArray"

-- | Functions for this inteface are in "JSDOM.MouseEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent Mozilla MouseEvent documentation>
newtype MouseEvent = MouseEvent { unMouseEvent :: JSVal }

instance PToJSVal MouseEvent where
  pToJSVal = unMouseEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MouseEvent where
  pFromJSVal = MouseEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MouseEvent where
  toJSVal = return . unMouseEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MouseEvent where
  fromJSVal v = fmap MouseEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MouseEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MouseEvent where
  makeObject = makeObject . unMouseEvent

class (IsUIEvent o, IsEvent o, IsGObject o) => IsMouseEvent o
toMouseEvent :: IsMouseEvent o => o -> MouseEvent
toMouseEvent = MouseEvent . coerce

instance IsMouseEvent MouseEvent
instance IsUIEvent MouseEvent
instance IsEvent MouseEvent
instance IsGObject MouseEvent where
  typeGType _ = gTypeMouseEvent
  {-# INLINE typeGType #-}

noMouseEvent :: Maybe MouseEvent
noMouseEvent = Nothing
{-# INLINE noMouseEvent #-}

gTypeMouseEvent :: JSM GType
gTypeMouseEvent = GType . Object <$> jsg "MouseEvent"

-- | Functions for this inteface are in "JSDOM.MouseEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventModifierInit"
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MouseEventInit Mozilla MouseEventInit documentation>
newtype MouseEventInit = MouseEventInit { unMouseEventInit :: JSVal }

instance PToJSVal MouseEventInit where
  pToJSVal = unMouseEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MouseEventInit where
  pFromJSVal = MouseEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MouseEventInit where
  toJSVal = return . unMouseEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal MouseEventInit where
  fromJSVal v = fmap MouseEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MouseEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MouseEventInit where
  makeObject = makeObject . unMouseEventInit

class (IsEventModifierInit o, IsUIEventInit o, IsEventInit o, IsGObject o) => IsMouseEventInit o
toMouseEventInit :: IsMouseEventInit o => o -> MouseEventInit
toMouseEventInit = MouseEventInit . coerce

instance IsMouseEventInit MouseEventInit
instance IsEventModifierInit MouseEventInit
instance IsUIEventInit MouseEventInit
instance IsEventInit MouseEventInit
instance IsGObject MouseEventInit where
  typeGType _ = gTypeMouseEventInit
  {-# INLINE typeGType #-}

noMouseEventInit :: Maybe MouseEventInit
noMouseEventInit = Nothing
{-# INLINE noMouseEventInit #-}

gTypeMouseEventInit :: JSM GType
gTypeMouseEventInit = GType . Object <$> jsg "MouseEventInit"

-- | Functions for this inteface are in "JSDOM.MutationEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MutationEvent Mozilla MutationEvent documentation>
newtype MutationEvent = MutationEvent { unMutationEvent :: JSVal }

instance PToJSVal MutationEvent where
  pToJSVal = unMutationEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal MutationEvent where
  pFromJSVal = MutationEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal MutationEvent where
  toJSVal = return . unMutationEvent
  {-# INLINE toJSVal #-}

instance FromJSVal MutationEvent where
  fromJSVal v = fmap MutationEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MutationEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MutationEvent where
  makeObject = makeObject . unMutationEvent

instance IsEvent MutationEvent
instance IsGObject MutationEvent where
  typeGType _ = gTypeMutationEvent
  {-# INLINE typeGType #-}

noMutationEvent :: Maybe MutationEvent
noMutationEvent = Nothing
{-# INLINE noMutationEvent #-}

gTypeMutationEvent :: JSM GType
gTypeMutationEvent = GType . Object <$> jsg "MutationEvent"

-- | Functions for this inteface are in "JSDOM.MutationObserver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver Mozilla MutationObserver documentation>
newtype MutationObserver = MutationObserver { unMutationObserver :: JSVal }

instance PToJSVal MutationObserver where
  pToJSVal = unMutationObserver
  {-# INLINE pToJSVal #-}

instance PFromJSVal MutationObserver where
  pFromJSVal = MutationObserver
  {-# INLINE pFromJSVal #-}

instance ToJSVal MutationObserver where
  toJSVal = return . unMutationObserver
  {-# INLINE toJSVal #-}

instance FromJSVal MutationObserver where
  fromJSVal v = fmap MutationObserver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MutationObserver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MutationObserver where
  makeObject = makeObject . unMutationObserver

instance IsGObject MutationObserver where
  typeGType _ = gTypeMutationObserver
  {-# INLINE typeGType #-}

noMutationObserver :: Maybe MutationObserver
noMutationObserver = Nothing
{-# INLINE noMutationObserver #-}

gTypeMutationObserver :: JSM GType
gTypeMutationObserver = GType . Object <$> jsg "MutationObserver"

-- | Functions for this inteface are in "JSDOM.MutationObserverInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MutationObserverInit Mozilla MutationObserverInit documentation>
newtype MutationObserverInit = MutationObserverInit { unMutationObserverInit :: JSVal }

instance PToJSVal MutationObserverInit where
  pToJSVal = unMutationObserverInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal MutationObserverInit where
  pFromJSVal = MutationObserverInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal MutationObserverInit where
  toJSVal = return . unMutationObserverInit
  {-# INLINE toJSVal #-}

instance FromJSVal MutationObserverInit where
  fromJSVal v = fmap MutationObserverInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MutationObserverInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MutationObserverInit where
  makeObject = makeObject . unMutationObserverInit

instance IsGObject MutationObserverInit where
  typeGType _ = gTypeMutationObserverInit
  {-# INLINE typeGType #-}

noMutationObserverInit :: Maybe MutationObserverInit
noMutationObserverInit = Nothing
{-# INLINE noMutationObserverInit #-}

gTypeMutationObserverInit :: JSM GType
gTypeMutationObserverInit = GType . Object <$> jsg "MutationObserverInit"

-- | Functions for this inteface are in "JSDOM.MutationRecord".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/MutationRecord Mozilla MutationRecord documentation>
newtype MutationRecord = MutationRecord { unMutationRecord :: JSVal }

instance PToJSVal MutationRecord where
  pToJSVal = unMutationRecord
  {-# INLINE pToJSVal #-}

instance PFromJSVal MutationRecord where
  pFromJSVal = MutationRecord
  {-# INLINE pFromJSVal #-}

instance ToJSVal MutationRecord where
  toJSVal = return . unMutationRecord
  {-# INLINE toJSVal #-}

instance FromJSVal MutationRecord where
  fromJSVal v = fmap MutationRecord <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . MutationRecord
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject MutationRecord where
  makeObject = makeObject . unMutationRecord

instance IsGObject MutationRecord where
  typeGType _ = gTypeMutationRecord
  {-# INLINE typeGType #-}

noMutationRecord :: Maybe MutationRecord
noMutationRecord = Nothing
{-# INLINE noMutationRecord #-}

gTypeMutationRecord :: JSM GType
gTypeMutationRecord = GType . Object <$> jsg "MutationRecord"

-- | Functions for this inteface are in "JSDOM.NamedNodeMap".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NamedNodeMap Mozilla NamedNodeMap documentation>
newtype NamedNodeMap = NamedNodeMap { unNamedNodeMap :: JSVal }

instance PToJSVal NamedNodeMap where
  pToJSVal = unNamedNodeMap
  {-# INLINE pToJSVal #-}

instance PFromJSVal NamedNodeMap where
  pFromJSVal = NamedNodeMap
  {-# INLINE pFromJSVal #-}

instance ToJSVal NamedNodeMap where
  toJSVal = return . unNamedNodeMap
  {-# INLINE toJSVal #-}

instance FromJSVal NamedNodeMap where
  fromJSVal v = fmap NamedNodeMap <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NamedNodeMap
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NamedNodeMap where
  makeObject = makeObject . unNamedNodeMap

instance IsGObject NamedNodeMap where
  typeGType _ = gTypeNamedNodeMap
  {-# INLINE typeGType #-}

noNamedNodeMap :: Maybe NamedNodeMap
noNamedNodeMap = Nothing
{-# INLINE noNamedNodeMap #-}

gTypeNamedNodeMap :: JSM GType
gTypeNamedNodeMap = GType . Object <$> jsg "NamedNodeMap"

-- | Functions for this inteface are in "JSDOM.Navigator".
-- Base interface functions are in:
--
--     * "JSDOM.NavigatorOnLine"
--     * "JSDOM.NavigatorLanguage"
--     * "JSDOM.NavigatorID"
--     * "JSDOM.NavigatorConcurrentHardware"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Navigator Mozilla Navigator documentation>
newtype Navigator = Navigator { unNavigator :: JSVal }

instance PToJSVal Navigator where
  pToJSVal = unNavigator
  {-# INLINE pToJSVal #-}

instance PFromJSVal Navigator where
  pFromJSVal = Navigator
  {-# INLINE pFromJSVal #-}

instance ToJSVal Navigator where
  toJSVal = return . unNavigator
  {-# INLINE toJSVal #-}

instance FromJSVal Navigator where
  fromJSVal v = fmap Navigator <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Navigator
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Navigator where
  makeObject = makeObject . unNavigator

instance IsNavigatorOnLine Navigator
instance IsNavigatorLanguage Navigator
instance IsNavigatorID Navigator
instance IsNavigatorConcurrentHardware Navigator
instance IsGObject Navigator where
  typeGType _ = gTypeNavigator
  {-# INLINE typeGType #-}

noNavigator :: Maybe Navigator
noNavigator = Nothing
{-# INLINE noNavigator #-}

gTypeNavigator :: JSM GType
gTypeNavigator = GType . Object <$> jsg "Navigator"

-- | Functions for this inteface are in "JSDOM.NavigatorConcurrentHardware".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorConcurrentHardware Mozilla NavigatorConcurrentHardware documentation>
newtype NavigatorConcurrentHardware = NavigatorConcurrentHardware { unNavigatorConcurrentHardware :: JSVal }

instance PToJSVal NavigatorConcurrentHardware where
  pToJSVal = unNavigatorConcurrentHardware
  {-# INLINE pToJSVal #-}

instance PFromJSVal NavigatorConcurrentHardware where
  pFromJSVal = NavigatorConcurrentHardware
  {-# INLINE pFromJSVal #-}

instance ToJSVal NavigatorConcurrentHardware where
  toJSVal = return . unNavigatorConcurrentHardware
  {-# INLINE toJSVal #-}

instance FromJSVal NavigatorConcurrentHardware where
  fromJSVal v = fmap NavigatorConcurrentHardware <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NavigatorConcurrentHardware
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NavigatorConcurrentHardware where
  makeObject = makeObject . unNavigatorConcurrentHardware

class (IsGObject o) => IsNavigatorConcurrentHardware o
toNavigatorConcurrentHardware :: IsNavigatorConcurrentHardware o => o -> NavigatorConcurrentHardware
toNavigatorConcurrentHardware = NavigatorConcurrentHardware . coerce

instance IsNavigatorConcurrentHardware NavigatorConcurrentHardware
instance IsGObject NavigatorConcurrentHardware where
  typeGType _ = gTypeNavigatorConcurrentHardware
  {-# INLINE typeGType #-}

noNavigatorConcurrentHardware :: Maybe NavigatorConcurrentHardware
noNavigatorConcurrentHardware = Nothing
{-# INLINE noNavigatorConcurrentHardware #-}

gTypeNavigatorConcurrentHardware :: JSM GType
gTypeNavigatorConcurrentHardware = GType . Object <$> jsg "NavigatorConcurrentHardware"

-- | Functions for this inteface are in "JSDOM.NavigatorID".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorID Mozilla NavigatorID documentation>
newtype NavigatorID = NavigatorID { unNavigatorID :: JSVal }

instance PToJSVal NavigatorID where
  pToJSVal = unNavigatorID
  {-# INLINE pToJSVal #-}

instance PFromJSVal NavigatorID where
  pFromJSVal = NavigatorID
  {-# INLINE pFromJSVal #-}

instance ToJSVal NavigatorID where
  toJSVal = return . unNavigatorID
  {-# INLINE toJSVal #-}

instance FromJSVal NavigatorID where
  fromJSVal v = fmap NavigatorID <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NavigatorID
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NavigatorID where
  makeObject = makeObject . unNavigatorID

class (IsGObject o) => IsNavigatorID o
toNavigatorID :: IsNavigatorID o => o -> NavigatorID
toNavigatorID = NavigatorID . coerce

instance IsNavigatorID NavigatorID
instance IsGObject NavigatorID where
  typeGType _ = gTypeNavigatorID
  {-# INLINE typeGType #-}

noNavigatorID :: Maybe NavigatorID
noNavigatorID = Nothing
{-# INLINE noNavigatorID #-}

gTypeNavigatorID :: JSM GType
gTypeNavigatorID = GType . Object <$> jsg "NavigatorID"

-- | Functions for this inteface are in "JSDOM.NavigatorLanguage".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorLanguage Mozilla NavigatorLanguage documentation>
newtype NavigatorLanguage = NavigatorLanguage { unNavigatorLanguage :: JSVal }

instance PToJSVal NavigatorLanguage where
  pToJSVal = unNavigatorLanguage
  {-# INLINE pToJSVal #-}

instance PFromJSVal NavigatorLanguage where
  pFromJSVal = NavigatorLanguage
  {-# INLINE pFromJSVal #-}

instance ToJSVal NavigatorLanguage where
  toJSVal = return . unNavigatorLanguage
  {-# INLINE toJSVal #-}

instance FromJSVal NavigatorLanguage where
  fromJSVal v = fmap NavigatorLanguage <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NavigatorLanguage
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NavigatorLanguage where
  makeObject = makeObject . unNavigatorLanguage

class (IsGObject o) => IsNavigatorLanguage o
toNavigatorLanguage :: IsNavigatorLanguage o => o -> NavigatorLanguage
toNavigatorLanguage = NavigatorLanguage . coerce

instance IsNavigatorLanguage NavigatorLanguage
instance IsGObject NavigatorLanguage where
  typeGType _ = gTypeNavigatorLanguage
  {-# INLINE typeGType #-}

noNavigatorLanguage :: Maybe NavigatorLanguage
noNavigatorLanguage = Nothing
{-# INLINE noNavigatorLanguage #-}

gTypeNavigatorLanguage :: JSM GType
gTypeNavigatorLanguage = GType . Object <$> jsg "NavigatorLanguage"

-- | Functions for this inteface are in "JSDOM.NavigatorOnLine".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorOnLine Mozilla NavigatorOnLine documentation>
newtype NavigatorOnLine = NavigatorOnLine { unNavigatorOnLine :: JSVal }

instance PToJSVal NavigatorOnLine where
  pToJSVal = unNavigatorOnLine
  {-# INLINE pToJSVal #-}

instance PFromJSVal NavigatorOnLine where
  pFromJSVal = NavigatorOnLine
  {-# INLINE pFromJSVal #-}

instance ToJSVal NavigatorOnLine where
  toJSVal = return . unNavigatorOnLine
  {-# INLINE toJSVal #-}

instance FromJSVal NavigatorOnLine where
  fromJSVal v = fmap NavigatorOnLine <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NavigatorOnLine
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NavigatorOnLine where
  makeObject = makeObject . unNavigatorOnLine

class (IsGObject o) => IsNavigatorOnLine o
toNavigatorOnLine :: IsNavigatorOnLine o => o -> NavigatorOnLine
toNavigatorOnLine = NavigatorOnLine . coerce

instance IsNavigatorOnLine NavigatorOnLine
instance IsGObject NavigatorOnLine where
  typeGType _ = gTypeNavigatorOnLine
  {-# INLINE typeGType #-}

noNavigatorOnLine :: Maybe NavigatorOnLine
noNavigatorOnLine = Nothing
{-# INLINE noNavigatorOnLine #-}

gTypeNavigatorOnLine :: JSM GType
gTypeNavigatorOnLine = GType . Object <$> jsg "NavigatorOnLine"

-- | Functions for this inteface are in "JSDOM.NavigatorUserMediaError".
-- Base interface functions are in:
--
--     * "JSDOM.DOMError"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NavigatorUserMediaError Mozilla NavigatorUserMediaError documentation>
newtype NavigatorUserMediaError = NavigatorUserMediaError { unNavigatorUserMediaError :: JSVal }

instance PToJSVal NavigatorUserMediaError where
  pToJSVal = unNavigatorUserMediaError
  {-# INLINE pToJSVal #-}

instance PFromJSVal NavigatorUserMediaError where
  pFromJSVal = NavigatorUserMediaError
  {-# INLINE pFromJSVal #-}

instance ToJSVal NavigatorUserMediaError where
  toJSVal = return . unNavigatorUserMediaError
  {-# INLINE toJSVal #-}

instance FromJSVal NavigatorUserMediaError where
  fromJSVal v = fmap NavigatorUserMediaError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NavigatorUserMediaError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NavigatorUserMediaError where
  makeObject = makeObject . unNavigatorUserMediaError

instance IsDOMError NavigatorUserMediaError
instance IsGObject NavigatorUserMediaError where
  typeGType _ = gTypeNavigatorUserMediaError
  {-# INLINE typeGType #-}

noNavigatorUserMediaError :: Maybe NavigatorUserMediaError
noNavigatorUserMediaError = Nothing
{-# INLINE noNavigatorUserMediaError #-}

gTypeNavigatorUserMediaError :: JSM GType
gTypeNavigatorUserMediaError = GType . Object <$> jsg "NavigatorUserMediaError"

-- | Functions for this inteface are in "JSDOM.NodeIterator".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NodeIterator Mozilla NodeIterator documentation>
newtype NodeIterator = NodeIterator { unNodeIterator :: JSVal }

instance PToJSVal NodeIterator where
  pToJSVal = unNodeIterator
  {-# INLINE pToJSVal #-}

instance PFromJSVal NodeIterator where
  pFromJSVal = NodeIterator
  {-# INLINE pFromJSVal #-}

instance ToJSVal NodeIterator where
  toJSVal = return . unNodeIterator
  {-# INLINE toJSVal #-}

instance FromJSVal NodeIterator where
  fromJSVal v = fmap NodeIterator <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NodeIterator
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NodeIterator where
  makeObject = makeObject . unNodeIterator

instance IsGObject NodeIterator where
  typeGType _ = gTypeNodeIterator
  {-# INLINE typeGType #-}

noNodeIterator :: Maybe NodeIterator
noNodeIterator = Nothing
{-# INLINE noNodeIterator #-}

gTypeNodeIterator :: JSM GType
gTypeNodeIterator = GType . Object <$> jsg "NodeIterator"

-- | Functions for this inteface are in "JSDOM.Notification".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Notification Mozilla Notification documentation>
newtype Notification = Notification { unNotification :: JSVal }

instance PToJSVal Notification where
  pToJSVal = unNotification
  {-# INLINE pToJSVal #-}

instance PFromJSVal Notification where
  pFromJSVal = Notification
  {-# INLINE pFromJSVal #-}

instance ToJSVal Notification where
  toJSVal = return . unNotification
  {-# INLINE toJSVal #-}

instance FromJSVal Notification where
  fromJSVal v = fmap Notification <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Notification
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Notification where
  makeObject = makeObject . unNotification

instance IsEventTarget Notification
instance IsGObject Notification where
  typeGType _ = gTypeNotification
  {-# INLINE typeGType #-}

noNotification :: Maybe Notification
noNotification = Nothing
{-# INLINE noNotification #-}

gTypeNotification :: JSM GType
gTypeNotification = GType . Object <$> jsg "Notification"

-- | Functions for this inteface are in "JSDOM.NotificationOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NotificationOptions Mozilla NotificationOptions documentation>
newtype NotificationOptions = NotificationOptions { unNotificationOptions :: JSVal }

instance PToJSVal NotificationOptions where
  pToJSVal = unNotificationOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal NotificationOptions where
  pFromJSVal = NotificationOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal NotificationOptions where
  toJSVal = return . unNotificationOptions
  {-# INLINE toJSVal #-}

instance FromJSVal NotificationOptions where
  fromJSVal v = fmap NotificationOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NotificationOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NotificationOptions where
  makeObject = makeObject . unNotificationOptions

instance IsGObject NotificationOptions where
  typeGType _ = gTypeNotificationOptions
  {-# INLINE typeGType #-}

noNotificationOptions :: Maybe NotificationOptions
noNotificationOptions = Nothing
{-# INLINE noNotificationOptions #-}

gTypeNotificationOptions :: JSM GType
gTypeNotificationOptions = GType . Object <$> jsg "NotificationOptions"

-- | Functions for this inteface are in "JSDOM.OESElementIndexUint".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESElementIndexUint Mozilla OESElementIndexUint documentation>
newtype OESElementIndexUint = OESElementIndexUint { unOESElementIndexUint :: JSVal }

instance PToJSVal OESElementIndexUint where
  pToJSVal = unOESElementIndexUint
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESElementIndexUint where
  pFromJSVal = OESElementIndexUint
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESElementIndexUint where
  toJSVal = return . unOESElementIndexUint
  {-# INLINE toJSVal #-}

instance FromJSVal OESElementIndexUint where
  fromJSVal v = fmap OESElementIndexUint <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESElementIndexUint
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESElementIndexUint where
  makeObject = makeObject . unOESElementIndexUint

instance IsGObject OESElementIndexUint where
  typeGType _ = gTypeOESElementIndexUint
  {-# INLINE typeGType #-}

noOESElementIndexUint :: Maybe OESElementIndexUint
noOESElementIndexUint = Nothing
{-# INLINE noOESElementIndexUint #-}

gTypeOESElementIndexUint :: JSM GType
gTypeOESElementIndexUint = GType . Object <$> jsg "OESElementIndexUint"

-- | Functions for this inteface are in "JSDOM.OESStandardDerivatives".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESStandardDerivatives Mozilla OESStandardDerivatives documentation>
newtype OESStandardDerivatives = OESStandardDerivatives { unOESStandardDerivatives :: JSVal }

instance PToJSVal OESStandardDerivatives where
  pToJSVal = unOESStandardDerivatives
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESStandardDerivatives where
  pFromJSVal = OESStandardDerivatives
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESStandardDerivatives where
  toJSVal = return . unOESStandardDerivatives
  {-# INLINE toJSVal #-}

instance FromJSVal OESStandardDerivatives where
  fromJSVal v = fmap OESStandardDerivatives <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESStandardDerivatives
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESStandardDerivatives where
  makeObject = makeObject . unOESStandardDerivatives

instance IsGObject OESStandardDerivatives where
  typeGType _ = gTypeOESStandardDerivatives
  {-# INLINE typeGType #-}

noOESStandardDerivatives :: Maybe OESStandardDerivatives
noOESStandardDerivatives = Nothing
{-# INLINE noOESStandardDerivatives #-}

gTypeOESStandardDerivatives :: JSM GType
gTypeOESStandardDerivatives = GType . Object <$> jsg "OESStandardDerivatives"

-- | Functions for this inteface are in "JSDOM.OESTextureFloat".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESTextureFloat Mozilla OESTextureFloat documentation>
newtype OESTextureFloat = OESTextureFloat { unOESTextureFloat :: JSVal }

instance PToJSVal OESTextureFloat where
  pToJSVal = unOESTextureFloat
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESTextureFloat where
  pFromJSVal = OESTextureFloat
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESTextureFloat where
  toJSVal = return . unOESTextureFloat
  {-# INLINE toJSVal #-}

instance FromJSVal OESTextureFloat where
  fromJSVal v = fmap OESTextureFloat <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESTextureFloat
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESTextureFloat where
  makeObject = makeObject . unOESTextureFloat

instance IsGObject OESTextureFloat where
  typeGType _ = gTypeOESTextureFloat
  {-# INLINE typeGType #-}

noOESTextureFloat :: Maybe OESTextureFloat
noOESTextureFloat = Nothing
{-# INLINE noOESTextureFloat #-}

gTypeOESTextureFloat :: JSM GType
gTypeOESTextureFloat = GType . Object <$> jsg "OESTextureFloat"

-- | Functions for this inteface are in "JSDOM.OESTextureFloatLinear".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESTextureFloatLinear Mozilla OESTextureFloatLinear documentation>
newtype OESTextureFloatLinear = OESTextureFloatLinear { unOESTextureFloatLinear :: JSVal }

instance PToJSVal OESTextureFloatLinear where
  pToJSVal = unOESTextureFloatLinear
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESTextureFloatLinear where
  pFromJSVal = OESTextureFloatLinear
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESTextureFloatLinear where
  toJSVal = return . unOESTextureFloatLinear
  {-# INLINE toJSVal #-}

instance FromJSVal OESTextureFloatLinear where
  fromJSVal v = fmap OESTextureFloatLinear <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESTextureFloatLinear
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESTextureFloatLinear where
  makeObject = makeObject . unOESTextureFloatLinear

instance IsGObject OESTextureFloatLinear where
  typeGType _ = gTypeOESTextureFloatLinear
  {-# INLINE typeGType #-}

noOESTextureFloatLinear :: Maybe OESTextureFloatLinear
noOESTextureFloatLinear = Nothing
{-# INLINE noOESTextureFloatLinear #-}

gTypeOESTextureFloatLinear :: JSM GType
gTypeOESTextureFloatLinear = GType . Object <$> jsg "OESTextureFloatLinear"

-- | Functions for this inteface are in "JSDOM.OESTextureHalfFloat".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESTextureHalfFloat Mozilla OESTextureHalfFloat documentation>
newtype OESTextureHalfFloat = OESTextureHalfFloat { unOESTextureHalfFloat :: JSVal }

instance PToJSVal OESTextureHalfFloat where
  pToJSVal = unOESTextureHalfFloat
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESTextureHalfFloat where
  pFromJSVal = OESTextureHalfFloat
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESTextureHalfFloat where
  toJSVal = return . unOESTextureHalfFloat
  {-# INLINE toJSVal #-}

instance FromJSVal OESTextureHalfFloat where
  fromJSVal v = fmap OESTextureHalfFloat <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESTextureHalfFloat
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESTextureHalfFloat where
  makeObject = makeObject . unOESTextureHalfFloat

instance IsGObject OESTextureHalfFloat where
  typeGType _ = gTypeOESTextureHalfFloat
  {-# INLINE typeGType #-}

noOESTextureHalfFloat :: Maybe OESTextureHalfFloat
noOESTextureHalfFloat = Nothing
{-# INLINE noOESTextureHalfFloat #-}

gTypeOESTextureHalfFloat :: JSM GType
gTypeOESTextureHalfFloat = GType . Object <$> jsg "OESTextureHalfFloat"

-- | Functions for this inteface are in "JSDOM.OESTextureHalfFloatLinear".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESTextureHalfFloatLinear Mozilla OESTextureHalfFloatLinear documentation>
newtype OESTextureHalfFloatLinear = OESTextureHalfFloatLinear { unOESTextureHalfFloatLinear :: JSVal }

instance PToJSVal OESTextureHalfFloatLinear where
  pToJSVal = unOESTextureHalfFloatLinear
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESTextureHalfFloatLinear where
  pFromJSVal = OESTextureHalfFloatLinear
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESTextureHalfFloatLinear where
  toJSVal = return . unOESTextureHalfFloatLinear
  {-# INLINE toJSVal #-}

instance FromJSVal OESTextureHalfFloatLinear where
  fromJSVal v = fmap OESTextureHalfFloatLinear <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESTextureHalfFloatLinear
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESTextureHalfFloatLinear where
  makeObject = makeObject . unOESTextureHalfFloatLinear

instance IsGObject OESTextureHalfFloatLinear where
  typeGType _ = gTypeOESTextureHalfFloatLinear
  {-# INLINE typeGType #-}

noOESTextureHalfFloatLinear :: Maybe OESTextureHalfFloatLinear
noOESTextureHalfFloatLinear = Nothing
{-# INLINE noOESTextureHalfFloatLinear #-}

gTypeOESTextureHalfFloatLinear :: JSM GType
gTypeOESTextureHalfFloatLinear = GType . Object <$> jsg "OESTextureHalfFloatLinear"

-- | Functions for this inteface are in "JSDOM.OESVertexArrayObject".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OESVertexArrayObject Mozilla OESVertexArrayObject documentation>
newtype OESVertexArrayObject = OESVertexArrayObject { unOESVertexArrayObject :: JSVal }

instance PToJSVal OESVertexArrayObject where
  pToJSVal = unOESVertexArrayObject
  {-# INLINE pToJSVal #-}

instance PFromJSVal OESVertexArrayObject where
  pFromJSVal = OESVertexArrayObject
  {-# INLINE pFromJSVal #-}

instance ToJSVal OESVertexArrayObject where
  toJSVal = return . unOESVertexArrayObject
  {-# INLINE toJSVal #-}

instance FromJSVal OESVertexArrayObject where
  fromJSVal v = fmap OESVertexArrayObject <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OESVertexArrayObject
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OESVertexArrayObject where
  makeObject = makeObject . unOESVertexArrayObject

instance IsGObject OESVertexArrayObject where
  typeGType _ = gTypeOESVertexArrayObject
  {-# INLINE typeGType #-}

noOESVertexArrayObject :: Maybe OESVertexArrayObject
noOESVertexArrayObject = Nothing
{-# INLINE noOESVertexArrayObject #-}

gTypeOESVertexArrayObject :: JSM GType
gTypeOESVertexArrayObject = GType . Object <$> jsg "OESVertexArrayObject"

-- | Functions for this inteface are in "JSDOM.OfflineAudioCompletionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioCompletionEvent Mozilla OfflineAudioCompletionEvent documentation>
newtype OfflineAudioCompletionEvent = OfflineAudioCompletionEvent { unOfflineAudioCompletionEvent :: JSVal }

instance PToJSVal OfflineAudioCompletionEvent where
  pToJSVal = unOfflineAudioCompletionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal OfflineAudioCompletionEvent where
  pFromJSVal = OfflineAudioCompletionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal OfflineAudioCompletionEvent where
  toJSVal = return . unOfflineAudioCompletionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal OfflineAudioCompletionEvent where
  fromJSVal v = fmap OfflineAudioCompletionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OfflineAudioCompletionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OfflineAudioCompletionEvent where
  makeObject = makeObject . unOfflineAudioCompletionEvent

instance IsEvent OfflineAudioCompletionEvent
instance IsGObject OfflineAudioCompletionEvent where
  typeGType _ = gTypeOfflineAudioCompletionEvent
  {-# INLINE typeGType #-}

noOfflineAudioCompletionEvent :: Maybe OfflineAudioCompletionEvent
noOfflineAudioCompletionEvent = Nothing
{-# INLINE noOfflineAudioCompletionEvent #-}

gTypeOfflineAudioCompletionEvent :: JSM GType
gTypeOfflineAudioCompletionEvent = GType . Object <$> jsg "OfflineAudioCompletionEvent"

-- | Functions for this inteface are in "JSDOM.OfflineAudioContext".
-- Base interface functions are in:
--
--     * "JSDOM.AudioContext"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OfflineAudioContext Mozilla OfflineAudioContext documentation>
newtype OfflineAudioContext = OfflineAudioContext { unOfflineAudioContext :: JSVal }

instance PToJSVal OfflineAudioContext where
  pToJSVal = unOfflineAudioContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal OfflineAudioContext where
  pFromJSVal = OfflineAudioContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal OfflineAudioContext where
  toJSVal = return . unOfflineAudioContext
  {-# INLINE toJSVal #-}

instance FromJSVal OfflineAudioContext where
  fromJSVal v = fmap OfflineAudioContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OfflineAudioContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OfflineAudioContext where
  makeObject = makeObject . unOfflineAudioContext

instance IsAudioContext OfflineAudioContext
instance IsEventTarget OfflineAudioContext
instance IsGObject OfflineAudioContext where
  typeGType _ = gTypeOfflineAudioContext
  {-# INLINE typeGType #-}

noOfflineAudioContext :: Maybe OfflineAudioContext
noOfflineAudioContext = Nothing
{-# INLINE noOfflineAudioContext #-}

gTypeOfflineAudioContext :: JSM GType
gTypeOfflineAudioContext = GType . Object <$> jsg "OfflineAudioContext"

-- | Functions for this inteface are in "JSDOM.OscillatorNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OscillatorNode Mozilla OscillatorNode documentation>
newtype OscillatorNode = OscillatorNode { unOscillatorNode :: JSVal }

instance PToJSVal OscillatorNode where
  pToJSVal = unOscillatorNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal OscillatorNode where
  pFromJSVal = OscillatorNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal OscillatorNode where
  toJSVal = return . unOscillatorNode
  {-# INLINE toJSVal #-}

instance FromJSVal OscillatorNode where
  fromJSVal v = fmap OscillatorNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OscillatorNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OscillatorNode where
  makeObject = makeObject . unOscillatorNode

instance IsAudioNode OscillatorNode
instance IsEventTarget OscillatorNode
instance IsGObject OscillatorNode where
  typeGType _ = gTypeOscillatorNode
  {-# INLINE typeGType #-}

noOscillatorNode :: Maybe OscillatorNode
noOscillatorNode = Nothing
{-# INLINE noOscillatorNode #-}

gTypeOscillatorNode :: JSM GType
gTypeOscillatorNode = GType . Object <$> jsg "OscillatorNode"

-- | Functions for this inteface are in "JSDOM.OverconstrainedError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OverconstrainedError Mozilla OverconstrainedError documentation>
newtype OverconstrainedError = OverconstrainedError { unOverconstrainedError :: JSVal }

instance PToJSVal OverconstrainedError where
  pToJSVal = unOverconstrainedError
  {-# INLINE pToJSVal #-}

instance PFromJSVal OverconstrainedError where
  pFromJSVal = OverconstrainedError
  {-# INLINE pFromJSVal #-}

instance ToJSVal OverconstrainedError where
  toJSVal = return . unOverconstrainedError
  {-# INLINE toJSVal #-}

instance FromJSVal OverconstrainedError where
  fromJSVal v = fmap OverconstrainedError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OverconstrainedError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OverconstrainedError where
  makeObject = makeObject . unOverconstrainedError

instance IsGObject OverconstrainedError where
  typeGType _ = gTypeOverconstrainedError
  {-# INLINE typeGType #-}

noOverconstrainedError :: Maybe OverconstrainedError
noOverconstrainedError = Nothing
{-# INLINE noOverconstrainedError #-}

gTypeOverconstrainedError :: JSM GType
gTypeOverconstrainedError = GType . Object <$> jsg "OverconstrainedError"

-- | Functions for this inteface are in "JSDOM.OverconstrainedErrorEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OverconstrainedErrorEvent Mozilla OverconstrainedErrorEvent documentation>
newtype OverconstrainedErrorEvent = OverconstrainedErrorEvent { unOverconstrainedErrorEvent :: JSVal }

instance PToJSVal OverconstrainedErrorEvent where
  pToJSVal = unOverconstrainedErrorEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal OverconstrainedErrorEvent where
  pFromJSVal = OverconstrainedErrorEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal OverconstrainedErrorEvent where
  toJSVal = return . unOverconstrainedErrorEvent
  {-# INLINE toJSVal #-}

instance FromJSVal OverconstrainedErrorEvent where
  fromJSVal v = fmap OverconstrainedErrorEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OverconstrainedErrorEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OverconstrainedErrorEvent where
  makeObject = makeObject . unOverconstrainedErrorEvent

instance IsEvent OverconstrainedErrorEvent
instance IsGObject OverconstrainedErrorEvent where
  typeGType _ = gTypeOverconstrainedErrorEvent
  {-# INLINE typeGType #-}

noOverconstrainedErrorEvent :: Maybe OverconstrainedErrorEvent
noOverconstrainedErrorEvent = Nothing
{-# INLINE noOverconstrainedErrorEvent #-}

gTypeOverconstrainedErrorEvent :: JSM GType
gTypeOverconstrainedErrorEvent = GType . Object <$> jsg "OverconstrainedErrorEvent"

-- | Functions for this inteface are in "JSDOM.OverconstrainedErrorEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OverconstrainedErrorEventInit Mozilla OverconstrainedErrorEventInit documentation>
newtype OverconstrainedErrorEventInit = OverconstrainedErrorEventInit { unOverconstrainedErrorEventInit :: JSVal }

instance PToJSVal OverconstrainedErrorEventInit where
  pToJSVal = unOverconstrainedErrorEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal OverconstrainedErrorEventInit where
  pFromJSVal = OverconstrainedErrorEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal OverconstrainedErrorEventInit where
  toJSVal = return . unOverconstrainedErrorEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal OverconstrainedErrorEventInit where
  fromJSVal v = fmap OverconstrainedErrorEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OverconstrainedErrorEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OverconstrainedErrorEventInit where
  makeObject = makeObject . unOverconstrainedErrorEventInit

instance IsEventInit OverconstrainedErrorEventInit
instance IsGObject OverconstrainedErrorEventInit where
  typeGType _ = gTypeOverconstrainedErrorEventInit
  {-# INLINE typeGType #-}

noOverconstrainedErrorEventInit :: Maybe OverconstrainedErrorEventInit
noOverconstrainedErrorEventInit = Nothing
{-# INLINE noOverconstrainedErrorEventInit #-}

gTypeOverconstrainedErrorEventInit :: JSM GType
gTypeOverconstrainedErrorEventInit = GType . Object <$> jsg "OverconstrainedErrorEventInit"

-- | Functions for this inteface are in "JSDOM.OverflowEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OverflowEvent Mozilla OverflowEvent documentation>
newtype OverflowEvent = OverflowEvent { unOverflowEvent :: JSVal }

instance PToJSVal OverflowEvent where
  pToJSVal = unOverflowEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal OverflowEvent where
  pFromJSVal = OverflowEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal OverflowEvent where
  toJSVal = return . unOverflowEvent
  {-# INLINE toJSVal #-}

instance FromJSVal OverflowEvent where
  fromJSVal v = fmap OverflowEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OverflowEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OverflowEvent where
  makeObject = makeObject . unOverflowEvent

instance IsEvent OverflowEvent
instance IsGObject OverflowEvent where
  typeGType _ = gTypeOverflowEvent
  {-# INLINE typeGType #-}

noOverflowEvent :: Maybe OverflowEvent
noOverflowEvent = Nothing
{-# INLINE noOverflowEvent #-}

gTypeOverflowEvent :: JSM GType
gTypeOverflowEvent = GType . Object <$> jsg "OverflowEvent"

-- | Functions for this inteface are in "JSDOM.OverflowEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/OverflowEventInit Mozilla OverflowEventInit documentation>
newtype OverflowEventInit = OverflowEventInit { unOverflowEventInit :: JSVal }

instance PToJSVal OverflowEventInit where
  pToJSVal = unOverflowEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal OverflowEventInit where
  pFromJSVal = OverflowEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal OverflowEventInit where
  toJSVal = return . unOverflowEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal OverflowEventInit where
  fromJSVal v = fmap OverflowEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . OverflowEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject OverflowEventInit where
  makeObject = makeObject . unOverflowEventInit

instance IsEventInit OverflowEventInit
instance IsGObject OverflowEventInit where
  typeGType _ = gTypeOverflowEventInit
  {-# INLINE typeGType #-}

noOverflowEventInit :: Maybe OverflowEventInit
noOverflowEventInit = Nothing
{-# INLINE noOverflowEventInit #-}

gTypeOverflowEventInit :: JSM GType
gTypeOverflowEventInit = GType . Object <$> jsg "OverflowEventInit"

-- | Functions for this inteface are in "JSDOM.PageTransitionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PageTransitionEvent Mozilla PageTransitionEvent documentation>
newtype PageTransitionEvent = PageTransitionEvent { unPageTransitionEvent :: JSVal }

instance PToJSVal PageTransitionEvent where
  pToJSVal = unPageTransitionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal PageTransitionEvent where
  pFromJSVal = PageTransitionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal PageTransitionEvent where
  toJSVal = return . unPageTransitionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal PageTransitionEvent where
  fromJSVal v = fmap PageTransitionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PageTransitionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PageTransitionEvent where
  makeObject = makeObject . unPageTransitionEvent

instance IsEvent PageTransitionEvent
instance IsGObject PageTransitionEvent where
  typeGType _ = gTypePageTransitionEvent
  {-# INLINE typeGType #-}

noPageTransitionEvent :: Maybe PageTransitionEvent
noPageTransitionEvent = Nothing
{-# INLINE noPageTransitionEvent #-}

gTypePageTransitionEvent :: JSM GType
gTypePageTransitionEvent = GType . Object <$> jsg "PageTransitionEvent"

-- | Functions for this inteface are in "JSDOM.PageTransitionEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PageTransitionEventInit Mozilla PageTransitionEventInit documentation>
newtype PageTransitionEventInit = PageTransitionEventInit { unPageTransitionEventInit :: JSVal }

instance PToJSVal PageTransitionEventInit where
  pToJSVal = unPageTransitionEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal PageTransitionEventInit where
  pFromJSVal = PageTransitionEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal PageTransitionEventInit where
  toJSVal = return . unPageTransitionEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal PageTransitionEventInit where
  fromJSVal v = fmap PageTransitionEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PageTransitionEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PageTransitionEventInit where
  makeObject = makeObject . unPageTransitionEventInit

instance IsEventInit PageTransitionEventInit
instance IsGObject PageTransitionEventInit where
  typeGType _ = gTypePageTransitionEventInit
  {-# INLINE typeGType #-}

noPageTransitionEventInit :: Maybe PageTransitionEventInit
noPageTransitionEventInit = Nothing
{-# INLINE noPageTransitionEventInit #-}

gTypePageTransitionEventInit :: JSM GType
gTypePageTransitionEventInit = GType . Object <$> jsg "PageTransitionEventInit"

-- | Functions for this inteface are in "JSDOM.PannerNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/webkitAudioPannerNode Mozilla webkitAudioPannerNode documentation>
newtype PannerNode = PannerNode { unPannerNode :: JSVal }

instance PToJSVal PannerNode where
  pToJSVal = unPannerNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal PannerNode where
  pFromJSVal = PannerNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal PannerNode where
  toJSVal = return . unPannerNode
  {-# INLINE toJSVal #-}

instance FromJSVal PannerNode where
  fromJSVal v = fmap PannerNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PannerNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PannerNode where
  makeObject = makeObject . unPannerNode

instance IsAudioNode PannerNode
instance IsEventTarget PannerNode
instance IsGObject PannerNode where
  typeGType _ = gTypePannerNode
  {-# INLINE typeGType #-}

noPannerNode :: Maybe PannerNode
noPannerNode = Nothing
{-# INLINE noPannerNode #-}

gTypePannerNode :: JSM GType
gTypePannerNode = GType . Object <$> jsg "webkitAudioPannerNode"

-- | Functions for this inteface are in "JSDOM.PasswordCredential".
-- Base interface functions are in:
--
--     * "JSDOM.SiteBoundCredential"
--     * "JSDOM.BasicCredential"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PasswordCredential Mozilla PasswordCredential documentation>
newtype PasswordCredential = PasswordCredential { unPasswordCredential :: JSVal }

instance PToJSVal PasswordCredential where
  pToJSVal = unPasswordCredential
  {-# INLINE pToJSVal #-}

instance PFromJSVal PasswordCredential where
  pFromJSVal = PasswordCredential
  {-# INLINE pFromJSVal #-}

instance ToJSVal PasswordCredential where
  toJSVal = return . unPasswordCredential
  {-# INLINE toJSVal #-}

instance FromJSVal PasswordCredential where
  fromJSVal v = fmap PasswordCredential <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PasswordCredential
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PasswordCredential where
  makeObject = makeObject . unPasswordCredential

instance IsSiteBoundCredential PasswordCredential
instance IsBasicCredential PasswordCredential
instance IsGObject PasswordCredential where
  typeGType _ = gTypePasswordCredential
  {-# INLINE typeGType #-}

noPasswordCredential :: Maybe PasswordCredential
noPasswordCredential = Nothing
{-# INLINE noPasswordCredential #-}

gTypePasswordCredential :: JSM GType
gTypePasswordCredential = GType . Object <$> jsg "PasswordCredential"

-- | Functions for this inteface are in "JSDOM.PasswordCredentialData".
-- Base interface functions are in:
--
--     * "JSDOM.SiteBoundCredentialData"
--     * "JSDOM.CredentialData"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PasswordCredentialData Mozilla PasswordCredentialData documentation>
newtype PasswordCredentialData = PasswordCredentialData { unPasswordCredentialData :: JSVal }

instance PToJSVal PasswordCredentialData where
  pToJSVal = unPasswordCredentialData
  {-# INLINE pToJSVal #-}

instance PFromJSVal PasswordCredentialData where
  pFromJSVal = PasswordCredentialData
  {-# INLINE pFromJSVal #-}

instance ToJSVal PasswordCredentialData where
  toJSVal = return . unPasswordCredentialData
  {-# INLINE toJSVal #-}

instance FromJSVal PasswordCredentialData where
  fromJSVal v = fmap PasswordCredentialData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PasswordCredentialData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PasswordCredentialData where
  makeObject = makeObject . unPasswordCredentialData

instance IsSiteBoundCredentialData PasswordCredentialData
instance IsCredentialData PasswordCredentialData
instance IsGObject PasswordCredentialData where
  typeGType _ = gTypePasswordCredentialData
  {-# INLINE typeGType #-}

noPasswordCredentialData :: Maybe PasswordCredentialData
noPasswordCredentialData = Nothing
{-# INLINE noPasswordCredentialData #-}

gTypePasswordCredentialData :: JSM GType
gTypePasswordCredentialData = GType . Object <$> jsg "PasswordCredentialData"

-- | Functions for this inteface are in "JSDOM.Path2D".
-- Base interface functions are in:
--
--     * "JSDOM.CanvasPath"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Path2D Mozilla Path2D documentation>
newtype Path2D = Path2D { unPath2D :: JSVal }

instance PToJSVal Path2D where
  pToJSVal = unPath2D
  {-# INLINE pToJSVal #-}

instance PFromJSVal Path2D where
  pFromJSVal = Path2D
  {-# INLINE pFromJSVal #-}

instance ToJSVal Path2D where
  toJSVal = return . unPath2D
  {-# INLINE toJSVal #-}

instance FromJSVal Path2D where
  fromJSVal v = fmap Path2D <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Path2D
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Path2D where
  makeObject = makeObject . unPath2D

instance IsCanvasPath Path2D
instance IsGObject Path2D where
  typeGType _ = gTypePath2D
  {-# INLINE typeGType #-}

noPath2D :: Maybe Path2D
noPath2D = Nothing
{-# INLINE noPath2D #-}

gTypePath2D :: JSM GType
gTypePath2D = GType . Object <$> jsg "Path2D"

-- | Functions for this inteface are in "JSDOM.Pbkdf2Params".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Pbkdf2Params Mozilla Pbkdf2Params documentation>
newtype Pbkdf2Params = Pbkdf2Params { unPbkdf2Params :: JSVal }

instance PToJSVal Pbkdf2Params where
  pToJSVal = unPbkdf2Params
  {-# INLINE pToJSVal #-}

instance PFromJSVal Pbkdf2Params where
  pFromJSVal = Pbkdf2Params
  {-# INLINE pFromJSVal #-}

instance ToJSVal Pbkdf2Params where
  toJSVal = return . unPbkdf2Params
  {-# INLINE toJSVal #-}

instance FromJSVal Pbkdf2Params where
  fromJSVal v = fmap Pbkdf2Params <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Pbkdf2Params
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Pbkdf2Params where
  makeObject = makeObject . unPbkdf2Params

instance IsCryptoAlgorithmParameters Pbkdf2Params
instance IsGObject Pbkdf2Params where
  typeGType _ = gTypePbkdf2Params
  {-# INLINE typeGType #-}

noPbkdf2Params :: Maybe Pbkdf2Params
noPbkdf2Params = Nothing
{-# INLINE noPbkdf2Params #-}

gTypePbkdf2Params :: JSM GType
gTypePbkdf2Params = GType . Object <$> jsg "Pbkdf2Params"

-- | Functions for this inteface are in "JSDOM.Performance".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Performance Mozilla Performance documentation>
newtype Performance = Performance { unPerformance :: JSVal }

instance PToJSVal Performance where
  pToJSVal = unPerformance
  {-# INLINE pToJSVal #-}

instance PFromJSVal Performance where
  pFromJSVal = Performance
  {-# INLINE pFromJSVal #-}

instance ToJSVal Performance where
  toJSVal = return . unPerformance
  {-# INLINE toJSVal #-}

instance FromJSVal Performance where
  fromJSVal v = fmap Performance <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Performance
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Performance where
  makeObject = makeObject . unPerformance

instance IsEventTarget Performance
instance IsGObject Performance where
  typeGType _ = gTypePerformance
  {-# INLINE typeGType #-}

noPerformance :: Maybe Performance
noPerformance = Nothing
{-# INLINE noPerformance #-}

gTypePerformance :: JSM GType
gTypePerformance = GType . Object <$> jsg "Performance"

-- | Functions for this inteface are in "JSDOM.PerformanceMark".
-- Base interface functions are in:
--
--     * "JSDOM.PerformanceEntry"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceMark Mozilla PerformanceMark documentation>
newtype PerformanceMark = PerformanceMark { unPerformanceMark :: JSVal }

instance PToJSVal PerformanceMark where
  pToJSVal = unPerformanceMark
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceMark where
  pFromJSVal = PerformanceMark
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceMark where
  toJSVal = return . unPerformanceMark
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceMark where
  fromJSVal v = fmap PerformanceMark <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceMark
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceMark where
  makeObject = makeObject . unPerformanceMark

instance IsPerformanceEntry PerformanceMark
instance IsGObject PerformanceMark where
  typeGType _ = gTypePerformanceMark
  {-# INLINE typeGType #-}

noPerformanceMark :: Maybe PerformanceMark
noPerformanceMark = Nothing
{-# INLINE noPerformanceMark #-}

gTypePerformanceMark :: JSM GType
gTypePerformanceMark = GType . Object <$> jsg "PerformanceMark"

-- | Functions for this inteface are in "JSDOM.PerformanceMeasure".
-- Base interface functions are in:
--
--     * "JSDOM.PerformanceEntry"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceMeasure Mozilla PerformanceMeasure documentation>
newtype PerformanceMeasure = PerformanceMeasure { unPerformanceMeasure :: JSVal }

instance PToJSVal PerformanceMeasure where
  pToJSVal = unPerformanceMeasure
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceMeasure where
  pFromJSVal = PerformanceMeasure
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceMeasure where
  toJSVal = return . unPerformanceMeasure
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceMeasure where
  fromJSVal v = fmap PerformanceMeasure <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceMeasure
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceMeasure where
  makeObject = makeObject . unPerformanceMeasure

instance IsPerformanceEntry PerformanceMeasure
instance IsGObject PerformanceMeasure where
  typeGType _ = gTypePerformanceMeasure
  {-# INLINE typeGType #-}

noPerformanceMeasure :: Maybe PerformanceMeasure
noPerformanceMeasure = Nothing
{-# INLINE noPerformanceMeasure #-}

gTypePerformanceMeasure :: JSM GType
gTypePerformanceMeasure = GType . Object <$> jsg "PerformanceMeasure"

-- | Functions for this inteface are in "JSDOM.PerformanceNavigation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceNavigation Mozilla PerformanceNavigation documentation>
newtype PerformanceNavigation = PerformanceNavigation { unPerformanceNavigation :: JSVal }

instance PToJSVal PerformanceNavigation where
  pToJSVal = unPerformanceNavigation
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceNavigation where
  pFromJSVal = PerformanceNavigation
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceNavigation where
  toJSVal = return . unPerformanceNavigation
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceNavigation where
  fromJSVal v = fmap PerformanceNavigation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceNavigation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceNavigation where
  makeObject = makeObject . unPerformanceNavigation

instance IsGObject PerformanceNavigation where
  typeGType _ = gTypePerformanceNavigation
  {-# INLINE typeGType #-}

noPerformanceNavigation :: Maybe PerformanceNavigation
noPerformanceNavigation = Nothing
{-# INLINE noPerformanceNavigation #-}

gTypePerformanceNavigation :: JSM GType
gTypePerformanceNavigation = GType . Object <$> jsg "PerformanceNavigation"

-- | Functions for this inteface are in "JSDOM.PerformanceObserver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserver Mozilla PerformanceObserver documentation>
newtype PerformanceObserver = PerformanceObserver { unPerformanceObserver :: JSVal }

instance PToJSVal PerformanceObserver where
  pToJSVal = unPerformanceObserver
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceObserver where
  pFromJSVal = PerformanceObserver
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceObserver where
  toJSVal = return . unPerformanceObserver
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceObserver where
  fromJSVal v = fmap PerformanceObserver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceObserver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceObserver where
  makeObject = makeObject . unPerformanceObserver

instance IsGObject PerformanceObserver where
  typeGType _ = gTypePerformanceObserver
  {-# INLINE typeGType #-}

noPerformanceObserver :: Maybe PerformanceObserver
noPerformanceObserver = Nothing
{-# INLINE noPerformanceObserver #-}

gTypePerformanceObserver :: JSM GType
gTypePerformanceObserver = GType . Object <$> jsg "PerformanceObserver"

-- | Functions for this inteface are in "JSDOM.PerformanceObserverEntryList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserverEntryList Mozilla PerformanceObserverEntryList documentation>
newtype PerformanceObserverEntryList = PerformanceObserverEntryList { unPerformanceObserverEntryList :: JSVal }

instance PToJSVal PerformanceObserverEntryList where
  pToJSVal = unPerformanceObserverEntryList
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceObserverEntryList where
  pFromJSVal = PerformanceObserverEntryList
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceObserverEntryList where
  toJSVal = return . unPerformanceObserverEntryList
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceObserverEntryList where
  fromJSVal v = fmap PerformanceObserverEntryList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceObserverEntryList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceObserverEntryList where
  makeObject = makeObject . unPerformanceObserverEntryList

instance IsGObject PerformanceObserverEntryList where
  typeGType _ = gTypePerformanceObserverEntryList
  {-# INLINE typeGType #-}

noPerformanceObserverEntryList :: Maybe PerformanceObserverEntryList
noPerformanceObserverEntryList = Nothing
{-# INLINE noPerformanceObserverEntryList #-}

gTypePerformanceObserverEntryList :: JSM GType
gTypePerformanceObserverEntryList = GType . Object <$> jsg "PerformanceObserverEntryList"

-- | Functions for this inteface are in "JSDOM.PerformanceObserverInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceObserverInit Mozilla PerformanceObserverInit documentation>
newtype PerformanceObserverInit = PerformanceObserverInit { unPerformanceObserverInit :: JSVal }

instance PToJSVal PerformanceObserverInit where
  pToJSVal = unPerformanceObserverInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceObserverInit where
  pFromJSVal = PerformanceObserverInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceObserverInit where
  toJSVal = return . unPerformanceObserverInit
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceObserverInit where
  fromJSVal v = fmap PerformanceObserverInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceObserverInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceObserverInit where
  makeObject = makeObject . unPerformanceObserverInit

instance IsGObject PerformanceObserverInit where
  typeGType _ = gTypePerformanceObserverInit
  {-# INLINE typeGType #-}

noPerformanceObserverInit :: Maybe PerformanceObserverInit
noPerformanceObserverInit = Nothing
{-# INLINE noPerformanceObserverInit #-}

gTypePerformanceObserverInit :: JSM GType
gTypePerformanceObserverInit = GType . Object <$> jsg "PerformanceObserverInit"

-- | Functions for this inteface are in "JSDOM.PerformanceResourceTiming".
-- Base interface functions are in:
--
--     * "JSDOM.PerformanceEntry"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceResourceTiming Mozilla PerformanceResourceTiming documentation>
newtype PerformanceResourceTiming = PerformanceResourceTiming { unPerformanceResourceTiming :: JSVal }

instance PToJSVal PerformanceResourceTiming where
  pToJSVal = unPerformanceResourceTiming
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceResourceTiming where
  pFromJSVal = PerformanceResourceTiming
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceResourceTiming where
  toJSVal = return . unPerformanceResourceTiming
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceResourceTiming where
  fromJSVal v = fmap PerformanceResourceTiming <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceResourceTiming
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceResourceTiming where
  makeObject = makeObject . unPerformanceResourceTiming

instance IsPerformanceEntry PerformanceResourceTiming
instance IsGObject PerformanceResourceTiming where
  typeGType _ = gTypePerformanceResourceTiming
  {-# INLINE typeGType #-}

noPerformanceResourceTiming :: Maybe PerformanceResourceTiming
noPerformanceResourceTiming = Nothing
{-# INLINE noPerformanceResourceTiming #-}

gTypePerformanceResourceTiming :: JSM GType
gTypePerformanceResourceTiming = GType . Object <$> jsg "PerformanceResourceTiming"

-- | Functions for this inteface are in "JSDOM.PerformanceTiming".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming Mozilla PerformanceTiming documentation>
newtype PerformanceTiming = PerformanceTiming { unPerformanceTiming :: JSVal }

instance PToJSVal PerformanceTiming where
  pToJSVal = unPerformanceTiming
  {-# INLINE pToJSVal #-}

instance PFromJSVal PerformanceTiming where
  pFromJSVal = PerformanceTiming
  {-# INLINE pFromJSVal #-}

instance ToJSVal PerformanceTiming where
  toJSVal = return . unPerformanceTiming
  {-# INLINE toJSVal #-}

instance FromJSVal PerformanceTiming where
  fromJSVal v = fmap PerformanceTiming <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PerformanceTiming
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PerformanceTiming where
  makeObject = makeObject . unPerformanceTiming

instance IsGObject PerformanceTiming where
  typeGType _ = gTypePerformanceTiming
  {-# INLINE typeGType #-}

noPerformanceTiming :: Maybe PerformanceTiming
noPerformanceTiming = Nothing
{-# INLINE noPerformanceTiming #-}

gTypePerformanceTiming :: JSM GType
gTypePerformanceTiming = GType . Object <$> jsg "PerformanceTiming"

-- | Functions for this inteface are in "JSDOM.PeriodicWave".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PeriodicWave Mozilla PeriodicWave documentation>
newtype PeriodicWave = PeriodicWave { unPeriodicWave :: JSVal }

instance PToJSVal PeriodicWave where
  pToJSVal = unPeriodicWave
  {-# INLINE pToJSVal #-}

instance PFromJSVal PeriodicWave where
  pFromJSVal = PeriodicWave
  {-# INLINE pFromJSVal #-}

instance ToJSVal PeriodicWave where
  toJSVal = return . unPeriodicWave
  {-# INLINE toJSVal #-}

instance FromJSVal PeriodicWave where
  fromJSVal v = fmap PeriodicWave <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PeriodicWave
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PeriodicWave where
  makeObject = makeObject . unPeriodicWave

instance IsGObject PeriodicWave where
  typeGType _ = gTypePeriodicWave
  {-# INLINE typeGType #-}

noPeriodicWave :: Maybe PeriodicWave
noPeriodicWave = Nothing
{-# INLINE noPeriodicWave #-}

gTypePeriodicWave :: JSM GType
gTypePeriodicWave = GType . Object <$> jsg "PeriodicWave"

-- | Functions for this inteface are in "JSDOM.Plugin".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Plugin Mozilla Plugin documentation>
newtype Plugin = Plugin { unPlugin :: JSVal }

instance PToJSVal Plugin where
  pToJSVal = unPlugin
  {-# INLINE pToJSVal #-}

instance PFromJSVal Plugin where
  pFromJSVal = Plugin
  {-# INLINE pFromJSVal #-}

instance ToJSVal Plugin where
  toJSVal = return . unPlugin
  {-# INLINE toJSVal #-}

instance FromJSVal Plugin where
  fromJSVal v = fmap Plugin <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Plugin
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Plugin where
  makeObject = makeObject . unPlugin

instance IsGObject Plugin where
  typeGType _ = gTypePlugin
  {-# INLINE typeGType #-}

noPlugin :: Maybe Plugin
noPlugin = Nothing
{-# INLINE noPlugin #-}

gTypePlugin :: JSM GType
gTypePlugin = GType . Object <$> jsg "Plugin"

-- | Functions for this inteface are in "JSDOM.PluginArray".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PluginArray Mozilla PluginArray documentation>
newtype PluginArray = PluginArray { unPluginArray :: JSVal }

instance PToJSVal PluginArray where
  pToJSVal = unPluginArray
  {-# INLINE pToJSVal #-}

instance PFromJSVal PluginArray where
  pFromJSVal = PluginArray
  {-# INLINE pFromJSVal #-}

instance ToJSVal PluginArray where
  toJSVal = return . unPluginArray
  {-# INLINE toJSVal #-}

instance FromJSVal PluginArray where
  fromJSVal v = fmap PluginArray <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PluginArray
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PluginArray where
  makeObject = makeObject . unPluginArray

instance IsGObject PluginArray where
  typeGType _ = gTypePluginArray
  {-# INLINE typeGType #-}

noPluginArray :: Maybe PluginArray
noPluginArray = Nothing
{-# INLINE noPluginArray #-}

gTypePluginArray :: JSM GType
gTypePluginArray = GType . Object <$> jsg "PluginArray"

-- | Functions for this inteface are in "JSDOM.PopStateEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PopStateEvent Mozilla PopStateEvent documentation>
newtype PopStateEvent = PopStateEvent { unPopStateEvent :: JSVal }

instance PToJSVal PopStateEvent where
  pToJSVal = unPopStateEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal PopStateEvent where
  pFromJSVal = PopStateEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal PopStateEvent where
  toJSVal = return . unPopStateEvent
  {-# INLINE toJSVal #-}

instance FromJSVal PopStateEvent where
  fromJSVal v = fmap PopStateEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PopStateEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PopStateEvent where
  makeObject = makeObject . unPopStateEvent

instance IsEvent PopStateEvent
instance IsGObject PopStateEvent where
  typeGType _ = gTypePopStateEvent
  {-# INLINE typeGType #-}

noPopStateEvent :: Maybe PopStateEvent
noPopStateEvent = Nothing
{-# INLINE noPopStateEvent #-}

gTypePopStateEvent :: JSM GType
gTypePopStateEvent = GType . Object <$> jsg "PopStateEvent"

-- | Functions for this inteface are in "JSDOM.PopStateEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PopStateEventInit Mozilla PopStateEventInit documentation>
newtype PopStateEventInit = PopStateEventInit { unPopStateEventInit :: JSVal }

instance PToJSVal PopStateEventInit where
  pToJSVal = unPopStateEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal PopStateEventInit where
  pFromJSVal = PopStateEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal PopStateEventInit where
  toJSVal = return . unPopStateEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal PopStateEventInit where
  fromJSVal v = fmap PopStateEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PopStateEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PopStateEventInit where
  makeObject = makeObject . unPopStateEventInit

instance IsEventInit PopStateEventInit
instance IsGObject PopStateEventInit where
  typeGType _ = gTypePopStateEventInit
  {-# INLINE typeGType #-}

noPopStateEventInit :: Maybe PopStateEventInit
noPopStateEventInit = Nothing
{-# INLINE noPopStateEventInit #-}

gTypePopStateEventInit :: JSM GType
gTypePopStateEventInit = GType . Object <$> jsg "PopStateEventInit"

-- | Functions for this inteface are in "JSDOM.PositionError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PositionError Mozilla PositionError documentation>
newtype PositionError = PositionError { unPositionError :: JSVal }

instance PToJSVal PositionError where
  pToJSVal = unPositionError
  {-# INLINE pToJSVal #-}

instance PFromJSVal PositionError where
  pFromJSVal = PositionError
  {-# INLINE pFromJSVal #-}

instance ToJSVal PositionError where
  toJSVal = return . unPositionError
  {-# INLINE toJSVal #-}

instance FromJSVal PositionError where
  fromJSVal v = fmap PositionError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PositionError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PositionError where
  makeObject = makeObject . unPositionError

instance IsGObject PositionError where
  typeGType _ = gTypePositionError
  {-# INLINE typeGType #-}

noPositionError :: Maybe PositionError
noPositionError = Nothing
{-# INLINE noPositionError #-}

gTypePositionError :: JSM GType
gTypePositionError = GType . Object <$> jsg "PositionError"

-- | Functions for this inteface are in "JSDOM.PositionOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PositionOptions Mozilla PositionOptions documentation>
newtype PositionOptions = PositionOptions { unPositionOptions :: JSVal }

instance PToJSVal PositionOptions where
  pToJSVal = unPositionOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal PositionOptions where
  pFromJSVal = PositionOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal PositionOptions where
  toJSVal = return . unPositionOptions
  {-# INLINE toJSVal #-}

instance FromJSVal PositionOptions where
  fromJSVal v = fmap PositionOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PositionOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PositionOptions where
  makeObject = makeObject . unPositionOptions

instance IsGObject PositionOptions where
  typeGType _ = gTypePositionOptions
  {-# INLINE typeGType #-}

noPositionOptions :: Maybe PositionOptions
noPositionOptions = Nothing
{-# INLINE noPositionOptions #-}

gTypePositionOptions :: JSM GType
gTypePositionOptions = GType . Object <$> jsg "PositionOptions"

-- | Functions for this inteface are in "JSDOM.ProgressEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ProgressEvent Mozilla ProgressEvent documentation>
newtype ProgressEvent = ProgressEvent { unProgressEvent :: JSVal }

instance PToJSVal ProgressEvent where
  pToJSVal = unProgressEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal ProgressEvent where
  pFromJSVal = ProgressEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal ProgressEvent where
  toJSVal = return . unProgressEvent
  {-# INLINE toJSVal #-}

instance FromJSVal ProgressEvent where
  fromJSVal v = fmap ProgressEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ProgressEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ProgressEvent where
  makeObject = makeObject . unProgressEvent

class (IsEvent o, IsGObject o) => IsProgressEvent o
toProgressEvent :: IsProgressEvent o => o -> ProgressEvent
toProgressEvent = ProgressEvent . coerce

instance IsProgressEvent ProgressEvent
instance IsEvent ProgressEvent
instance IsGObject ProgressEvent where
  typeGType _ = gTypeProgressEvent
  {-# INLINE typeGType #-}

noProgressEvent :: Maybe ProgressEvent
noProgressEvent = Nothing
{-# INLINE noProgressEvent #-}

gTypeProgressEvent :: JSM GType
gTypeProgressEvent = GType . Object <$> jsg "ProgressEvent"

-- | Functions for this inteface are in "JSDOM.ProgressEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ProgressEventInit Mozilla ProgressEventInit documentation>
newtype ProgressEventInit = ProgressEventInit { unProgressEventInit :: JSVal }

instance PToJSVal ProgressEventInit where
  pToJSVal = unProgressEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal ProgressEventInit where
  pFromJSVal = ProgressEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal ProgressEventInit where
  toJSVal = return . unProgressEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal ProgressEventInit where
  fromJSVal v = fmap ProgressEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ProgressEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ProgressEventInit where
  makeObject = makeObject . unProgressEventInit

instance IsEventInit ProgressEventInit
instance IsGObject ProgressEventInit where
  typeGType _ = gTypeProgressEventInit
  {-# INLINE typeGType #-}

noProgressEventInit :: Maybe ProgressEventInit
noProgressEventInit = Nothing
{-# INLINE noProgressEventInit #-}

gTypeProgressEventInit :: JSM GType
gTypeProgressEventInit = GType . Object <$> jsg "ProgressEventInit"

-- | Functions for this inteface are in "JSDOM.PromiseRejectionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PromiseRejectionEvent Mozilla PromiseRejectionEvent documentation>
newtype PromiseRejectionEvent = PromiseRejectionEvent { unPromiseRejectionEvent :: JSVal }

instance PToJSVal PromiseRejectionEvent where
  pToJSVal = unPromiseRejectionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal PromiseRejectionEvent where
  pFromJSVal = PromiseRejectionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal PromiseRejectionEvent where
  toJSVal = return . unPromiseRejectionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal PromiseRejectionEvent where
  fromJSVal v = fmap PromiseRejectionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PromiseRejectionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PromiseRejectionEvent where
  makeObject = makeObject . unPromiseRejectionEvent

instance IsEvent PromiseRejectionEvent
instance IsGObject PromiseRejectionEvent where
  typeGType _ = gTypePromiseRejectionEvent
  {-# INLINE typeGType #-}

noPromiseRejectionEvent :: Maybe PromiseRejectionEvent
noPromiseRejectionEvent = Nothing
{-# INLINE noPromiseRejectionEvent #-}

gTypePromiseRejectionEvent :: JSM GType
gTypePromiseRejectionEvent = GType . Object <$> jsg "PromiseRejectionEvent"

-- | Functions for this inteface are in "JSDOM.PromiseRejectionEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/PromiseRejectionEventInit Mozilla PromiseRejectionEventInit documentation>
newtype PromiseRejectionEventInit = PromiseRejectionEventInit { unPromiseRejectionEventInit :: JSVal }

instance PToJSVal PromiseRejectionEventInit where
  pToJSVal = unPromiseRejectionEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal PromiseRejectionEventInit where
  pFromJSVal = PromiseRejectionEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal PromiseRejectionEventInit where
  toJSVal = return . unPromiseRejectionEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal PromiseRejectionEventInit where
  fromJSVal v = fmap PromiseRejectionEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . PromiseRejectionEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject PromiseRejectionEventInit where
  makeObject = makeObject . unPromiseRejectionEventInit

instance IsEventInit PromiseRejectionEventInit
instance IsGObject PromiseRejectionEventInit where
  typeGType _ = gTypePromiseRejectionEventInit
  {-# INLINE typeGType #-}

noPromiseRejectionEventInit :: Maybe PromiseRejectionEventInit
noPromiseRejectionEventInit = Nothing
{-# INLINE noPromiseRejectionEventInit #-}

gTypePromiseRejectionEventInit :: JSM GType
gTypePromiseRejectionEventInit = GType . Object <$> jsg "PromiseRejectionEventInit"

-- | Functions for this inteface are in "JSDOM.QuickTimePluginReplacement".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/QuickTimePluginReplacement Mozilla QuickTimePluginReplacement documentation>
newtype QuickTimePluginReplacement = QuickTimePluginReplacement { unQuickTimePluginReplacement :: JSVal }

instance PToJSVal QuickTimePluginReplacement where
  pToJSVal = unQuickTimePluginReplacement
  {-# INLINE pToJSVal #-}

instance PFromJSVal QuickTimePluginReplacement where
  pFromJSVal = QuickTimePluginReplacement
  {-# INLINE pFromJSVal #-}

instance ToJSVal QuickTimePluginReplacement where
  toJSVal = return . unQuickTimePluginReplacement
  {-# INLINE toJSVal #-}

instance FromJSVal QuickTimePluginReplacement where
  fromJSVal v = fmap QuickTimePluginReplacement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . QuickTimePluginReplacement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject QuickTimePluginReplacement where
  makeObject = makeObject . unQuickTimePluginReplacement

instance IsGObject QuickTimePluginReplacement where
  typeGType _ = gTypeQuickTimePluginReplacement
  {-# INLINE typeGType #-}

noQuickTimePluginReplacement :: Maybe QuickTimePluginReplacement
noQuickTimePluginReplacement = Nothing
{-# INLINE noQuickTimePluginReplacement #-}

gTypeQuickTimePluginReplacement :: JSM GType
gTypeQuickTimePluginReplacement = GType . Object <$> jsg "QuickTimePluginReplacement"

-- | Functions for this inteface are in "JSDOM.RGBColor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RGBColor Mozilla RGBColor documentation>
newtype RGBColor = RGBColor { unRGBColor :: JSVal }

instance PToJSVal RGBColor where
  pToJSVal = unRGBColor
  {-# INLINE pToJSVal #-}

instance PFromJSVal RGBColor where
  pFromJSVal = RGBColor
  {-# INLINE pFromJSVal #-}

instance ToJSVal RGBColor where
  toJSVal = return . unRGBColor
  {-# INLINE toJSVal #-}

instance FromJSVal RGBColor where
  fromJSVal v = fmap RGBColor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RGBColor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RGBColor where
  makeObject = makeObject . unRGBColor

instance IsGObject RGBColor where
  typeGType _ = gTypeRGBColor
  {-# INLINE typeGType #-}

noRGBColor :: Maybe RGBColor
noRGBColor = Nothing
{-# INLINE noRGBColor #-}

gTypeRGBColor :: JSM GType
gTypeRGBColor = GType . Object <$> jsg "RGBColor"

-- | Functions for this inteface are in "JSDOM.RTCAnswerOptions".
-- Base interface functions are in:
--
--     * "JSDOM.RTCOfferAnswerOptions"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCAnswerOptions Mozilla RTCAnswerOptions documentation>
newtype RTCAnswerOptions = RTCAnswerOptions { unRTCAnswerOptions :: JSVal }

instance PToJSVal RTCAnswerOptions where
  pToJSVal = unRTCAnswerOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCAnswerOptions where
  pFromJSVal = RTCAnswerOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCAnswerOptions where
  toJSVal = return . unRTCAnswerOptions
  {-# INLINE toJSVal #-}

instance FromJSVal RTCAnswerOptions where
  fromJSVal v = fmap RTCAnswerOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCAnswerOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCAnswerOptions where
  makeObject = makeObject . unRTCAnswerOptions

instance IsRTCOfferAnswerOptions RTCAnswerOptions
instance IsGObject RTCAnswerOptions where
  typeGType _ = gTypeRTCAnswerOptions
  {-# INLINE typeGType #-}

noRTCAnswerOptions :: Maybe RTCAnswerOptions
noRTCAnswerOptions = Nothing
{-# INLINE noRTCAnswerOptions #-}

gTypeRTCAnswerOptions :: JSM GType
gTypeRTCAnswerOptions = GType . Object <$> jsg "RTCAnswerOptions"

-- | Functions for this inteface are in "JSDOM.RTCConfiguration".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCConfiguration Mozilla RTCConfiguration documentation>
newtype RTCConfiguration = RTCConfiguration { unRTCConfiguration :: JSVal }

instance PToJSVal RTCConfiguration where
  pToJSVal = unRTCConfiguration
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCConfiguration where
  pFromJSVal = RTCConfiguration
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCConfiguration where
  toJSVal = return . unRTCConfiguration
  {-# INLINE toJSVal #-}

instance FromJSVal RTCConfiguration where
  fromJSVal v = fmap RTCConfiguration <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCConfiguration
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCConfiguration where
  makeObject = makeObject . unRTCConfiguration

instance IsGObject RTCConfiguration where
  typeGType _ = gTypeRTCConfiguration
  {-# INLINE typeGType #-}

noRTCConfiguration :: Maybe RTCConfiguration
noRTCConfiguration = Nothing
{-# INLINE noRTCConfiguration #-}

gTypeRTCConfiguration :: JSM GType
gTypeRTCConfiguration = GType . Object <$> jsg "RTCConfiguration"

-- | Functions for this inteface are in "JSDOM.RTCDTMFSender".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDTMFSender Mozilla RTCDTMFSender documentation>
newtype RTCDTMFSender = RTCDTMFSender { unRTCDTMFSender :: JSVal }

instance PToJSVal RTCDTMFSender where
  pToJSVal = unRTCDTMFSender
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDTMFSender where
  pFromJSVal = RTCDTMFSender
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDTMFSender where
  toJSVal = return . unRTCDTMFSender
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDTMFSender where
  fromJSVal v = fmap RTCDTMFSender <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDTMFSender
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDTMFSender where
  makeObject = makeObject . unRTCDTMFSender

instance IsEventTarget RTCDTMFSender
instance IsGObject RTCDTMFSender where
  typeGType _ = gTypeRTCDTMFSender
  {-# INLINE typeGType #-}

noRTCDTMFSender :: Maybe RTCDTMFSender
noRTCDTMFSender = Nothing
{-# INLINE noRTCDTMFSender #-}

gTypeRTCDTMFSender :: JSM GType
gTypeRTCDTMFSender = GType . Object <$> jsg "RTCDTMFSender"

-- | Functions for this inteface are in "JSDOM.RTCDTMFToneChangeEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDTMFToneChangeEvent Mozilla RTCDTMFToneChangeEvent documentation>
newtype RTCDTMFToneChangeEvent = RTCDTMFToneChangeEvent { unRTCDTMFToneChangeEvent :: JSVal }

instance PToJSVal RTCDTMFToneChangeEvent where
  pToJSVal = unRTCDTMFToneChangeEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDTMFToneChangeEvent where
  pFromJSVal = RTCDTMFToneChangeEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDTMFToneChangeEvent where
  toJSVal = return . unRTCDTMFToneChangeEvent
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDTMFToneChangeEvent where
  fromJSVal v = fmap RTCDTMFToneChangeEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDTMFToneChangeEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDTMFToneChangeEvent where
  makeObject = makeObject . unRTCDTMFToneChangeEvent

instance IsEvent RTCDTMFToneChangeEvent
instance IsGObject RTCDTMFToneChangeEvent where
  typeGType _ = gTypeRTCDTMFToneChangeEvent
  {-# INLINE typeGType #-}

noRTCDTMFToneChangeEvent :: Maybe RTCDTMFToneChangeEvent
noRTCDTMFToneChangeEvent = Nothing
{-# INLINE noRTCDTMFToneChangeEvent #-}

gTypeRTCDTMFToneChangeEvent :: JSM GType
gTypeRTCDTMFToneChangeEvent = GType . Object <$> jsg "RTCDTMFToneChangeEvent"

-- | Functions for this inteface are in "JSDOM.RTCDTMFToneChangeEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDTMFToneChangeEventInit Mozilla RTCDTMFToneChangeEventInit documentation>
newtype RTCDTMFToneChangeEventInit = RTCDTMFToneChangeEventInit { unRTCDTMFToneChangeEventInit :: JSVal }

instance PToJSVal RTCDTMFToneChangeEventInit where
  pToJSVal = unRTCDTMFToneChangeEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDTMFToneChangeEventInit where
  pFromJSVal = RTCDTMFToneChangeEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDTMFToneChangeEventInit where
  toJSVal = return . unRTCDTMFToneChangeEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDTMFToneChangeEventInit where
  fromJSVal v = fmap RTCDTMFToneChangeEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDTMFToneChangeEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDTMFToneChangeEventInit where
  makeObject = makeObject . unRTCDTMFToneChangeEventInit

instance IsEventInit RTCDTMFToneChangeEventInit
instance IsGObject RTCDTMFToneChangeEventInit where
  typeGType _ = gTypeRTCDTMFToneChangeEventInit
  {-# INLINE typeGType #-}

noRTCDTMFToneChangeEventInit :: Maybe RTCDTMFToneChangeEventInit
noRTCDTMFToneChangeEventInit = Nothing
{-# INLINE noRTCDTMFToneChangeEventInit #-}

gTypeRTCDTMFToneChangeEventInit :: JSM GType
gTypeRTCDTMFToneChangeEventInit = GType . Object <$> jsg "RTCDTMFToneChangeEventInit"

-- | Functions for this inteface are in "JSDOM.RTCDataChannel".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannel Mozilla RTCDataChannel documentation>
newtype RTCDataChannel = RTCDataChannel { unRTCDataChannel :: JSVal }

instance PToJSVal RTCDataChannel where
  pToJSVal = unRTCDataChannel
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDataChannel where
  pFromJSVal = RTCDataChannel
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDataChannel where
  toJSVal = return . unRTCDataChannel
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDataChannel where
  fromJSVal v = fmap RTCDataChannel <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDataChannel
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDataChannel where
  makeObject = makeObject . unRTCDataChannel

instance IsEventTarget RTCDataChannel
instance IsGObject RTCDataChannel where
  typeGType _ = gTypeRTCDataChannel
  {-# INLINE typeGType #-}

noRTCDataChannel :: Maybe RTCDataChannel
noRTCDataChannel = Nothing
{-# INLINE noRTCDataChannel #-}

gTypeRTCDataChannel :: JSM GType
gTypeRTCDataChannel = GType . Object <$> jsg "RTCDataChannel"

-- | Functions for this inteface are in "JSDOM.RTCDataChannelEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelEvent Mozilla RTCDataChannelEvent documentation>
newtype RTCDataChannelEvent = RTCDataChannelEvent { unRTCDataChannelEvent :: JSVal }

instance PToJSVal RTCDataChannelEvent where
  pToJSVal = unRTCDataChannelEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDataChannelEvent where
  pFromJSVal = RTCDataChannelEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDataChannelEvent where
  toJSVal = return . unRTCDataChannelEvent
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDataChannelEvent where
  fromJSVal v = fmap RTCDataChannelEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDataChannelEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDataChannelEvent where
  makeObject = makeObject . unRTCDataChannelEvent

instance IsEvent RTCDataChannelEvent
instance IsGObject RTCDataChannelEvent where
  typeGType _ = gTypeRTCDataChannelEvent
  {-# INLINE typeGType #-}

noRTCDataChannelEvent :: Maybe RTCDataChannelEvent
noRTCDataChannelEvent = Nothing
{-# INLINE noRTCDataChannelEvent #-}

gTypeRTCDataChannelEvent :: JSM GType
gTypeRTCDataChannelEvent = GType . Object <$> jsg "RTCDataChannelEvent"

-- | Functions for this inteface are in "JSDOM.RTCDataChannelEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelEventInit Mozilla RTCDataChannelEventInit documentation>
newtype RTCDataChannelEventInit = RTCDataChannelEventInit { unRTCDataChannelEventInit :: JSVal }

instance PToJSVal RTCDataChannelEventInit where
  pToJSVal = unRTCDataChannelEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDataChannelEventInit where
  pFromJSVal = RTCDataChannelEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDataChannelEventInit where
  toJSVal = return . unRTCDataChannelEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDataChannelEventInit where
  fromJSVal v = fmap RTCDataChannelEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDataChannelEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDataChannelEventInit where
  makeObject = makeObject . unRTCDataChannelEventInit

instance IsEventInit RTCDataChannelEventInit
instance IsGObject RTCDataChannelEventInit where
  typeGType _ = gTypeRTCDataChannelEventInit
  {-# INLINE typeGType #-}

noRTCDataChannelEventInit :: Maybe RTCDataChannelEventInit
noRTCDataChannelEventInit = Nothing
{-# INLINE noRTCDataChannelEventInit #-}

gTypeRTCDataChannelEventInit :: JSM GType
gTypeRTCDataChannelEventInit = GType . Object <$> jsg "RTCDataChannelEventInit"

-- | Functions for this inteface are in "JSDOM.RTCDataChannelInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelInit Mozilla RTCDataChannelInit documentation>
newtype RTCDataChannelInit = RTCDataChannelInit { unRTCDataChannelInit :: JSVal }

instance PToJSVal RTCDataChannelInit where
  pToJSVal = unRTCDataChannelInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDataChannelInit where
  pFromJSVal = RTCDataChannelInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDataChannelInit where
  toJSVal = return . unRTCDataChannelInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDataChannelInit where
  fromJSVal v = fmap RTCDataChannelInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDataChannelInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDataChannelInit where
  makeObject = makeObject . unRTCDataChannelInit

instance IsGObject RTCDataChannelInit where
  typeGType _ = gTypeRTCDataChannelInit
  {-# INLINE typeGType #-}

noRTCDataChannelInit :: Maybe RTCDataChannelInit
noRTCDataChannelInit = Nothing
{-# INLINE noRTCDataChannelInit #-}

gTypeRTCDataChannelInit :: JSM GType
gTypeRTCDataChannelInit = GType . Object <$> jsg "RTCDataChannelInit"

-- | Functions for this inteface are in "JSDOM.RTCDataChannelStats".
-- Base interface functions are in:
--
--     * "JSDOM.RTCStats"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCDataChannelStats Mozilla RTCDataChannelStats documentation>
newtype RTCDataChannelStats = RTCDataChannelStats { unRTCDataChannelStats :: JSVal }

instance PToJSVal RTCDataChannelStats where
  pToJSVal = unRTCDataChannelStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCDataChannelStats where
  pFromJSVal = RTCDataChannelStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCDataChannelStats where
  toJSVal = return . unRTCDataChannelStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCDataChannelStats where
  fromJSVal v = fmap RTCDataChannelStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCDataChannelStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCDataChannelStats where
  makeObject = makeObject . unRTCDataChannelStats

instance IsRTCStats RTCDataChannelStats
instance IsGObject RTCDataChannelStats where
  typeGType _ = gTypeRTCDataChannelStats
  {-# INLINE typeGType #-}

noRTCDataChannelStats :: Maybe RTCDataChannelStats
noRTCDataChannelStats = Nothing
{-# INLINE noRTCDataChannelStats #-}

gTypeRTCDataChannelStats :: JSM GType
gTypeRTCDataChannelStats = GType . Object <$> jsg "RTCDataChannelStats"


-- | Functions for this inteface are in "JSDOM.RTCIceServer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceServer Mozilla RTCIceServer documentation>
newtype RTCIceServer = RTCIceServer { unRTCIceServer :: JSVal }

instance PToJSVal RTCIceServer where
  pToJSVal = unRTCIceServer
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceServer where
  pFromJSVal = RTCIceServer
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceServer where
  toJSVal = return . unRTCIceServer
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceServer where
  fromJSVal v = fmap RTCIceServer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceServer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceServer where
  makeObject = makeObject . unRTCIceServer

instance IsGObject RTCIceServer where
  typeGType _ = gTypeRTCIceServer
  {-# INLINE typeGType #-}

noRTCIceServer :: Maybe RTCIceServer
noRTCIceServer = Nothing
{-# INLINE noRTCIceServer #-}

gTypeRTCIceServer :: JSM GType
gTypeRTCIceServer = GType . Object <$> jsg "RTCIceServer"

-- | Functions for this inteface are in "JSDOM.RTCIceTransport".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCIceTransport Mozilla RTCIceTransport documentation>
newtype RTCIceTransport = RTCIceTransport { unRTCIceTransport :: JSVal }

instance PToJSVal RTCIceTransport where
  pToJSVal = unRTCIceTransport
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCIceTransport where
  pFromJSVal = RTCIceTransport
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCIceTransport where
  toJSVal = return . unRTCIceTransport
  {-# INLINE toJSVal #-}

instance FromJSVal RTCIceTransport where
  fromJSVal v = fmap RTCIceTransport <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCIceTransport
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCIceTransport where
  makeObject = makeObject . unRTCIceTransport

instance IsGObject RTCIceTransport where
  typeGType _ = gTypeRTCIceTransport
  {-# INLINE typeGType #-}

noRTCIceTransport :: Maybe RTCIceTransport
noRTCIceTransport = Nothing
{-# INLINE noRTCIceTransport #-}

gTypeRTCIceTransport :: JSM GType
gTypeRTCIceTransport = GType . Object <$> jsg "RTCIceTransport"

-- | Functions for this inteface are in "JSDOM.RTCInboundRTPStreamStats".
-- Base interface functions are in:
--
--     * "JSDOM.RTCRTPStreamStats"
--     * "JSDOM.RTCStats"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCInboundRTPStreamStats Mozilla RTCInboundRTPStreamStats documentation>
newtype RTCInboundRTPStreamStats = RTCInboundRTPStreamStats { unRTCInboundRTPStreamStats :: JSVal }

instance PToJSVal RTCInboundRTPStreamStats where
  pToJSVal = unRTCInboundRTPStreamStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCInboundRTPStreamStats where
  pFromJSVal = RTCInboundRTPStreamStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCInboundRTPStreamStats where
  toJSVal = return . unRTCInboundRTPStreamStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCInboundRTPStreamStats where
  fromJSVal v = fmap RTCInboundRTPStreamStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCInboundRTPStreamStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCInboundRTPStreamStats where
  makeObject = makeObject . unRTCInboundRTPStreamStats

instance IsRTCRTPStreamStats RTCInboundRTPStreamStats
instance IsRTCStats RTCInboundRTPStreamStats
instance IsGObject RTCInboundRTPStreamStats where
  typeGType _ = gTypeRTCInboundRTPStreamStats
  {-# INLINE typeGType #-}

noRTCInboundRTPStreamStats :: Maybe RTCInboundRTPStreamStats
noRTCInboundRTPStreamStats = Nothing
{-# INLINE noRTCInboundRTPStreamStats #-}

gTypeRTCInboundRTPStreamStats :: JSM GType
gTypeRTCInboundRTPStreamStats = GType . Object <$> jsg "RTCInboundRTPStreamStats"

-- | Functions for this inteface are in "JSDOM.RTCMediaStreamTrackStats".
-- Base interface functions are in:
--
--     * "JSDOM.RTCStats"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCMediaStreamTrackStats Mozilla RTCMediaStreamTrackStats documentation>
newtype RTCMediaStreamTrackStats = RTCMediaStreamTrackStats { unRTCMediaStreamTrackStats :: JSVal }

instance PToJSVal RTCMediaStreamTrackStats where
  pToJSVal = unRTCMediaStreamTrackStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCMediaStreamTrackStats where
  pFromJSVal = RTCMediaStreamTrackStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCMediaStreamTrackStats where
  toJSVal = return . unRTCMediaStreamTrackStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCMediaStreamTrackStats where
  fromJSVal v = fmap RTCMediaStreamTrackStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCMediaStreamTrackStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCMediaStreamTrackStats where
  makeObject = makeObject . unRTCMediaStreamTrackStats

instance IsRTCStats RTCMediaStreamTrackStats
instance IsGObject RTCMediaStreamTrackStats where
  typeGType _ = gTypeRTCMediaStreamTrackStats
  {-# INLINE typeGType #-}

noRTCMediaStreamTrackStats :: Maybe RTCMediaStreamTrackStats
noRTCMediaStreamTrackStats = Nothing
{-# INLINE noRTCMediaStreamTrackStats #-}

gTypeRTCMediaStreamTrackStats :: JSM GType
gTypeRTCMediaStreamTrackStats = GType . Object <$> jsg "RTCMediaStreamTrackStats"

-- | Functions for this inteface are in "JSDOM.RTCOfferAnswerOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCOfferAnswerOptions Mozilla RTCOfferAnswerOptions documentation>
newtype RTCOfferAnswerOptions = RTCOfferAnswerOptions { unRTCOfferAnswerOptions :: JSVal }

instance PToJSVal RTCOfferAnswerOptions where
  pToJSVal = unRTCOfferAnswerOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCOfferAnswerOptions where
  pFromJSVal = RTCOfferAnswerOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCOfferAnswerOptions where
  toJSVal = return . unRTCOfferAnswerOptions
  {-# INLINE toJSVal #-}

instance FromJSVal RTCOfferAnswerOptions where
  fromJSVal v = fmap RTCOfferAnswerOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCOfferAnswerOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCOfferAnswerOptions where
  makeObject = makeObject . unRTCOfferAnswerOptions

class (IsGObject o) => IsRTCOfferAnswerOptions o
toRTCOfferAnswerOptions :: IsRTCOfferAnswerOptions o => o -> RTCOfferAnswerOptions
toRTCOfferAnswerOptions = RTCOfferAnswerOptions . coerce

instance IsRTCOfferAnswerOptions RTCOfferAnswerOptions
instance IsGObject RTCOfferAnswerOptions where
  typeGType _ = gTypeRTCOfferAnswerOptions
  {-# INLINE typeGType #-}

noRTCOfferAnswerOptions :: Maybe RTCOfferAnswerOptions
noRTCOfferAnswerOptions = Nothing
{-# INLINE noRTCOfferAnswerOptions #-}

gTypeRTCOfferAnswerOptions :: JSM GType
gTypeRTCOfferAnswerOptions = GType . Object <$> jsg "RTCOfferAnswerOptions"

-- | Functions for this inteface are in "JSDOM.RTCOfferOptions".
-- Base interface functions are in:
--
--     * "JSDOM.RTCOfferAnswerOptions"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCOfferOptions Mozilla RTCOfferOptions documentation>
newtype RTCOfferOptions = RTCOfferOptions { unRTCOfferOptions :: JSVal }

instance PToJSVal RTCOfferOptions where
  pToJSVal = unRTCOfferOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCOfferOptions where
  pFromJSVal = RTCOfferOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCOfferOptions where
  toJSVal = return . unRTCOfferOptions
  {-# INLINE toJSVal #-}

instance FromJSVal RTCOfferOptions where
  fromJSVal v = fmap RTCOfferOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCOfferOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCOfferOptions where
  makeObject = makeObject . unRTCOfferOptions

instance IsRTCOfferAnswerOptions RTCOfferOptions
instance IsGObject RTCOfferOptions where
  typeGType _ = gTypeRTCOfferOptions
  {-# INLINE typeGType #-}

noRTCOfferOptions :: Maybe RTCOfferOptions
noRTCOfferOptions = Nothing
{-# INLINE noRTCOfferOptions #-}

gTypeRTCOfferOptions :: JSM GType
gTypeRTCOfferOptions = GType . Object <$> jsg "RTCOfferOptions"

-- | Functions for this inteface are in "JSDOM.RTCOutboundRTPStreamStats".
-- Base interface functions are in:
--
--     * "JSDOM.RTCRTPStreamStats"
--     * "JSDOM.RTCStats"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCOutboundRTPStreamStats Mozilla RTCOutboundRTPStreamStats documentation>
newtype RTCOutboundRTPStreamStats = RTCOutboundRTPStreamStats { unRTCOutboundRTPStreamStats :: JSVal }

instance PToJSVal RTCOutboundRTPStreamStats where
  pToJSVal = unRTCOutboundRTPStreamStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCOutboundRTPStreamStats where
  pFromJSVal = RTCOutboundRTPStreamStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCOutboundRTPStreamStats where
  toJSVal = return . unRTCOutboundRTPStreamStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCOutboundRTPStreamStats where
  fromJSVal v = fmap RTCOutboundRTPStreamStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCOutboundRTPStreamStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCOutboundRTPStreamStats where
  makeObject = makeObject . unRTCOutboundRTPStreamStats

instance IsRTCRTPStreamStats RTCOutboundRTPStreamStats
instance IsRTCStats RTCOutboundRTPStreamStats
instance IsGObject RTCOutboundRTPStreamStats where
  typeGType _ = gTypeRTCOutboundRTPStreamStats
  {-# INLINE typeGType #-}

noRTCOutboundRTPStreamStats :: Maybe RTCOutboundRTPStreamStats
noRTCOutboundRTPStreamStats = Nothing
{-# INLINE noRTCOutboundRTPStreamStats #-}

gTypeRTCOutboundRTPStreamStats :: JSM GType
gTypeRTCOutboundRTPStreamStats = GType . Object <$> jsg "RTCOutboundRTPStreamStats"

-- | Functions for this inteface are in "JSDOM.RTCPeerConnection".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/webkitRTCPeerConnection Mozilla webkitRTCPeerConnection documentation>
newtype RTCPeerConnection = RTCPeerConnection { unRTCPeerConnection :: JSVal }

instance PToJSVal RTCPeerConnection where
  pToJSVal = unRTCPeerConnection
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCPeerConnection where
  pFromJSVal = RTCPeerConnection
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCPeerConnection where
  toJSVal = return . unRTCPeerConnection
  {-# INLINE toJSVal #-}

instance FromJSVal RTCPeerConnection where
  fromJSVal v = fmap RTCPeerConnection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCPeerConnection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCPeerConnection where
  makeObject = makeObject . unRTCPeerConnection

instance IsEventTarget RTCPeerConnection
instance IsGObject RTCPeerConnection where
  typeGType _ = gTypeRTCPeerConnection
  {-# INLINE typeGType #-}

noRTCPeerConnection :: Maybe RTCPeerConnection
noRTCPeerConnection = Nothing
{-# INLINE noRTCPeerConnection #-}

gTypeRTCPeerConnection :: JSM GType
gTypeRTCPeerConnection = GType . Object <$> jsg "webkitRTCPeerConnection"

-- | Functions for this inteface are in "JSDOM.RTCPeerConnectionIceEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCPeerConnectionIceEvent Mozilla RTCPeerConnectionIceEvent documentation>
newtype RTCPeerConnectionIceEvent = RTCPeerConnectionIceEvent { unRTCPeerConnectionIceEvent :: JSVal }

instance PToJSVal RTCPeerConnectionIceEvent where
  pToJSVal = unRTCPeerConnectionIceEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCPeerConnectionIceEvent where
  pFromJSVal = RTCPeerConnectionIceEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCPeerConnectionIceEvent where
  toJSVal = return . unRTCPeerConnectionIceEvent
  {-# INLINE toJSVal #-}

instance FromJSVal RTCPeerConnectionIceEvent where
  fromJSVal v = fmap RTCPeerConnectionIceEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCPeerConnectionIceEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCPeerConnectionIceEvent where
  makeObject = makeObject . unRTCPeerConnectionIceEvent

instance IsEvent RTCPeerConnectionIceEvent
instance IsGObject RTCPeerConnectionIceEvent where
  typeGType _ = gTypeRTCPeerConnectionIceEvent
  {-# INLINE typeGType #-}

noRTCPeerConnectionIceEvent :: Maybe RTCPeerConnectionIceEvent
noRTCPeerConnectionIceEvent = Nothing
{-# INLINE noRTCPeerConnectionIceEvent #-}

gTypeRTCPeerConnectionIceEvent :: JSM GType
gTypeRTCPeerConnectionIceEvent = GType . Object <$> jsg "RTCPeerConnectionIceEvent"

-- | Functions for this inteface are in "JSDOM.RTCRTPStreamStats".
-- Base interface functions are in:
--
--     * "JSDOM.RTCStats"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRTPStreamStats Mozilla RTCRTPStreamStats documentation>
newtype RTCRTPStreamStats = RTCRTPStreamStats { unRTCRTPStreamStats :: JSVal }

instance PToJSVal RTCRTPStreamStats where
  pToJSVal = unRTCRTPStreamStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRTPStreamStats where
  pFromJSVal = RTCRTPStreamStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRTPStreamStats where
  toJSVal = return . unRTCRTPStreamStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRTPStreamStats where
  fromJSVal v = fmap RTCRTPStreamStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRTPStreamStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRTPStreamStats where
  makeObject = makeObject . unRTCRTPStreamStats

class (IsRTCStats o, IsGObject o) => IsRTCRTPStreamStats o
toRTCRTPStreamStats :: IsRTCRTPStreamStats o => o -> RTCRTPStreamStats
toRTCRTPStreamStats = RTCRTPStreamStats . coerce

instance IsRTCRTPStreamStats RTCRTPStreamStats
instance IsRTCStats RTCRTPStreamStats
instance IsGObject RTCRTPStreamStats where
  typeGType _ = gTypeRTCRTPStreamStats
  {-# INLINE typeGType #-}

noRTCRTPStreamStats :: Maybe RTCRTPStreamStats
noRTCRTPStreamStats = Nothing
{-# INLINE noRTCRTPStreamStats #-}

gTypeRTCRTPStreamStats :: JSM GType
gTypeRTCRTPStreamStats = GType . Object <$> jsg "RTCRTPStreamStats"

-- | Functions for this inteface are in "JSDOM.RTCRtpCodecParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpCodecParameters Mozilla RTCRtpCodecParameters documentation>
newtype RTCRtpCodecParameters = RTCRtpCodecParameters { unRTCRtpCodecParameters :: JSVal }

instance PToJSVal RTCRtpCodecParameters where
  pToJSVal = unRTCRtpCodecParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpCodecParameters where
  pFromJSVal = RTCRtpCodecParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpCodecParameters where
  toJSVal = return . unRTCRtpCodecParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpCodecParameters where
  fromJSVal v = fmap RTCRtpCodecParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpCodecParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpCodecParameters where
  makeObject = makeObject . unRTCRtpCodecParameters

instance IsGObject RTCRtpCodecParameters where
  typeGType _ = gTypeRTCRtpCodecParameters
  {-# INLINE typeGType #-}

noRTCRtpCodecParameters :: Maybe RTCRtpCodecParameters
noRTCRtpCodecParameters = Nothing
{-# INLINE noRTCRtpCodecParameters #-}

gTypeRTCRtpCodecParameters :: JSM GType
gTypeRTCRtpCodecParameters = GType . Object <$> jsg "RTCRtpCodecParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpEncodingParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpEncodingParameters Mozilla RTCRtpEncodingParameters documentation>
newtype RTCRtpEncodingParameters = RTCRtpEncodingParameters { unRTCRtpEncodingParameters :: JSVal }

instance PToJSVal RTCRtpEncodingParameters where
  pToJSVal = unRTCRtpEncodingParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpEncodingParameters where
  pFromJSVal = RTCRtpEncodingParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpEncodingParameters where
  toJSVal = return . unRTCRtpEncodingParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpEncodingParameters where
  fromJSVal v = fmap RTCRtpEncodingParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpEncodingParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpEncodingParameters where
  makeObject = makeObject . unRTCRtpEncodingParameters

instance IsGObject RTCRtpEncodingParameters where
  typeGType _ = gTypeRTCRtpEncodingParameters
  {-# INLINE typeGType #-}

noRTCRtpEncodingParameters :: Maybe RTCRtpEncodingParameters
noRTCRtpEncodingParameters = Nothing
{-# INLINE noRTCRtpEncodingParameters #-}

gTypeRTCRtpEncodingParameters :: JSM GType
gTypeRTCRtpEncodingParameters = GType . Object <$> jsg "RTCRtpEncodingParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpFecParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpFecParameters Mozilla RTCRtpFecParameters documentation>
newtype RTCRtpFecParameters = RTCRtpFecParameters { unRTCRtpFecParameters :: JSVal }

instance PToJSVal RTCRtpFecParameters where
  pToJSVal = unRTCRtpFecParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpFecParameters where
  pFromJSVal = RTCRtpFecParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpFecParameters where
  toJSVal = return . unRTCRtpFecParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpFecParameters where
  fromJSVal v = fmap RTCRtpFecParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpFecParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpFecParameters where
  makeObject = makeObject . unRTCRtpFecParameters

instance IsGObject RTCRtpFecParameters where
  typeGType _ = gTypeRTCRtpFecParameters
  {-# INLINE typeGType #-}

noRTCRtpFecParameters :: Maybe RTCRtpFecParameters
noRTCRtpFecParameters = Nothing
{-# INLINE noRTCRtpFecParameters #-}

gTypeRTCRtpFecParameters :: JSM GType
gTypeRTCRtpFecParameters = GType . Object <$> jsg "RTCRtpFecParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpHeaderExtensionParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpHeaderExtensionParameters Mozilla RTCRtpHeaderExtensionParameters documentation>
newtype RTCRtpHeaderExtensionParameters = RTCRtpHeaderExtensionParameters { unRTCRtpHeaderExtensionParameters :: JSVal }

instance PToJSVal RTCRtpHeaderExtensionParameters where
  pToJSVal = unRTCRtpHeaderExtensionParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpHeaderExtensionParameters where
  pFromJSVal = RTCRtpHeaderExtensionParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpHeaderExtensionParameters where
  toJSVal = return . unRTCRtpHeaderExtensionParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpHeaderExtensionParameters where
  fromJSVal v = fmap RTCRtpHeaderExtensionParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpHeaderExtensionParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpHeaderExtensionParameters where
  makeObject = makeObject . unRTCRtpHeaderExtensionParameters

instance IsGObject RTCRtpHeaderExtensionParameters where
  typeGType _ = gTypeRTCRtpHeaderExtensionParameters
  {-# INLINE typeGType #-}

noRTCRtpHeaderExtensionParameters :: Maybe RTCRtpHeaderExtensionParameters
noRTCRtpHeaderExtensionParameters = Nothing
{-# INLINE noRTCRtpHeaderExtensionParameters #-}

gTypeRTCRtpHeaderExtensionParameters :: JSM GType
gTypeRTCRtpHeaderExtensionParameters = GType . Object <$> jsg "RTCRtpHeaderExtensionParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpParameters Mozilla RTCRtpParameters documentation>
newtype RTCRtpParameters = RTCRtpParameters { unRTCRtpParameters :: JSVal }

instance PToJSVal RTCRtpParameters where
  pToJSVal = unRTCRtpParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpParameters where
  pFromJSVal = RTCRtpParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpParameters where
  toJSVal = return . unRTCRtpParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpParameters where
  fromJSVal v = fmap RTCRtpParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpParameters where
  makeObject = makeObject . unRTCRtpParameters

instance IsGObject RTCRtpParameters where
  typeGType _ = gTypeRTCRtpParameters
  {-# INLINE typeGType #-}

noRTCRtpParameters :: Maybe RTCRtpParameters
noRTCRtpParameters = Nothing
{-# INLINE noRTCRtpParameters #-}

gTypeRTCRtpParameters :: JSM GType
gTypeRTCRtpParameters = GType . Object <$> jsg "RTCRtpParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpReceiver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpReceiver Mozilla RTCRtpReceiver documentation>
newtype RTCRtpReceiver = RTCRtpReceiver { unRTCRtpReceiver :: JSVal }

instance PToJSVal RTCRtpReceiver where
  pToJSVal = unRTCRtpReceiver
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpReceiver where
  pFromJSVal = RTCRtpReceiver
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpReceiver where
  toJSVal = return . unRTCRtpReceiver
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpReceiver where
  fromJSVal v = fmap RTCRtpReceiver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpReceiver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpReceiver where
  makeObject = makeObject . unRTCRtpReceiver

instance IsGObject RTCRtpReceiver where
  typeGType _ = gTypeRTCRtpReceiver
  {-# INLINE typeGType #-}

noRTCRtpReceiver :: Maybe RTCRtpReceiver
noRTCRtpReceiver = Nothing
{-# INLINE noRTCRtpReceiver #-}

gTypeRTCRtpReceiver :: JSM GType
gTypeRTCRtpReceiver = GType . Object <$> jsg "RTCRtpReceiver"

-- | Functions for this inteface are in "JSDOM.RTCRtpRtxParameters".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpRtxParameters Mozilla RTCRtpRtxParameters documentation>
newtype RTCRtpRtxParameters = RTCRtpRtxParameters { unRTCRtpRtxParameters :: JSVal }

instance PToJSVal RTCRtpRtxParameters where
  pToJSVal = unRTCRtpRtxParameters
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpRtxParameters where
  pFromJSVal = RTCRtpRtxParameters
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpRtxParameters where
  toJSVal = return . unRTCRtpRtxParameters
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpRtxParameters where
  fromJSVal v = fmap RTCRtpRtxParameters <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpRtxParameters
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpRtxParameters where
  makeObject = makeObject . unRTCRtpRtxParameters

instance IsGObject RTCRtpRtxParameters where
  typeGType _ = gTypeRTCRtpRtxParameters
  {-# INLINE typeGType #-}

noRTCRtpRtxParameters :: Maybe RTCRtpRtxParameters
noRTCRtpRtxParameters = Nothing
{-# INLINE noRTCRtpRtxParameters #-}

gTypeRTCRtpRtxParameters :: JSM GType
gTypeRTCRtpRtxParameters = GType . Object <$> jsg "RTCRtpRtxParameters"

-- | Functions for this inteface are in "JSDOM.RTCRtpSender".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpSender Mozilla RTCRtpSender documentation>
newtype RTCRtpSender = RTCRtpSender { unRTCRtpSender :: JSVal }

instance PToJSVal RTCRtpSender where
  pToJSVal = unRTCRtpSender
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpSender where
  pFromJSVal = RTCRtpSender
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpSender where
  toJSVal = return . unRTCRtpSender
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpSender where
  fromJSVal v = fmap RTCRtpSender <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpSender
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpSender where
  makeObject = makeObject . unRTCRtpSender

instance IsGObject RTCRtpSender where
  typeGType _ = gTypeRTCRtpSender
  {-# INLINE typeGType #-}

noRTCRtpSender :: Maybe RTCRtpSender
noRTCRtpSender = Nothing
{-# INLINE noRTCRtpSender #-}

gTypeRTCRtpSender :: JSM GType
gTypeRTCRtpSender = GType . Object <$> jsg "RTCRtpSender"

-- | Functions for this inteface are in "JSDOM.RTCRtpTransceiver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpTransceiver Mozilla RTCRtpTransceiver documentation>
newtype RTCRtpTransceiver = RTCRtpTransceiver { unRTCRtpTransceiver :: JSVal }

instance PToJSVal RTCRtpTransceiver where
  pToJSVal = unRTCRtpTransceiver
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpTransceiver where
  pFromJSVal = RTCRtpTransceiver
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpTransceiver where
  toJSVal = return . unRTCRtpTransceiver
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpTransceiver where
  fromJSVal v = fmap RTCRtpTransceiver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpTransceiver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpTransceiver where
  makeObject = makeObject . unRTCRtpTransceiver

instance IsGObject RTCRtpTransceiver where
  typeGType _ = gTypeRTCRtpTransceiver
  {-# INLINE typeGType #-}

noRTCRtpTransceiver :: Maybe RTCRtpTransceiver
noRTCRtpTransceiver = Nothing
{-# INLINE noRTCRtpTransceiver #-}

gTypeRTCRtpTransceiver :: JSM GType
gTypeRTCRtpTransceiver = GType . Object <$> jsg "RTCRtpTransceiver"

-- | Functions for this inteface are in "JSDOM.RTCRtpTransceiverInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCRtpTransceiverInit Mozilla RTCRtpTransceiverInit documentation>
newtype RTCRtpTransceiverInit = RTCRtpTransceiverInit { unRTCRtpTransceiverInit :: JSVal }

instance PToJSVal RTCRtpTransceiverInit where
  pToJSVal = unRTCRtpTransceiverInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCRtpTransceiverInit where
  pFromJSVal = RTCRtpTransceiverInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCRtpTransceiverInit where
  toJSVal = return . unRTCRtpTransceiverInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCRtpTransceiverInit where
  fromJSVal v = fmap RTCRtpTransceiverInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCRtpTransceiverInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCRtpTransceiverInit where
  makeObject = makeObject . unRTCRtpTransceiverInit

instance IsGObject RTCRtpTransceiverInit where
  typeGType _ = gTypeRTCRtpTransceiverInit
  {-# INLINE typeGType #-}

noRTCRtpTransceiverInit :: Maybe RTCRtpTransceiverInit
noRTCRtpTransceiverInit = Nothing
{-# INLINE noRTCRtpTransceiverInit #-}

gTypeRTCRtpTransceiverInit :: JSM GType
gTypeRTCRtpTransceiverInit = GType . Object <$> jsg "RTCRtpTransceiverInit"

-- | Functions for this inteface are in "JSDOM.RTCSessionDescription".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCSessionDescription Mozilla RTCSessionDescription documentation>
newtype RTCSessionDescription = RTCSessionDescription { unRTCSessionDescription :: JSVal }

instance PToJSVal RTCSessionDescription where
  pToJSVal = unRTCSessionDescription
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCSessionDescription where
  pFromJSVal = RTCSessionDescription
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCSessionDescription where
  toJSVal = return . unRTCSessionDescription
  {-# INLINE toJSVal #-}

instance FromJSVal RTCSessionDescription where
  fromJSVal v = fmap RTCSessionDescription <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCSessionDescription
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCSessionDescription where
  makeObject = makeObject . unRTCSessionDescription

instance IsGObject RTCSessionDescription where
  typeGType _ = gTypeRTCSessionDescription
  {-# INLINE typeGType #-}

noRTCSessionDescription :: Maybe RTCSessionDescription
noRTCSessionDescription = Nothing
{-# INLINE noRTCSessionDescription #-}

gTypeRTCSessionDescription :: JSM GType
gTypeRTCSessionDescription = GType . Object <$> jsg "RTCSessionDescription"

-- | Functions for this inteface are in "JSDOM.RTCSessionDescriptionInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCSessionDescriptionInit Mozilla RTCSessionDescriptionInit documentation>
newtype RTCSessionDescriptionInit = RTCSessionDescriptionInit { unRTCSessionDescriptionInit :: JSVal }

instance PToJSVal RTCSessionDescriptionInit where
  pToJSVal = unRTCSessionDescriptionInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCSessionDescriptionInit where
  pFromJSVal = RTCSessionDescriptionInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCSessionDescriptionInit where
  toJSVal = return . unRTCSessionDescriptionInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCSessionDescriptionInit where
  fromJSVal v = fmap RTCSessionDescriptionInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCSessionDescriptionInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCSessionDescriptionInit where
  makeObject = makeObject . unRTCSessionDescriptionInit

instance IsGObject RTCSessionDescriptionInit where
  typeGType _ = gTypeRTCSessionDescriptionInit
  {-# INLINE typeGType #-}

noRTCSessionDescriptionInit :: Maybe RTCSessionDescriptionInit
noRTCSessionDescriptionInit = Nothing
{-# INLINE noRTCSessionDescriptionInit #-}

gTypeRTCSessionDescriptionInit :: JSM GType
gTypeRTCSessionDescriptionInit = GType . Object <$> jsg "RTCSessionDescriptionInit"

-- | Functions for this inteface are in "JSDOM.RTCStats".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCStats Mozilla RTCStats documentation>
newtype RTCStats = RTCStats { unRTCStats :: JSVal }

instance PToJSVal RTCStats where
  pToJSVal = unRTCStats
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCStats where
  pFromJSVal = RTCStats
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCStats where
  toJSVal = return . unRTCStats
  {-# INLINE toJSVal #-}

instance FromJSVal RTCStats where
  fromJSVal v = fmap RTCStats <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCStats
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCStats where
  makeObject = makeObject . unRTCStats

class (IsGObject o) => IsRTCStats o
toRTCStats :: IsRTCStats o => o -> RTCStats
toRTCStats = RTCStats . coerce

instance IsRTCStats RTCStats
instance IsGObject RTCStats where
  typeGType _ = gTypeRTCStats
  {-# INLINE typeGType #-}

noRTCStats :: Maybe RTCStats
noRTCStats = Nothing
{-# INLINE noRTCStats #-}

gTypeRTCStats :: JSM GType
gTypeRTCStats = GType . Object <$> jsg "RTCStats"

-- | Functions for this inteface are in "JSDOM.RTCStatsReport".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCStatsReport Mozilla RTCStatsReport documentation>
newtype RTCStatsReport = RTCStatsReport { unRTCStatsReport :: JSVal }

instance PToJSVal RTCStatsReport where
  pToJSVal = unRTCStatsReport
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCStatsReport where
  pFromJSVal = RTCStatsReport
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCStatsReport where
  toJSVal = return . unRTCStatsReport
  {-# INLINE toJSVal #-}

instance FromJSVal RTCStatsReport where
  fromJSVal v = fmap RTCStatsReport <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCStatsReport
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCStatsReport where
  makeObject = makeObject . unRTCStatsReport

instance IsGObject RTCStatsReport where
  typeGType _ = gTypeRTCStatsReport
  {-# INLINE typeGType #-}

noRTCStatsReport :: Maybe RTCStatsReport
noRTCStatsReport = Nothing
{-# INLINE noRTCStatsReport #-}

gTypeRTCStatsReport :: JSM GType
gTypeRTCStatsReport = GType . Object <$> jsg "RTCStatsReport"

-- | Functions for this inteface are in "JSDOM.RTCTrackEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCTrackEvent Mozilla RTCTrackEvent documentation>
newtype RTCTrackEvent = RTCTrackEvent { unRTCTrackEvent :: JSVal }

instance PToJSVal RTCTrackEvent where
  pToJSVal = unRTCTrackEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCTrackEvent where
  pFromJSVal = RTCTrackEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCTrackEvent where
  toJSVal = return . unRTCTrackEvent
  {-# INLINE toJSVal #-}

instance FromJSVal RTCTrackEvent where
  fromJSVal v = fmap RTCTrackEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCTrackEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCTrackEvent where
  makeObject = makeObject . unRTCTrackEvent

instance IsEvent RTCTrackEvent
instance IsGObject RTCTrackEvent where
  typeGType _ = gTypeRTCTrackEvent
  {-# INLINE typeGType #-}

noRTCTrackEvent :: Maybe RTCTrackEvent
noRTCTrackEvent = Nothing
{-# INLINE noRTCTrackEvent #-}

gTypeRTCTrackEvent :: JSM GType
gTypeRTCTrackEvent = GType . Object <$> jsg "RTCTrackEvent"

-- | Functions for this inteface are in "JSDOM.RTCTrackEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RTCTrackEventInit Mozilla RTCTrackEventInit documentation>
newtype RTCTrackEventInit = RTCTrackEventInit { unRTCTrackEventInit :: JSVal }

instance PToJSVal RTCTrackEventInit where
  pToJSVal = unRTCTrackEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RTCTrackEventInit where
  pFromJSVal = RTCTrackEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RTCTrackEventInit where
  toJSVal = return . unRTCTrackEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal RTCTrackEventInit where
  fromJSVal v = fmap RTCTrackEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RTCTrackEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RTCTrackEventInit where
  makeObject = makeObject . unRTCTrackEventInit

instance IsEventInit RTCTrackEventInit
instance IsGObject RTCTrackEventInit where
  typeGType _ = gTypeRTCTrackEventInit
  {-# INLINE typeGType #-}

noRTCTrackEventInit :: Maybe RTCTrackEventInit
noRTCTrackEventInit = Nothing
{-# INLINE noRTCTrackEventInit #-}

gTypeRTCTrackEventInit :: JSM GType
gTypeRTCTrackEventInit = GType . Object <$> jsg "RTCTrackEventInit"

-- | Functions for this inteface are in "JSDOM.Range".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Range Mozilla Range documentation>
newtype Range = Range { unRange :: JSVal }

instance PToJSVal Range where
  pToJSVal = unRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal Range where
  pFromJSVal = Range
  {-# INLINE pFromJSVal #-}

instance ToJSVal Range where
  toJSVal = return . unRange
  {-# INLINE toJSVal #-}

instance FromJSVal Range where
  fromJSVal v = fmap Range <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Range
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Range where
  makeObject = makeObject . unRange

instance IsGObject Range where
  typeGType _ = gTypeRange
  {-# INLINE typeGType #-}

noRange :: Maybe Range
noRange = Nothing
{-# INLINE noRange #-}

gTypeRange :: JSM GType
gTypeRange = GType . Object <$> jsg "Range"

-- | Functions for this inteface are in "JSDOM.ReadableByteStreamController".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableByteStreamController Mozilla ReadableByteStreamController documentation>
newtype ReadableByteStreamController = ReadableByteStreamController { unReadableByteStreamController :: JSVal }

instance PToJSVal ReadableByteStreamController where
  pToJSVal = unReadableByteStreamController
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableByteStreamController where
  pFromJSVal = ReadableByteStreamController
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableByteStreamController where
  toJSVal = return . unReadableByteStreamController
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableByteStreamController where
  fromJSVal v = fmap ReadableByteStreamController <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableByteStreamController
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableByteStreamController where
  makeObject = makeObject . unReadableByteStreamController

instance IsGObject ReadableByteStreamController where
  typeGType _ = gTypeReadableByteStreamController
  {-# INLINE typeGType #-}

noReadableByteStreamController :: Maybe ReadableByteStreamController
noReadableByteStreamController = Nothing
{-# INLINE noReadableByteStreamController #-}

gTypeReadableByteStreamController :: JSM GType
gTypeReadableByteStreamController = GType . Object <$> jsg "ReadableByteStreamController"

-- | Functions for this inteface are in "JSDOM.ReadableStream".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStream Mozilla ReadableStream documentation>
newtype ReadableStream = ReadableStream { unReadableStream :: JSVal }

instance PToJSVal ReadableStream where
  pToJSVal = unReadableStream
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStream where
  pFromJSVal = ReadableStream
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStream where
  toJSVal = return . unReadableStream
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStream where
  fromJSVal v = fmap ReadableStream <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStream
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStream where
  makeObject = makeObject . unReadableStream

instance IsGObject ReadableStream where
  typeGType _ = gTypeReadableStream
  {-# INLINE typeGType #-}

noReadableStream :: Maybe ReadableStream
noReadableStream = Nothing
{-# INLINE noReadableStream #-}

gTypeReadableStream :: JSM GType
gTypeReadableStream = GType . Object <$> jsg "ReadableStream"

-- | Functions for this inteface are in "JSDOM.ReadableStreamBYOBReader".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamBYOBReader Mozilla ReadableStreamBYOBReader documentation>
newtype ReadableStreamBYOBReader = ReadableStreamBYOBReader { unReadableStreamBYOBReader :: JSVal }

instance PToJSVal ReadableStreamBYOBReader where
  pToJSVal = unReadableStreamBYOBReader
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStreamBYOBReader where
  pFromJSVal = ReadableStreamBYOBReader
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStreamBYOBReader where
  toJSVal = return . unReadableStreamBYOBReader
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStreamBYOBReader where
  fromJSVal v = fmap ReadableStreamBYOBReader <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStreamBYOBReader
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStreamBYOBReader where
  makeObject = makeObject . unReadableStreamBYOBReader

instance IsGObject ReadableStreamBYOBReader where
  typeGType _ = gTypeReadableStreamBYOBReader
  {-# INLINE typeGType #-}

noReadableStreamBYOBReader :: Maybe ReadableStreamBYOBReader
noReadableStreamBYOBReader = Nothing
{-# INLINE noReadableStreamBYOBReader #-}

gTypeReadableStreamBYOBReader :: JSM GType
gTypeReadableStreamBYOBReader = GType . Object <$> jsg "ReadableStreamBYOBReader"

-- | Functions for this inteface are in "JSDOM.ReadableStreamBYOBRequest".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamBYOBRequest Mozilla ReadableStreamBYOBRequest documentation>
newtype ReadableStreamBYOBRequest = ReadableStreamBYOBRequest { unReadableStreamBYOBRequest :: JSVal }

instance PToJSVal ReadableStreamBYOBRequest where
  pToJSVal = unReadableStreamBYOBRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStreamBYOBRequest where
  pFromJSVal = ReadableStreamBYOBRequest
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStreamBYOBRequest where
  toJSVal = return . unReadableStreamBYOBRequest
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStreamBYOBRequest where
  fromJSVal v = fmap ReadableStreamBYOBRequest <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStreamBYOBRequest
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStreamBYOBRequest where
  makeObject = makeObject . unReadableStreamBYOBRequest

instance IsGObject ReadableStreamBYOBRequest where
  typeGType _ = gTypeReadableStreamBYOBRequest
  {-# INLINE typeGType #-}

noReadableStreamBYOBRequest :: Maybe ReadableStreamBYOBRequest
noReadableStreamBYOBRequest = Nothing
{-# INLINE noReadableStreamBYOBRequest #-}

gTypeReadableStreamBYOBRequest :: JSM GType
gTypeReadableStreamBYOBRequest = GType . Object <$> jsg "ReadableStreamBYOBRequest"

-- | Functions for this inteface are in "JSDOM.ReadableStreamDefaultController".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultController Mozilla ReadableStreamDefaultController documentation>
newtype ReadableStreamDefaultController = ReadableStreamDefaultController { unReadableStreamDefaultController :: JSVal }

instance PToJSVal ReadableStreamDefaultController where
  pToJSVal = unReadableStreamDefaultController
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStreamDefaultController where
  pFromJSVal = ReadableStreamDefaultController
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStreamDefaultController where
  toJSVal = return . unReadableStreamDefaultController
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStreamDefaultController where
  fromJSVal v = fmap ReadableStreamDefaultController <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStreamDefaultController
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStreamDefaultController where
  makeObject = makeObject . unReadableStreamDefaultController

instance IsGObject ReadableStreamDefaultController where
  typeGType _ = gTypeReadableStreamDefaultController
  {-# INLINE typeGType #-}

noReadableStreamDefaultController :: Maybe ReadableStreamDefaultController
noReadableStreamDefaultController = Nothing
{-# INLINE noReadableStreamDefaultController #-}

gTypeReadableStreamDefaultController :: JSM GType
gTypeReadableStreamDefaultController = GType . Object <$> jsg "ReadableStreamDefaultController"

-- | Functions for this inteface are in "JSDOM.ReadableStreamDefaultReader".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamDefaultReader Mozilla ReadableStreamDefaultReader documentation>
newtype ReadableStreamDefaultReader = ReadableStreamDefaultReader { unReadableStreamDefaultReader :: JSVal }

instance PToJSVal ReadableStreamDefaultReader where
  pToJSVal = unReadableStreamDefaultReader
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStreamDefaultReader where
  pFromJSVal = ReadableStreamDefaultReader
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStreamDefaultReader where
  toJSVal = return . unReadableStreamDefaultReader
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStreamDefaultReader where
  fromJSVal v = fmap ReadableStreamDefaultReader <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStreamDefaultReader
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStreamDefaultReader where
  makeObject = makeObject . unReadableStreamDefaultReader

instance IsGObject ReadableStreamDefaultReader where
  typeGType _ = gTypeReadableStreamDefaultReader
  {-# INLINE typeGType #-}

noReadableStreamDefaultReader :: Maybe ReadableStreamDefaultReader
noReadableStreamDefaultReader = Nothing
{-# INLINE noReadableStreamDefaultReader #-}

gTypeReadableStreamDefaultReader :: JSM GType
gTypeReadableStreamDefaultReader = GType . Object <$> jsg "ReadableStreamDefaultReader"

-- | Functions for this inteface are in "JSDOM.ReadableStreamSource".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ReadableStreamSource Mozilla ReadableStreamSource documentation>
newtype ReadableStreamSource = ReadableStreamSource { unReadableStreamSource :: JSVal }

instance PToJSVal ReadableStreamSource where
  pToJSVal = unReadableStreamSource
  {-# INLINE pToJSVal #-}

instance PFromJSVal ReadableStreamSource where
  pFromJSVal = ReadableStreamSource
  {-# INLINE pFromJSVal #-}

instance ToJSVal ReadableStreamSource where
  toJSVal = return . unReadableStreamSource
  {-# INLINE toJSVal #-}

instance FromJSVal ReadableStreamSource where
  fromJSVal v = fmap ReadableStreamSource <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ReadableStreamSource
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ReadableStreamSource where
  makeObject = makeObject . unReadableStreamSource

instance IsGObject ReadableStreamSource where
  typeGType _ = gTypeReadableStreamSource
  {-# INLINE typeGType #-}

noReadableStreamSource :: Maybe ReadableStreamSource
noReadableStreamSource = Nothing
{-# INLINE noReadableStreamSource #-}

gTypeReadableStreamSource :: JSM GType
gTypeReadableStreamSource = GType . Object <$> jsg "ReadableStreamSource"

-- | Functions for this inteface are in "JSDOM.Rect".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Rect Mozilla Rect documentation>
newtype Rect = Rect { unRect :: JSVal }

instance PToJSVal Rect where
  pToJSVal = unRect
  {-# INLINE pToJSVal #-}

instance PFromJSVal Rect where
  pFromJSVal = Rect
  {-# INLINE pFromJSVal #-}

instance ToJSVal Rect where
  toJSVal = return . unRect
  {-# INLINE toJSVal #-}

instance FromJSVal Rect where
  fromJSVal v = fmap Rect <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Rect
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Rect where
  makeObject = makeObject . unRect

instance IsGObject Rect where
  typeGType _ = gTypeRect
  {-# INLINE typeGType #-}

noRect :: Maybe Rect
noRect = Nothing
{-# INLINE noRect #-}

gTypeRect :: JSM GType
gTypeRect = GType . Object <$> jsg "Rect"

-- | Functions for this inteface are in "JSDOM.Request".
-- Base interface functions are in:
--
--     * "JSDOM.Body"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Request Mozilla Request documentation>
newtype Request = Request { unRequest :: JSVal }

instance PToJSVal Request where
  pToJSVal = unRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal Request where
  pFromJSVal = Request
  {-# INLINE pFromJSVal #-}

instance ToJSVal Request where
  toJSVal = return . unRequest
  {-# INLINE toJSVal #-}

instance FromJSVal Request where
  fromJSVal v = fmap Request <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Request
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Request where
  makeObject = makeObject . unRequest

instance IsBody Request
instance IsGObject Request where
  typeGType _ = gTypeRequest
  {-# INLINE typeGType #-}

noRequest :: Maybe Request
noRequest = Nothing
{-# INLINE noRequest #-}

gTypeRequest :: JSM GType
gTypeRequest = GType . Object <$> jsg "Request"

-- | Functions for this inteface are in "JSDOM.RequestInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RequestInit Mozilla RequestInit documentation>
newtype RequestInit = RequestInit { unRequestInit :: JSVal }

instance PToJSVal RequestInit where
  pToJSVal = unRequestInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal RequestInit where
  pFromJSVal = RequestInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal RequestInit where
  toJSVal = return . unRequestInit
  {-# INLINE toJSVal #-}

instance FromJSVal RequestInit where
  fromJSVal v = fmap RequestInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RequestInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RequestInit where
  makeObject = makeObject . unRequestInit

instance IsGObject RequestInit where
  typeGType _ = gTypeRequestInit
  {-# INLINE typeGType #-}

noRequestInit :: Maybe RequestInit
noRequestInit = Nothing
{-# INLINE noRequestInit #-}

gTypeRequestInit :: JSM GType
gTypeRequestInit = GType . Object <$> jsg "RequestInit"

-- | Functions for this inteface are in "JSDOM.Response".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Response Mozilla Response documentation>
newtype Response = Response { unResponse :: JSVal }

instance PToJSVal Response where
  pToJSVal = unResponse
  {-# INLINE pToJSVal #-}

instance PFromJSVal Response where
  pFromJSVal = Response
  {-# INLINE pFromJSVal #-}

instance ToJSVal Response where
  toJSVal = return . unResponse
  {-# INLINE toJSVal #-}

instance FromJSVal Response where
  fromJSVal v = fmap Response <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Response
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Response where
  makeObject = makeObject . unResponse

instance IsGObject Response where
  typeGType _ = gTypeResponse
  {-# INLINE typeGType #-}

noResponse :: Maybe Response
noResponse = Nothing
{-# INLINE noResponse #-}

gTypeResponse :: JSM GType
gTypeResponse = GType . Object <$> jsg "Response"

-- | Functions for this inteface are in "JSDOM.RotationRate".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RotationRate Mozilla RotationRate documentation>
newtype RotationRate = RotationRate { unRotationRate :: JSVal }

instance PToJSVal RotationRate where
  pToJSVal = unRotationRate
  {-# INLINE pToJSVal #-}

instance PFromJSVal RotationRate where
  pFromJSVal = RotationRate
  {-# INLINE pFromJSVal #-}

instance ToJSVal RotationRate where
  toJSVal = return . unRotationRate
  {-# INLINE toJSVal #-}

instance FromJSVal RotationRate where
  fromJSVal v = fmap RotationRate <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RotationRate
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RotationRate where
  makeObject = makeObject . unRotationRate

instance IsGObject RotationRate where
  typeGType _ = gTypeRotationRate
  {-# INLINE typeGType #-}

noRotationRate :: Maybe RotationRate
noRotationRate = Nothing
{-# INLINE noRotationRate #-}

gTypeRotationRate :: JSM GType
gTypeRotationRate = GType . Object <$> jsg "RotationRate"

-- | Functions for this inteface are in "JSDOM.RsaHashedImportParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RsaHashedImportParams Mozilla RsaHashedImportParams documentation>
newtype RsaHashedImportParams = RsaHashedImportParams { unRsaHashedImportParams :: JSVal }

instance PToJSVal RsaHashedImportParams where
  pToJSVal = unRsaHashedImportParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal RsaHashedImportParams where
  pFromJSVal = RsaHashedImportParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal RsaHashedImportParams where
  toJSVal = return . unRsaHashedImportParams
  {-# INLINE toJSVal #-}

instance FromJSVal RsaHashedImportParams where
  fromJSVal v = fmap RsaHashedImportParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RsaHashedImportParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RsaHashedImportParams where
  makeObject = makeObject . unRsaHashedImportParams

instance IsCryptoAlgorithmParameters RsaHashedImportParams
instance IsGObject RsaHashedImportParams where
  typeGType _ = gTypeRsaHashedImportParams
  {-# INLINE typeGType #-}

noRsaHashedImportParams :: Maybe RsaHashedImportParams
noRsaHashedImportParams = Nothing
{-# INLINE noRsaHashedImportParams #-}

gTypeRsaHashedImportParams :: JSM GType
gTypeRsaHashedImportParams = GType . Object <$> jsg "RsaHashedImportParams"

-- | Functions for this inteface are in "JSDOM.RsaHashedKeyGenParams".
-- Base interface functions are in:
--
--     * "JSDOM.RsaKeyGenParams"
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RsaHashedKeyGenParams Mozilla RsaHashedKeyGenParams documentation>
newtype RsaHashedKeyGenParams = RsaHashedKeyGenParams { unRsaHashedKeyGenParams :: JSVal }

instance PToJSVal RsaHashedKeyGenParams where
  pToJSVal = unRsaHashedKeyGenParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal RsaHashedKeyGenParams where
  pFromJSVal = RsaHashedKeyGenParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal RsaHashedKeyGenParams where
  toJSVal = return . unRsaHashedKeyGenParams
  {-# INLINE toJSVal #-}

instance FromJSVal RsaHashedKeyGenParams where
  fromJSVal v = fmap RsaHashedKeyGenParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RsaHashedKeyGenParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RsaHashedKeyGenParams where
  makeObject = makeObject . unRsaHashedKeyGenParams

instance IsRsaKeyGenParams RsaHashedKeyGenParams
instance IsCryptoAlgorithmParameters RsaHashedKeyGenParams
instance IsGObject RsaHashedKeyGenParams where
  typeGType _ = gTypeRsaHashedKeyGenParams
  {-# INLINE typeGType #-}

noRsaHashedKeyGenParams :: Maybe RsaHashedKeyGenParams
noRsaHashedKeyGenParams = Nothing
{-# INLINE noRsaHashedKeyGenParams #-}

gTypeRsaHashedKeyGenParams :: JSM GType
gTypeRsaHashedKeyGenParams = GType . Object <$> jsg "RsaHashedKeyGenParams"

-- | Functions for this inteface are in "JSDOM.RsaKeyGenParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RsaKeyGenParams Mozilla RsaKeyGenParams documentation>
newtype RsaKeyGenParams = RsaKeyGenParams { unRsaKeyGenParams :: JSVal }

instance PToJSVal RsaKeyGenParams where
  pToJSVal = unRsaKeyGenParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal RsaKeyGenParams where
  pFromJSVal = RsaKeyGenParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal RsaKeyGenParams where
  toJSVal = return . unRsaKeyGenParams
  {-# INLINE toJSVal #-}

instance FromJSVal RsaKeyGenParams where
  fromJSVal v = fmap RsaKeyGenParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RsaKeyGenParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RsaKeyGenParams where
  makeObject = makeObject . unRsaKeyGenParams

class (IsCryptoAlgorithmParameters o, IsGObject o) => IsRsaKeyGenParams o
toRsaKeyGenParams :: IsRsaKeyGenParams o => o -> RsaKeyGenParams
toRsaKeyGenParams = RsaKeyGenParams . coerce

instance IsRsaKeyGenParams RsaKeyGenParams
instance IsCryptoAlgorithmParameters RsaKeyGenParams
instance IsGObject RsaKeyGenParams where
  typeGType _ = gTypeRsaKeyGenParams
  {-# INLINE typeGType #-}

noRsaKeyGenParams :: Maybe RsaKeyGenParams
noRsaKeyGenParams = Nothing
{-# INLINE noRsaKeyGenParams #-}

gTypeRsaKeyGenParams :: JSM GType
gTypeRsaKeyGenParams = GType . Object <$> jsg "RsaKeyGenParams"

-- | Functions for this inteface are in "JSDOM.RsaOaepParams".
-- Base interface functions are in:
--
--     * "JSDOM.CryptoAlgorithmParameters"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RsaOaepParams Mozilla RsaOaepParams documentation>
newtype RsaOaepParams = RsaOaepParams { unRsaOaepParams :: JSVal }

instance PToJSVal RsaOaepParams where
  pToJSVal = unRsaOaepParams
  {-# INLINE pToJSVal #-}

instance PFromJSVal RsaOaepParams where
  pFromJSVal = RsaOaepParams
  {-# INLINE pFromJSVal #-}

instance ToJSVal RsaOaepParams where
  toJSVal = return . unRsaOaepParams
  {-# INLINE toJSVal #-}

instance FromJSVal RsaOaepParams where
  fromJSVal v = fmap RsaOaepParams <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RsaOaepParams
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RsaOaepParams where
  makeObject = makeObject . unRsaOaepParams

instance IsCryptoAlgorithmParameters RsaOaepParams
instance IsGObject RsaOaepParams where
  typeGType _ = gTypeRsaOaepParams
  {-# INLINE typeGType #-}

noRsaOaepParams :: Maybe RsaOaepParams
noRsaOaepParams = Nothing
{-# INLINE noRsaOaepParams #-}

gTypeRsaOaepParams :: JSM GType
gTypeRsaOaepParams = GType . Object <$> jsg "RsaOaepParams"

-- | Functions for this inteface are in "JSDOM.RsaOtherPrimesInfo".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RsaOtherPrimesInfo Mozilla RsaOtherPrimesInfo documentation>
newtype RsaOtherPrimesInfo = RsaOtherPrimesInfo { unRsaOtherPrimesInfo :: JSVal }

instance PToJSVal RsaOtherPrimesInfo where
  pToJSVal = unRsaOtherPrimesInfo
  {-# INLINE pToJSVal #-}

instance PFromJSVal RsaOtherPrimesInfo where
  pFromJSVal = RsaOtherPrimesInfo
  {-# INLINE pFromJSVal #-}

instance ToJSVal RsaOtherPrimesInfo where
  toJSVal = return . unRsaOtherPrimesInfo
  {-# INLINE toJSVal #-}

instance FromJSVal RsaOtherPrimesInfo where
  fromJSVal v = fmap RsaOtherPrimesInfo <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RsaOtherPrimesInfo
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RsaOtherPrimesInfo where
  makeObject = makeObject . unRsaOtherPrimesInfo

instance IsGObject RsaOtherPrimesInfo where
  typeGType _ = gTypeRsaOtherPrimesInfo
  {-# INLINE typeGType #-}

noRsaOtherPrimesInfo :: Maybe RsaOtherPrimesInfo
noRsaOtherPrimesInfo = Nothing
{-# INLINE noRsaOtherPrimesInfo #-}

gTypeRsaOtherPrimesInfo :: JSM GType
gTypeRsaOtherPrimesInfo = GType . Object <$> jsg "RsaOtherPrimesInfo"

-- | Functions for this inteface are in "JSDOM.SQLError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SQLError Mozilla SQLError documentation>
newtype SQLError = SQLError { unSQLError :: JSVal }

instance PToJSVal SQLError where
  pToJSVal = unSQLError
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLError where
  pFromJSVal = SQLError
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLError where
  toJSVal = return . unSQLError
  {-# INLINE toJSVal #-}

instance FromJSVal SQLError where
  fromJSVal v = fmap SQLError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLError where
  makeObject = makeObject . unSQLError

instance IsGObject SQLError where
  typeGType _ = gTypeSQLError
  {-# INLINE typeGType #-}

noSQLError :: Maybe SQLError
noSQLError = Nothing
{-# INLINE noSQLError #-}

gTypeSQLError :: JSM GType
gTypeSQLError = GType . Object <$> jsg "SQLError"

-- | Functions for this inteface are in "JSDOM.SQLException".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SQLException Mozilla SQLException documentation>
newtype SQLException = SQLException { unSQLException :: JSVal }

instance PToJSVal SQLException where
  pToJSVal = unSQLException
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLException where
  pFromJSVal = SQLException
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLException where
  toJSVal = return . unSQLException
  {-# INLINE toJSVal #-}

instance FromJSVal SQLException where
  fromJSVal v = fmap SQLException <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLException
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLException where
  makeObject = makeObject . unSQLException

instance IsGObject SQLException where
  typeGType _ = gTypeSQLException
  {-# INLINE typeGType #-}

noSQLException :: Maybe SQLException
noSQLException = Nothing
{-# INLINE noSQLException #-}

gTypeSQLException :: JSM GType
gTypeSQLException = GType . Object <$> jsg "SQLException"

-- | Functions for this inteface are in "JSDOM.SQLResultSet".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SQLResultSet Mozilla SQLResultSet documentation>
newtype SQLResultSet = SQLResultSet { unSQLResultSet :: JSVal }

instance PToJSVal SQLResultSet where
  pToJSVal = unSQLResultSet
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLResultSet where
  pFromJSVal = SQLResultSet
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLResultSet where
  toJSVal = return . unSQLResultSet
  {-# INLINE toJSVal #-}

instance FromJSVal SQLResultSet where
  fromJSVal v = fmap SQLResultSet <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLResultSet
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLResultSet where
  makeObject = makeObject . unSQLResultSet

instance IsGObject SQLResultSet where
  typeGType _ = gTypeSQLResultSet
  {-# INLINE typeGType #-}

noSQLResultSet :: Maybe SQLResultSet
noSQLResultSet = Nothing
{-# INLINE noSQLResultSet #-}

gTypeSQLResultSet :: JSM GType
gTypeSQLResultSet = GType . Object <$> jsg "SQLResultSet"

-- | Functions for this inteface are in "JSDOM.SQLResultSetRowList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SQLResultSetRowList Mozilla SQLResultSetRowList documentation>
newtype SQLResultSetRowList = SQLResultSetRowList { unSQLResultSetRowList :: JSVal }

instance PToJSVal SQLResultSetRowList where
  pToJSVal = unSQLResultSetRowList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLResultSetRowList where
  pFromJSVal = SQLResultSetRowList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLResultSetRowList where
  toJSVal = return . unSQLResultSetRowList
  {-# INLINE toJSVal #-}

instance FromJSVal SQLResultSetRowList where
  fromJSVal v = fmap SQLResultSetRowList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLResultSetRowList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLResultSetRowList where
  makeObject = makeObject . unSQLResultSetRowList

instance IsGObject SQLResultSetRowList where
  typeGType _ = gTypeSQLResultSetRowList
  {-# INLINE typeGType #-}

noSQLResultSetRowList :: Maybe SQLResultSetRowList
noSQLResultSetRowList = Nothing
{-# INLINE noSQLResultSetRowList #-}

gTypeSQLResultSetRowList :: JSM GType
gTypeSQLResultSetRowList = GType . Object <$> jsg "SQLResultSetRowList"

-- | Functions for this inteface are in "JSDOM.SQLTransaction".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SQLTransaction Mozilla SQLTransaction documentation>
newtype SQLTransaction = SQLTransaction { unSQLTransaction :: JSVal }

instance PToJSVal SQLTransaction where
  pToJSVal = unSQLTransaction
  {-# INLINE pToJSVal #-}

instance PFromJSVal SQLTransaction where
  pFromJSVal = SQLTransaction
  {-# INLINE pFromJSVal #-}

instance ToJSVal SQLTransaction where
  toJSVal = return . unSQLTransaction
  {-# INLINE toJSVal #-}

instance FromJSVal SQLTransaction where
  fromJSVal v = fmap SQLTransaction <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SQLTransaction
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SQLTransaction where
  makeObject = makeObject . unSQLTransaction

instance IsGObject SQLTransaction where
  typeGType _ = gTypeSQLTransaction
  {-# INLINE typeGType #-}

noSQLTransaction :: Maybe SQLTransaction
noSQLTransaction = Nothing
{-# INLINE noSQLTransaction #-}

gTypeSQLTransaction :: JSM GType
gTypeSQLTransaction = GType . Object <$> jsg "SQLTransaction"


-- | Functions for this inteface are in "JSDOM.Screen".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Screen Mozilla Screen documentation>
newtype Screen = Screen { unScreen :: JSVal }

instance PToJSVal Screen where
  pToJSVal = unScreen
  {-# INLINE pToJSVal #-}

instance PFromJSVal Screen where
  pFromJSVal = Screen
  {-# INLINE pFromJSVal #-}

instance ToJSVal Screen where
  toJSVal = return . unScreen
  {-# INLINE toJSVal #-}

instance FromJSVal Screen where
  fromJSVal v = fmap Screen <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Screen
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Screen where
  makeObject = makeObject . unScreen

instance IsGObject Screen where
  typeGType _ = gTypeScreen
  {-# INLINE typeGType #-}

noScreen :: Maybe Screen
noScreen = Nothing
{-# INLINE noScreen #-}

gTypeScreen :: JSM GType
gTypeScreen = GType . Object <$> jsg "Screen"

-- | Functions for this inteface are in "JSDOM.ScriptProcessorNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ScriptProcessorNode Mozilla ScriptProcessorNode documentation>
newtype ScriptProcessorNode = ScriptProcessorNode { unScriptProcessorNode :: JSVal }

instance PToJSVal ScriptProcessorNode where
  pToJSVal = unScriptProcessorNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ScriptProcessorNode where
  pFromJSVal = ScriptProcessorNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ScriptProcessorNode where
  toJSVal = return . unScriptProcessorNode
  {-# INLINE toJSVal #-}

instance FromJSVal ScriptProcessorNode where
  fromJSVal v = fmap ScriptProcessorNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ScriptProcessorNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ScriptProcessorNode where
  makeObject = makeObject . unScriptProcessorNode

instance IsAudioNode ScriptProcessorNode
instance IsEventTarget ScriptProcessorNode
instance IsGObject ScriptProcessorNode where
  typeGType _ = gTypeScriptProcessorNode
  {-# INLINE typeGType #-}

noScriptProcessorNode :: Maybe ScriptProcessorNode
noScriptProcessorNode = Nothing
{-# INLINE noScriptProcessorNode #-}

gTypeScriptProcessorNode :: JSM GType
gTypeScriptProcessorNode = GType . Object <$> jsg "ScriptProcessorNode"

-- | Functions for this inteface are in "JSDOM.ScrollToOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ScrollToOptions Mozilla ScrollToOptions documentation>
newtype ScrollToOptions = ScrollToOptions { unScrollToOptions :: JSVal }

instance PToJSVal ScrollToOptions where
  pToJSVal = unScrollToOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal ScrollToOptions where
  pFromJSVal = ScrollToOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal ScrollToOptions where
  toJSVal = return . unScrollToOptions
  {-# INLINE toJSVal #-}

instance FromJSVal ScrollToOptions where
  fromJSVal v = fmap ScrollToOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ScrollToOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ScrollToOptions where
  makeObject = makeObject . unScrollToOptions

instance IsGObject ScrollToOptions where
  typeGType _ = gTypeScrollToOptions
  {-# INLINE typeGType #-}

noScrollToOptions :: Maybe ScrollToOptions
noScrollToOptions = Nothing
{-# INLINE noScrollToOptions #-}

gTypeScrollToOptions :: JSM GType
gTypeScrollToOptions = GType . Object <$> jsg "ScrollToOptions"

-- | Functions for this inteface are in "JSDOM.SecurityPolicyViolationEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SecurityPolicyViolationEvent Mozilla SecurityPolicyViolationEvent documentation>
newtype SecurityPolicyViolationEvent = SecurityPolicyViolationEvent { unSecurityPolicyViolationEvent :: JSVal }

instance PToJSVal SecurityPolicyViolationEvent where
  pToJSVal = unSecurityPolicyViolationEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal SecurityPolicyViolationEvent where
  pFromJSVal = SecurityPolicyViolationEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal SecurityPolicyViolationEvent where
  toJSVal = return . unSecurityPolicyViolationEvent
  {-# INLINE toJSVal #-}

instance FromJSVal SecurityPolicyViolationEvent where
  fromJSVal v = fmap SecurityPolicyViolationEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SecurityPolicyViolationEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SecurityPolicyViolationEvent where
  makeObject = makeObject . unSecurityPolicyViolationEvent

instance IsEvent SecurityPolicyViolationEvent
instance IsGObject SecurityPolicyViolationEvent where
  typeGType _ = gTypeSecurityPolicyViolationEvent
  {-# INLINE typeGType #-}

noSecurityPolicyViolationEvent :: Maybe SecurityPolicyViolationEvent
noSecurityPolicyViolationEvent = Nothing
{-# INLINE noSecurityPolicyViolationEvent #-}

gTypeSecurityPolicyViolationEvent :: JSM GType
gTypeSecurityPolicyViolationEvent = GType . Object <$> jsg "SecurityPolicyViolationEvent"

-- | Functions for this inteface are in "JSDOM.SecurityPolicyViolationEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SecurityPolicyViolationEventInit Mozilla SecurityPolicyViolationEventInit documentation>
newtype SecurityPolicyViolationEventInit = SecurityPolicyViolationEventInit { unSecurityPolicyViolationEventInit :: JSVal }

instance PToJSVal SecurityPolicyViolationEventInit where
  pToJSVal = unSecurityPolicyViolationEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal SecurityPolicyViolationEventInit where
  pFromJSVal = SecurityPolicyViolationEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal SecurityPolicyViolationEventInit where
  toJSVal = return . unSecurityPolicyViolationEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal SecurityPolicyViolationEventInit where
  fromJSVal v = fmap SecurityPolicyViolationEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SecurityPolicyViolationEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SecurityPolicyViolationEventInit where
  makeObject = makeObject . unSecurityPolicyViolationEventInit

instance IsEventInit SecurityPolicyViolationEventInit
instance IsGObject SecurityPolicyViolationEventInit where
  typeGType _ = gTypeSecurityPolicyViolationEventInit
  {-# INLINE typeGType #-}

noSecurityPolicyViolationEventInit :: Maybe SecurityPolicyViolationEventInit
noSecurityPolicyViolationEventInit = Nothing
{-# INLINE noSecurityPolicyViolationEventInit #-}

gTypeSecurityPolicyViolationEventInit :: JSM GType
gTypeSecurityPolicyViolationEventInit = GType . Object <$> jsg "SecurityPolicyViolationEventInit"

-- | Functions for this inteface are in "JSDOM.Selection".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Selection Mozilla Selection documentation>
newtype Selection = Selection { unSelection :: JSVal }

instance PToJSVal Selection where
  pToJSVal = unSelection
  {-# INLINE pToJSVal #-}

instance PFromJSVal Selection where
  pFromJSVal = Selection
  {-# INLINE pFromJSVal #-}

instance ToJSVal Selection where
  toJSVal = return . unSelection
  {-# INLINE toJSVal #-}

instance FromJSVal Selection where
  fromJSVal v = fmap Selection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Selection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Selection where
  makeObject = makeObject . unSelection

instance IsGObject Selection where
  typeGType _ = gTypeSelection
  {-# INLINE typeGType #-}

noSelection :: Maybe Selection
noSelection = Nothing
{-# INLINE noSelection #-}

gTypeSelection :: JSM GType
gTypeSelection = GType . Object <$> jsg "Selection"

-- | Functions for this inteface are in "JSDOM.ShadowRootInit".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ShadowRootInit Mozilla ShadowRootInit documentation>
newtype ShadowRootInit = ShadowRootInit { unShadowRootInit :: JSVal }

instance PToJSVal ShadowRootInit where
  pToJSVal = unShadowRootInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal ShadowRootInit where
  pFromJSVal = ShadowRootInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal ShadowRootInit where
  toJSVal = return . unShadowRootInit
  {-# INLINE toJSVal #-}

instance FromJSVal ShadowRootInit where
  fromJSVal v = fmap ShadowRootInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ShadowRootInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ShadowRootInit where
  makeObject = makeObject . unShadowRootInit

instance IsGObject ShadowRootInit where
  typeGType _ = gTypeShadowRootInit
  {-# INLINE typeGType #-}

noShadowRootInit :: Maybe ShadowRootInit
noShadowRootInit = Nothing
{-# INLINE noShadowRootInit #-}

gTypeShadowRootInit :: JSM GType
gTypeShadowRootInit = GType . Object <$> jsg "ShadowRootInit"

-- | Functions for this inteface are in "JSDOM.SiteBoundCredential".
-- Base interface functions are in:
--
--     * "JSDOM.BasicCredential"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SiteBoundCredential Mozilla SiteBoundCredential documentation>
newtype SiteBoundCredential = SiteBoundCredential { unSiteBoundCredential :: JSVal }

instance PToJSVal SiteBoundCredential where
  pToJSVal = unSiteBoundCredential
  {-# INLINE pToJSVal #-}

instance PFromJSVal SiteBoundCredential where
  pFromJSVal = SiteBoundCredential
  {-# INLINE pFromJSVal #-}

instance ToJSVal SiteBoundCredential where
  toJSVal = return . unSiteBoundCredential
  {-# INLINE toJSVal #-}

instance FromJSVal SiteBoundCredential where
  fromJSVal v = fmap SiteBoundCredential <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SiteBoundCredential
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SiteBoundCredential where
  makeObject = makeObject . unSiteBoundCredential

class (IsBasicCredential o, IsGObject o) => IsSiteBoundCredential o
toSiteBoundCredential :: IsSiteBoundCredential o => o -> SiteBoundCredential
toSiteBoundCredential = SiteBoundCredential . coerce

instance IsSiteBoundCredential SiteBoundCredential
instance IsBasicCredential SiteBoundCredential
instance IsGObject SiteBoundCredential where
  typeGType _ = gTypeSiteBoundCredential
  {-# INLINE typeGType #-}

noSiteBoundCredential :: Maybe SiteBoundCredential
noSiteBoundCredential = Nothing
{-# INLINE noSiteBoundCredential #-}

gTypeSiteBoundCredential :: JSM GType
gTypeSiteBoundCredential = GType . Object <$> jsg "SiteBoundCredential"

-- | Functions for this inteface are in "JSDOM.SiteBoundCredentialData".
-- Base interface functions are in:
--
--     * "JSDOM.CredentialData"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SiteBoundCredentialData Mozilla SiteBoundCredentialData documentation>
newtype SiteBoundCredentialData = SiteBoundCredentialData { unSiteBoundCredentialData :: JSVal }

instance PToJSVal SiteBoundCredentialData where
  pToJSVal = unSiteBoundCredentialData
  {-# INLINE pToJSVal #-}

instance PFromJSVal SiteBoundCredentialData where
  pFromJSVal = SiteBoundCredentialData
  {-# INLINE pFromJSVal #-}

instance ToJSVal SiteBoundCredentialData where
  toJSVal = return . unSiteBoundCredentialData
  {-# INLINE toJSVal #-}

instance FromJSVal SiteBoundCredentialData where
  fromJSVal v = fmap SiteBoundCredentialData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SiteBoundCredentialData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SiteBoundCredentialData where
  makeObject = makeObject . unSiteBoundCredentialData

class (IsCredentialData o, IsGObject o) => IsSiteBoundCredentialData o
toSiteBoundCredentialData :: IsSiteBoundCredentialData o => o -> SiteBoundCredentialData
toSiteBoundCredentialData = SiteBoundCredentialData . coerce

instance IsSiteBoundCredentialData SiteBoundCredentialData
instance IsCredentialData SiteBoundCredentialData
instance IsGObject SiteBoundCredentialData where
  typeGType _ = gTypeSiteBoundCredentialData
  {-# INLINE typeGType #-}

noSiteBoundCredentialData :: Maybe SiteBoundCredentialData
noSiteBoundCredentialData = Nothing
{-# INLINE noSiteBoundCredentialData #-}

gTypeSiteBoundCredentialData :: JSM GType
gTypeSiteBoundCredentialData = GType . Object <$> jsg "SiteBoundCredentialData"

-- | Functions for this inteface are in "JSDOM.SourceBuffer".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SourceBuffer Mozilla SourceBuffer documentation>
newtype SourceBuffer = SourceBuffer { unSourceBuffer :: JSVal }

instance PToJSVal SourceBuffer where
  pToJSVal = unSourceBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal SourceBuffer where
  pFromJSVal = SourceBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal SourceBuffer where
  toJSVal = return . unSourceBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal SourceBuffer where
  fromJSVal v = fmap SourceBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SourceBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SourceBuffer where
  makeObject = makeObject . unSourceBuffer

instance IsEventTarget SourceBuffer
instance IsGObject SourceBuffer where
  typeGType _ = gTypeSourceBuffer
  {-# INLINE typeGType #-}

noSourceBuffer :: Maybe SourceBuffer
noSourceBuffer = Nothing
{-# INLINE noSourceBuffer #-}

gTypeSourceBuffer :: JSM GType
gTypeSourceBuffer = GType . Object <$> jsg "SourceBuffer"

-- | Functions for this inteface are in "JSDOM.SourceBufferList".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SourceBufferList Mozilla SourceBufferList documentation>
newtype SourceBufferList = SourceBufferList { unSourceBufferList :: JSVal }

instance PToJSVal SourceBufferList where
  pToJSVal = unSourceBufferList
  {-# INLINE pToJSVal #-}

instance PFromJSVal SourceBufferList where
  pFromJSVal = SourceBufferList
  {-# INLINE pFromJSVal #-}

instance ToJSVal SourceBufferList where
  toJSVal = return . unSourceBufferList
  {-# INLINE toJSVal #-}

instance FromJSVal SourceBufferList where
  fromJSVal v = fmap SourceBufferList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SourceBufferList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SourceBufferList where
  makeObject = makeObject . unSourceBufferList

instance IsEventTarget SourceBufferList
instance IsGObject SourceBufferList where
  typeGType _ = gTypeSourceBufferList
  {-# INLINE typeGType #-}

noSourceBufferList :: Maybe SourceBufferList
noSourceBufferList = Nothing
{-# INLINE noSourceBufferList #-}

gTypeSourceBufferList :: JSM GType
gTypeSourceBufferList = GType . Object <$> jsg "SourceBufferList"

-- | Functions for this inteface are in "JSDOM.SpeechSynthesis".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesis Mozilla SpeechSynthesis documentation>
newtype SpeechSynthesis = SpeechSynthesis { unSpeechSynthesis :: JSVal }

instance PToJSVal SpeechSynthesis where
  pToJSVal = unSpeechSynthesis
  {-# INLINE pToJSVal #-}

instance PFromJSVal SpeechSynthesis where
  pFromJSVal = SpeechSynthesis
  {-# INLINE pFromJSVal #-}

instance ToJSVal SpeechSynthesis where
  toJSVal = return . unSpeechSynthesis
  {-# INLINE toJSVal #-}

instance FromJSVal SpeechSynthesis where
  fromJSVal v = fmap SpeechSynthesis <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SpeechSynthesis
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SpeechSynthesis where
  makeObject = makeObject . unSpeechSynthesis

instance IsGObject SpeechSynthesis where
  typeGType _ = gTypeSpeechSynthesis
  {-# INLINE typeGType #-}

noSpeechSynthesis :: Maybe SpeechSynthesis
noSpeechSynthesis = Nothing
{-# INLINE noSpeechSynthesis #-}

gTypeSpeechSynthesis :: JSM GType
gTypeSpeechSynthesis = GType . Object <$> jsg "SpeechSynthesis"

-- | Functions for this inteface are in "JSDOM.SpeechSynthesisEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisEvent Mozilla SpeechSynthesisEvent documentation>
newtype SpeechSynthesisEvent = SpeechSynthesisEvent { unSpeechSynthesisEvent :: JSVal }

instance PToJSVal SpeechSynthesisEvent where
  pToJSVal = unSpeechSynthesisEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal SpeechSynthesisEvent where
  pFromJSVal = SpeechSynthesisEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal SpeechSynthesisEvent where
  toJSVal = return . unSpeechSynthesisEvent
  {-# INLINE toJSVal #-}

instance FromJSVal SpeechSynthesisEvent where
  fromJSVal v = fmap SpeechSynthesisEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SpeechSynthesisEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SpeechSynthesisEvent where
  makeObject = makeObject . unSpeechSynthesisEvent

instance IsEvent SpeechSynthesisEvent
instance IsGObject SpeechSynthesisEvent where
  typeGType _ = gTypeSpeechSynthesisEvent
  {-# INLINE typeGType #-}

noSpeechSynthesisEvent :: Maybe SpeechSynthesisEvent
noSpeechSynthesisEvent = Nothing
{-# INLINE noSpeechSynthesisEvent #-}

gTypeSpeechSynthesisEvent :: JSM GType
gTypeSpeechSynthesisEvent = GType . Object <$> jsg "SpeechSynthesisEvent"

-- | Functions for this inteface are in "JSDOM.SpeechSynthesisUtterance".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisUtterance Mozilla SpeechSynthesisUtterance documentation>
newtype SpeechSynthesisUtterance = SpeechSynthesisUtterance { unSpeechSynthesisUtterance :: JSVal }

instance PToJSVal SpeechSynthesisUtterance where
  pToJSVal = unSpeechSynthesisUtterance
  {-# INLINE pToJSVal #-}

instance PFromJSVal SpeechSynthesisUtterance where
  pFromJSVal = SpeechSynthesisUtterance
  {-# INLINE pFromJSVal #-}

instance ToJSVal SpeechSynthesisUtterance where
  toJSVal = return . unSpeechSynthesisUtterance
  {-# INLINE toJSVal #-}

instance FromJSVal SpeechSynthesisUtterance where
  fromJSVal v = fmap SpeechSynthesisUtterance <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SpeechSynthesisUtterance
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SpeechSynthesisUtterance where
  makeObject = makeObject . unSpeechSynthesisUtterance

instance IsEventTarget SpeechSynthesisUtterance
instance IsGObject SpeechSynthesisUtterance where
  typeGType _ = gTypeSpeechSynthesisUtterance
  {-# INLINE typeGType #-}

noSpeechSynthesisUtterance :: Maybe SpeechSynthesisUtterance
noSpeechSynthesisUtterance = Nothing
{-# INLINE noSpeechSynthesisUtterance #-}

gTypeSpeechSynthesisUtterance :: JSM GType
gTypeSpeechSynthesisUtterance = GType . Object <$> jsg "SpeechSynthesisUtterance"

-- | Functions for this inteface are in "JSDOM.SpeechSynthesisVoice".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/SpeechSynthesisVoice Mozilla SpeechSynthesisVoice documentation>
newtype SpeechSynthesisVoice = SpeechSynthesisVoice { unSpeechSynthesisVoice :: JSVal }

instance PToJSVal SpeechSynthesisVoice where
  pToJSVal = unSpeechSynthesisVoice
  {-# INLINE pToJSVal #-}

instance PFromJSVal SpeechSynthesisVoice where
  pFromJSVal = SpeechSynthesisVoice
  {-# INLINE pFromJSVal #-}

instance ToJSVal SpeechSynthesisVoice where
  toJSVal = return . unSpeechSynthesisVoice
  {-# INLINE toJSVal #-}

instance FromJSVal SpeechSynthesisVoice where
  fromJSVal v = fmap SpeechSynthesisVoice <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SpeechSynthesisVoice
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SpeechSynthesisVoice where
  makeObject = makeObject . unSpeechSynthesisVoice

instance IsGObject SpeechSynthesisVoice where
  typeGType _ = gTypeSpeechSynthesisVoice
  {-# INLINE typeGType #-}

noSpeechSynthesisVoice :: Maybe SpeechSynthesisVoice
noSpeechSynthesisVoice = Nothing
{-# INLINE noSpeechSynthesisVoice #-}

gTypeSpeechSynthesisVoice :: JSM GType
gTypeSpeechSynthesisVoice = GType . Object <$> jsg "SpeechSynthesisVoice"

-- | Functions for this inteface are in "JSDOM.StaticRange".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StaticRange Mozilla StaticRange documentation>
newtype StaticRange = StaticRange { unStaticRange :: JSVal }

instance PToJSVal StaticRange where
  pToJSVal = unStaticRange
  {-# INLINE pToJSVal #-}

instance PFromJSVal StaticRange where
  pFromJSVal = StaticRange
  {-# INLINE pFromJSVal #-}

instance ToJSVal StaticRange where
  toJSVal = return . unStaticRange
  {-# INLINE toJSVal #-}

instance FromJSVal StaticRange where
  fromJSVal v = fmap StaticRange <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StaticRange
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StaticRange where
  makeObject = makeObject . unStaticRange

instance IsGObject StaticRange where
  typeGType _ = gTypeStaticRange
  {-# INLINE typeGType #-}

noStaticRange :: Maybe StaticRange
noStaticRange = Nothing
{-# INLINE noStaticRange #-}

gTypeStaticRange :: JSM GType
gTypeStaticRange = GType . Object <$> jsg "StaticRange"

-- | Functions for this inteface are in "JSDOM.Storage".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Storage Mozilla Storage documentation>
newtype Storage = Storage { unStorage :: JSVal }

instance PToJSVal Storage where
  pToJSVal = unStorage
  {-# INLINE pToJSVal #-}

instance PFromJSVal Storage where
  pFromJSVal = Storage
  {-# INLINE pFromJSVal #-}

instance ToJSVal Storage where
  toJSVal = return . unStorage
  {-# INLINE toJSVal #-}

instance FromJSVal Storage where
  fromJSVal v = fmap Storage <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Storage
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Storage where
  makeObject = makeObject . unStorage

instance IsGObject Storage where
  typeGType _ = gTypeStorage
  {-# INLINE typeGType #-}

noStorage :: Maybe Storage
noStorage = Nothing
{-# INLINE noStorage #-}

gTypeStorage :: JSM GType
gTypeStorage = GType . Object <$> jsg "Storage"

-- | Functions for this inteface are in "JSDOM.StorageEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StorageEvent Mozilla StorageEvent documentation>
newtype StorageEvent = StorageEvent { unStorageEvent :: JSVal }

instance PToJSVal StorageEvent where
  pToJSVal = unStorageEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal StorageEvent where
  pFromJSVal = StorageEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal StorageEvent where
  toJSVal = return . unStorageEvent
  {-# INLINE toJSVal #-}

instance FromJSVal StorageEvent where
  fromJSVal v = fmap StorageEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StorageEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StorageEvent where
  makeObject = makeObject . unStorageEvent

instance IsEvent StorageEvent
instance IsGObject StorageEvent where
  typeGType _ = gTypeStorageEvent
  {-# INLINE typeGType #-}

noStorageEvent :: Maybe StorageEvent
noStorageEvent = Nothing
{-# INLINE noStorageEvent #-}

gTypeStorageEvent :: JSM GType
gTypeStorageEvent = GType . Object <$> jsg "StorageEvent"

-- | Functions for this inteface are in "JSDOM.StorageEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StorageEventInit Mozilla StorageEventInit documentation>
newtype StorageEventInit = StorageEventInit { unStorageEventInit :: JSVal }

instance PToJSVal StorageEventInit where
  pToJSVal = unStorageEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal StorageEventInit where
  pFromJSVal = StorageEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal StorageEventInit where
  toJSVal = return . unStorageEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal StorageEventInit where
  fromJSVal v = fmap StorageEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StorageEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StorageEventInit where
  makeObject = makeObject . unStorageEventInit

instance IsEventInit StorageEventInit
instance IsGObject StorageEventInit where
  typeGType _ = gTypeStorageEventInit
  {-# INLINE typeGType #-}

noStorageEventInit :: Maybe StorageEventInit
noStorageEventInit = Nothing
{-# INLINE noStorageEventInit #-}

gTypeStorageEventInit :: JSM GType
gTypeStorageEventInit = GType . Object <$> jsg "StorageEventInit"

-- | Functions for this inteface are in "JSDOM.StorageInfo".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StorageInfo Mozilla StorageInfo documentation>
newtype StorageInfo = StorageInfo { unStorageInfo :: JSVal }

instance PToJSVal StorageInfo where
  pToJSVal = unStorageInfo
  {-# INLINE pToJSVal #-}

instance PFromJSVal StorageInfo where
  pFromJSVal = StorageInfo
  {-# INLINE pFromJSVal #-}

instance ToJSVal StorageInfo where
  toJSVal = return . unStorageInfo
  {-# INLINE toJSVal #-}

instance FromJSVal StorageInfo where
  fromJSVal v = fmap StorageInfo <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StorageInfo
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StorageInfo where
  makeObject = makeObject . unStorageInfo

instance IsGObject StorageInfo where
  typeGType _ = gTypeStorageInfo
  {-# INLINE typeGType #-}

noStorageInfo :: Maybe StorageInfo
noStorageInfo = Nothing
{-# INLINE noStorageInfo #-}

gTypeStorageInfo :: JSM GType
gTypeStorageInfo = GType . Object <$> jsg "StorageInfo"

-- | Functions for this inteface are in "JSDOM.StorageQuota".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StorageQuota Mozilla StorageQuota documentation>
newtype StorageQuota = StorageQuota { unStorageQuota :: JSVal }

instance PToJSVal StorageQuota where
  pToJSVal = unStorageQuota
  {-# INLINE pToJSVal #-}

instance PFromJSVal StorageQuota where
  pFromJSVal = StorageQuota
  {-# INLINE pFromJSVal #-}

instance ToJSVal StorageQuota where
  toJSVal = return . unStorageQuota
  {-# INLINE toJSVal #-}

instance FromJSVal StorageQuota where
  fromJSVal v = fmap StorageQuota <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StorageQuota
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StorageQuota where
  makeObject = makeObject . unStorageQuota

instance IsGObject StorageQuota where
  typeGType _ = gTypeStorageQuota
  {-# INLINE typeGType #-}

noStorageQuota :: Maybe StorageQuota
noStorageQuota = Nothing
{-# INLINE noStorageQuota #-}

gTypeStorageQuota :: JSM GType
gTypeStorageQuota = GType . Object <$> jsg "StorageQuota"

-- | Functions for this inteface are in "JSDOM.StyleMedia".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StyleMedia Mozilla StyleMedia documentation>
newtype StyleMedia = StyleMedia { unStyleMedia :: JSVal }

instance PToJSVal StyleMedia where
  pToJSVal = unStyleMedia
  {-# INLINE pToJSVal #-}

instance PFromJSVal StyleMedia where
  pFromJSVal = StyleMedia
  {-# INLINE pFromJSVal #-}

instance ToJSVal StyleMedia where
  toJSVal = return . unStyleMedia
  {-# INLINE toJSVal #-}

instance FromJSVal StyleMedia where
  fromJSVal v = fmap StyleMedia <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StyleMedia
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StyleMedia where
  makeObject = makeObject . unStyleMedia

instance IsGObject StyleMedia where
  typeGType _ = gTypeStyleMedia
  {-# INLINE typeGType #-}

noStyleMedia :: Maybe StyleMedia
noStyleMedia = Nothing
{-# INLINE noStyleMedia #-}

gTypeStyleMedia :: JSM GType
gTypeStyleMedia = GType . Object <$> jsg "StyleMedia"

-- | Functions for this inteface are in "JSDOM.StyleSheetList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/StyleSheetList Mozilla StyleSheetList documentation>
newtype StyleSheetList = StyleSheetList { unStyleSheetList :: JSVal }

instance PToJSVal StyleSheetList where
  pToJSVal = unStyleSheetList
  {-# INLINE pToJSVal #-}

instance PFromJSVal StyleSheetList where
  pFromJSVal = StyleSheetList
  {-# INLINE pFromJSVal #-}

instance ToJSVal StyleSheetList where
  toJSVal = return . unStyleSheetList
  {-# INLINE toJSVal #-}

instance FromJSVal StyleSheetList where
  fromJSVal v = fmap StyleSheetList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . StyleSheetList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject StyleSheetList where
  makeObject = makeObject . unStyleSheetList

instance IsGObject StyleSheetList where
  typeGType _ = gTypeStyleSheetList
  {-# INLINE typeGType #-}

noStyleSheetList :: Maybe StyleSheetList
noStyleSheetList = Nothing
{-# INLINE noStyleSheetList #-}

gTypeStyleSheetList :: JSM GType
gTypeStyleSheetList = GType . Object <$> jsg "StyleSheetList"

-- | Functions for this inteface are in "JSDOM.SubtleCrypto".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitSubtleCrypto Mozilla WebKitSubtleCrypto documentation>
newtype SubtleCrypto = SubtleCrypto { unSubtleCrypto :: JSVal }

instance PToJSVal SubtleCrypto where
  pToJSVal = unSubtleCrypto
  {-# INLINE pToJSVal #-}

instance PFromJSVal SubtleCrypto where
  pFromJSVal = SubtleCrypto
  {-# INLINE pFromJSVal #-}

instance ToJSVal SubtleCrypto where
  toJSVal = return . unSubtleCrypto
  {-# INLINE toJSVal #-}

instance FromJSVal SubtleCrypto where
  fromJSVal v = fmap SubtleCrypto <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . SubtleCrypto
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject SubtleCrypto where
  makeObject = makeObject . unSubtleCrypto

instance IsGObject SubtleCrypto where
  typeGType _ = gTypeSubtleCrypto
  {-# INLINE typeGType #-}

noSubtleCrypto :: Maybe SubtleCrypto
noSubtleCrypto = Nothing
{-# INLINE noSubtleCrypto #-}

gTypeSubtleCrypto :: JSM GType
gTypeSubtleCrypto = GType . Object <$> jsg "WebKitSubtleCrypto"


-- | Functions for this inteface are in "JSDOM.TextDecodeOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextDecodeOptions Mozilla TextDecodeOptions documentation>
newtype TextDecodeOptions = TextDecodeOptions { unTextDecodeOptions :: JSVal }

instance PToJSVal TextDecodeOptions where
  pToJSVal = unTextDecodeOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextDecodeOptions where
  pFromJSVal = TextDecodeOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextDecodeOptions where
  toJSVal = return . unTextDecodeOptions
  {-# INLINE toJSVal #-}

instance FromJSVal TextDecodeOptions where
  fromJSVal v = fmap TextDecodeOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextDecodeOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextDecodeOptions where
  makeObject = makeObject . unTextDecodeOptions

instance IsGObject TextDecodeOptions where
  typeGType _ = gTypeTextDecodeOptions
  {-# INLINE typeGType #-}

noTextDecodeOptions :: Maybe TextDecodeOptions
noTextDecodeOptions = Nothing
{-# INLINE noTextDecodeOptions #-}

gTypeTextDecodeOptions :: JSM GType
gTypeTextDecodeOptions = GType . Object <$> jsg "TextDecodeOptions"

-- | Functions for this inteface are in "JSDOM.TextDecoder".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextDecoder Mozilla TextDecoder documentation>
newtype TextDecoder = TextDecoder { unTextDecoder :: JSVal }

instance PToJSVal TextDecoder where
  pToJSVal = unTextDecoder
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextDecoder where
  pFromJSVal = TextDecoder
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextDecoder where
  toJSVal = return . unTextDecoder
  {-# INLINE toJSVal #-}

instance FromJSVal TextDecoder where
  fromJSVal v = fmap TextDecoder <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextDecoder
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextDecoder where
  makeObject = makeObject . unTextDecoder

instance IsGObject TextDecoder where
  typeGType _ = gTypeTextDecoder
  {-# INLINE typeGType #-}

noTextDecoder :: Maybe TextDecoder
noTextDecoder = Nothing
{-# INLINE noTextDecoder #-}

gTypeTextDecoder :: JSM GType
gTypeTextDecoder = GType . Object <$> jsg "TextDecoder"

-- | Functions for this inteface are in "JSDOM.TextDecoderOptions".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextDecoderOptions Mozilla TextDecoderOptions documentation>
newtype TextDecoderOptions = TextDecoderOptions { unTextDecoderOptions :: JSVal }

instance PToJSVal TextDecoderOptions where
  pToJSVal = unTextDecoderOptions
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextDecoderOptions where
  pFromJSVal = TextDecoderOptions
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextDecoderOptions where
  toJSVal = return . unTextDecoderOptions
  {-# INLINE toJSVal #-}

instance FromJSVal TextDecoderOptions where
  fromJSVal v = fmap TextDecoderOptions <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextDecoderOptions
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextDecoderOptions where
  makeObject = makeObject . unTextDecoderOptions

instance IsGObject TextDecoderOptions where
  typeGType _ = gTypeTextDecoderOptions
  {-# INLINE typeGType #-}

noTextDecoderOptions :: Maybe TextDecoderOptions
noTextDecoderOptions = Nothing
{-# INLINE noTextDecoderOptions #-}

gTypeTextDecoderOptions :: JSM GType
gTypeTextDecoderOptions = GType . Object <$> jsg "TextDecoderOptions"

-- | Functions for this inteface are in "JSDOM.TextEncoder".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextEncoder Mozilla TextEncoder documentation>
newtype TextEncoder = TextEncoder { unTextEncoder :: JSVal }

instance PToJSVal TextEncoder where
  pToJSVal = unTextEncoder
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextEncoder where
  pFromJSVal = TextEncoder
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextEncoder where
  toJSVal = return . unTextEncoder
  {-# INLINE toJSVal #-}

instance FromJSVal TextEncoder where
  fromJSVal v = fmap TextEncoder <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextEncoder
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextEncoder where
  makeObject = makeObject . unTextEncoder

instance IsGObject TextEncoder where
  typeGType _ = gTypeTextEncoder
  {-# INLINE typeGType #-}

noTextEncoder :: Maybe TextEncoder
noTextEncoder = Nothing
{-# INLINE noTextEncoder #-}

gTypeTextEncoder :: JSM GType
gTypeTextEncoder = GType . Object <$> jsg "TextEncoder"

-- | Functions for this inteface are in "JSDOM.TextEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextEvent Mozilla TextEvent documentation>
newtype TextEvent = TextEvent { unTextEvent :: JSVal }

instance PToJSVal TextEvent where
  pToJSVal = unTextEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextEvent where
  pFromJSVal = TextEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextEvent where
  toJSVal = return . unTextEvent
  {-# INLINE toJSVal #-}

instance FromJSVal TextEvent where
  fromJSVal v = fmap TextEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextEvent where
  makeObject = makeObject . unTextEvent

instance IsUIEvent TextEvent
instance IsEvent TextEvent
instance IsGObject TextEvent where
  typeGType _ = gTypeTextEvent
  {-# INLINE typeGType #-}

noTextEvent :: Maybe TextEvent
noTextEvent = Nothing
{-# INLINE noTextEvent #-}

gTypeTextEvent :: JSM GType
gTypeTextEvent = GType . Object <$> jsg "TextEvent"

-- | Functions for this inteface are in "JSDOM.TextMetrics".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextMetrics Mozilla TextMetrics documentation>
newtype TextMetrics = TextMetrics { unTextMetrics :: JSVal }

instance PToJSVal TextMetrics where
  pToJSVal = unTextMetrics
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextMetrics where
  pFromJSVal = TextMetrics
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextMetrics where
  toJSVal = return . unTextMetrics
  {-# INLINE toJSVal #-}

instance FromJSVal TextMetrics where
  fromJSVal v = fmap TextMetrics <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextMetrics
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextMetrics where
  makeObject = makeObject . unTextMetrics

instance IsGObject TextMetrics where
  typeGType _ = gTypeTextMetrics
  {-# INLINE typeGType #-}

noTextMetrics :: Maybe TextMetrics
noTextMetrics = Nothing
{-# INLINE noTextMetrics #-}

gTypeTextMetrics :: JSM GType
gTypeTextMetrics = GType . Object <$> jsg "TextMetrics"

-- | Functions for this inteface are in "JSDOM.TextTrackCueList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextTrackCueList Mozilla TextTrackCueList documentation>
newtype TextTrackCueList = TextTrackCueList { unTextTrackCueList :: JSVal }

instance PToJSVal TextTrackCueList where
  pToJSVal = unTextTrackCueList
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextTrackCueList where
  pFromJSVal = TextTrackCueList
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextTrackCueList where
  toJSVal = return . unTextTrackCueList
  {-# INLINE toJSVal #-}

instance FromJSVal TextTrackCueList where
  fromJSVal v = fmap TextTrackCueList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextTrackCueList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextTrackCueList where
  makeObject = makeObject . unTextTrackCueList

instance IsGObject TextTrackCueList where
  typeGType _ = gTypeTextTrackCueList
  {-# INLINE typeGType #-}

noTextTrackCueList :: Maybe TextTrackCueList
noTextTrackCueList = Nothing
{-# INLINE noTextTrackCueList #-}

gTypeTextTrackCueList :: JSM GType
gTypeTextTrackCueList = GType . Object <$> jsg "TextTrackCueList"

-- | Functions for this inteface are in "JSDOM.TextTrackList".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TextTrackList Mozilla TextTrackList documentation>
newtype TextTrackList = TextTrackList { unTextTrackList :: JSVal }

instance PToJSVal TextTrackList where
  pToJSVal = unTextTrackList
  {-# INLINE pToJSVal #-}

instance PFromJSVal TextTrackList where
  pFromJSVal = TextTrackList
  {-# INLINE pFromJSVal #-}

instance ToJSVal TextTrackList where
  toJSVal = return . unTextTrackList
  {-# INLINE toJSVal #-}

instance FromJSVal TextTrackList where
  fromJSVal v = fmap TextTrackList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TextTrackList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TextTrackList where
  makeObject = makeObject . unTextTrackList

instance IsEventTarget TextTrackList
instance IsGObject TextTrackList where
  typeGType _ = gTypeTextTrackList
  {-# INLINE typeGType #-}

noTextTrackList :: Maybe TextTrackList
noTextTrackList = Nothing
{-# INLINE noTextTrackList #-}

gTypeTextTrackList :: JSM GType
gTypeTextTrackList = GType . Object <$> jsg "TextTrackList"

-- | Functions for this inteface are in "JSDOM.TimeRanges".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TimeRanges Mozilla TimeRanges documentation>
newtype TimeRanges = TimeRanges { unTimeRanges :: JSVal }

instance PToJSVal TimeRanges where
  pToJSVal = unTimeRanges
  {-# INLINE pToJSVal #-}

instance PFromJSVal TimeRanges where
  pFromJSVal = TimeRanges
  {-# INLINE pFromJSVal #-}

instance ToJSVal TimeRanges where
  toJSVal = return . unTimeRanges
  {-# INLINE toJSVal #-}

instance FromJSVal TimeRanges where
  fromJSVal v = fmap TimeRanges <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TimeRanges
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TimeRanges where
  makeObject = makeObject . unTimeRanges

instance IsGObject TimeRanges where
  typeGType _ = gTypeTimeRanges
  {-# INLINE typeGType #-}

noTimeRanges :: Maybe TimeRanges
noTimeRanges = Nothing
{-# INLINE noTimeRanges #-}

gTypeTimeRanges :: JSM GType
gTypeTimeRanges = GType . Object <$> jsg "TimeRanges"

-- | Functions for this inteface are in "JSDOM.Touch".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Touch Mozilla Touch documentation>
newtype Touch = Touch { unTouch :: JSVal }

instance PToJSVal Touch where
  pToJSVal = unTouch
  {-# INLINE pToJSVal #-}

instance PFromJSVal Touch where
  pFromJSVal = Touch
  {-# INLINE pFromJSVal #-}

instance ToJSVal Touch where
  toJSVal = return . unTouch
  {-# INLINE toJSVal #-}

instance FromJSVal Touch where
  fromJSVal v = fmap Touch <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Touch
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Touch where
  makeObject = makeObject . unTouch

instance IsGObject Touch where
  typeGType _ = gTypeTouch
  {-# INLINE typeGType #-}

noTouch :: Maybe Touch
noTouch = Nothing
{-# INLINE noTouch #-}

gTypeTouch :: JSM GType
gTypeTouch = GType . Object <$> jsg "Touch"

-- | Functions for this inteface are in "JSDOM.TouchEvent".
-- Base interface functions are in:
--
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent Mozilla TouchEvent documentation>
newtype TouchEvent = TouchEvent { unTouchEvent :: JSVal }

instance PToJSVal TouchEvent where
  pToJSVal = unTouchEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal TouchEvent where
  pFromJSVal = TouchEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal TouchEvent where
  toJSVal = return . unTouchEvent
  {-# INLINE toJSVal #-}

instance FromJSVal TouchEvent where
  fromJSVal v = fmap TouchEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TouchEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TouchEvent where
  makeObject = makeObject . unTouchEvent

instance IsUIEvent TouchEvent
instance IsEvent TouchEvent
instance IsGObject TouchEvent where
  typeGType _ = gTypeTouchEvent
  {-# INLINE typeGType #-}

noTouchEvent :: Maybe TouchEvent
noTouchEvent = Nothing
{-# INLINE noTouchEvent #-}

gTypeTouchEvent :: JSM GType
gTypeTouchEvent = GType . Object <$> jsg "TouchEvent"

-- | Functions for this inteface are in "JSDOM.TouchEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TouchEventInit Mozilla TouchEventInit documentation>
newtype TouchEventInit = TouchEventInit { unTouchEventInit :: JSVal }

instance PToJSVal TouchEventInit where
  pToJSVal = unTouchEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal TouchEventInit where
  pFromJSVal = TouchEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal TouchEventInit where
  toJSVal = return . unTouchEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal TouchEventInit where
  fromJSVal v = fmap TouchEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TouchEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TouchEventInit where
  makeObject = makeObject . unTouchEventInit

instance IsUIEventInit TouchEventInit
instance IsEventInit TouchEventInit
instance IsGObject TouchEventInit where
  typeGType _ = gTypeTouchEventInit
  {-# INLINE typeGType #-}

noTouchEventInit :: Maybe TouchEventInit
noTouchEventInit = Nothing
{-# INLINE noTouchEventInit #-}

gTypeTouchEventInit :: JSM GType
gTypeTouchEventInit = GType . Object <$> jsg "TouchEventInit"

-- | Functions for this inteface are in "JSDOM.TouchList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TouchList Mozilla TouchList documentation>
newtype TouchList = TouchList { unTouchList :: JSVal }

instance PToJSVal TouchList where
  pToJSVal = unTouchList
  {-# INLINE pToJSVal #-}

instance PFromJSVal TouchList where
  pFromJSVal = TouchList
  {-# INLINE pFromJSVal #-}

instance ToJSVal TouchList where
  toJSVal = return . unTouchList
  {-# INLINE toJSVal #-}

instance FromJSVal TouchList where
  fromJSVal v = fmap TouchList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TouchList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TouchList where
  makeObject = makeObject . unTouchList

instance IsGObject TouchList where
  typeGType _ = gTypeTouchList
  {-# INLINE typeGType #-}

noTouchList :: Maybe TouchList
noTouchList = Nothing
{-# INLINE noTouchList #-}

gTypeTouchList :: JSM GType
gTypeTouchList = GType . Object <$> jsg "TouchList"

-- | Functions for this inteface are in "JSDOM.TrackEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TrackEvent Mozilla TrackEvent documentation>
newtype TrackEvent = TrackEvent { unTrackEvent :: JSVal }

instance PToJSVal TrackEvent where
  pToJSVal = unTrackEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal TrackEvent where
  pFromJSVal = TrackEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal TrackEvent where
  toJSVal = return . unTrackEvent
  {-# INLINE toJSVal #-}

instance FromJSVal TrackEvent where
  fromJSVal v = fmap TrackEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TrackEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TrackEvent where
  makeObject = makeObject . unTrackEvent

instance IsEvent TrackEvent
instance IsGObject TrackEvent where
  typeGType _ = gTypeTrackEvent
  {-# INLINE typeGType #-}

noTrackEvent :: Maybe TrackEvent
noTrackEvent = Nothing
{-# INLINE noTrackEvent #-}

gTypeTrackEvent :: JSM GType
gTypeTrackEvent = GType . Object <$> jsg "TrackEvent"

-- | Functions for this inteface are in "JSDOM.TrackEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TrackEventInit Mozilla TrackEventInit documentation>
newtype TrackEventInit = TrackEventInit { unTrackEventInit :: JSVal }

instance PToJSVal TrackEventInit where
  pToJSVal = unTrackEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal TrackEventInit where
  pFromJSVal = TrackEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal TrackEventInit where
  toJSVal = return . unTrackEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal TrackEventInit where
  fromJSVal v = fmap TrackEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TrackEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TrackEventInit where
  makeObject = makeObject . unTrackEventInit

instance IsEventInit TrackEventInit
instance IsGObject TrackEventInit where
  typeGType _ = gTypeTrackEventInit
  {-# INLINE typeGType #-}

noTrackEventInit :: Maybe TrackEventInit
noTrackEventInit = Nothing
{-# INLINE noTrackEventInit #-}

gTypeTrackEventInit :: JSM GType
gTypeTrackEventInit = GType . Object <$> jsg "TrackEventInit"

-- | Functions for this inteface are in "JSDOM.TransitionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TransitionEvent Mozilla TransitionEvent documentation>
newtype TransitionEvent = TransitionEvent { unTransitionEvent :: JSVal }

instance PToJSVal TransitionEvent where
  pToJSVal = unTransitionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal TransitionEvent where
  pFromJSVal = TransitionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal TransitionEvent where
  toJSVal = return . unTransitionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal TransitionEvent where
  fromJSVal v = fmap TransitionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TransitionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TransitionEvent where
  makeObject = makeObject . unTransitionEvent

instance IsEvent TransitionEvent
instance IsGObject TransitionEvent where
  typeGType _ = gTypeTransitionEvent
  {-# INLINE typeGType #-}

noTransitionEvent :: Maybe TransitionEvent
noTransitionEvent = Nothing
{-# INLINE noTransitionEvent #-}

gTypeTransitionEvent :: JSM GType
gTypeTransitionEvent = GType . Object <$> jsg "TransitionEvent"

-- | Functions for this inteface are in "JSDOM.TransitionEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TransitionEventInit Mozilla TransitionEventInit documentation>
newtype TransitionEventInit = TransitionEventInit { unTransitionEventInit :: JSVal }

instance PToJSVal TransitionEventInit where
  pToJSVal = unTransitionEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal TransitionEventInit where
  pFromJSVal = TransitionEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal TransitionEventInit where
  toJSVal = return . unTransitionEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal TransitionEventInit where
  fromJSVal v = fmap TransitionEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TransitionEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TransitionEventInit where
  makeObject = makeObject . unTransitionEventInit

instance IsEventInit TransitionEventInit
instance IsGObject TransitionEventInit where
  typeGType _ = gTypeTransitionEventInit
  {-# INLINE typeGType #-}

noTransitionEventInit :: Maybe TransitionEventInit
noTransitionEventInit = Nothing
{-# INLINE noTransitionEventInit #-}

gTypeTransitionEventInit :: JSM GType
gTypeTransitionEventInit = GType . Object <$> jsg "TransitionEventInit"

-- | Functions for this inteface are in "JSDOM.TreeWalker".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/TreeWalker Mozilla TreeWalker documentation>
newtype TreeWalker = TreeWalker { unTreeWalker :: JSVal }

instance PToJSVal TreeWalker where
  pToJSVal = unTreeWalker
  {-# INLINE pToJSVal #-}

instance PFromJSVal TreeWalker where
  pFromJSVal = TreeWalker
  {-# INLINE pFromJSVal #-}

instance ToJSVal TreeWalker where
  toJSVal = return . unTreeWalker
  {-# INLINE toJSVal #-}

instance FromJSVal TreeWalker where
  fromJSVal v = fmap TreeWalker <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . TreeWalker
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject TreeWalker where
  makeObject = makeObject . unTreeWalker

instance IsGObject TreeWalker where
  typeGType _ = gTypeTreeWalker
  {-# INLINE typeGType #-}

noTreeWalker :: Maybe TreeWalker
noTreeWalker = Nothing
{-# INLINE noTreeWalker #-}

gTypeTreeWalker :: JSM GType
gTypeTreeWalker = GType . Object <$> jsg "TreeWalker"


-- | Functions for this inteface are in "JSDOM.URL".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/URL Mozilla URL documentation>
newtype URL = URL { unURL :: JSVal }

instance PToJSVal URL where
  pToJSVal = unURL
  {-# INLINE pToJSVal #-}

instance PFromJSVal URL where
  pFromJSVal = URL
  {-# INLINE pFromJSVal #-}

instance ToJSVal URL where
  toJSVal = return . unURL
  {-# INLINE toJSVal #-}

instance FromJSVal URL where
  fromJSVal v = fmap URL <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . URL
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject URL where
  makeObject = makeObject . unURL

instance IsGObject URL where
  typeGType _ = gTypeURL
  {-# INLINE typeGType #-}

noURL :: Maybe URL
noURL = Nothing
{-# INLINE noURL #-}

gTypeURL :: JSM GType
gTypeURL = GType . Object <$> jsg "URL"

-- | Functions for this inteface are in "JSDOM.UserMessageHandler".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/UserMessageHandler Mozilla UserMessageHandler documentation>
newtype UserMessageHandler = UserMessageHandler { unUserMessageHandler :: JSVal }

instance PToJSVal UserMessageHandler where
  pToJSVal = unUserMessageHandler
  {-# INLINE pToJSVal #-}

instance PFromJSVal UserMessageHandler where
  pFromJSVal = UserMessageHandler
  {-# INLINE pFromJSVal #-}

instance ToJSVal UserMessageHandler where
  toJSVal = return . unUserMessageHandler
  {-# INLINE toJSVal #-}

instance FromJSVal UserMessageHandler where
  fromJSVal v = fmap UserMessageHandler <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . UserMessageHandler
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject UserMessageHandler where
  makeObject = makeObject . unUserMessageHandler

instance IsGObject UserMessageHandler where
  typeGType _ = gTypeUserMessageHandler
  {-# INLINE typeGType #-}

noUserMessageHandler :: Maybe UserMessageHandler
noUserMessageHandler = Nothing
{-# INLINE noUserMessageHandler #-}

gTypeUserMessageHandler :: JSM GType
gTypeUserMessageHandler = GType . Object <$> jsg "UserMessageHandler"

-- | Functions for this inteface are in "JSDOM.UserMessageHandlersNamespace".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/UserMessageHandlersNamespace Mozilla UserMessageHandlersNamespace documentation>
newtype UserMessageHandlersNamespace = UserMessageHandlersNamespace { unUserMessageHandlersNamespace :: JSVal }

instance PToJSVal UserMessageHandlersNamespace where
  pToJSVal = unUserMessageHandlersNamespace
  {-# INLINE pToJSVal #-}

instance PFromJSVal UserMessageHandlersNamespace where
  pFromJSVal = UserMessageHandlersNamespace
  {-# INLINE pFromJSVal #-}

instance ToJSVal UserMessageHandlersNamespace where
  toJSVal = return . unUserMessageHandlersNamespace
  {-# INLINE toJSVal #-}

instance FromJSVal UserMessageHandlersNamespace where
  fromJSVal v = fmap UserMessageHandlersNamespace <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . UserMessageHandlersNamespace
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject UserMessageHandlersNamespace where
  makeObject = makeObject . unUserMessageHandlersNamespace

instance IsGObject UserMessageHandlersNamespace where
  typeGType _ = gTypeUserMessageHandlersNamespace
  {-# INLINE typeGType #-}

noUserMessageHandlersNamespace :: Maybe UserMessageHandlersNamespace
noUserMessageHandlersNamespace = Nothing
{-# INLINE noUserMessageHandlersNamespace #-}

gTypeUserMessageHandlersNamespace :: JSM GType
gTypeUserMessageHandlersNamespace = GType . Object <$> jsg "UserMessageHandlersNamespace"

-- | Functions for this inteface are in "JSDOM.VTTCue".
-- Base interface functions are in:
--
--     * "JSDOM.TextTrackCue"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VTTCue Mozilla VTTCue documentation>
newtype VTTCue = VTTCue { unVTTCue :: JSVal }

instance PToJSVal VTTCue where
  pToJSVal = unVTTCue
  {-# INLINE pToJSVal #-}

instance PFromJSVal VTTCue where
  pFromJSVal = VTTCue
  {-# INLINE pFromJSVal #-}

instance ToJSVal VTTCue where
  toJSVal = return . unVTTCue
  {-# INLINE toJSVal #-}

instance FromJSVal VTTCue where
  fromJSVal v = fmap VTTCue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VTTCue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VTTCue where
  makeObject = makeObject . unVTTCue

instance IsTextTrackCue VTTCue
instance IsEventTarget VTTCue
instance IsGObject VTTCue where
  typeGType _ = gTypeVTTCue
  {-# INLINE typeGType #-}

noVTTCue :: Maybe VTTCue
noVTTCue = Nothing
{-# INLINE noVTTCue #-}

gTypeVTTCue :: JSM GType
gTypeVTTCue = GType . Object <$> jsg "VTTCue"

-- | Functions for this inteface are in "JSDOM.VTTRegion".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VTTRegion Mozilla VTTRegion documentation>
newtype VTTRegion = VTTRegion { unVTTRegion :: JSVal }

instance PToJSVal VTTRegion where
  pToJSVal = unVTTRegion
  {-# INLINE pToJSVal #-}

instance PFromJSVal VTTRegion where
  pFromJSVal = VTTRegion
  {-# INLINE pFromJSVal #-}

instance ToJSVal VTTRegion where
  toJSVal = return . unVTTRegion
  {-# INLINE toJSVal #-}

instance FromJSVal VTTRegion where
  fromJSVal v = fmap VTTRegion <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VTTRegion
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VTTRegion where
  makeObject = makeObject . unVTTRegion

instance IsGObject VTTRegion where
  typeGType _ = gTypeVTTRegion
  {-# INLINE typeGType #-}

noVTTRegion :: Maybe VTTRegion
noVTTRegion = Nothing
{-# INLINE noVTTRegion #-}

gTypeVTTRegion :: JSM GType
gTypeVTTRegion = GType . Object <$> jsg "VTTRegion"

-- | Functions for this inteface are in "JSDOM.VTTRegionList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VTTRegionList Mozilla VTTRegionList documentation>
newtype VTTRegionList = VTTRegionList { unVTTRegionList :: JSVal }

instance PToJSVal VTTRegionList where
  pToJSVal = unVTTRegionList
  {-# INLINE pToJSVal #-}

instance PFromJSVal VTTRegionList where
  pFromJSVal = VTTRegionList
  {-# INLINE pFromJSVal #-}

instance ToJSVal VTTRegionList where
  toJSVal = return . unVTTRegionList
  {-# INLINE toJSVal #-}

instance FromJSVal VTTRegionList where
  fromJSVal v = fmap VTTRegionList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VTTRegionList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VTTRegionList where
  makeObject = makeObject . unVTTRegionList

instance IsGObject VTTRegionList where
  typeGType _ = gTypeVTTRegionList
  {-# INLINE typeGType #-}

noVTTRegionList :: Maybe VTTRegionList
noVTTRegionList = Nothing
{-# INLINE noVTTRegionList #-}

gTypeVTTRegionList :: JSM GType
gTypeVTTRegionList = GType . Object <$> jsg "VTTRegionList"

-- | Functions for this inteface are in "JSDOM.ValidityState".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ValidityState Mozilla ValidityState documentation>
newtype ValidityState = ValidityState { unValidityState :: JSVal }

instance PToJSVal ValidityState where
  pToJSVal = unValidityState
  {-# INLINE pToJSVal #-}

instance PFromJSVal ValidityState where
  pFromJSVal = ValidityState
  {-# INLINE pFromJSVal #-}

instance ToJSVal ValidityState where
  toJSVal = return . unValidityState
  {-# INLINE toJSVal #-}

instance FromJSVal ValidityState where
  fromJSVal v = fmap ValidityState <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ValidityState
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ValidityState where
  makeObject = makeObject . unValidityState

instance IsGObject ValidityState where
  typeGType _ = gTypeValidityState
  {-# INLINE typeGType #-}

noValidityState :: Maybe ValidityState
noValidityState = Nothing
{-# INLINE noValidityState #-}

gTypeValidityState :: JSM GType
gTypeValidityState = GType . Object <$> jsg "ValidityState"

-- | Functions for this inteface are in "JSDOM.VideoPlaybackQuality".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VideoPlaybackQuality Mozilla VideoPlaybackQuality documentation>
newtype VideoPlaybackQuality = VideoPlaybackQuality { unVideoPlaybackQuality :: JSVal }

instance PToJSVal VideoPlaybackQuality where
  pToJSVal = unVideoPlaybackQuality
  {-# INLINE pToJSVal #-}

instance PFromJSVal VideoPlaybackQuality where
  pFromJSVal = VideoPlaybackQuality
  {-# INLINE pFromJSVal #-}

instance ToJSVal VideoPlaybackQuality where
  toJSVal = return . unVideoPlaybackQuality
  {-# INLINE toJSVal #-}

instance FromJSVal VideoPlaybackQuality where
  fromJSVal v = fmap VideoPlaybackQuality <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VideoPlaybackQuality
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VideoPlaybackQuality where
  makeObject = makeObject . unVideoPlaybackQuality

instance IsGObject VideoPlaybackQuality where
  typeGType _ = gTypeVideoPlaybackQuality
  {-# INLINE typeGType #-}

noVideoPlaybackQuality :: Maybe VideoPlaybackQuality
noVideoPlaybackQuality = Nothing
{-# INLINE noVideoPlaybackQuality #-}

gTypeVideoPlaybackQuality :: JSM GType
gTypeVideoPlaybackQuality = GType . Object <$> jsg "VideoPlaybackQuality"

-- | Functions for this inteface are in "JSDOM.VideoTrackList".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/VideoTrackList Mozilla VideoTrackList documentation>
newtype VideoTrackList = VideoTrackList { unVideoTrackList :: JSVal }

instance PToJSVal VideoTrackList where
  pToJSVal = unVideoTrackList
  {-# INLINE pToJSVal #-}

instance PFromJSVal VideoTrackList where
  pFromJSVal = VideoTrackList
  {-# INLINE pFromJSVal #-}

instance ToJSVal VideoTrackList where
  toJSVal = return . unVideoTrackList
  {-# INLINE toJSVal #-}

instance FromJSVal VideoTrackList where
  fromJSVal v = fmap VideoTrackList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . VideoTrackList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject VideoTrackList where
  makeObject = makeObject . unVideoTrackList

instance IsEventTarget VideoTrackList
instance IsGObject VideoTrackList where
  typeGType _ = gTypeVideoTrackList
  {-# INLINE typeGType #-}

noVideoTrackList :: Maybe VideoTrackList
noVideoTrackList = Nothing
{-# INLINE noVideoTrackList #-}

gTypeVideoTrackList :: JSM GType
gTypeVideoTrackList = GType . Object <$> jsg "VideoTrackList"

-- | Functions for this inteface are in "JSDOM.WaveShaperNode".
-- Base interface functions are in:
--
--     * "JSDOM.AudioNode"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WaveShaperNode Mozilla WaveShaperNode documentation>
newtype WaveShaperNode = WaveShaperNode { unWaveShaperNode :: JSVal }

instance PToJSVal WaveShaperNode where
  pToJSVal = unWaveShaperNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal WaveShaperNode where
  pFromJSVal = WaveShaperNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal WaveShaperNode where
  toJSVal = return . unWaveShaperNode
  {-# INLINE toJSVal #-}

instance FromJSVal WaveShaperNode where
  fromJSVal v = fmap WaveShaperNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WaveShaperNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WaveShaperNode where
  makeObject = makeObject . unWaveShaperNode

instance IsAudioNode WaveShaperNode
instance IsEventTarget WaveShaperNode
instance IsGObject WaveShaperNode where
  typeGType _ = gTypeWaveShaperNode
  {-# INLINE typeGType #-}

noWaveShaperNode :: Maybe WaveShaperNode
noWaveShaperNode = Nothing
{-# INLINE noWaveShaperNode #-}

gTypeWaveShaperNode :: JSM GType
gTypeWaveShaperNode = GType . Object <$> jsg "WaveShaperNode"

-- | Functions for this inteface are in "JSDOM.WebKitAnimationEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitAnimationEvent Mozilla WebKitAnimationEvent documentation>
newtype WebKitAnimationEvent = WebKitAnimationEvent { unWebKitAnimationEvent :: JSVal }

instance PToJSVal WebKitAnimationEvent where
  pToJSVal = unWebKitAnimationEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitAnimationEvent where
  pFromJSVal = WebKitAnimationEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitAnimationEvent where
  toJSVal = return . unWebKitAnimationEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitAnimationEvent where
  fromJSVal v = fmap WebKitAnimationEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitAnimationEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitAnimationEvent where
  makeObject = makeObject . unWebKitAnimationEvent

instance IsEvent WebKitAnimationEvent
instance IsGObject WebKitAnimationEvent where
  typeGType _ = gTypeWebKitAnimationEvent
  {-# INLINE typeGType #-}

noWebKitAnimationEvent :: Maybe WebKitAnimationEvent
noWebKitAnimationEvent = Nothing
{-# INLINE noWebKitAnimationEvent #-}

gTypeWebKitAnimationEvent :: JSM GType
gTypeWebKitAnimationEvent = GType . Object <$> jsg "WebKitAnimationEvent"

-- | Functions for this inteface are in "JSDOM.WebKitAnimationEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitAnimationEventInit Mozilla WebKitAnimationEventInit documentation>
newtype WebKitAnimationEventInit = WebKitAnimationEventInit { unWebKitAnimationEventInit :: JSVal }

instance PToJSVal WebKitAnimationEventInit where
  pToJSVal = unWebKitAnimationEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitAnimationEventInit where
  pFromJSVal = WebKitAnimationEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitAnimationEventInit where
  toJSVal = return . unWebKitAnimationEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitAnimationEventInit where
  fromJSVal v = fmap WebKitAnimationEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitAnimationEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitAnimationEventInit where
  makeObject = makeObject . unWebKitAnimationEventInit

instance IsEventInit WebKitAnimationEventInit
instance IsGObject WebKitAnimationEventInit where
  typeGType _ = gTypeWebKitAnimationEventInit
  {-# INLINE typeGType #-}

noWebKitAnimationEventInit :: Maybe WebKitAnimationEventInit
noWebKitAnimationEventInit = Nothing
{-# INLINE noWebKitAnimationEventInit #-}

gTypeWebKitAnimationEventInit :: JSM GType
gTypeWebKitAnimationEventInit = GType . Object <$> jsg "WebKitAnimationEventInit"

-- | Functions for this inteface are in "JSDOM.WebKitCSSMatrix".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitCSSMatrix Mozilla WebKitCSSMatrix documentation>
newtype WebKitCSSMatrix = WebKitCSSMatrix { unWebKitCSSMatrix :: JSVal }

instance PToJSVal WebKitCSSMatrix where
  pToJSVal = unWebKitCSSMatrix
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitCSSMatrix where
  pFromJSVal = WebKitCSSMatrix
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitCSSMatrix where
  toJSVal = return . unWebKitCSSMatrix
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitCSSMatrix where
  fromJSVal v = fmap WebKitCSSMatrix <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitCSSMatrix
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitCSSMatrix where
  makeObject = makeObject . unWebKitCSSMatrix

instance IsGObject WebKitCSSMatrix where
  typeGType _ = gTypeWebKitCSSMatrix
  {-# INLINE typeGType #-}

noWebKitCSSMatrix :: Maybe WebKitCSSMatrix
noWebKitCSSMatrix = Nothing
{-# INLINE noWebKitCSSMatrix #-}

gTypeWebKitCSSMatrix :: JSM GType
gTypeWebKitCSSMatrix = GType . Object <$> jsg "WebKitCSSMatrix"

-- | Functions for this inteface are in "JSDOM.WebKitCSSRegionRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitCSSRegionRule Mozilla WebKitCSSRegionRule documentation>
newtype WebKitCSSRegionRule = WebKitCSSRegionRule { unWebKitCSSRegionRule :: JSVal }

instance PToJSVal WebKitCSSRegionRule where
  pToJSVal = unWebKitCSSRegionRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitCSSRegionRule where
  pFromJSVal = WebKitCSSRegionRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitCSSRegionRule where
  toJSVal = return . unWebKitCSSRegionRule
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitCSSRegionRule where
  fromJSVal v = fmap WebKitCSSRegionRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitCSSRegionRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitCSSRegionRule where
  makeObject = makeObject . unWebKitCSSRegionRule

instance IsCSSRule WebKitCSSRegionRule
instance IsGObject WebKitCSSRegionRule where
  typeGType _ = gTypeWebKitCSSRegionRule
  {-# INLINE typeGType #-}

noWebKitCSSRegionRule :: Maybe WebKitCSSRegionRule
noWebKitCSSRegionRule = Nothing
{-# INLINE noWebKitCSSRegionRule #-}

gTypeWebKitCSSRegionRule :: JSM GType
gTypeWebKitCSSRegionRule = GType . Object <$> jsg "WebKitCSSRegionRule"

-- | Functions for this inteface are in "JSDOM.WebKitCSSViewportRule".
-- Base interface functions are in:
--
--     * "JSDOM.CSSRule"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitCSSViewportRule Mozilla WebKitCSSViewportRule documentation>
newtype WebKitCSSViewportRule = WebKitCSSViewportRule { unWebKitCSSViewportRule :: JSVal }

instance PToJSVal WebKitCSSViewportRule where
  pToJSVal = unWebKitCSSViewportRule
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitCSSViewportRule where
  pFromJSVal = WebKitCSSViewportRule
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitCSSViewportRule where
  toJSVal = return . unWebKitCSSViewportRule
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitCSSViewportRule where
  fromJSVal v = fmap WebKitCSSViewportRule <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitCSSViewportRule
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitCSSViewportRule where
  makeObject = makeObject . unWebKitCSSViewportRule

instance IsCSSRule WebKitCSSViewportRule
instance IsGObject WebKitCSSViewportRule where
  typeGType _ = gTypeWebKitCSSViewportRule
  {-# INLINE typeGType #-}

noWebKitCSSViewportRule :: Maybe WebKitCSSViewportRule
noWebKitCSSViewportRule = Nothing
{-# INLINE noWebKitCSSViewportRule #-}

gTypeWebKitCSSViewportRule :: JSM GType
gTypeWebKitCSSViewportRule = GType . Object <$> jsg "WebKitCSSViewportRule"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeyError".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyError Mozilla WebKitMediaKeyError documentation>
newtype WebKitMediaKeyError = WebKitMediaKeyError { unWebKitMediaKeyError :: JSVal }

instance PToJSVal WebKitMediaKeyError where
  pToJSVal = unWebKitMediaKeyError
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeyError where
  pFromJSVal = WebKitMediaKeyError
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeyError where
  toJSVal = return . unWebKitMediaKeyError
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeyError where
  fromJSVal v = fmap WebKitMediaKeyError <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeyError
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeyError where
  makeObject = makeObject . unWebKitMediaKeyError

instance IsGObject WebKitMediaKeyError where
  typeGType _ = gTypeWebKitMediaKeyError
  {-# INLINE typeGType #-}

noWebKitMediaKeyError :: Maybe WebKitMediaKeyError
noWebKitMediaKeyError = Nothing
{-# INLINE noWebKitMediaKeyError #-}

gTypeWebKitMediaKeyError :: JSM GType
gTypeWebKitMediaKeyError = GType . Object <$> jsg "WebKitMediaKeyError"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeyMessageEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyMessageEvent Mozilla WebKitMediaKeyMessageEvent documentation>
newtype WebKitMediaKeyMessageEvent = WebKitMediaKeyMessageEvent { unWebKitMediaKeyMessageEvent :: JSVal }

instance PToJSVal WebKitMediaKeyMessageEvent where
  pToJSVal = unWebKitMediaKeyMessageEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeyMessageEvent where
  pFromJSVal = WebKitMediaKeyMessageEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeyMessageEvent where
  toJSVal = return . unWebKitMediaKeyMessageEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeyMessageEvent where
  fromJSVal v = fmap WebKitMediaKeyMessageEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeyMessageEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeyMessageEvent where
  makeObject = makeObject . unWebKitMediaKeyMessageEvent

instance IsEvent WebKitMediaKeyMessageEvent
instance IsGObject WebKitMediaKeyMessageEvent where
  typeGType _ = gTypeWebKitMediaKeyMessageEvent
  {-# INLINE typeGType #-}

noWebKitMediaKeyMessageEvent :: Maybe WebKitMediaKeyMessageEvent
noWebKitMediaKeyMessageEvent = Nothing
{-# INLINE noWebKitMediaKeyMessageEvent #-}

gTypeWebKitMediaKeyMessageEvent :: JSM GType
gTypeWebKitMediaKeyMessageEvent = GType . Object <$> jsg "WebKitMediaKeyMessageEvent"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeyMessageEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyMessageEventInit Mozilla WebKitMediaKeyMessageEventInit documentation>
newtype WebKitMediaKeyMessageEventInit = WebKitMediaKeyMessageEventInit { unWebKitMediaKeyMessageEventInit :: JSVal }

instance PToJSVal WebKitMediaKeyMessageEventInit where
  pToJSVal = unWebKitMediaKeyMessageEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeyMessageEventInit where
  pFromJSVal = WebKitMediaKeyMessageEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeyMessageEventInit where
  toJSVal = return . unWebKitMediaKeyMessageEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeyMessageEventInit where
  fromJSVal v = fmap WebKitMediaKeyMessageEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeyMessageEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeyMessageEventInit where
  makeObject = makeObject . unWebKitMediaKeyMessageEventInit

instance IsEventInit WebKitMediaKeyMessageEventInit
instance IsGObject WebKitMediaKeyMessageEventInit where
  typeGType _ = gTypeWebKitMediaKeyMessageEventInit
  {-# INLINE typeGType #-}

noWebKitMediaKeyMessageEventInit :: Maybe WebKitMediaKeyMessageEventInit
noWebKitMediaKeyMessageEventInit = Nothing
{-# INLINE noWebKitMediaKeyMessageEventInit #-}

gTypeWebKitMediaKeyMessageEventInit :: JSM GType
gTypeWebKitMediaKeyMessageEventInit = GType . Object <$> jsg "WebKitMediaKeyMessageEventInit"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeyNeededEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyNeededEvent Mozilla WebKitMediaKeyNeededEvent documentation>
newtype WebKitMediaKeyNeededEvent = WebKitMediaKeyNeededEvent { unWebKitMediaKeyNeededEvent :: JSVal }

instance PToJSVal WebKitMediaKeyNeededEvent where
  pToJSVal = unWebKitMediaKeyNeededEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeyNeededEvent where
  pFromJSVal = WebKitMediaKeyNeededEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeyNeededEvent where
  toJSVal = return . unWebKitMediaKeyNeededEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeyNeededEvent where
  fromJSVal v = fmap WebKitMediaKeyNeededEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeyNeededEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeyNeededEvent where
  makeObject = makeObject . unWebKitMediaKeyNeededEvent

instance IsEvent WebKitMediaKeyNeededEvent
instance IsGObject WebKitMediaKeyNeededEvent where
  typeGType _ = gTypeWebKitMediaKeyNeededEvent
  {-# INLINE typeGType #-}

noWebKitMediaKeyNeededEvent :: Maybe WebKitMediaKeyNeededEvent
noWebKitMediaKeyNeededEvent = Nothing
{-# INLINE noWebKitMediaKeyNeededEvent #-}

gTypeWebKitMediaKeyNeededEvent :: JSM GType
gTypeWebKitMediaKeyNeededEvent = GType . Object <$> jsg "WebKitMediaKeyNeededEvent"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeyNeededEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeyNeededEventInit Mozilla WebKitMediaKeyNeededEventInit documentation>
newtype WebKitMediaKeyNeededEventInit = WebKitMediaKeyNeededEventInit { unWebKitMediaKeyNeededEventInit :: JSVal }

instance PToJSVal WebKitMediaKeyNeededEventInit where
  pToJSVal = unWebKitMediaKeyNeededEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeyNeededEventInit where
  pFromJSVal = WebKitMediaKeyNeededEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeyNeededEventInit where
  toJSVal = return . unWebKitMediaKeyNeededEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeyNeededEventInit where
  fromJSVal v = fmap WebKitMediaKeyNeededEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeyNeededEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeyNeededEventInit where
  makeObject = makeObject . unWebKitMediaKeyNeededEventInit

instance IsEventInit WebKitMediaKeyNeededEventInit
instance IsGObject WebKitMediaKeyNeededEventInit where
  typeGType _ = gTypeWebKitMediaKeyNeededEventInit
  {-# INLINE typeGType #-}

noWebKitMediaKeyNeededEventInit :: Maybe WebKitMediaKeyNeededEventInit
noWebKitMediaKeyNeededEventInit = Nothing
{-# INLINE noWebKitMediaKeyNeededEventInit #-}

gTypeWebKitMediaKeyNeededEventInit :: JSM GType
gTypeWebKitMediaKeyNeededEventInit = GType . Object <$> jsg "WebKitMediaKeyNeededEventInit"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeySession".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeySession Mozilla WebKitMediaKeySession documentation>
newtype WebKitMediaKeySession = WebKitMediaKeySession { unWebKitMediaKeySession :: JSVal }

instance PToJSVal WebKitMediaKeySession where
  pToJSVal = unWebKitMediaKeySession
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeySession where
  pFromJSVal = WebKitMediaKeySession
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeySession where
  toJSVal = return . unWebKitMediaKeySession
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeySession where
  fromJSVal v = fmap WebKitMediaKeySession <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeySession
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeySession where
  makeObject = makeObject . unWebKitMediaKeySession

instance IsEventTarget WebKitMediaKeySession
instance IsGObject WebKitMediaKeySession where
  typeGType _ = gTypeWebKitMediaKeySession
  {-# INLINE typeGType #-}

noWebKitMediaKeySession :: Maybe WebKitMediaKeySession
noWebKitMediaKeySession = Nothing
{-# INLINE noWebKitMediaKeySession #-}

gTypeWebKitMediaKeySession :: JSM GType
gTypeWebKitMediaKeySession = GType . Object <$> jsg "WebKitMediaKeySession"

-- | Functions for this inteface are in "JSDOM.WebKitMediaKeys".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitMediaKeys Mozilla WebKitMediaKeys documentation>
newtype WebKitMediaKeys = WebKitMediaKeys { unWebKitMediaKeys :: JSVal }

instance PToJSVal WebKitMediaKeys where
  pToJSVal = unWebKitMediaKeys
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitMediaKeys where
  pFromJSVal = WebKitMediaKeys
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitMediaKeys where
  toJSVal = return . unWebKitMediaKeys
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitMediaKeys where
  fromJSVal v = fmap WebKitMediaKeys <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitMediaKeys
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitMediaKeys where
  makeObject = makeObject . unWebKitMediaKeys

instance IsGObject WebKitMediaKeys where
  typeGType _ = gTypeWebKitMediaKeys
  {-# INLINE typeGType #-}

noWebKitMediaKeys :: Maybe WebKitMediaKeys
noWebKitMediaKeys = Nothing
{-# INLINE noWebKitMediaKeys #-}

gTypeWebKitMediaKeys :: JSM GType
gTypeWebKitMediaKeys = GType . Object <$> jsg "WebKitMediaKeys"

-- | Functions for this inteface are in "JSDOM.WebKitNamedFlow".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitNamedFlow Mozilla WebKitNamedFlow documentation>
newtype WebKitNamedFlow = WebKitNamedFlow { unWebKitNamedFlow :: JSVal }

instance PToJSVal WebKitNamedFlow where
  pToJSVal = unWebKitNamedFlow
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitNamedFlow where
  pFromJSVal = WebKitNamedFlow
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitNamedFlow where
  toJSVal = return . unWebKitNamedFlow
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitNamedFlow where
  fromJSVal v = fmap WebKitNamedFlow <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitNamedFlow
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitNamedFlow where
  makeObject = makeObject . unWebKitNamedFlow

instance IsEventTarget WebKitNamedFlow
instance IsGObject WebKitNamedFlow where
  typeGType _ = gTypeWebKitNamedFlow
  {-# INLINE typeGType #-}

noWebKitNamedFlow :: Maybe WebKitNamedFlow
noWebKitNamedFlow = Nothing
{-# INLINE noWebKitNamedFlow #-}

gTypeWebKitNamedFlow :: JSM GType
gTypeWebKitNamedFlow = GType . Object <$> jsg "WebKitNamedFlow"

-- | Functions for this inteface are in "JSDOM.WebKitNamespace".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitNamespace Mozilla WebKitNamespace documentation>
newtype WebKitNamespace = WebKitNamespace { unWebKitNamespace :: JSVal }

instance PToJSVal WebKitNamespace where
  pToJSVal = unWebKitNamespace
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitNamespace where
  pFromJSVal = WebKitNamespace
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitNamespace where
  toJSVal = return . unWebKitNamespace
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitNamespace where
  fromJSVal v = fmap WebKitNamespace <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitNamespace
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitNamespace where
  makeObject = makeObject . unWebKitNamespace

instance IsGObject WebKitNamespace where
  typeGType _ = gTypeWebKitNamespace
  {-# INLINE typeGType #-}

noWebKitNamespace :: Maybe WebKitNamespace
noWebKitNamespace = Nothing
{-# INLINE noWebKitNamespace #-}

gTypeWebKitNamespace :: JSM GType
gTypeWebKitNamespace = GType . Object <$> jsg "WebKitNamespace"

-- | Functions for this inteface are in "JSDOM.WebKitPlaybackTargetAvailabilityEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitPlaybackTargetAvailabilityEvent Mozilla WebKitPlaybackTargetAvailabilityEvent documentation>
newtype WebKitPlaybackTargetAvailabilityEvent = WebKitPlaybackTargetAvailabilityEvent { unWebKitPlaybackTargetAvailabilityEvent :: JSVal }

instance PToJSVal WebKitPlaybackTargetAvailabilityEvent where
  pToJSVal = unWebKitPlaybackTargetAvailabilityEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitPlaybackTargetAvailabilityEvent where
  pFromJSVal = WebKitPlaybackTargetAvailabilityEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitPlaybackTargetAvailabilityEvent where
  toJSVal = return . unWebKitPlaybackTargetAvailabilityEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitPlaybackTargetAvailabilityEvent where
  fromJSVal v = fmap WebKitPlaybackTargetAvailabilityEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitPlaybackTargetAvailabilityEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitPlaybackTargetAvailabilityEvent where
  makeObject = makeObject . unWebKitPlaybackTargetAvailabilityEvent

instance IsEvent WebKitPlaybackTargetAvailabilityEvent
instance IsGObject WebKitPlaybackTargetAvailabilityEvent where
  typeGType _ = gTypeWebKitPlaybackTargetAvailabilityEvent
  {-# INLINE typeGType #-}

noWebKitPlaybackTargetAvailabilityEvent :: Maybe WebKitPlaybackTargetAvailabilityEvent
noWebKitPlaybackTargetAvailabilityEvent = Nothing
{-# INLINE noWebKitPlaybackTargetAvailabilityEvent #-}

gTypeWebKitPlaybackTargetAvailabilityEvent :: JSM GType
gTypeWebKitPlaybackTargetAvailabilityEvent = GType . Object <$> jsg "WebKitPlaybackTargetAvailabilityEvent"

-- | Functions for this inteface are in "JSDOM.WebKitPlaybackTargetAvailabilityEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitPlaybackTargetAvailabilityEventInit Mozilla WebKitPlaybackTargetAvailabilityEventInit documentation>
newtype WebKitPlaybackTargetAvailabilityEventInit = WebKitPlaybackTargetAvailabilityEventInit { unWebKitPlaybackTargetAvailabilityEventInit :: JSVal }

instance PToJSVal WebKitPlaybackTargetAvailabilityEventInit where
  pToJSVal = unWebKitPlaybackTargetAvailabilityEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitPlaybackTargetAvailabilityEventInit where
  pFromJSVal = WebKitPlaybackTargetAvailabilityEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitPlaybackTargetAvailabilityEventInit where
  toJSVal = return . unWebKitPlaybackTargetAvailabilityEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitPlaybackTargetAvailabilityEventInit where
  fromJSVal v = fmap WebKitPlaybackTargetAvailabilityEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitPlaybackTargetAvailabilityEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitPlaybackTargetAvailabilityEventInit where
  makeObject = makeObject . unWebKitPlaybackTargetAvailabilityEventInit

instance IsEventInit WebKitPlaybackTargetAvailabilityEventInit
instance IsGObject WebKitPlaybackTargetAvailabilityEventInit where
  typeGType _ = gTypeWebKitPlaybackTargetAvailabilityEventInit
  {-# INLINE typeGType #-}

noWebKitPlaybackTargetAvailabilityEventInit :: Maybe WebKitPlaybackTargetAvailabilityEventInit
noWebKitPlaybackTargetAvailabilityEventInit = Nothing
{-# INLINE noWebKitPlaybackTargetAvailabilityEventInit #-}

gTypeWebKitPlaybackTargetAvailabilityEventInit :: JSM GType
gTypeWebKitPlaybackTargetAvailabilityEventInit = GType . Object <$> jsg "WebKitPlaybackTargetAvailabilityEventInit"

-- | Functions for this inteface are in "JSDOM.WebKitPoint".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitPoint Mozilla WebKitPoint documentation>
newtype WebKitPoint = WebKitPoint { unWebKitPoint :: JSVal }

instance PToJSVal WebKitPoint where
  pToJSVal = unWebKitPoint
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitPoint where
  pFromJSVal = WebKitPoint
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitPoint where
  toJSVal = return . unWebKitPoint
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitPoint where
  fromJSVal v = fmap WebKitPoint <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitPoint
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitPoint where
  makeObject = makeObject . unWebKitPoint

instance IsGObject WebKitPoint where
  typeGType _ = gTypeWebKitPoint
  {-# INLINE typeGType #-}

noWebKitPoint :: Maybe WebKitPoint
noWebKitPoint = Nothing
{-# INLINE noWebKitPoint #-}

gTypeWebKitPoint :: JSM GType
gTypeWebKitPoint = GType . Object <$> jsg "WebKitPoint"

-- | Functions for this inteface are in "JSDOM.WebKitSubtleCrypto".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitSubtleCrypto Mozilla WebKitSubtleCrypto documentation>
newtype WebKitSubtleCrypto = WebKitSubtleCrypto { unWebKitSubtleCrypto :: JSVal }

instance PToJSVal WebKitSubtleCrypto where
  pToJSVal = unWebKitSubtleCrypto
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitSubtleCrypto where
  pFromJSVal = WebKitSubtleCrypto
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitSubtleCrypto where
  toJSVal = return . unWebKitSubtleCrypto
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitSubtleCrypto where
  fromJSVal v = fmap WebKitSubtleCrypto <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitSubtleCrypto
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitSubtleCrypto where
  makeObject = makeObject . unWebKitSubtleCrypto

instance IsGObject WebKitSubtleCrypto where
  typeGType _ = gTypeWebKitSubtleCrypto
  {-# INLINE typeGType #-}

noWebKitSubtleCrypto :: Maybe WebKitSubtleCrypto
noWebKitSubtleCrypto = Nothing
{-# INLINE noWebKitSubtleCrypto #-}

gTypeWebKitSubtleCrypto :: JSM GType
gTypeWebKitSubtleCrypto = GType . Object <$> jsg "WebKitSubtleCrypto"

-- | Functions for this inteface are in "JSDOM.WebKitTransitionEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitTransitionEvent Mozilla WebKitTransitionEvent documentation>
newtype WebKitTransitionEvent = WebKitTransitionEvent { unWebKitTransitionEvent :: JSVal }

instance PToJSVal WebKitTransitionEvent where
  pToJSVal = unWebKitTransitionEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitTransitionEvent where
  pFromJSVal = WebKitTransitionEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitTransitionEvent where
  toJSVal = return . unWebKitTransitionEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitTransitionEvent where
  fromJSVal v = fmap WebKitTransitionEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitTransitionEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitTransitionEvent where
  makeObject = makeObject . unWebKitTransitionEvent

instance IsEvent WebKitTransitionEvent
instance IsGObject WebKitTransitionEvent where
  typeGType _ = gTypeWebKitTransitionEvent
  {-# INLINE typeGType #-}

noWebKitTransitionEvent :: Maybe WebKitTransitionEvent
noWebKitTransitionEvent = Nothing
{-# INLINE noWebKitTransitionEvent #-}

gTypeWebKitTransitionEvent :: JSM GType
gTypeWebKitTransitionEvent = GType . Object <$> jsg "WebKitTransitionEvent"

-- | Functions for this inteface are in "JSDOM.WebKitTransitionEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebKitTransitionEventInit Mozilla WebKitTransitionEventInit documentation>
newtype WebKitTransitionEventInit = WebKitTransitionEventInit { unWebKitTransitionEventInit :: JSVal }

instance PToJSVal WebKitTransitionEventInit where
  pToJSVal = unWebKitTransitionEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebKitTransitionEventInit where
  pFromJSVal = WebKitTransitionEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebKitTransitionEventInit where
  toJSVal = return . unWebKitTransitionEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebKitTransitionEventInit where
  fromJSVal v = fmap WebKitTransitionEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebKitTransitionEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebKitTransitionEventInit where
  makeObject = makeObject . unWebKitTransitionEventInit

instance IsEventInit WebKitTransitionEventInit
instance IsGObject WebKitTransitionEventInit where
  typeGType _ = gTypeWebKitTransitionEventInit
  {-# INLINE typeGType #-}

noWebKitTransitionEventInit :: Maybe WebKitTransitionEventInit
noWebKitTransitionEventInit = Nothing
{-# INLINE noWebKitTransitionEventInit #-}

gTypeWebKitTransitionEventInit :: JSM GType
gTypeWebKitTransitionEventInit = GType . Object <$> jsg "WebKitTransitionEventInit"

-- | Functions for this inteface are in "JSDOM.WebSocket".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebSocket Mozilla WebSocket documentation>
newtype WebSocket = WebSocket { unWebSocket :: JSVal }

instance PToJSVal WebSocket where
  pToJSVal = unWebSocket
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebSocket where
  pFromJSVal = WebSocket
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebSocket where
  toJSVal = return . unWebSocket
  {-# INLINE toJSVal #-}

instance FromJSVal WebSocket where
  fromJSVal v = fmap WebSocket <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebSocket
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebSocket where
  makeObject = makeObject . unWebSocket

instance IsEventTarget WebSocket
instance IsGObject WebSocket where
  typeGType _ = gTypeWebSocket
  {-# INLINE typeGType #-}

noWebSocket :: Maybe WebSocket
noWebSocket = Nothing
{-# INLINE noWebSocket #-}

gTypeWebSocket :: JSM GType
gTypeWebSocket = GType . Object <$> jsg "WebSocket"

-- | Functions for this inteface are in "JSDOM.WheelEvent".
-- Base interface functions are in:
--
--     * "JSDOM.MouseEvent"
--     * "JSDOM.UIEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent Mozilla WheelEvent documentation>
newtype WheelEvent = WheelEvent { unWheelEvent :: JSVal }

instance PToJSVal WheelEvent where
  pToJSVal = unWheelEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WheelEvent where
  pFromJSVal = WheelEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WheelEvent where
  toJSVal = return . unWheelEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WheelEvent where
  fromJSVal v = fmap WheelEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WheelEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WheelEvent where
  makeObject = makeObject . unWheelEvent

instance IsMouseEvent WheelEvent
instance IsUIEvent WheelEvent
instance IsEvent WheelEvent
instance IsGObject WheelEvent where
  typeGType _ = gTypeWheelEvent
  {-# INLINE typeGType #-}

noWheelEvent :: Maybe WheelEvent
noWheelEvent = Nothing
{-# INLINE noWheelEvent #-}

gTypeWheelEvent :: JSM GType
gTypeWheelEvent = GType . Object <$> jsg "WheelEvent"

-- | Functions for this inteface are in "JSDOM.WheelEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.MouseEventInit"
--     * "JSDOM.EventModifierInit"
--     * "JSDOM.UIEventInit"
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WheelEventInit Mozilla WheelEventInit documentation>
newtype WheelEventInit = WheelEventInit { unWheelEventInit :: JSVal }

instance PToJSVal WheelEventInit where
  pToJSVal = unWheelEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WheelEventInit where
  pFromJSVal = WheelEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WheelEventInit where
  toJSVal = return . unWheelEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WheelEventInit where
  fromJSVal v = fmap WheelEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WheelEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WheelEventInit where
  makeObject = makeObject . unWheelEventInit

instance IsMouseEventInit WheelEventInit
instance IsEventModifierInit WheelEventInit
instance IsUIEventInit WheelEventInit
instance IsEventInit WheelEventInit
instance IsGObject WheelEventInit where
  typeGType _ = gTypeWheelEventInit
  {-# INLINE typeGType #-}

noWheelEventInit :: Maybe WheelEventInit
noWheelEventInit = Nothing
{-# INLINE noWheelEventInit #-}

gTypeWheelEventInit :: JSM GType
gTypeWheelEventInit = GType . Object <$> jsg "WheelEventInit"


-- | Functions for this inteface are in "JSDOM.Worker".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--     * "JSDOM.AbstractWorker"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Worker Mozilla Worker documentation>
newtype Worker = Worker { unWorker :: JSVal }

instance PToJSVal Worker where
  pToJSVal = unWorker
  {-# INLINE pToJSVal #-}

instance PFromJSVal Worker where
  pFromJSVal = Worker
  {-# INLINE pFromJSVal #-}

instance ToJSVal Worker where
  toJSVal = return . unWorker
  {-# INLINE toJSVal #-}

instance FromJSVal Worker where
  fromJSVal v = fmap Worker <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Worker
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Worker where
  makeObject = makeObject . unWorker

instance IsEventTarget Worker
instance IsAbstractWorker Worker
instance IsGObject Worker where
  typeGType _ = gTypeWorker
  {-# INLINE typeGType #-}

noWorker :: Maybe Worker
noWorker = Nothing
{-# INLINE noWorker #-}

gTypeWorker :: JSM GType
gTypeWorker = GType . Object <$> jsg "Worker"

-- | Functions for this inteface are in "JSDOM.WorkerLocation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WorkerLocation Mozilla WorkerLocation documentation>
newtype WorkerLocation = WorkerLocation { unWorkerLocation :: JSVal }

instance PToJSVal WorkerLocation where
  pToJSVal = unWorkerLocation
  {-# INLINE pToJSVal #-}

instance PFromJSVal WorkerLocation where
  pFromJSVal = WorkerLocation
  {-# INLINE pFromJSVal #-}

instance ToJSVal WorkerLocation where
  toJSVal = return . unWorkerLocation
  {-# INLINE toJSVal #-}

instance FromJSVal WorkerLocation where
  fromJSVal v = fmap WorkerLocation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WorkerLocation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WorkerLocation where
  makeObject = makeObject . unWorkerLocation

instance IsGObject WorkerLocation where
  typeGType _ = gTypeWorkerLocation
  {-# INLINE typeGType #-}

noWorkerLocation :: Maybe WorkerLocation
noWorkerLocation = Nothing
{-# INLINE noWorkerLocation #-}

gTypeWorkerLocation :: JSM GType
gTypeWorkerLocation = GType . Object <$> jsg "WorkerLocation"

-- | Functions for this inteface are in "JSDOM.WorkerNavigator".
-- Base interface functions are in:
--
--     * "JSDOM.NavigatorOnLine"
--     * "JSDOM.NavigatorLanguage"
--     * "JSDOM.NavigatorID"
--     * "JSDOM.NavigatorConcurrentHardware"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WorkerNavigator Mozilla WorkerNavigator documentation>
newtype WorkerNavigator = WorkerNavigator { unWorkerNavigator :: JSVal }

instance PToJSVal WorkerNavigator where
  pToJSVal = unWorkerNavigator
  {-# INLINE pToJSVal #-}

instance PFromJSVal WorkerNavigator where
  pFromJSVal = WorkerNavigator
  {-# INLINE pFromJSVal #-}

instance ToJSVal WorkerNavigator where
  toJSVal = return . unWorkerNavigator
  {-# INLINE toJSVal #-}

instance FromJSVal WorkerNavigator where
  fromJSVal v = fmap WorkerNavigator <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WorkerNavigator
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WorkerNavigator where
  makeObject = makeObject . unWorkerNavigator

instance IsNavigatorOnLine WorkerNavigator
instance IsNavigatorLanguage WorkerNavigator
instance IsNavigatorID WorkerNavigator
instance IsNavigatorConcurrentHardware WorkerNavigator
instance IsGObject WorkerNavigator where
  typeGType _ = gTypeWorkerNavigator
  {-# INLINE typeGType #-}

noWorkerNavigator :: Maybe WorkerNavigator
noWorkerNavigator = Nothing
{-# INLINE noWorkerNavigator #-}

gTypeWorkerNavigator :: JSM GType
gTypeWorkerNavigator = GType . Object <$> jsg "WorkerNavigator"

-- | Functions for this inteface are in "JSDOM.WritableStream".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WritableStream Mozilla WritableStream documentation>
newtype WritableStream = WritableStream { unWritableStream :: JSVal }

instance PToJSVal WritableStream where
  pToJSVal = unWritableStream
  {-# INLINE pToJSVal #-}

instance PFromJSVal WritableStream where
  pFromJSVal = WritableStream
  {-# INLINE pFromJSVal #-}

instance ToJSVal WritableStream where
  toJSVal = return . unWritableStream
  {-# INLINE toJSVal #-}

instance FromJSVal WritableStream where
  fromJSVal v = fmap WritableStream <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WritableStream
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WritableStream where
  makeObject = makeObject . unWritableStream

instance IsGObject WritableStream where
  typeGType _ = gTypeWritableStream
  {-# INLINE typeGType #-}

noWritableStream :: Maybe WritableStream
noWritableStream = Nothing
{-# INLINE noWritableStream #-}

gTypeWritableStream :: JSM GType
gTypeWritableStream = GType . Object <$> jsg "WritableStream"

-- | Functions for this inteface are in "JSDOM.XMLHttpRequest".
-- Base interface functions are in:
--
--     * "JSDOM.XMLHttpRequestEventTarget"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest Mozilla XMLHttpRequest documentation>
newtype XMLHttpRequest = XMLHttpRequest { unXMLHttpRequest :: JSVal }

instance PToJSVal XMLHttpRequest where
  pToJSVal = unXMLHttpRequest
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequest where
  pFromJSVal = XMLHttpRequest
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLHttpRequest where
  toJSVal = return . unXMLHttpRequest
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequest where
  fromJSVal v = fmap XMLHttpRequest <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLHttpRequest
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLHttpRequest where
  makeObject = makeObject . unXMLHttpRequest

instance IsXMLHttpRequestEventTarget XMLHttpRequest
instance IsEventTarget XMLHttpRequest
instance IsGObject XMLHttpRequest where
  typeGType _ = gTypeXMLHttpRequest
  {-# INLINE typeGType #-}

noXMLHttpRequest :: Maybe XMLHttpRequest
noXMLHttpRequest = Nothing
{-# INLINE noXMLHttpRequest #-}

gTypeXMLHttpRequest :: JSM GType
gTypeXMLHttpRequest = GType . Object <$> jsg "XMLHttpRequest"

-- | Functions for this inteface are in "JSDOM.XMLHttpRequestEventTarget".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget Mozilla XMLHttpRequestEventTarget documentation>
newtype XMLHttpRequestEventTarget = XMLHttpRequestEventTarget { unXMLHttpRequestEventTarget :: JSVal }

instance PToJSVal XMLHttpRequestEventTarget where
  pToJSVal = unXMLHttpRequestEventTarget
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequestEventTarget where
  pFromJSVal = XMLHttpRequestEventTarget
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLHttpRequestEventTarget where
  toJSVal = return . unXMLHttpRequestEventTarget
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequestEventTarget where
  fromJSVal v = fmap XMLHttpRequestEventTarget <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLHttpRequestEventTarget
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLHttpRequestEventTarget where
  makeObject = makeObject . unXMLHttpRequestEventTarget

class (IsEventTarget o, IsGObject o) => IsXMLHttpRequestEventTarget o
toXMLHttpRequestEventTarget :: IsXMLHttpRequestEventTarget o => o -> XMLHttpRequestEventTarget
toXMLHttpRequestEventTarget = XMLHttpRequestEventTarget . coerce

instance IsXMLHttpRequestEventTarget XMLHttpRequestEventTarget
instance IsEventTarget XMLHttpRequestEventTarget
instance IsGObject XMLHttpRequestEventTarget where
  typeGType _ = gTypeXMLHttpRequestEventTarget
  {-# INLINE typeGType #-}

noXMLHttpRequestEventTarget :: Maybe XMLHttpRequestEventTarget
noXMLHttpRequestEventTarget = Nothing
{-# INLINE noXMLHttpRequestEventTarget #-}

gTypeXMLHttpRequestEventTarget :: JSM GType
gTypeXMLHttpRequestEventTarget = GType . Object <$> jsg "XMLHttpRequestEventTarget"

-- | Functions for this inteface are in "JSDOM.XMLHttpRequestProgressEvent".
-- Base interface functions are in:
--
--     * "JSDOM.ProgressEvent"
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestProgressEvent Mozilla XMLHttpRequestProgressEvent documentation>
newtype XMLHttpRequestProgressEvent = XMLHttpRequestProgressEvent { unXMLHttpRequestProgressEvent :: JSVal }

instance PToJSVal XMLHttpRequestProgressEvent where
  pToJSVal = unXMLHttpRequestProgressEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequestProgressEvent where
  pFromJSVal = XMLHttpRequestProgressEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLHttpRequestProgressEvent where
  toJSVal = return . unXMLHttpRequestProgressEvent
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequestProgressEvent where
  fromJSVal v = fmap XMLHttpRequestProgressEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLHttpRequestProgressEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLHttpRequestProgressEvent where
  makeObject = makeObject . unXMLHttpRequestProgressEvent

instance IsProgressEvent XMLHttpRequestProgressEvent
instance IsEvent XMLHttpRequestProgressEvent
instance IsGObject XMLHttpRequestProgressEvent where
  typeGType _ = gTypeXMLHttpRequestProgressEvent
  {-# INLINE typeGType #-}

noXMLHttpRequestProgressEvent :: Maybe XMLHttpRequestProgressEvent
noXMLHttpRequestProgressEvent = Nothing
{-# INLINE noXMLHttpRequestProgressEvent #-}

gTypeXMLHttpRequestProgressEvent :: JSM GType
gTypeXMLHttpRequestProgressEvent = GType . Object <$> jsg "XMLHttpRequestProgressEvent"

-- | Functions for this inteface are in "JSDOM.XMLHttpRequestUpload".
-- Base interface functions are in:
--
--     * "JSDOM.XMLHttpRequestEventTarget"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestUpload Mozilla XMLHttpRequestUpload documentation>
newtype XMLHttpRequestUpload = XMLHttpRequestUpload { unXMLHttpRequestUpload :: JSVal }

instance PToJSVal XMLHttpRequestUpload where
  pToJSVal = unXMLHttpRequestUpload
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLHttpRequestUpload where
  pFromJSVal = XMLHttpRequestUpload
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLHttpRequestUpload where
  toJSVal = return . unXMLHttpRequestUpload
  {-# INLINE toJSVal #-}

instance FromJSVal XMLHttpRequestUpload where
  fromJSVal v = fmap XMLHttpRequestUpload <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLHttpRequestUpload
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLHttpRequestUpload where
  makeObject = makeObject . unXMLHttpRequestUpload

instance IsXMLHttpRequestEventTarget XMLHttpRequestUpload
instance IsEventTarget XMLHttpRequestUpload
instance IsGObject XMLHttpRequestUpload where
  typeGType _ = gTypeXMLHttpRequestUpload
  {-# INLINE typeGType #-}

noXMLHttpRequestUpload :: Maybe XMLHttpRequestUpload
noXMLHttpRequestUpload = Nothing
{-# INLINE noXMLHttpRequestUpload #-}

gTypeXMLHttpRequestUpload :: JSM GType
gTypeXMLHttpRequestUpload = GType . Object <$> jsg "XMLHttpRequestUpload"

-- | Functions for this inteface are in "JSDOM.XMLSerializer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XMLSerializer Mozilla XMLSerializer documentation>
newtype XMLSerializer = XMLSerializer { unXMLSerializer :: JSVal }

instance PToJSVal XMLSerializer where
  pToJSVal = unXMLSerializer
  {-# INLINE pToJSVal #-}

instance PFromJSVal XMLSerializer where
  pFromJSVal = XMLSerializer
  {-# INLINE pFromJSVal #-}

instance ToJSVal XMLSerializer where
  toJSVal = return . unXMLSerializer
  {-# INLINE toJSVal #-}

instance FromJSVal XMLSerializer where
  fromJSVal v = fmap XMLSerializer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XMLSerializer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XMLSerializer where
  makeObject = makeObject . unXMLSerializer

instance IsGObject XMLSerializer where
  typeGType _ = gTypeXMLSerializer
  {-# INLINE typeGType #-}

noXMLSerializer :: Maybe XMLSerializer
noXMLSerializer = Nothing
{-# INLINE noXMLSerializer #-}

gTypeXMLSerializer :: JSM GType
gTypeXMLSerializer = GType . Object <$> jsg "XMLSerializer"

-- | Functions for this inteface are in "JSDOM.XPathEvaluator".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XPathEvaluator Mozilla XPathEvaluator documentation>
newtype XPathEvaluator = XPathEvaluator { unXPathEvaluator :: JSVal }

instance PToJSVal XPathEvaluator where
  pToJSVal = unXPathEvaluator
  {-# INLINE pToJSVal #-}

instance PFromJSVal XPathEvaluator where
  pFromJSVal = XPathEvaluator
  {-# INLINE pFromJSVal #-}

instance ToJSVal XPathEvaluator where
  toJSVal = return . unXPathEvaluator
  {-# INLINE toJSVal #-}

instance FromJSVal XPathEvaluator where
  fromJSVal v = fmap XPathEvaluator <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XPathEvaluator
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XPathEvaluator where
  makeObject = makeObject . unXPathEvaluator

instance IsGObject XPathEvaluator where
  typeGType _ = gTypeXPathEvaluator
  {-# INLINE typeGType #-}

noXPathEvaluator :: Maybe XPathEvaluator
noXPathEvaluator = Nothing
{-# INLINE noXPathEvaluator #-}

gTypeXPathEvaluator :: JSM GType
gTypeXPathEvaluator = GType . Object <$> jsg "XPathEvaluator"

-- | Functions for this inteface are in "JSDOM.XPathException".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XPathException Mozilla XPathException documentation>
newtype XPathException = XPathException { unXPathException :: JSVal }

instance PToJSVal XPathException where
  pToJSVal = unXPathException
  {-# INLINE pToJSVal #-}

instance PFromJSVal XPathException where
  pFromJSVal = XPathException
  {-# INLINE pFromJSVal #-}

instance ToJSVal XPathException where
  toJSVal = return . unXPathException
  {-# INLINE toJSVal #-}

instance FromJSVal XPathException where
  fromJSVal v = fmap XPathException <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XPathException
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XPathException where
  makeObject = makeObject . unXPathException

instance IsGObject XPathException where
  typeGType _ = gTypeXPathException
  {-# INLINE typeGType #-}

noXPathException :: Maybe XPathException
noXPathException = Nothing
{-# INLINE noXPathException #-}

gTypeXPathException :: JSM GType
gTypeXPathException = GType . Object <$> jsg "XPathException"

-- | Functions for this inteface are in "JSDOM.XPathExpression".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XPathExpression Mozilla XPathExpression documentation>
newtype XPathExpression = XPathExpression { unXPathExpression :: JSVal }

instance PToJSVal XPathExpression where
  pToJSVal = unXPathExpression
  {-# INLINE pToJSVal #-}

instance PFromJSVal XPathExpression where
  pFromJSVal = XPathExpression
  {-# INLINE pFromJSVal #-}

instance ToJSVal XPathExpression where
  toJSVal = return . unXPathExpression
  {-# INLINE toJSVal #-}

instance FromJSVal XPathExpression where
  fromJSVal v = fmap XPathExpression <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XPathExpression
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XPathExpression where
  makeObject = makeObject . unXPathExpression

instance IsGObject XPathExpression where
  typeGType _ = gTypeXPathExpression
  {-# INLINE typeGType #-}

noXPathExpression :: Maybe XPathExpression
noXPathExpression = Nothing
{-# INLINE noXPathExpression #-}

gTypeXPathExpression :: JSM GType
gTypeXPathExpression = GType . Object <$> jsg "XPathExpression"

-- | Functions for this inteface are in "JSDOM.XPathNSResolver".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XPathNSResolver Mozilla XPathNSResolver documentation>
newtype XPathNSResolver = XPathNSResolver { unXPathNSResolver :: JSVal }

instance PToJSVal XPathNSResolver where
  pToJSVal = unXPathNSResolver
  {-# INLINE pToJSVal #-}

instance PFromJSVal XPathNSResolver where
  pFromJSVal = XPathNSResolver
  {-# INLINE pFromJSVal #-}

instance ToJSVal XPathNSResolver where
  toJSVal = return . unXPathNSResolver
  {-# INLINE toJSVal #-}

instance FromJSVal XPathNSResolver where
  fromJSVal v = fmap XPathNSResolver <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XPathNSResolver
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XPathNSResolver where
  makeObject = makeObject . unXPathNSResolver

instance IsGObject XPathNSResolver where
  typeGType _ = gTypeXPathNSResolver
  {-# INLINE typeGType #-}

noXPathNSResolver :: Maybe XPathNSResolver
noXPathNSResolver = Nothing
{-# INLINE noXPathNSResolver #-}

gTypeXPathNSResolver :: JSM GType
gTypeXPathNSResolver = GType . Object <$> jsg "XPathNSResolver"

-- | Functions for this inteface are in "JSDOM.XPathResult".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XPathResult Mozilla XPathResult documentation>
newtype XPathResult = XPathResult { unXPathResult :: JSVal }

instance PToJSVal XPathResult where
  pToJSVal = unXPathResult
  {-# INLINE pToJSVal #-}

instance PFromJSVal XPathResult where
  pFromJSVal = XPathResult
  {-# INLINE pFromJSVal #-}

instance ToJSVal XPathResult where
  toJSVal = return . unXPathResult
  {-# INLINE toJSVal #-}

instance FromJSVal XPathResult where
  fromJSVal v = fmap XPathResult <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XPathResult
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XPathResult where
  makeObject = makeObject . unXPathResult

instance IsGObject XPathResult where
  typeGType _ = gTypeXPathResult
  {-# INLINE typeGType #-}

noXPathResult :: Maybe XPathResult
noXPathResult = Nothing
{-# INLINE noXPathResult #-}

gTypeXPathResult :: JSM GType
gTypeXPathResult = GType . Object <$> jsg "XPathResult"

-- | Functions for this inteface are in "JSDOM.XSLTProcessor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/XSLTProcessor Mozilla XSLTProcessor documentation>
newtype XSLTProcessor = XSLTProcessor { unXSLTProcessor :: JSVal }

instance PToJSVal XSLTProcessor where
  pToJSVal = unXSLTProcessor
  {-# INLINE pToJSVal #-}

instance PFromJSVal XSLTProcessor where
  pFromJSVal = XSLTProcessor
  {-# INLINE pFromJSVal #-}

instance ToJSVal XSLTProcessor where
  toJSVal = return . unXSLTProcessor
  {-# INLINE toJSVal #-}

instance FromJSVal XSLTProcessor where
  fromJSVal v = fmap XSLTProcessor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . XSLTProcessor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject XSLTProcessor where
  makeObject = makeObject . unXSLTProcessor

instance IsGObject XSLTProcessor where
  typeGType _ = gTypeXSLTProcessor
  {-# INLINE typeGType #-}

noXSLTProcessor :: Maybe XSLTProcessor
noXSLTProcessor = Nothing
{-# INLINE noXSLTProcessor #-}

gTypeXSLTProcessor :: JSM GType
gTypeXSLTProcessor = GType . Object <$> jsg "XSLTProcessor"

