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

  , GlobalThis(GlobalThis), unGlobalThis, noGlobalThis

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

import JSDOM.Types.Manual
-- import JSDOM.Types.Generated
