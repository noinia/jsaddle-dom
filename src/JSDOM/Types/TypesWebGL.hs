{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.TypesWebGL where

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

-- | Functions for this inteface are in "JSDOM.WebGL2RenderingContext".
-- Base interface functions are in:
--
--     * "JSDOM.WebGLRenderingContextBase"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGL2RenderingContext Mozilla WebGL2RenderingContext documentation>
newtype WebGL2RenderingContext = WebGL2RenderingContext { unWebGL2RenderingContext :: JSVal }

instance PToJSVal WebGL2RenderingContext where
  pToJSVal = unWebGL2RenderingContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGL2RenderingContext where
  pFromJSVal = WebGL2RenderingContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGL2RenderingContext where
  toJSVal = return . unWebGL2RenderingContext
  {-# INLINE toJSVal #-}

instance FromJSVal WebGL2RenderingContext where
  fromJSVal v = fmap WebGL2RenderingContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGL2RenderingContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGL2RenderingContext where
  makeObject = makeObject . unWebGL2RenderingContext

instance IsWebGLRenderingContextBase WebGL2RenderingContext
instance IsGObject WebGL2RenderingContext where
  typeGType _ = gTypeWebGL2RenderingContext
  {-# INLINE typeGType #-}

noWebGL2RenderingContext :: Maybe WebGL2RenderingContext
noWebGL2RenderingContext = Nothing
{-# INLINE noWebGL2RenderingContext #-}

gTypeWebGL2RenderingContext :: JSM GType
gTypeWebGL2RenderingContext = GType . Object <$> jsg "WebGL2RenderingContext"

-- | Functions for this inteface are in "JSDOM.WebGLActiveInfo".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLActiveInfo Mozilla WebGLActiveInfo documentation>
newtype WebGLActiveInfo = WebGLActiveInfo { unWebGLActiveInfo :: JSVal }

instance PToJSVal WebGLActiveInfo where
  pToJSVal = unWebGLActiveInfo
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLActiveInfo where
  pFromJSVal = WebGLActiveInfo
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLActiveInfo where
  toJSVal = return . unWebGLActiveInfo
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLActiveInfo where
  fromJSVal v = fmap WebGLActiveInfo <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLActiveInfo
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLActiveInfo where
  makeObject = makeObject . unWebGLActiveInfo

instance IsGObject WebGLActiveInfo where
  typeGType _ = gTypeWebGLActiveInfo
  {-# INLINE typeGType #-}

noWebGLActiveInfo :: Maybe WebGLActiveInfo
noWebGLActiveInfo = Nothing
{-# INLINE noWebGLActiveInfo #-}

gTypeWebGLActiveInfo :: JSM GType
gTypeWebGLActiveInfo = GType . Object <$> jsg "WebGLActiveInfo"

-- | Functions for this inteface are in "JSDOM.WebGLBuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLBuffer Mozilla WebGLBuffer documentation>
newtype WebGLBuffer = WebGLBuffer { unWebGLBuffer :: JSVal }

instance PToJSVal WebGLBuffer where
  pToJSVal = unWebGLBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLBuffer where
  pFromJSVal = WebGLBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLBuffer where
  toJSVal = return . unWebGLBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLBuffer where
  fromJSVal v = fmap WebGLBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLBuffer where
  makeObject = makeObject . unWebGLBuffer

instance IsGObject WebGLBuffer where
  typeGType _ = gTypeWebGLBuffer
  {-# INLINE typeGType #-}

noWebGLBuffer :: Maybe WebGLBuffer
noWebGLBuffer = Nothing
{-# INLINE noWebGLBuffer #-}

gTypeWebGLBuffer :: JSM GType
gTypeWebGLBuffer = GType . Object <$> jsg "WebGLBuffer"

-- | Functions for this inteface are in "JSDOM.WebGLCompressedTextureATC".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLCompressedTextureATC Mozilla WebGLCompressedTextureATC documentation>
newtype WebGLCompressedTextureATC = WebGLCompressedTextureATC { unWebGLCompressedTextureATC :: JSVal }

instance PToJSVal WebGLCompressedTextureATC where
  pToJSVal = unWebGLCompressedTextureATC
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLCompressedTextureATC where
  pFromJSVal = WebGLCompressedTextureATC
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLCompressedTextureATC where
  toJSVal = return . unWebGLCompressedTextureATC
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLCompressedTextureATC where
  fromJSVal v = fmap WebGLCompressedTextureATC <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLCompressedTextureATC
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLCompressedTextureATC where
  makeObject = makeObject . unWebGLCompressedTextureATC

instance IsGObject WebGLCompressedTextureATC where
  typeGType _ = gTypeWebGLCompressedTextureATC
  {-# INLINE typeGType #-}

noWebGLCompressedTextureATC :: Maybe WebGLCompressedTextureATC
noWebGLCompressedTextureATC = Nothing
{-# INLINE noWebGLCompressedTextureATC #-}

gTypeWebGLCompressedTextureATC :: JSM GType
gTypeWebGLCompressedTextureATC = GType . Object <$> jsg "WebGLCompressedTextureATC"

-- | Functions for this inteface are in "JSDOM.WebGLCompressedTexturePVRTC".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLCompressedTexturePVRTC Mozilla WebGLCompressedTexturePVRTC documentation>
newtype WebGLCompressedTexturePVRTC = WebGLCompressedTexturePVRTC { unWebGLCompressedTexturePVRTC :: JSVal }

instance PToJSVal WebGLCompressedTexturePVRTC where
  pToJSVal = unWebGLCompressedTexturePVRTC
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLCompressedTexturePVRTC where
  pFromJSVal = WebGLCompressedTexturePVRTC
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLCompressedTexturePVRTC where
  toJSVal = return . unWebGLCompressedTexturePVRTC
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLCompressedTexturePVRTC where
  fromJSVal v = fmap WebGLCompressedTexturePVRTC <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLCompressedTexturePVRTC
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLCompressedTexturePVRTC where
  makeObject = makeObject . unWebGLCompressedTexturePVRTC

instance IsGObject WebGLCompressedTexturePVRTC where
  typeGType _ = gTypeWebGLCompressedTexturePVRTC
  {-# INLINE typeGType #-}

noWebGLCompressedTexturePVRTC :: Maybe WebGLCompressedTexturePVRTC
noWebGLCompressedTexturePVRTC = Nothing
{-# INLINE noWebGLCompressedTexturePVRTC #-}

gTypeWebGLCompressedTexturePVRTC :: JSM GType
gTypeWebGLCompressedTexturePVRTC = GType . Object <$> jsg "WebGLCompressedTexturePVRTC"

-- | Functions for this inteface are in "JSDOM.WebGLCompressedTextureS3TC".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLCompressedTextureS3TC Mozilla WebGLCompressedTextureS3TC documentation>
newtype WebGLCompressedTextureS3TC = WebGLCompressedTextureS3TC { unWebGLCompressedTextureS3TC :: JSVal }

instance PToJSVal WebGLCompressedTextureS3TC where
  pToJSVal = unWebGLCompressedTextureS3TC
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLCompressedTextureS3TC where
  pFromJSVal = WebGLCompressedTextureS3TC
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLCompressedTextureS3TC where
  toJSVal = return . unWebGLCompressedTextureS3TC
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLCompressedTextureS3TC where
  fromJSVal v = fmap WebGLCompressedTextureS3TC <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLCompressedTextureS3TC
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLCompressedTextureS3TC where
  makeObject = makeObject . unWebGLCompressedTextureS3TC

instance IsGObject WebGLCompressedTextureS3TC where
  typeGType _ = gTypeWebGLCompressedTextureS3TC
  {-# INLINE typeGType #-}

noWebGLCompressedTextureS3TC :: Maybe WebGLCompressedTextureS3TC
noWebGLCompressedTextureS3TC = Nothing
{-# INLINE noWebGLCompressedTextureS3TC #-}

gTypeWebGLCompressedTextureS3TC :: JSM GType
gTypeWebGLCompressedTextureS3TC = GType . Object <$> jsg "WebGLCompressedTextureS3TC"

-- | Functions for this inteface are in "JSDOM.WebGLContextAttributes".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLContextAttributes Mozilla WebGLContextAttributes documentation>
newtype WebGLContextAttributes = WebGLContextAttributes { unWebGLContextAttributes :: JSVal }

instance PToJSVal WebGLContextAttributes where
  pToJSVal = unWebGLContextAttributes
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLContextAttributes where
  pFromJSVal = WebGLContextAttributes
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLContextAttributes where
  toJSVal = return . unWebGLContextAttributes
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLContextAttributes where
  fromJSVal v = fmap WebGLContextAttributes <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLContextAttributes
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLContextAttributes where
  makeObject = makeObject . unWebGLContextAttributes

instance IsGObject WebGLContextAttributes where
  typeGType _ = gTypeWebGLContextAttributes
  {-# INLINE typeGType #-}

noWebGLContextAttributes :: Maybe WebGLContextAttributes
noWebGLContextAttributes = Nothing
{-# INLINE noWebGLContextAttributes #-}

gTypeWebGLContextAttributes :: JSM GType
gTypeWebGLContextAttributes = GType . Object <$> jsg "WebGLContextAttributes"

-- | Functions for this inteface are in "JSDOM.WebGLContextEvent".
-- Base interface functions are in:
--
--     * "JSDOM.Event"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLContextEvent Mozilla WebGLContextEvent documentation>
newtype WebGLContextEvent = WebGLContextEvent { unWebGLContextEvent :: JSVal }

instance PToJSVal WebGLContextEvent where
  pToJSVal = unWebGLContextEvent
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLContextEvent where
  pFromJSVal = WebGLContextEvent
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLContextEvent where
  toJSVal = return . unWebGLContextEvent
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLContextEvent where
  fromJSVal v = fmap WebGLContextEvent <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLContextEvent
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLContextEvent where
  makeObject = makeObject . unWebGLContextEvent

instance IsEvent WebGLContextEvent
instance IsGObject WebGLContextEvent where
  typeGType _ = gTypeWebGLContextEvent
  {-# INLINE typeGType #-}

noWebGLContextEvent :: Maybe WebGLContextEvent
noWebGLContextEvent = Nothing
{-# INLINE noWebGLContextEvent #-}

gTypeWebGLContextEvent :: JSM GType
gTypeWebGLContextEvent = GType . Object <$> jsg "WebGLContextEvent"

-- | Functions for this inteface are in "JSDOM.WebGLContextEventInit".
-- Base interface functions are in:
--
--     * "JSDOM.EventInit"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLContextEventInit Mozilla WebGLContextEventInit documentation>
newtype WebGLContextEventInit = WebGLContextEventInit { unWebGLContextEventInit :: JSVal }

instance PToJSVal WebGLContextEventInit where
  pToJSVal = unWebGLContextEventInit
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLContextEventInit where
  pFromJSVal = WebGLContextEventInit
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLContextEventInit where
  toJSVal = return . unWebGLContextEventInit
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLContextEventInit where
  fromJSVal v = fmap WebGLContextEventInit <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLContextEventInit
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLContextEventInit where
  makeObject = makeObject . unWebGLContextEventInit

instance IsEventInit WebGLContextEventInit
instance IsGObject WebGLContextEventInit where
  typeGType _ = gTypeWebGLContextEventInit
  {-# INLINE typeGType #-}

noWebGLContextEventInit :: Maybe WebGLContextEventInit
noWebGLContextEventInit = Nothing
{-# INLINE noWebGLContextEventInit #-}

gTypeWebGLContextEventInit :: JSM GType
gTypeWebGLContextEventInit = GType . Object <$> jsg "WebGLContextEventInit"

-- | Functions for this inteface are in "JSDOM.WebGLDebugRendererInfo".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLDebugRendererInfo Mozilla WebGLDebugRendererInfo documentation>
newtype WebGLDebugRendererInfo = WebGLDebugRendererInfo { unWebGLDebugRendererInfo :: JSVal }

instance PToJSVal WebGLDebugRendererInfo where
  pToJSVal = unWebGLDebugRendererInfo
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLDebugRendererInfo where
  pFromJSVal = WebGLDebugRendererInfo
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLDebugRendererInfo where
  toJSVal = return . unWebGLDebugRendererInfo
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLDebugRendererInfo where
  fromJSVal v = fmap WebGLDebugRendererInfo <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLDebugRendererInfo
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLDebugRendererInfo where
  makeObject = makeObject . unWebGLDebugRendererInfo

instance IsGObject WebGLDebugRendererInfo where
  typeGType _ = gTypeWebGLDebugRendererInfo
  {-# INLINE typeGType #-}

noWebGLDebugRendererInfo :: Maybe WebGLDebugRendererInfo
noWebGLDebugRendererInfo = Nothing
{-# INLINE noWebGLDebugRendererInfo #-}

gTypeWebGLDebugRendererInfo :: JSM GType
gTypeWebGLDebugRendererInfo = GType . Object <$> jsg "WebGLDebugRendererInfo"

-- | Functions for this inteface are in "JSDOM.WebGLDebugShaders".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLDebugShaders Mozilla WebGLDebugShaders documentation>
newtype WebGLDebugShaders = WebGLDebugShaders { unWebGLDebugShaders :: JSVal }

instance PToJSVal WebGLDebugShaders where
  pToJSVal = unWebGLDebugShaders
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLDebugShaders where
  pFromJSVal = WebGLDebugShaders
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLDebugShaders where
  toJSVal = return . unWebGLDebugShaders
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLDebugShaders where
  fromJSVal v = fmap WebGLDebugShaders <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLDebugShaders
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLDebugShaders where
  makeObject = makeObject . unWebGLDebugShaders

instance IsGObject WebGLDebugShaders where
  typeGType _ = gTypeWebGLDebugShaders
  {-# INLINE typeGType #-}

noWebGLDebugShaders :: Maybe WebGLDebugShaders
noWebGLDebugShaders = Nothing
{-# INLINE noWebGLDebugShaders #-}

gTypeWebGLDebugShaders :: JSM GType
gTypeWebGLDebugShaders = GType . Object <$> jsg "WebGLDebugShaders"

-- | Functions for this inteface are in "JSDOM.WebGLDepthTexture".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLDepthTexture Mozilla WebGLDepthTexture documentation>
newtype WebGLDepthTexture = WebGLDepthTexture { unWebGLDepthTexture :: JSVal }

instance PToJSVal WebGLDepthTexture where
  pToJSVal = unWebGLDepthTexture
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLDepthTexture where
  pFromJSVal = WebGLDepthTexture
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLDepthTexture where
  toJSVal = return . unWebGLDepthTexture
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLDepthTexture where
  fromJSVal v = fmap WebGLDepthTexture <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLDepthTexture
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLDepthTexture where
  makeObject = makeObject . unWebGLDepthTexture

instance IsGObject WebGLDepthTexture where
  typeGType _ = gTypeWebGLDepthTexture
  {-# INLINE typeGType #-}

noWebGLDepthTexture :: Maybe WebGLDepthTexture
noWebGLDepthTexture = Nothing
{-# INLINE noWebGLDepthTexture #-}

gTypeWebGLDepthTexture :: JSM GType
gTypeWebGLDepthTexture = GType . Object <$> jsg "WebGLDepthTexture"

-- | Functions for this inteface are in "JSDOM.WebGLDrawBuffers".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLDrawBuffers Mozilla WebGLDrawBuffers documentation>
newtype WebGLDrawBuffers = WebGLDrawBuffers { unWebGLDrawBuffers :: JSVal }

instance PToJSVal WebGLDrawBuffers where
  pToJSVal = unWebGLDrawBuffers
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLDrawBuffers where
  pFromJSVal = WebGLDrawBuffers
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLDrawBuffers where
  toJSVal = return . unWebGLDrawBuffers
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLDrawBuffers where
  fromJSVal v = fmap WebGLDrawBuffers <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLDrawBuffers
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLDrawBuffers where
  makeObject = makeObject . unWebGLDrawBuffers

instance IsGObject WebGLDrawBuffers where
  typeGType _ = gTypeWebGLDrawBuffers
  {-# INLINE typeGType #-}

noWebGLDrawBuffers :: Maybe WebGLDrawBuffers
noWebGLDrawBuffers = Nothing
{-# INLINE noWebGLDrawBuffers #-}

gTypeWebGLDrawBuffers :: JSM GType
gTypeWebGLDrawBuffers = GType . Object <$> jsg "WebGLDrawBuffers"

-- | Functions for this inteface are in "JSDOM.WebGLFramebuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLFramebuffer Mozilla WebGLFramebuffer documentation>
newtype WebGLFramebuffer = WebGLFramebuffer { unWebGLFramebuffer :: JSVal }

instance PToJSVal WebGLFramebuffer where
  pToJSVal = unWebGLFramebuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLFramebuffer where
  pFromJSVal = WebGLFramebuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLFramebuffer where
  toJSVal = return . unWebGLFramebuffer
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLFramebuffer where
  fromJSVal v = fmap WebGLFramebuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLFramebuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLFramebuffer where
  makeObject = makeObject . unWebGLFramebuffer

instance IsGObject WebGLFramebuffer where
  typeGType _ = gTypeWebGLFramebuffer
  {-# INLINE typeGType #-}

noWebGLFramebuffer :: Maybe WebGLFramebuffer
noWebGLFramebuffer = Nothing
{-# INLINE noWebGLFramebuffer #-}

gTypeWebGLFramebuffer :: JSM GType
gTypeWebGLFramebuffer = GType . Object <$> jsg "WebGLFramebuffer"

-- | Functions for this inteface are in "JSDOM.WebGLLoseContext".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLLoseContext Mozilla WebGLLoseContext documentation>
newtype WebGLLoseContext = WebGLLoseContext { unWebGLLoseContext :: JSVal }

instance PToJSVal WebGLLoseContext where
  pToJSVal = unWebGLLoseContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLLoseContext where
  pFromJSVal = WebGLLoseContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLLoseContext where
  toJSVal = return . unWebGLLoseContext
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLLoseContext where
  fromJSVal v = fmap WebGLLoseContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLLoseContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLLoseContext where
  makeObject = makeObject . unWebGLLoseContext

instance IsGObject WebGLLoseContext where
  typeGType _ = gTypeWebGLLoseContext
  {-# INLINE typeGType #-}

noWebGLLoseContext :: Maybe WebGLLoseContext
noWebGLLoseContext = Nothing
{-# INLINE noWebGLLoseContext #-}

gTypeWebGLLoseContext :: JSM GType
gTypeWebGLLoseContext = GType . Object <$> jsg "WebGLLoseContext"

-- | Functions for this inteface are in "JSDOM.WebGLProgram".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLProgram Mozilla WebGLProgram documentation>
newtype WebGLProgram = WebGLProgram { unWebGLProgram :: JSVal }

instance PToJSVal WebGLProgram where
  pToJSVal = unWebGLProgram
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLProgram where
  pFromJSVal = WebGLProgram
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLProgram where
  toJSVal = return . unWebGLProgram
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLProgram where
  fromJSVal v = fmap WebGLProgram <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLProgram
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLProgram where
  makeObject = makeObject . unWebGLProgram

instance IsGObject WebGLProgram where
  typeGType _ = gTypeWebGLProgram
  {-# INLINE typeGType #-}

noWebGLProgram :: Maybe WebGLProgram
noWebGLProgram = Nothing
{-# INLINE noWebGLProgram #-}

gTypeWebGLProgram :: JSM GType
gTypeWebGLProgram = GType . Object <$> jsg "WebGLProgram"

-- | Functions for this inteface are in "JSDOM.WebGLQuery".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLQuery Mozilla WebGLQuery documentation>
newtype WebGLQuery = WebGLQuery { unWebGLQuery :: JSVal }

instance PToJSVal WebGLQuery where
  pToJSVal = unWebGLQuery
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLQuery where
  pFromJSVal = WebGLQuery
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLQuery where
  toJSVal = return . unWebGLQuery
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLQuery where
  fromJSVal v = fmap WebGLQuery <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLQuery
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLQuery where
  makeObject = makeObject . unWebGLQuery

instance IsGObject WebGLQuery where
  typeGType _ = gTypeWebGLQuery
  {-# INLINE typeGType #-}

noWebGLQuery :: Maybe WebGLQuery
noWebGLQuery = Nothing
{-# INLINE noWebGLQuery #-}

gTypeWebGLQuery :: JSM GType
gTypeWebGLQuery = GType . Object <$> jsg "WebGLQuery"

-- | Functions for this inteface are in "JSDOM.WebGLRenderbuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderbuffer Mozilla WebGLRenderbuffer documentation>
newtype WebGLRenderbuffer = WebGLRenderbuffer { unWebGLRenderbuffer :: JSVal }

instance PToJSVal WebGLRenderbuffer where
  pToJSVal = unWebGLRenderbuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLRenderbuffer where
  pFromJSVal = WebGLRenderbuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLRenderbuffer where
  toJSVal = return . unWebGLRenderbuffer
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLRenderbuffer where
  fromJSVal v = fmap WebGLRenderbuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLRenderbuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLRenderbuffer where
  makeObject = makeObject . unWebGLRenderbuffer

instance IsGObject WebGLRenderbuffer where
  typeGType _ = gTypeWebGLRenderbuffer
  {-# INLINE typeGType #-}

noWebGLRenderbuffer :: Maybe WebGLRenderbuffer
noWebGLRenderbuffer = Nothing
{-# INLINE noWebGLRenderbuffer #-}

gTypeWebGLRenderbuffer :: JSM GType
gTypeWebGLRenderbuffer = GType . Object <$> jsg "WebGLRenderbuffer"

-- | Functions for this inteface are in "JSDOM.WebGLRenderingContext".
-- Base interface functions are in:
--
--     * "JSDOM.WebGLRenderingContextBase"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext Mozilla WebGLRenderingContext documentation>
newtype WebGLRenderingContext = WebGLRenderingContext { unWebGLRenderingContext :: JSVal }

instance PToJSVal WebGLRenderingContext where
  pToJSVal = unWebGLRenderingContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLRenderingContext where
  pFromJSVal = WebGLRenderingContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLRenderingContext where
  toJSVal = return . unWebGLRenderingContext
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLRenderingContext where
  fromJSVal v = fmap WebGLRenderingContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLRenderingContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLRenderingContext where
  makeObject = makeObject . unWebGLRenderingContext

instance IsWebGLRenderingContextBase WebGLRenderingContext
instance IsGObject WebGLRenderingContext where
  typeGType _ = gTypeWebGLRenderingContext
  {-# INLINE typeGType #-}

noWebGLRenderingContext :: Maybe WebGLRenderingContext
noWebGLRenderingContext = Nothing
{-# INLINE noWebGLRenderingContext #-}

gTypeWebGLRenderingContext :: JSM GType
gTypeWebGLRenderingContext = GType . Object <$> jsg "WebGLRenderingContext"

-- | Functions for this inteface are in "JSDOM.WebGLRenderingContextBase".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContextBase Mozilla WebGLRenderingContextBase documentation>
newtype WebGLRenderingContextBase = WebGLRenderingContextBase { unWebGLRenderingContextBase :: JSVal }

instance PToJSVal WebGLRenderingContextBase where
  pToJSVal = unWebGLRenderingContextBase
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLRenderingContextBase where
  pFromJSVal = WebGLRenderingContextBase
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLRenderingContextBase where
  toJSVal = return . unWebGLRenderingContextBase
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLRenderingContextBase where
  fromJSVal v = fmap WebGLRenderingContextBase <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLRenderingContextBase
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLRenderingContextBase where
  makeObject = makeObject . unWebGLRenderingContextBase

class (IsGObject o) => IsWebGLRenderingContextBase o
toWebGLRenderingContextBase :: IsWebGLRenderingContextBase o => o -> WebGLRenderingContextBase
toWebGLRenderingContextBase = WebGLRenderingContextBase . coerce

instance IsWebGLRenderingContextBase WebGLRenderingContextBase
instance IsGObject WebGLRenderingContextBase where
  typeGType _ = gTypeWebGLRenderingContextBase
  {-# INLINE typeGType #-}

noWebGLRenderingContextBase :: Maybe WebGLRenderingContextBase
noWebGLRenderingContextBase = Nothing
{-# INLINE noWebGLRenderingContextBase #-}

gTypeWebGLRenderingContextBase :: JSM GType
gTypeWebGLRenderingContextBase = GType . Object <$> jsg "WebGLRenderingContextBase"

-- | Functions for this inteface are in "JSDOM.WebGLSampler".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLSampler Mozilla WebGLSampler documentation>
newtype WebGLSampler = WebGLSampler { unWebGLSampler :: JSVal }

instance PToJSVal WebGLSampler where
  pToJSVal = unWebGLSampler
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLSampler where
  pFromJSVal = WebGLSampler
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLSampler where
  toJSVal = return . unWebGLSampler
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLSampler where
  fromJSVal v = fmap WebGLSampler <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLSampler
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLSampler where
  makeObject = makeObject . unWebGLSampler

instance IsGObject WebGLSampler where
  typeGType _ = gTypeWebGLSampler
  {-# INLINE typeGType #-}

noWebGLSampler :: Maybe WebGLSampler
noWebGLSampler = Nothing
{-# INLINE noWebGLSampler #-}

gTypeWebGLSampler :: JSM GType
gTypeWebGLSampler = GType . Object <$> jsg "WebGLSampler"

-- | Functions for this inteface are in "JSDOM.WebGLShader".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLShader Mozilla WebGLShader documentation>
newtype WebGLShader = WebGLShader { unWebGLShader :: JSVal }

instance PToJSVal WebGLShader where
  pToJSVal = unWebGLShader
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLShader where
  pFromJSVal = WebGLShader
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLShader where
  toJSVal = return . unWebGLShader
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLShader where
  fromJSVal v = fmap WebGLShader <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLShader
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLShader where
  makeObject = makeObject . unWebGLShader

instance IsGObject WebGLShader where
  typeGType _ = gTypeWebGLShader
  {-# INLINE typeGType #-}

noWebGLShader :: Maybe WebGLShader
noWebGLShader = Nothing
{-# INLINE noWebGLShader #-}

gTypeWebGLShader :: JSM GType
gTypeWebGLShader = GType . Object <$> jsg "WebGLShader"

-- | Functions for this inteface are in "JSDOM.WebGLShaderPrecisionFormat".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLShaderPrecisionFormat Mozilla WebGLShaderPrecisionFormat documentation>
newtype WebGLShaderPrecisionFormat = WebGLShaderPrecisionFormat { unWebGLShaderPrecisionFormat :: JSVal }

instance PToJSVal WebGLShaderPrecisionFormat where
  pToJSVal = unWebGLShaderPrecisionFormat
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLShaderPrecisionFormat where
  pFromJSVal = WebGLShaderPrecisionFormat
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLShaderPrecisionFormat where
  toJSVal = return . unWebGLShaderPrecisionFormat
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLShaderPrecisionFormat where
  fromJSVal v = fmap WebGLShaderPrecisionFormat <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLShaderPrecisionFormat
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLShaderPrecisionFormat where
  makeObject = makeObject . unWebGLShaderPrecisionFormat

instance IsGObject WebGLShaderPrecisionFormat where
  typeGType _ = gTypeWebGLShaderPrecisionFormat
  {-# INLINE typeGType #-}

noWebGLShaderPrecisionFormat :: Maybe WebGLShaderPrecisionFormat
noWebGLShaderPrecisionFormat = Nothing
{-# INLINE noWebGLShaderPrecisionFormat #-}

gTypeWebGLShaderPrecisionFormat :: JSM GType
gTypeWebGLShaderPrecisionFormat = GType . Object <$> jsg "WebGLShaderPrecisionFormat"

-- | Functions for this inteface are in "JSDOM.WebGLSync".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLSync Mozilla WebGLSync documentation>
newtype WebGLSync = WebGLSync { unWebGLSync :: JSVal }

instance PToJSVal WebGLSync where
  pToJSVal = unWebGLSync
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLSync where
  pFromJSVal = WebGLSync
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLSync where
  toJSVal = return . unWebGLSync
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLSync where
  fromJSVal v = fmap WebGLSync <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLSync
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLSync where
  makeObject = makeObject . unWebGLSync

instance IsGObject WebGLSync where
  typeGType _ = gTypeWebGLSync
  {-# INLINE typeGType #-}

noWebGLSync :: Maybe WebGLSync
noWebGLSync = Nothing
{-# INLINE noWebGLSync #-}

gTypeWebGLSync :: JSM GType
gTypeWebGLSync = GType . Object <$> jsg "WebGLSync"

-- | Functions for this inteface are in "JSDOM.WebGLTexture".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLTexture Mozilla WebGLTexture documentation>
newtype WebGLTexture = WebGLTexture { unWebGLTexture :: JSVal }

instance PToJSVal WebGLTexture where
  pToJSVal = unWebGLTexture
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLTexture where
  pFromJSVal = WebGLTexture
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLTexture where
  toJSVal = return . unWebGLTexture
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLTexture where
  fromJSVal v = fmap WebGLTexture <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLTexture
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLTexture where
  makeObject = makeObject . unWebGLTexture

instance IsGObject WebGLTexture where
  typeGType _ = gTypeWebGLTexture
  {-# INLINE typeGType #-}

noWebGLTexture :: Maybe WebGLTexture
noWebGLTexture = Nothing
{-# INLINE noWebGLTexture #-}

gTypeWebGLTexture :: JSM GType
gTypeWebGLTexture = GType . Object <$> jsg "WebGLTexture"

-- | Functions for this inteface are in "JSDOM.WebGLTransformFeedback".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLTransformFeedback Mozilla WebGLTransformFeedback documentation>
newtype WebGLTransformFeedback = WebGLTransformFeedback { unWebGLTransformFeedback :: JSVal }

instance PToJSVal WebGLTransformFeedback where
  pToJSVal = unWebGLTransformFeedback
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLTransformFeedback where
  pFromJSVal = WebGLTransformFeedback
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLTransformFeedback where
  toJSVal = return . unWebGLTransformFeedback
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLTransformFeedback where
  fromJSVal v = fmap WebGLTransformFeedback <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLTransformFeedback
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLTransformFeedback where
  makeObject = makeObject . unWebGLTransformFeedback

instance IsGObject WebGLTransformFeedback where
  typeGType _ = gTypeWebGLTransformFeedback
  {-# INLINE typeGType #-}

noWebGLTransformFeedback :: Maybe WebGLTransformFeedback
noWebGLTransformFeedback = Nothing
{-# INLINE noWebGLTransformFeedback #-}

gTypeWebGLTransformFeedback :: JSM GType
gTypeWebGLTransformFeedback = GType . Object <$> jsg "WebGLTransformFeedback"

-- | Functions for this inteface are in "JSDOM.WebGLUniformLocation".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLUniformLocation Mozilla WebGLUniformLocation documentation>
newtype WebGLUniformLocation = WebGLUniformLocation { unWebGLUniformLocation :: JSVal }

instance PToJSVal WebGLUniformLocation where
  pToJSVal = unWebGLUniformLocation
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLUniformLocation where
  pFromJSVal = WebGLUniformLocation
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLUniformLocation where
  toJSVal = return . unWebGLUniformLocation
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLUniformLocation where
  fromJSVal v = fmap WebGLUniformLocation <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLUniformLocation
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLUniformLocation where
  makeObject = makeObject . unWebGLUniformLocation

instance IsGObject WebGLUniformLocation where
  typeGType _ = gTypeWebGLUniformLocation
  {-# INLINE typeGType #-}

noWebGLUniformLocation :: Maybe WebGLUniformLocation
noWebGLUniformLocation = Nothing
{-# INLINE noWebGLUniformLocation #-}

gTypeWebGLUniformLocation :: JSM GType
gTypeWebGLUniformLocation = GType . Object <$> jsg "WebGLUniformLocation"

-- | Functions for this inteface are in "JSDOM.WebGLVertexArrayObject".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLVertexArrayObject Mozilla WebGLVertexArrayObject documentation>
newtype WebGLVertexArrayObject = WebGLVertexArrayObject { unWebGLVertexArrayObject :: JSVal }

instance PToJSVal WebGLVertexArrayObject where
  pToJSVal = unWebGLVertexArrayObject
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLVertexArrayObject where
  pFromJSVal = WebGLVertexArrayObject
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLVertexArrayObject where
  toJSVal = return . unWebGLVertexArrayObject
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLVertexArrayObject where
  fromJSVal v = fmap WebGLVertexArrayObject <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLVertexArrayObject
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLVertexArrayObject where
  makeObject = makeObject . unWebGLVertexArrayObject

instance IsGObject WebGLVertexArrayObject where
  typeGType _ = gTypeWebGLVertexArrayObject
  {-# INLINE typeGType #-}

noWebGLVertexArrayObject :: Maybe WebGLVertexArrayObject
noWebGLVertexArrayObject = Nothing
{-# INLINE noWebGLVertexArrayObject #-}

gTypeWebGLVertexArrayObject :: JSM GType
gTypeWebGLVertexArrayObject = GType . Object <$> jsg "WebGLVertexArrayObject"

-- | Functions for this inteface are in "JSDOM.WebGLVertexArrayObjectOES".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGLVertexArrayObjectOES Mozilla WebGLVertexArrayObjectOES documentation>
newtype WebGLVertexArrayObjectOES = WebGLVertexArrayObjectOES { unWebGLVertexArrayObjectOES :: JSVal }

instance PToJSVal WebGLVertexArrayObjectOES where
  pToJSVal = unWebGLVertexArrayObjectOES
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGLVertexArrayObjectOES where
  pFromJSVal = WebGLVertexArrayObjectOES
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGLVertexArrayObjectOES where
  toJSVal = return . unWebGLVertexArrayObjectOES
  {-# INLINE toJSVal #-}

instance FromJSVal WebGLVertexArrayObjectOES where
  fromJSVal v = fmap WebGLVertexArrayObjectOES <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGLVertexArrayObjectOES
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGLVertexArrayObjectOES where
  makeObject = makeObject . unWebGLVertexArrayObjectOES

instance IsGObject WebGLVertexArrayObjectOES where
  typeGType _ = gTypeWebGLVertexArrayObjectOES
  {-# INLINE typeGType #-}

noWebGLVertexArrayObjectOES :: Maybe WebGLVertexArrayObjectOES
noWebGLVertexArrayObjectOES = Nothing
{-# INLINE noWebGLVertexArrayObjectOES #-}

gTypeWebGLVertexArrayObjectOES :: JSM GType
gTypeWebGLVertexArrayObjectOES = GType . Object <$> jsg "WebGLVertexArrayObjectOES"

-- | Functions for this inteface are in "JSDOM.WebGPUBuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUBuffer Mozilla WebGPUBuffer documentation>
newtype WebGPUBuffer = WebGPUBuffer { unWebGPUBuffer :: JSVal }

instance PToJSVal WebGPUBuffer where
  pToJSVal = unWebGPUBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUBuffer where
  pFromJSVal = WebGPUBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUBuffer where
  toJSVal = return . unWebGPUBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUBuffer where
  fromJSVal v = fmap WebGPUBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUBuffer where
  makeObject = makeObject . unWebGPUBuffer

instance IsGObject WebGPUBuffer where
  typeGType _ = gTypeWebGPUBuffer
  {-# INLINE typeGType #-}

noWebGPUBuffer :: Maybe WebGPUBuffer
noWebGPUBuffer = Nothing
{-# INLINE noWebGPUBuffer #-}

gTypeWebGPUBuffer :: JSM GType
gTypeWebGPUBuffer = GType . Object <$> jsg "WebGPUBuffer"

-- | Functions for this inteface are in "JSDOM.WebGPUCommandBuffer".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUCommandBuffer Mozilla WebGPUCommandBuffer documentation>
newtype WebGPUCommandBuffer = WebGPUCommandBuffer { unWebGPUCommandBuffer :: JSVal }

instance PToJSVal WebGPUCommandBuffer where
  pToJSVal = unWebGPUCommandBuffer
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUCommandBuffer where
  pFromJSVal = WebGPUCommandBuffer
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUCommandBuffer where
  toJSVal = return . unWebGPUCommandBuffer
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUCommandBuffer where
  fromJSVal v = fmap WebGPUCommandBuffer <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUCommandBuffer
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUCommandBuffer where
  makeObject = makeObject . unWebGPUCommandBuffer

instance IsGObject WebGPUCommandBuffer where
  typeGType _ = gTypeWebGPUCommandBuffer
  {-# INLINE typeGType #-}

noWebGPUCommandBuffer :: Maybe WebGPUCommandBuffer
noWebGPUCommandBuffer = Nothing
{-# INLINE noWebGPUCommandBuffer #-}

gTypeWebGPUCommandBuffer :: JSM GType
gTypeWebGPUCommandBuffer = GType . Object <$> jsg "WebGPUCommandBuffer"

-- | Functions for this inteface are in "JSDOM.WebGPUCommandQueue".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUCommandQueue Mozilla WebGPUCommandQueue documentation>
newtype WebGPUCommandQueue = WebGPUCommandQueue { unWebGPUCommandQueue :: JSVal }

instance PToJSVal WebGPUCommandQueue where
  pToJSVal = unWebGPUCommandQueue
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUCommandQueue where
  pFromJSVal = WebGPUCommandQueue
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUCommandQueue where
  toJSVal = return . unWebGPUCommandQueue
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUCommandQueue where
  fromJSVal v = fmap WebGPUCommandQueue <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUCommandQueue
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUCommandQueue where
  makeObject = makeObject . unWebGPUCommandQueue

instance IsGObject WebGPUCommandQueue where
  typeGType _ = gTypeWebGPUCommandQueue
  {-# INLINE typeGType #-}

noWebGPUCommandQueue :: Maybe WebGPUCommandQueue
noWebGPUCommandQueue = Nothing
{-# INLINE noWebGPUCommandQueue #-}

gTypeWebGPUCommandQueue :: JSM GType
gTypeWebGPUCommandQueue = GType . Object <$> jsg "WebGPUCommandQueue"

-- | Functions for this inteface are in "JSDOM.WebGPUComputeCommandEncoder".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUComputeCommandEncoder Mozilla WebGPUComputeCommandEncoder documentation>
newtype WebGPUComputeCommandEncoder = WebGPUComputeCommandEncoder { unWebGPUComputeCommandEncoder :: JSVal }

instance PToJSVal WebGPUComputeCommandEncoder where
  pToJSVal = unWebGPUComputeCommandEncoder
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUComputeCommandEncoder where
  pFromJSVal = WebGPUComputeCommandEncoder
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUComputeCommandEncoder where
  toJSVal = return . unWebGPUComputeCommandEncoder
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUComputeCommandEncoder where
  fromJSVal v = fmap WebGPUComputeCommandEncoder <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUComputeCommandEncoder
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUComputeCommandEncoder where
  makeObject = makeObject . unWebGPUComputeCommandEncoder

instance IsGObject WebGPUComputeCommandEncoder where
  typeGType _ = gTypeWebGPUComputeCommandEncoder
  {-# INLINE typeGType #-}

noWebGPUComputeCommandEncoder :: Maybe WebGPUComputeCommandEncoder
noWebGPUComputeCommandEncoder = Nothing
{-# INLINE noWebGPUComputeCommandEncoder #-}

gTypeWebGPUComputeCommandEncoder :: JSM GType
gTypeWebGPUComputeCommandEncoder = GType . Object <$> jsg "WebGPUComputeCommandEncoder"

-- | Functions for this inteface are in "JSDOM.WebGPUComputePipelineState".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUComputePipelineState Mozilla WebGPUComputePipelineState documentation>
newtype WebGPUComputePipelineState = WebGPUComputePipelineState { unWebGPUComputePipelineState :: JSVal }

instance PToJSVal WebGPUComputePipelineState where
  pToJSVal = unWebGPUComputePipelineState
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUComputePipelineState where
  pFromJSVal = WebGPUComputePipelineState
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUComputePipelineState where
  toJSVal = return . unWebGPUComputePipelineState
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUComputePipelineState where
  fromJSVal v = fmap WebGPUComputePipelineState <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUComputePipelineState
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUComputePipelineState where
  makeObject = makeObject . unWebGPUComputePipelineState

instance IsGObject WebGPUComputePipelineState where
  typeGType _ = gTypeWebGPUComputePipelineState
  {-# INLINE typeGType #-}

noWebGPUComputePipelineState :: Maybe WebGPUComputePipelineState
noWebGPUComputePipelineState = Nothing
{-# INLINE noWebGPUComputePipelineState #-}

gTypeWebGPUComputePipelineState :: JSM GType
gTypeWebGPUComputePipelineState = GType . Object <$> jsg "WebGPUComputePipelineState"

-- | Functions for this inteface are in "JSDOM.WebGPUDepthStencilDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUDepthStencilDescriptor Mozilla WebGPUDepthStencilDescriptor documentation>
newtype WebGPUDepthStencilDescriptor = WebGPUDepthStencilDescriptor { unWebGPUDepthStencilDescriptor :: JSVal }

instance PToJSVal WebGPUDepthStencilDescriptor where
  pToJSVal = unWebGPUDepthStencilDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUDepthStencilDescriptor where
  pFromJSVal = WebGPUDepthStencilDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUDepthStencilDescriptor where
  toJSVal = return . unWebGPUDepthStencilDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUDepthStencilDescriptor where
  fromJSVal v = fmap WebGPUDepthStencilDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUDepthStencilDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUDepthStencilDescriptor where
  makeObject = makeObject . unWebGPUDepthStencilDescriptor

instance IsGObject WebGPUDepthStencilDescriptor where
  typeGType _ = gTypeWebGPUDepthStencilDescriptor
  {-# INLINE typeGType #-}

noWebGPUDepthStencilDescriptor :: Maybe WebGPUDepthStencilDescriptor
noWebGPUDepthStencilDescriptor = Nothing
{-# INLINE noWebGPUDepthStencilDescriptor #-}

gTypeWebGPUDepthStencilDescriptor :: JSM GType
gTypeWebGPUDepthStencilDescriptor = GType . Object <$> jsg "WebGPUDepthStencilDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPUDepthStencilState".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUDepthStencilState Mozilla WebGPUDepthStencilState documentation>
newtype WebGPUDepthStencilState = WebGPUDepthStencilState { unWebGPUDepthStencilState :: JSVal }

instance PToJSVal WebGPUDepthStencilState where
  pToJSVal = unWebGPUDepthStencilState
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUDepthStencilState where
  pFromJSVal = WebGPUDepthStencilState
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUDepthStencilState where
  toJSVal = return . unWebGPUDepthStencilState
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUDepthStencilState where
  fromJSVal v = fmap WebGPUDepthStencilState <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUDepthStencilState
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUDepthStencilState where
  makeObject = makeObject . unWebGPUDepthStencilState

instance IsGObject WebGPUDepthStencilState where
  typeGType _ = gTypeWebGPUDepthStencilState
  {-# INLINE typeGType #-}

noWebGPUDepthStencilState :: Maybe WebGPUDepthStencilState
noWebGPUDepthStencilState = Nothing
{-# INLINE noWebGPUDepthStencilState #-}

gTypeWebGPUDepthStencilState :: JSM GType
gTypeWebGPUDepthStencilState = GType . Object <$> jsg "WebGPUDepthStencilState"

-- | Functions for this inteface are in "JSDOM.WebGPUDrawable".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUDrawable Mozilla WebGPUDrawable documentation>
newtype WebGPUDrawable = WebGPUDrawable { unWebGPUDrawable :: JSVal }

instance PToJSVal WebGPUDrawable where
  pToJSVal = unWebGPUDrawable
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUDrawable where
  pFromJSVal = WebGPUDrawable
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUDrawable where
  toJSVal = return . unWebGPUDrawable
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUDrawable where
  fromJSVal v = fmap WebGPUDrawable <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUDrawable
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUDrawable where
  makeObject = makeObject . unWebGPUDrawable

instance IsGObject WebGPUDrawable where
  typeGType _ = gTypeWebGPUDrawable
  {-# INLINE typeGType #-}

noWebGPUDrawable :: Maybe WebGPUDrawable
noWebGPUDrawable = Nothing
{-# INLINE noWebGPUDrawable #-}

gTypeWebGPUDrawable :: JSM GType
gTypeWebGPUDrawable = GType . Object <$> jsg "WebGPUDrawable"

-- | Functions for this inteface are in "JSDOM.WebGPUFunction".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUFunction Mozilla WebGPUFunction documentation>
newtype WebGPUFunction = WebGPUFunction { unWebGPUFunction :: JSVal }

instance PToJSVal WebGPUFunction where
  pToJSVal = unWebGPUFunction
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUFunction where
  pFromJSVal = WebGPUFunction
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUFunction where
  toJSVal = return . unWebGPUFunction
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUFunction where
  fromJSVal v = fmap WebGPUFunction <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUFunction
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUFunction where
  makeObject = makeObject . unWebGPUFunction

instance IsGObject WebGPUFunction where
  typeGType _ = gTypeWebGPUFunction
  {-# INLINE typeGType #-}

noWebGPUFunction :: Maybe WebGPUFunction
noWebGPUFunction = Nothing
{-# INLINE noWebGPUFunction #-}

gTypeWebGPUFunction :: JSM GType
gTypeWebGPUFunction = GType . Object <$> jsg "WebGPUFunction"

-- | Functions for this inteface are in "JSDOM.WebGPULibrary".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPULibrary Mozilla WebGPULibrary documentation>
newtype WebGPULibrary = WebGPULibrary { unWebGPULibrary :: JSVal }

instance PToJSVal WebGPULibrary where
  pToJSVal = unWebGPULibrary
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPULibrary where
  pFromJSVal = WebGPULibrary
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPULibrary where
  toJSVal = return . unWebGPULibrary
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPULibrary where
  fromJSVal v = fmap WebGPULibrary <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPULibrary
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPULibrary where
  makeObject = makeObject . unWebGPULibrary

instance IsGObject WebGPULibrary where
  typeGType _ = gTypeWebGPULibrary
  {-# INLINE typeGType #-}

noWebGPULibrary :: Maybe WebGPULibrary
noWebGPULibrary = Nothing
{-# INLINE noWebGPULibrary #-}

gTypeWebGPULibrary :: JSM GType
gTypeWebGPULibrary = GType . Object <$> jsg "WebGPULibrary"

-- | Functions for this inteface are in "JSDOM.WebGPURenderCommandEncoder".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderCommandEncoder Mozilla WebGPURenderCommandEncoder documentation>
newtype WebGPURenderCommandEncoder = WebGPURenderCommandEncoder { unWebGPURenderCommandEncoder :: JSVal }

instance PToJSVal WebGPURenderCommandEncoder where
  pToJSVal = unWebGPURenderCommandEncoder
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderCommandEncoder where
  pFromJSVal = WebGPURenderCommandEncoder
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderCommandEncoder where
  toJSVal = return . unWebGPURenderCommandEncoder
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderCommandEncoder where
  fromJSVal v = fmap WebGPURenderCommandEncoder <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderCommandEncoder
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderCommandEncoder where
  makeObject = makeObject . unWebGPURenderCommandEncoder

instance IsGObject WebGPURenderCommandEncoder where
  typeGType _ = gTypeWebGPURenderCommandEncoder
  {-# INLINE typeGType #-}

noWebGPURenderCommandEncoder :: Maybe WebGPURenderCommandEncoder
noWebGPURenderCommandEncoder = Nothing
{-# INLINE noWebGPURenderCommandEncoder #-}

gTypeWebGPURenderCommandEncoder :: JSM GType
gTypeWebGPURenderCommandEncoder = GType . Object <$> jsg "WebGPURenderCommandEncoder"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPassAttachmentDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPassAttachmentDescriptor Mozilla WebGPURenderPassAttachmentDescriptor documentation>
newtype WebGPURenderPassAttachmentDescriptor = WebGPURenderPassAttachmentDescriptor { unWebGPURenderPassAttachmentDescriptor :: JSVal }

instance PToJSVal WebGPURenderPassAttachmentDescriptor where
  pToJSVal = unWebGPURenderPassAttachmentDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPassAttachmentDescriptor where
  pFromJSVal = WebGPURenderPassAttachmentDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPassAttachmentDescriptor where
  toJSVal = return . unWebGPURenderPassAttachmentDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPassAttachmentDescriptor where
  fromJSVal v = fmap WebGPURenderPassAttachmentDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPassAttachmentDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPassAttachmentDescriptor where
  makeObject = makeObject . unWebGPURenderPassAttachmentDescriptor

class (IsGObject o) => IsWebGPURenderPassAttachmentDescriptor o
toWebGPURenderPassAttachmentDescriptor :: IsWebGPURenderPassAttachmentDescriptor o => o -> WebGPURenderPassAttachmentDescriptor
toWebGPURenderPassAttachmentDescriptor = WebGPURenderPassAttachmentDescriptor . coerce

instance IsWebGPURenderPassAttachmentDescriptor WebGPURenderPassAttachmentDescriptor
instance IsGObject WebGPURenderPassAttachmentDescriptor where
  typeGType _ = gTypeWebGPURenderPassAttachmentDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPassAttachmentDescriptor :: Maybe WebGPURenderPassAttachmentDescriptor
noWebGPURenderPassAttachmentDescriptor = Nothing
{-# INLINE noWebGPURenderPassAttachmentDescriptor #-}

gTypeWebGPURenderPassAttachmentDescriptor :: JSM GType
gTypeWebGPURenderPassAttachmentDescriptor = GType . Object <$> jsg "WebGPURenderPassAttachmentDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPassColorAttachmentDescriptor".
-- Base interface functions are in:
--
--     * "JSDOM.WebGPURenderPassAttachmentDescriptor"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPassColorAttachmentDescriptor Mozilla WebGPURenderPassColorAttachmentDescriptor documentation>
newtype WebGPURenderPassColorAttachmentDescriptor = WebGPURenderPassColorAttachmentDescriptor { unWebGPURenderPassColorAttachmentDescriptor :: JSVal }

instance PToJSVal WebGPURenderPassColorAttachmentDescriptor where
  pToJSVal = unWebGPURenderPassColorAttachmentDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPassColorAttachmentDescriptor where
  pFromJSVal = WebGPURenderPassColorAttachmentDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPassColorAttachmentDescriptor where
  toJSVal = return . unWebGPURenderPassColorAttachmentDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPassColorAttachmentDescriptor where
  fromJSVal v = fmap WebGPURenderPassColorAttachmentDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPassColorAttachmentDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPassColorAttachmentDescriptor where
  makeObject = makeObject . unWebGPURenderPassColorAttachmentDescriptor

instance IsWebGPURenderPassAttachmentDescriptor WebGPURenderPassColorAttachmentDescriptor
instance IsGObject WebGPURenderPassColorAttachmentDescriptor where
  typeGType _ = gTypeWebGPURenderPassColorAttachmentDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPassColorAttachmentDescriptor :: Maybe WebGPURenderPassColorAttachmentDescriptor
noWebGPURenderPassColorAttachmentDescriptor = Nothing
{-# INLINE noWebGPURenderPassColorAttachmentDescriptor #-}

gTypeWebGPURenderPassColorAttachmentDescriptor :: JSM GType
gTypeWebGPURenderPassColorAttachmentDescriptor = GType . Object <$> jsg "WebGPURenderPassColorAttachmentDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPassDepthAttachmentDescriptor".
-- Base interface functions are in:
--
--     * "JSDOM.WebGPURenderPassAttachmentDescriptor"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPassDepthAttachmentDescriptor Mozilla WebGPURenderPassDepthAttachmentDescriptor documentation>
newtype WebGPURenderPassDepthAttachmentDescriptor = WebGPURenderPassDepthAttachmentDescriptor { unWebGPURenderPassDepthAttachmentDescriptor :: JSVal }

instance PToJSVal WebGPURenderPassDepthAttachmentDescriptor where
  pToJSVal = unWebGPURenderPassDepthAttachmentDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPassDepthAttachmentDescriptor where
  pFromJSVal = WebGPURenderPassDepthAttachmentDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPassDepthAttachmentDescriptor where
  toJSVal = return . unWebGPURenderPassDepthAttachmentDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPassDepthAttachmentDescriptor where
  fromJSVal v = fmap WebGPURenderPassDepthAttachmentDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPassDepthAttachmentDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPassDepthAttachmentDescriptor where
  makeObject = makeObject . unWebGPURenderPassDepthAttachmentDescriptor

instance IsWebGPURenderPassAttachmentDescriptor WebGPURenderPassDepthAttachmentDescriptor
instance IsGObject WebGPURenderPassDepthAttachmentDescriptor where
  typeGType _ = gTypeWebGPURenderPassDepthAttachmentDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPassDepthAttachmentDescriptor :: Maybe WebGPURenderPassDepthAttachmentDescriptor
noWebGPURenderPassDepthAttachmentDescriptor = Nothing
{-# INLINE noWebGPURenderPassDepthAttachmentDescriptor #-}

gTypeWebGPURenderPassDepthAttachmentDescriptor :: JSM GType
gTypeWebGPURenderPassDepthAttachmentDescriptor = GType . Object <$> jsg "WebGPURenderPassDepthAttachmentDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPassDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPassDescriptor Mozilla WebGPURenderPassDescriptor documentation>
newtype WebGPURenderPassDescriptor = WebGPURenderPassDescriptor { unWebGPURenderPassDescriptor :: JSVal }

instance PToJSVal WebGPURenderPassDescriptor where
  pToJSVal = unWebGPURenderPassDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPassDescriptor where
  pFromJSVal = WebGPURenderPassDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPassDescriptor where
  toJSVal = return . unWebGPURenderPassDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPassDescriptor where
  fromJSVal v = fmap WebGPURenderPassDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPassDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPassDescriptor where
  makeObject = makeObject . unWebGPURenderPassDescriptor

instance IsGObject WebGPURenderPassDescriptor where
  typeGType _ = gTypeWebGPURenderPassDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPassDescriptor :: Maybe WebGPURenderPassDescriptor
noWebGPURenderPassDescriptor = Nothing
{-# INLINE noWebGPURenderPassDescriptor #-}

gTypeWebGPURenderPassDescriptor :: JSM GType
gTypeWebGPURenderPassDescriptor = GType . Object <$> jsg "WebGPURenderPassDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPipelineColorAttachmentDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPipelineColorAttachmentDescriptor Mozilla WebGPURenderPipelineColorAttachmentDescriptor documentation>
newtype WebGPURenderPipelineColorAttachmentDescriptor = WebGPURenderPipelineColorAttachmentDescriptor { unWebGPURenderPipelineColorAttachmentDescriptor :: JSVal }

instance PToJSVal WebGPURenderPipelineColorAttachmentDescriptor where
  pToJSVal = unWebGPURenderPipelineColorAttachmentDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPipelineColorAttachmentDescriptor where
  pFromJSVal = WebGPURenderPipelineColorAttachmentDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPipelineColorAttachmentDescriptor where
  toJSVal = return . unWebGPURenderPipelineColorAttachmentDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPipelineColorAttachmentDescriptor where
  fromJSVal v = fmap WebGPURenderPipelineColorAttachmentDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPipelineColorAttachmentDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPipelineColorAttachmentDescriptor where
  makeObject = makeObject . unWebGPURenderPipelineColorAttachmentDescriptor

instance IsGObject WebGPURenderPipelineColorAttachmentDescriptor where
  typeGType _ = gTypeWebGPURenderPipelineColorAttachmentDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPipelineColorAttachmentDescriptor :: Maybe WebGPURenderPipelineColorAttachmentDescriptor
noWebGPURenderPipelineColorAttachmentDescriptor = Nothing
{-# INLINE noWebGPURenderPipelineColorAttachmentDescriptor #-}

gTypeWebGPURenderPipelineColorAttachmentDescriptor :: JSM GType
gTypeWebGPURenderPipelineColorAttachmentDescriptor = GType . Object <$> jsg "WebGPURenderPipelineColorAttachmentDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPipelineDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPipelineDescriptor Mozilla WebGPURenderPipelineDescriptor documentation>
newtype WebGPURenderPipelineDescriptor = WebGPURenderPipelineDescriptor { unWebGPURenderPipelineDescriptor :: JSVal }

instance PToJSVal WebGPURenderPipelineDescriptor where
  pToJSVal = unWebGPURenderPipelineDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPipelineDescriptor where
  pFromJSVal = WebGPURenderPipelineDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPipelineDescriptor where
  toJSVal = return . unWebGPURenderPipelineDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPipelineDescriptor where
  fromJSVal v = fmap WebGPURenderPipelineDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPipelineDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPipelineDescriptor where
  makeObject = makeObject . unWebGPURenderPipelineDescriptor

instance IsGObject WebGPURenderPipelineDescriptor where
  typeGType _ = gTypeWebGPURenderPipelineDescriptor
  {-# INLINE typeGType #-}

noWebGPURenderPipelineDescriptor :: Maybe WebGPURenderPipelineDescriptor
noWebGPURenderPipelineDescriptor = Nothing
{-# INLINE noWebGPURenderPipelineDescriptor #-}

gTypeWebGPURenderPipelineDescriptor :: JSM GType
gTypeWebGPURenderPipelineDescriptor = GType . Object <$> jsg "WebGPURenderPipelineDescriptor"

-- | Functions for this inteface are in "JSDOM.WebGPURenderPipelineState".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderPipelineState Mozilla WebGPURenderPipelineState documentation>
newtype WebGPURenderPipelineState = WebGPURenderPipelineState { unWebGPURenderPipelineState :: JSVal }

instance PToJSVal WebGPURenderPipelineState where
  pToJSVal = unWebGPURenderPipelineState
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderPipelineState where
  pFromJSVal = WebGPURenderPipelineState
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderPipelineState where
  toJSVal = return . unWebGPURenderPipelineState
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderPipelineState where
  fromJSVal v = fmap WebGPURenderPipelineState <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderPipelineState
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderPipelineState where
  makeObject = makeObject . unWebGPURenderPipelineState

instance IsGObject WebGPURenderPipelineState where
  typeGType _ = gTypeWebGPURenderPipelineState
  {-# INLINE typeGType #-}

noWebGPURenderPipelineState :: Maybe WebGPURenderPipelineState
noWebGPURenderPipelineState = Nothing
{-# INLINE noWebGPURenderPipelineState #-}

gTypeWebGPURenderPipelineState :: JSM GType
gTypeWebGPURenderPipelineState = GType . Object <$> jsg "WebGPURenderPipelineState"

-- | Functions for this inteface are in "JSDOM.WebGPURenderingContext".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPURenderingContext Mozilla WebGPURenderingContext documentation>
newtype WebGPURenderingContext = WebGPURenderingContext { unWebGPURenderingContext :: JSVal }

instance PToJSVal WebGPURenderingContext where
  pToJSVal = unWebGPURenderingContext
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPURenderingContext where
  pFromJSVal = WebGPURenderingContext
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPURenderingContext where
  toJSVal = return . unWebGPURenderingContext
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPURenderingContext where
  fromJSVal v = fmap WebGPURenderingContext <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPURenderingContext
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPURenderingContext where
  makeObject = makeObject . unWebGPURenderingContext

instance IsGObject WebGPURenderingContext where
  typeGType _ = gTypeWebGPURenderingContext
  {-# INLINE typeGType #-}

noWebGPURenderingContext :: Maybe WebGPURenderingContext
noWebGPURenderingContext = Nothing
{-# INLINE noWebGPURenderingContext #-}

gTypeWebGPURenderingContext :: JSM GType
gTypeWebGPURenderingContext = GType . Object <$> jsg "WebGPURenderingContext"

-- | Functions for this inteface are in "JSDOM.WebGPUSize".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUSize Mozilla WebGPUSize documentation>
newtype WebGPUSize = WebGPUSize { unWebGPUSize :: JSVal }

instance PToJSVal WebGPUSize where
  pToJSVal = unWebGPUSize
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUSize where
  pFromJSVal = WebGPUSize
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUSize where
  toJSVal = return . unWebGPUSize
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUSize where
  fromJSVal v = fmap WebGPUSize <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUSize
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUSize where
  makeObject = makeObject . unWebGPUSize

instance IsGObject WebGPUSize where
  typeGType _ = gTypeWebGPUSize
  {-# INLINE typeGType #-}

noWebGPUSize :: Maybe WebGPUSize
noWebGPUSize = Nothing
{-# INLINE noWebGPUSize #-}

gTypeWebGPUSize :: JSM GType
gTypeWebGPUSize = GType . Object <$> jsg "WebGPUSize"

-- | Functions for this inteface are in "JSDOM.WebGPUTexture".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUTexture Mozilla WebGPUTexture documentation>
newtype WebGPUTexture = WebGPUTexture { unWebGPUTexture :: JSVal }

instance PToJSVal WebGPUTexture where
  pToJSVal = unWebGPUTexture
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUTexture where
  pFromJSVal = WebGPUTexture
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUTexture where
  toJSVal = return . unWebGPUTexture
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUTexture where
  fromJSVal v = fmap WebGPUTexture <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUTexture
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUTexture where
  makeObject = makeObject . unWebGPUTexture

instance IsGObject WebGPUTexture where
  typeGType _ = gTypeWebGPUTexture
  {-# INLINE typeGType #-}

noWebGPUTexture :: Maybe WebGPUTexture
noWebGPUTexture = Nothing
{-# INLINE noWebGPUTexture #-}

gTypeWebGPUTexture :: JSM GType
gTypeWebGPUTexture = GType . Object <$> jsg "WebGPUTexture"

-- | Functions for this inteface are in "JSDOM.WebGPUTextureDescriptor".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WebGPUTextureDescriptor Mozilla WebGPUTextureDescriptor documentation>
newtype WebGPUTextureDescriptor = WebGPUTextureDescriptor { unWebGPUTextureDescriptor :: JSVal }

instance PToJSVal WebGPUTextureDescriptor where
  pToJSVal = unWebGPUTextureDescriptor
  {-# INLINE pToJSVal #-}

instance PFromJSVal WebGPUTextureDescriptor where
  pFromJSVal = WebGPUTextureDescriptor
  {-# INLINE pFromJSVal #-}

instance ToJSVal WebGPUTextureDescriptor where
  toJSVal = return . unWebGPUTextureDescriptor
  {-# INLINE toJSVal #-}

instance FromJSVal WebGPUTextureDescriptor where
  fromJSVal v = fmap WebGPUTextureDescriptor <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WebGPUTextureDescriptor
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WebGPUTextureDescriptor where
  makeObject = makeObject . unWebGPUTextureDescriptor

instance IsGObject WebGPUTextureDescriptor where
  typeGType _ = gTypeWebGPUTextureDescriptor
  {-# INLINE typeGType #-}

noWebGPUTextureDescriptor :: Maybe WebGPUTextureDescriptor
noWebGPUTextureDescriptor = Nothing
{-# INLINE noWebGPUTextureDescriptor #-}

gTypeWebGPUTextureDescriptor :: JSM GType
gTypeWebGPUTextureDescriptor = GType . Object <$> jsg "WebGPUTextureDescriptor"

