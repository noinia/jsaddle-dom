{-# LANGUAGE
  FlexibleContexts,
  FlexibleInstances #-}
module JSDOM.Types.TypesHTML where

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

-- | Functions for this inteface are in "JSDOM.Text".
-- Base interface functions are in:
--
--     * "JSDOM.CharacterData"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Slotable"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Text Mozilla Text documentation>
newtype Text = Text { unText :: JSVal }

instance PToJSVal Text where
  pToJSVal = unText
  {-# INLINE pToJSVal #-}

instance PFromJSVal Text where
  pFromJSVal = Text
  {-# INLINE pFromJSVal #-}

instance ToJSVal Text where
  toJSVal = return . unText
  {-# INLINE toJSVal #-}

instance FromJSVal Text where
  fromJSVal v = fmap Text <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Text
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Text where
  makeObject = makeObject . unText

class (IsCharacterData o, IsNode o, IsEventTarget o, IsNonDocumentTypeChildNode o, IsChildNode o, IsSlotable o, IsGObject o) => IsText o
toText :: IsText o => o -> Text
toText = Text . coerce

instance IsText Text
instance IsCharacterData Text
instance IsNode Text
instance IsEventTarget Text
instance IsNonDocumentTypeChildNode Text
instance IsChildNode Text
instance IsSlotable Text
instance IsGObject Text where
  typeGType _ = gTypeText
  {-# INLINE typeGType #-}

noText :: Maybe Text
noText = Nothing
{-# INLINE noText #-}

gTypeText :: JSM GType
gTypeText = GType . Object <$> jsg "Text"

-- | Functions for this inteface are in "JSDOM.NodeList".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NodeList Mozilla NodeList documentation>
newtype NodeList = NodeList { unNodeList :: JSVal }

instance PToJSVal NodeList where
  pToJSVal = unNodeList
  {-# INLINE pToJSVal #-}

instance PFromJSVal NodeList where
  pFromJSVal = NodeList
  {-# INLINE pFromJSVal #-}

instance ToJSVal NodeList where
  toJSVal = return . unNodeList
  {-# INLINE toJSVal #-}

instance FromJSVal NodeList where
  fromJSVal v = fmap NodeList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NodeList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NodeList where
  makeObject = makeObject . unNodeList

class (IsGObject o) => IsNodeList o
toNodeList :: IsNodeList o => o -> NodeList
toNodeList = NodeList . coerce

instance IsNodeList NodeList
instance IsGObject NodeList where
  typeGType _ = gTypeNodeList
  {-# INLINE typeGType #-}

noNodeList :: Maybe NodeList
noNodeList = Nothing
{-# INLINE noNodeList #-}

gTypeNodeList :: JSM GType
gTypeNodeList = GType . Object <$> jsg "NodeList"


-- | Functions for this inteface are in "JSDOM.RadioNodeList".
-- Base interface functions are in:
--
--     * "JSDOM.NodeList"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/RadioNodeList Mozilla RadioNodeList documentation>
newtype RadioNodeList = RadioNodeList { unRadioNodeList :: JSVal }

instance PToJSVal RadioNodeList where
  pToJSVal = unRadioNodeList
  {-# INLINE pToJSVal #-}

instance PFromJSVal RadioNodeList where
  pFromJSVal = RadioNodeList
  {-# INLINE pFromJSVal #-}

instance ToJSVal RadioNodeList where
  toJSVal = return . unRadioNodeList
  {-# INLINE toJSVal #-}

instance FromJSVal RadioNodeList where
  fromJSVal v = fmap RadioNodeList <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . RadioNodeList
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject RadioNodeList where
  makeObject = makeObject . unRadioNodeList

instance IsNodeList RadioNodeList
instance IsGObject RadioNodeList where
  typeGType _ = gTypeRadioNodeList
  {-# INLINE typeGType #-}

noRadioNodeList :: Maybe RadioNodeList
noRadioNodeList = Nothing
{-# INLINE noRadioNodeList #-}

gTypeRadioNodeList :: JSM GType
gTypeRadioNodeList = GType . Object <$> jsg "RadioNodeList"


-- | Functions for this inteface are in "JSDOM.Attr".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Attr Mozilla Attr documentation>
newtype Attr = Attr { unAttr :: JSVal }

instance PToJSVal Attr where
  pToJSVal = unAttr
  {-# INLINE pToJSVal #-}

instance PFromJSVal Attr where
  pFromJSVal = Attr
  {-# INLINE pFromJSVal #-}

instance ToJSVal Attr where
  toJSVal = return . unAttr
  {-# INLINE toJSVal #-}

instance FromJSVal Attr where
  fromJSVal v = fmap Attr <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Attr
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Attr where
  makeObject = makeObject . unAttr

instance IsNode Attr
instance IsEventTarget Attr
instance IsGObject Attr where
  typeGType _ = gTypeAttr
  {-# INLINE typeGType #-}

noAttr :: Maybe Attr
noAttr = Nothing
{-# INLINE noAttr #-}

gTypeAttr :: JSM GType
gTypeAttr = GType . Object <$> jsg "Attr"

-- | Functions for this inteface are in "JSDOM.DocumentFragment".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonElementParentNode"
--     * "JSDOM.ParentNode"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment Mozilla DocumentFragment documentation>
newtype DocumentFragment = DocumentFragment { unDocumentFragment :: JSVal }

instance PToJSVal DocumentFragment where
  pToJSVal = unDocumentFragment
  {-# INLINE pToJSVal #-}

instance PFromJSVal DocumentFragment where
  pFromJSVal = DocumentFragment
  {-# INLINE pFromJSVal #-}

instance ToJSVal DocumentFragment where
  toJSVal = return . unDocumentFragment
  {-# INLINE toJSVal #-}

instance FromJSVal DocumentFragment where
  fromJSVal v = fmap DocumentFragment <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DocumentFragment
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DocumentFragment where
  makeObject = makeObject . unDocumentFragment

class (IsNode o, IsEventTarget o, IsNonElementParentNode o, IsParentNode o, IsGObject o) => IsDocumentFragment o
toDocumentFragment :: IsDocumentFragment o => o -> DocumentFragment
toDocumentFragment = DocumentFragment . coerce

instance IsDocumentFragment DocumentFragment
instance IsNode DocumentFragment
instance IsEventTarget DocumentFragment
instance IsNonElementParentNode DocumentFragment
instance IsParentNode DocumentFragment
instance IsGObject DocumentFragment where
  typeGType _ = gTypeDocumentFragment
  {-# INLINE typeGType #-}

noDocumentFragment :: Maybe DocumentFragment
noDocumentFragment = Nothing
{-# INLINE noDocumentFragment #-}

gTypeDocumentFragment :: JSM GType
gTypeDocumentFragment = GType . Object <$> jsg "DocumentFragment"

-- | Functions for this inteface are in "JSDOM.DocumentType".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.ChildNode"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DocumentType Mozilla DocumentType documentation>
newtype DocumentType = DocumentType { unDocumentType :: JSVal }

instance PToJSVal DocumentType where
  pToJSVal = unDocumentType
  {-# INLINE pToJSVal #-}

instance PFromJSVal DocumentType where
  pFromJSVal = DocumentType
  {-# INLINE pFromJSVal #-}

instance ToJSVal DocumentType where
  toJSVal = return . unDocumentType
  {-# INLINE toJSVal #-}

instance FromJSVal DocumentType where
  fromJSVal v = fmap DocumentType <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DocumentType
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DocumentType where
  makeObject = makeObject . unDocumentType

instance IsNode DocumentType
instance IsEventTarget DocumentType
instance IsChildNode DocumentType
instance IsGObject DocumentType where
  typeGType _ = gTypeDocumentType
  {-# INLINE typeGType #-}

noDocumentType :: Maybe DocumentType
noDocumentType = Nothing
{-# INLINE noDocumentType #-}

gTypeDocumentType :: JSM GType
gTypeDocumentType = GType . Object <$> jsg "DocumentType"

-- | Functions for this inteface are in "JSDOM.Comment".
-- Base interface functions are in:
--
--     * "JSDOM.CharacterData"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.ChildNode"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Comment Mozilla Comment documentation>
newtype Comment = Comment { unComment :: JSVal }

instance PToJSVal Comment where
  pToJSVal = unComment
  {-# INLINE pToJSVal #-}

instance PFromJSVal Comment where
  pFromJSVal = Comment
  {-# INLINE pFromJSVal #-}

instance ToJSVal Comment where
  toJSVal = return . unComment
  {-# INLINE toJSVal #-}

instance FromJSVal Comment where
  fromJSVal v = fmap Comment <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Comment
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Comment where
  makeObject = makeObject . unComment

instance IsCharacterData Comment
instance IsNode Comment
instance IsEventTarget Comment
instance IsNonDocumentTypeChildNode Comment
instance IsChildNode Comment
instance IsGObject Comment where
  typeGType _ = gTypeComment
  {-# INLINE typeGType #-}

noComment :: Maybe Comment
noComment = Nothing
{-# INLINE noComment #-}

gTypeComment :: JSM GType
gTypeComment = GType . Object <$> jsg "Comment"

-- | Functions for this inteface are in "JSDOM.CharacterData".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.ChildNode"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CharacterData Mozilla CharacterData documentation>
newtype CharacterData = CharacterData { unCharacterData :: JSVal }

instance PToJSVal CharacterData where
  pToJSVal = unCharacterData
  {-# INLINE pToJSVal #-}

instance PFromJSVal CharacterData where
  pFromJSVal = CharacterData
  {-# INLINE pFromJSVal #-}

instance ToJSVal CharacterData where
  toJSVal = return . unCharacterData
  {-# INLINE toJSVal #-}

instance FromJSVal CharacterData where
  fromJSVal v = fmap CharacterData <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CharacterData
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CharacterData where
  makeObject = makeObject . unCharacterData

class (IsNode o, IsEventTarget o, IsNonDocumentTypeChildNode o, IsChildNode o, IsGObject o) => IsCharacterData o
toCharacterData :: IsCharacterData o => o -> CharacterData
toCharacterData = CharacterData . coerce

instance IsCharacterData CharacterData
instance IsNode CharacterData
instance IsEventTarget CharacterData
instance IsNonDocumentTypeChildNode CharacterData
instance IsChildNode CharacterData
instance IsGObject CharacterData where
  typeGType _ = gTypeCharacterData
  {-# INLINE typeGType #-}

noCharacterData :: Maybe CharacterData
noCharacterData = Nothing
{-# INLINE noCharacterData #-}

gTypeCharacterData :: JSM GType
gTypeCharacterData = GType . Object <$> jsg "CharacterData"

-- | Functions for this inteface are in "JSDOM.CDATASection".
-- Base interface functions are in:
--
--     * "JSDOM.Text"
--     * "JSDOM.CharacterData"
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Slotable"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/CDATASection Mozilla CDATASection documentation>
newtype CDATASection = CDATASection { unCDATASection :: JSVal }

instance PToJSVal CDATASection where
  pToJSVal = unCDATASection
  {-# INLINE pToJSVal #-}

instance PFromJSVal CDATASection where
  pFromJSVal = CDATASection
  {-# INLINE pFromJSVal #-}

instance ToJSVal CDATASection where
  toJSVal = return . unCDATASection
  {-# INLINE toJSVal #-}

instance FromJSVal CDATASection where
  fromJSVal v = fmap CDATASection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . CDATASection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject CDATASection where
  makeObject = makeObject . unCDATASection

instance IsText CDATASection
instance IsCharacterData CDATASection
instance IsNode CDATASection
instance IsEventTarget CDATASection
instance IsNonDocumentTypeChildNode CDATASection
instance IsChildNode CDATASection
instance IsSlotable CDATASection
instance IsGObject CDATASection where
  typeGType _ = gTypeCDATASection
  {-# INLINE typeGType #-}

noCDATASection :: Maybe CDATASection
noCDATASection = Nothing
{-# INLINE noCDATASection #-}

gTypeCDATASection :: JSM GType
gTypeCDATASection = GType . Object <$> jsg "CDATASection"

-- | Functions for this inteface are in "JSDOM.NonElementParentNode".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NonElementParentNode Mozilla NonElementParentNode documentation>
newtype NonElementParentNode = NonElementParentNode { unNonElementParentNode :: JSVal }

instance PToJSVal NonElementParentNode where
  pToJSVal = unNonElementParentNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal NonElementParentNode where
  pFromJSVal = NonElementParentNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal NonElementParentNode where
  toJSVal = return . unNonElementParentNode
  {-# INLINE toJSVal #-}

instance FromJSVal NonElementParentNode where
  fromJSVal v = fmap NonElementParentNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NonElementParentNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NonElementParentNode where
  makeObject = makeObject . unNonElementParentNode

class (IsGObject o) => IsNonElementParentNode o
toNonElementParentNode :: IsNonElementParentNode o => o -> NonElementParentNode
toNonElementParentNode = NonElementParentNode . coerce

instance IsNonElementParentNode NonElementParentNode
instance IsGObject NonElementParentNode where
  typeGType _ = gTypeNonElementParentNode
  {-# INLINE typeGType #-}

noNonElementParentNode :: Maybe NonElementParentNode
noNonElementParentNode = Nothing
{-# INLINE noNonElementParentNode #-}

gTypeNonElementParentNode :: JSM GType
gTypeNonElementParentNode = GType . Object <$> jsg "NonElementParentNode"

-- | Functions for this inteface are in "JSDOM.DocumentOrShadowRoot".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DocumentOrShadowRoot Mozilla DocumentOrShadowRoot documentation>
newtype DocumentOrShadowRoot = DocumentOrShadowRoot { unDocumentOrShadowRoot :: JSVal }

instance PToJSVal DocumentOrShadowRoot where
  pToJSVal = unDocumentOrShadowRoot
  {-# INLINE pToJSVal #-}

instance PFromJSVal DocumentOrShadowRoot where
  pFromJSVal = DocumentOrShadowRoot
  {-# INLINE pFromJSVal #-}

instance ToJSVal DocumentOrShadowRoot where
  toJSVal = return . unDocumentOrShadowRoot
  {-# INLINE toJSVal #-}

instance FromJSVal DocumentOrShadowRoot where
  fromJSVal v = fmap DocumentOrShadowRoot <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DocumentOrShadowRoot
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DocumentOrShadowRoot where
  makeObject = makeObject . unDocumentOrShadowRoot

class (IsGObject o) => IsDocumentOrShadowRoot o
toDocumentOrShadowRoot :: IsDocumentOrShadowRoot o => o -> DocumentOrShadowRoot
toDocumentOrShadowRoot = DocumentOrShadowRoot . coerce

instance IsDocumentOrShadowRoot DocumentOrShadowRoot
instance IsGObject DocumentOrShadowRoot where
  typeGType _ = gTypeDocumentOrShadowRoot
  {-# INLINE typeGType #-}

noDocumentOrShadowRoot :: Maybe DocumentOrShadowRoot
noDocumentOrShadowRoot = Nothing
{-# INLINE noDocumentOrShadowRoot #-}

gTypeDocumentOrShadowRoot :: JSM GType
gTypeDocumentOrShadowRoot = GType . Object <$> jsg "DocumentOrShadowRoot"

-- | Functions for this inteface are in "JSDOM.Document".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.GlobalEventHandlers"
--     * "JSDOM.DocumentOrShadowRoot"
--     * "JSDOM.NonElementParentNode"
--     * "JSDOM.ParentNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Document Mozilla Document documentation>
newtype Document = Document { unDocument :: JSVal }

instance PToJSVal Document where
  pToJSVal = unDocument
  {-# INLINE pToJSVal #-}

instance PFromJSVal Document where
  pFromJSVal = Document
  {-# INLINE pFromJSVal #-}

instance ToJSVal Document where
  toJSVal = return . unDocument
  {-# INLINE toJSVal #-}

instance FromJSVal Document where
  fromJSVal v = fmap Document <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Document
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Document where
  makeObject = makeObject . unDocument

class (IsNode o, IsEventTarget o, IsGlobalEventHandlers o, IsDocumentOrShadowRoot o, IsNonElementParentNode o, IsParentNode o, IsDocumentAndElementEventHandlers o, IsGObject o) => IsDocument o
toDocument :: IsDocument o => o -> Document
toDocument = Document . coerce

instance IsDocument Document
instance IsNode Document
instance IsEventTarget Document
instance IsGlobalEventHandlers Document
instance IsDocumentOrShadowRoot Document
instance IsNonElementParentNode Document
instance IsParentNode Document
instance IsDocumentAndElementEventHandlers Document
instance IsGObject Document where
  typeGType _ = gTypeDocument
  {-# INLINE typeGType #-}

noDocument :: Maybe Document
noDocument = Nothing
{-# INLINE noDocument #-}

gTypeDocument :: JSM GType
gTypeDocument = GType . Object <$> jsg "Document"

-- | Functions for this inteface are in "JSDOM.WindowEventHandlers".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/WindowEventHandlers Mozilla WindowEventHandlers documentation>
newtype WindowEventHandlers = WindowEventHandlers { unWindowEventHandlers :: JSVal }

instance PToJSVal WindowEventHandlers where
  pToJSVal = unWindowEventHandlers
  {-# INLINE pToJSVal #-}

instance PFromJSVal WindowEventHandlers where
  pFromJSVal = WindowEventHandlers
  {-# INLINE pFromJSVal #-}

instance ToJSVal WindowEventHandlers where
  toJSVal = return . unWindowEventHandlers
  {-# INLINE toJSVal #-}

instance FromJSVal WindowEventHandlers where
  fromJSVal v = fmap WindowEventHandlers <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . WindowEventHandlers
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject WindowEventHandlers where
  makeObject = makeObject . unWindowEventHandlers

class (IsGObject o) => IsWindowEventHandlers o
toWindowEventHandlers :: IsWindowEventHandlers o => o -> WindowEventHandlers
toWindowEventHandlers = WindowEventHandlers . coerce

instance IsWindowEventHandlers WindowEventHandlers
instance IsGObject WindowEventHandlers where
  typeGType _ = gTypeWindowEventHandlers
  {-# INLINE typeGType #-}

noWindowEventHandlers :: Maybe WindowEventHandlers
noWindowEventHandlers = Nothing
{-# INLINE noWindowEventHandlers #-}

gTypeWindowEventHandlers :: JSM GType
gTypeWindowEventHandlers = GType . Object <$> jsg "WindowEventHandlers"

-- | Functions for this inteface are in "JSDOM.DocumentAndElementEventHandlers".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/DocumentAndElementEventHandlers Mozilla DocumentAndElementEventHandlers documentation>
newtype DocumentAndElementEventHandlers = DocumentAndElementEventHandlers { unDocumentAndElementEventHandlers :: JSVal }

instance PToJSVal DocumentAndElementEventHandlers where
  pToJSVal = unDocumentAndElementEventHandlers
  {-# INLINE pToJSVal #-}

instance PFromJSVal DocumentAndElementEventHandlers where
  pFromJSVal = DocumentAndElementEventHandlers
  {-# INLINE pFromJSVal #-}

instance ToJSVal DocumentAndElementEventHandlers where
  toJSVal = return . unDocumentAndElementEventHandlers
  {-# INLINE toJSVal #-}

instance FromJSVal DocumentAndElementEventHandlers where
  fromJSVal v = fmap DocumentAndElementEventHandlers <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . DocumentAndElementEventHandlers
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject DocumentAndElementEventHandlers where
  makeObject = makeObject . unDocumentAndElementEventHandlers

class (IsGObject o) => IsDocumentAndElementEventHandlers o
toDocumentAndElementEventHandlers :: IsDocumentAndElementEventHandlers o => o -> DocumentAndElementEventHandlers
toDocumentAndElementEventHandlers = DocumentAndElementEventHandlers . coerce

instance IsDocumentAndElementEventHandlers DocumentAndElementEventHandlers
instance IsGObject DocumentAndElementEventHandlers where
  typeGType _ = gTypeDocumentAndElementEventHandlers
  {-# INLINE typeGType #-}

noDocumentAndElementEventHandlers :: Maybe DocumentAndElementEventHandlers
noDocumentAndElementEventHandlers = Nothing
{-# INLINE noDocumentAndElementEventHandlers #-}

gTypeDocumentAndElementEventHandlers :: JSM GType
gTypeDocumentAndElementEventHandlers = GType . Object <$> jsg "DocumentAndElementEventHandlers"


-- | Functions for this inteface are in "JSDOM.NonDocumentTypeChildNode".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/NonDocumentTypeChildNode Mozilla NonDocumentTypeChildNode documentation>
newtype NonDocumentTypeChildNode = NonDocumentTypeChildNode { unNonDocumentTypeChildNode :: JSVal }

instance PToJSVal NonDocumentTypeChildNode where
  pToJSVal = unNonDocumentTypeChildNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal NonDocumentTypeChildNode where
  pFromJSVal = NonDocumentTypeChildNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal NonDocumentTypeChildNode where
  toJSVal = return . unNonDocumentTypeChildNode
  {-# INLINE toJSVal #-}

instance FromJSVal NonDocumentTypeChildNode where
  fromJSVal v = fmap NonDocumentTypeChildNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . NonDocumentTypeChildNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject NonDocumentTypeChildNode where
  makeObject = makeObject . unNonDocumentTypeChildNode

class (IsGObject o) => IsNonDocumentTypeChildNode o
toNonDocumentTypeChildNode :: IsNonDocumentTypeChildNode o => o -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = NonDocumentTypeChildNode . coerce

instance IsNonDocumentTypeChildNode NonDocumentTypeChildNode
instance IsGObject NonDocumentTypeChildNode where
  typeGType _ = gTypeNonDocumentTypeChildNode
  {-# INLINE typeGType #-}

noNonDocumentTypeChildNode :: Maybe NonDocumentTypeChildNode
noNonDocumentTypeChildNode = Nothing
{-# INLINE noNonDocumentTypeChildNode #-}

gTypeNonDocumentTypeChildNode :: JSM GType
gTypeNonDocumentTypeChildNode = GType . Object <$> jsg "NonDocumentTypeChildNode"

-- | Functions for this inteface are in "JSDOM.EventTarget".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/EventTarget Mozilla EventTarget documentation>
newtype EventTarget = EventTarget { unEventTarget :: JSVal }

instance PToJSVal EventTarget where
  pToJSVal = unEventTarget
  {-# INLINE pToJSVal #-}

instance PFromJSVal EventTarget where
  pFromJSVal = EventTarget
  {-# INLINE pFromJSVal #-}

instance ToJSVal EventTarget where
  toJSVal = return . unEventTarget
  {-# INLINE toJSVal #-}

instance FromJSVal EventTarget where
  fromJSVal v = fmap EventTarget <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . EventTarget
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject EventTarget where
  makeObject = makeObject . unEventTarget

class (IsGObject o) => IsEventTarget o
toEventTarget :: IsEventTarget o => o -> EventTarget
toEventTarget = EventTarget . coerce

instance IsEventTarget EventTarget
instance IsGObject EventTarget where
  typeGType _ = gTypeEventTarget
  {-# INLINE typeGType #-}

noEventTarget :: Maybe EventTarget
noEventTarget = Nothing
{-# INLINE noEventTarget #-}

gTypeEventTarget :: JSM GType
gTypeEventTarget = GType . Object <$> jsg "EventTarget"

-- | Functions for this inteface are in "JSDOM.Node".
-- Base interface functions are in:
--
--     * "JSDOM.EventTarget"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Node Mozilla Node documentation>
newtype Node = Node { unNode :: JSVal }

instance PToJSVal Node where
  pToJSVal = unNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal Node where
  pFromJSVal = Node
  {-# INLINE pFromJSVal #-}

instance ToJSVal Node where
  toJSVal = return . unNode
  {-# INLINE toJSVal #-}

instance FromJSVal Node where
  fromJSVal v = fmap Node <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Node
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Node where
  makeObject = makeObject . unNode

class (IsEventTarget o, IsGObject o) => IsNode o
toNode :: IsNode o => o -> Node
toNode = Node . coerce

instance IsNode Node
instance IsEventTarget Node
instance IsGObject Node where
  typeGType _ = gTypeNode
  {-# INLINE typeGType #-}

noNode :: Maybe Node
noNode = Nothing
{-# INLINE noNode #-}

gTypeNode :: JSM GType
gTypeNode = GType . Object <$> jsg "Node"


-- | Functions for this inteface are in "JSDOM.ParentNode".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ParentNode Mozilla ParentNode documentation>
newtype ParentNode = ParentNode { unParentNode :: JSVal }

instance PToJSVal ParentNode where
  pToJSVal = unParentNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ParentNode where
  pFromJSVal = ParentNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ParentNode where
  toJSVal = return . unParentNode
  {-# INLINE toJSVal #-}

instance FromJSVal ParentNode where
  fromJSVal v = fmap ParentNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ParentNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ParentNode where
  makeObject = makeObject . unParentNode

class (IsGObject o) => IsParentNode o
toParentNode :: IsParentNode o => o -> ParentNode
toParentNode = ParentNode . coerce

instance IsParentNode ParentNode
instance IsGObject ParentNode where
  typeGType _ = gTypeParentNode
  {-# INLINE typeGType #-}

noParentNode :: Maybe ParentNode
noParentNode = Nothing
{-# INLINE noParentNode #-}

gTypeParentNode :: JSM GType
gTypeParentNode = GType . Object <$> jsg "ParentNode"
-- | Functions for this inteface are in "JSDOM.ElementCSSInlineStyle".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ElementCSSInlineStyle Mozilla ElementCSSInlineStyle documentation>
newtype ElementCSSInlineStyle = ElementCSSInlineStyle { unElementCSSInlineStyle :: JSVal }

instance PToJSVal ElementCSSInlineStyle where
  pToJSVal = unElementCSSInlineStyle
  {-# INLINE pToJSVal #-}

instance PFromJSVal ElementCSSInlineStyle where
  pFromJSVal = ElementCSSInlineStyle
  {-# INLINE pFromJSVal #-}

instance ToJSVal ElementCSSInlineStyle where
  toJSVal = return . unElementCSSInlineStyle
  {-# INLINE toJSVal #-}

instance FromJSVal ElementCSSInlineStyle where
  fromJSVal v = fmap ElementCSSInlineStyle <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ElementCSSInlineStyle
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ElementCSSInlineStyle where
  makeObject = makeObject . unElementCSSInlineStyle

class (IsGObject o) => IsElementCSSInlineStyle o
toElementCSSInlineStyle :: IsElementCSSInlineStyle o => o -> ElementCSSInlineStyle
toElementCSSInlineStyle = ElementCSSInlineStyle . coerce

instance IsElementCSSInlineStyle ElementCSSInlineStyle
instance IsGObject ElementCSSInlineStyle where
  typeGType _ = gTypeElementCSSInlineStyle
  {-# INLINE typeGType #-}

noElementCSSInlineStyle :: Maybe ElementCSSInlineStyle
noElementCSSInlineStyle = Nothing
{-# INLINE noElementCSSInlineStyle #-}

gTypeElementCSSInlineStyle :: JSM GType
gTypeElementCSSInlineStyle = GType . Object <$> jsg "ElementCSSInlineStyle"

-- | Functions for this inteface are in "JSDOM.Animatable".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Animatable Mozilla Animatable documentation>
newtype Animatable = Animatable { unAnimatable :: JSVal }

instance PToJSVal Animatable where
  pToJSVal = unAnimatable
  {-# INLINE pToJSVal #-}

instance PFromJSVal Animatable where
  pFromJSVal = Animatable
  {-# INLINE pFromJSVal #-}

instance ToJSVal Animatable where
  toJSVal = return . unAnimatable
  {-# INLINE toJSVal #-}

instance FromJSVal Animatable where
  fromJSVal v = fmap Animatable <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Animatable
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Animatable where
  makeObject = makeObject . unAnimatable

class (IsGObject o) => IsAnimatable o
toAnimatable :: IsAnimatable o => o -> Animatable
toAnimatable = Animatable . coerce

instance IsAnimatable Animatable
instance IsGObject Animatable where
  typeGType _ = gTypeAnimatable
  {-# INLINE typeGType #-}

noAnimatable :: Maybe Animatable
noAnimatable = Nothing
{-# INLINE noAnimatable #-}

gTypeAnimatable :: JSM GType
gTypeAnimatable = GType . Object <$> jsg "Animatable"

-- | Functions for this inteface are in "JSDOM.GlobalEventHandlers".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/GlobalEventHandlers Mozilla GlobalEventHandlers documentation>
newtype GlobalEventHandlers = GlobalEventHandlers { unGlobalEventHandlers :: JSVal }

instance PToJSVal GlobalEventHandlers where
  pToJSVal = unGlobalEventHandlers
  {-# INLINE pToJSVal #-}

instance PFromJSVal GlobalEventHandlers where
  pFromJSVal = GlobalEventHandlers
  {-# INLINE pFromJSVal #-}

instance ToJSVal GlobalEventHandlers where
  toJSVal = return . unGlobalEventHandlers
  {-# INLINE toJSVal #-}

instance FromJSVal GlobalEventHandlers where
  fromJSVal v = fmap GlobalEventHandlers <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . GlobalEventHandlers
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject GlobalEventHandlers where
  makeObject = makeObject . unGlobalEventHandlers

class (IsGObject o) => IsGlobalEventHandlers o
toGlobalEventHandlers :: IsGlobalEventHandlers o => o -> GlobalEventHandlers
toGlobalEventHandlers = GlobalEventHandlers . coerce

instance IsGlobalEventHandlers GlobalEventHandlers
instance IsGObject GlobalEventHandlers where
  typeGType _ = gTypeGlobalEventHandlers
  {-# INLINE typeGType #-}

noGlobalEventHandlers :: Maybe GlobalEventHandlers
noGlobalEventHandlers = Nothing
{-# INLINE noGlobalEventHandlers #-}

gTypeGlobalEventHandlers :: JSM GType
gTypeGlobalEventHandlers = GType . Object <$> jsg "GlobalEventHandlers"

-- | Functions for this inteface are in "JSDOM.ChildNode".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/ChildNode Mozilla ChildNode documentation>
newtype ChildNode = ChildNode { unChildNode :: JSVal }

instance PToJSVal ChildNode where
  pToJSVal = unChildNode
  {-# INLINE pToJSVal #-}

instance PFromJSVal ChildNode where
  pFromJSVal = ChildNode
  {-# INLINE pFromJSVal #-}

instance ToJSVal ChildNode where
  toJSVal = return . unChildNode
  {-# INLINE toJSVal #-}

instance FromJSVal ChildNode where
  fromJSVal v = fmap ChildNode <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . ChildNode
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject ChildNode where
  makeObject = makeObject . unChildNode

class (IsGObject o) => IsChildNode o
toChildNode :: IsChildNode o => o -> ChildNode
toChildNode = ChildNode . coerce

instance IsChildNode ChildNode
instance IsGObject ChildNode where
  typeGType _ = gTypeChildNode
  {-# INLINE typeGType #-}

noChildNode :: Maybe ChildNode
noChildNode = Nothing
{-# INLINE noChildNode #-}

gTypeChildNode :: JSM GType
gTypeChildNode = GType . Object <$> jsg "ChildNode"

-- | Functions for this inteface are in "JSDOM.Slotable".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Slotable Mozilla Slotable documentation>
newtype Slotable = Slotable { unSlotable :: JSVal }

instance PToJSVal Slotable where
  pToJSVal = unSlotable
  {-# INLINE pToJSVal #-}

instance PFromJSVal Slotable where
  pFromJSVal = Slotable
  {-# INLINE pFromJSVal #-}

instance ToJSVal Slotable where
  toJSVal = return . unSlotable
  {-# INLINE toJSVal #-}

instance FromJSVal Slotable where
  fromJSVal v = fmap Slotable <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Slotable
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Slotable where
  makeObject = makeObject . unSlotable

class (IsGObject o) => IsSlotable o
toSlotable :: IsSlotable o => o -> Slotable
toSlotable = Slotable . coerce

instance IsSlotable Slotable
instance IsGObject Slotable where
  typeGType _ = gTypeSlotable
  {-# INLINE typeGType #-}

noSlotable :: Maybe Slotable
noSlotable = Nothing
{-# INLINE noSlotable #-}

gTypeSlotable :: JSM GType
gTypeSlotable = GType . Object <$> jsg "Slotable"

-- | Functions for this inteface are in "JSDOM.Element".
-- Base interface functions are in:
--
--     * "JSDOM.Node"
--     * "JSDOM.EventTarget"
--     * "JSDOM.Slotable"
--     * "JSDOM.ParentNode"
--     * "JSDOM.NonDocumentTypeChildNode"
--     * "JSDOM.DocumentAndElementEventHandlers"
--     * "JSDOM.ChildNode"
--     * "JSDOM.Animatable"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/Element Mozilla Element documentation>
newtype Element = Element { unElement :: JSVal }

instance PToJSVal Element where
  pToJSVal = unElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal Element where
  pFromJSVal = Element
  {-# INLINE pFromJSVal #-}

instance ToJSVal Element where
  toJSVal = return . unElement
  {-# INLINE toJSVal #-}

instance FromJSVal Element where
  fromJSVal v = fmap Element <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . Element
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject Element where
  makeObject = makeObject . unElement

class (IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGObject o) => IsElement o
toElement :: IsElement o => o -> Element
toElement = Element . coerce

instance IsElement Element
instance IsNode Element
instance IsEventTarget Element
instance IsSlotable Element
instance IsParentNode Element
instance IsNonDocumentTypeChildNode Element
instance IsDocumentAndElementEventHandlers Element
instance IsChildNode Element
instance IsAnimatable Element
instance IsGObject Element where
  typeGType _ = gTypeElement
  {-# INLINE typeGType #-}

noElement :: Maybe Element
noElement = Nothing
{-# INLINE noElement #-}

gTypeElement :: JSM GType
gTypeElement = GType . Object <$> jsg "Element"

-- | Functions for this inteface are in "JSDOM.HTMLAnchorElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
--     * "JSDOM.HTMLHyperlinkElementUtils"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAnchorElement Mozilla HTMLAnchorElement documentation>
newtype HTMLAnchorElement = HTMLAnchorElement { unHTMLAnchorElement :: JSVal }

instance PToJSVal HTMLAnchorElement where
  pToJSVal = unHTMLAnchorElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAnchorElement where
  pFromJSVal = HTMLAnchorElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAnchorElement where
  toJSVal = return . unHTMLAnchorElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAnchorElement where
  fromJSVal v = fmap HTMLAnchorElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAnchorElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAnchorElement where
  makeObject = makeObject . unHTMLAnchorElement

instance IsHTMLElement HTMLAnchorElement
instance IsElement HTMLAnchorElement
instance IsNode HTMLAnchorElement
instance IsEventTarget HTMLAnchorElement
instance IsSlotable HTMLAnchorElement
instance IsParentNode HTMLAnchorElement
instance IsNonDocumentTypeChildNode HTMLAnchorElement
instance IsDocumentAndElementEventHandlers HTMLAnchorElement
instance IsChildNode HTMLAnchorElement
instance IsAnimatable HTMLAnchorElement
instance IsGlobalEventHandlers HTMLAnchorElement
instance IsElementCSSInlineStyle HTMLAnchorElement
instance IsHTMLHyperlinkElementUtils HTMLAnchorElement
instance IsGObject HTMLAnchorElement where
  typeGType _ = gTypeHTMLAnchorElement
  {-# INLINE typeGType #-}

noHTMLAnchorElement :: Maybe HTMLAnchorElement
noHTMLAnchorElement = Nothing
{-# INLINE noHTMLAnchorElement #-}

gTypeHTMLAnchorElement :: JSM GType
gTypeHTMLAnchorElement = GType . Object <$> jsg "HTMLAnchorElement"

-- | Functions for this inteface are in "JSDOM.HTMLAppletElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAppletElement Mozilla HTMLAppletElement documentation>
newtype HTMLAppletElement = HTMLAppletElement { unHTMLAppletElement :: JSVal }

instance PToJSVal HTMLAppletElement where
  pToJSVal = unHTMLAppletElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAppletElement where
  pFromJSVal = HTMLAppletElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAppletElement where
  toJSVal = return . unHTMLAppletElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAppletElement where
  fromJSVal v = fmap HTMLAppletElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAppletElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAppletElement where
  makeObject = makeObject . unHTMLAppletElement

instance IsHTMLElement HTMLAppletElement
instance IsElement HTMLAppletElement
instance IsNode HTMLAppletElement
instance IsEventTarget HTMLAppletElement
instance IsSlotable HTMLAppletElement
instance IsParentNode HTMLAppletElement
instance IsNonDocumentTypeChildNode HTMLAppletElement
instance IsDocumentAndElementEventHandlers HTMLAppletElement
instance IsChildNode HTMLAppletElement
instance IsAnimatable HTMLAppletElement
instance IsGlobalEventHandlers HTMLAppletElement
instance IsElementCSSInlineStyle HTMLAppletElement
instance IsGObject HTMLAppletElement where
  typeGType _ = gTypeHTMLAppletElement
  {-# INLINE typeGType #-}

noHTMLAppletElement :: Maybe HTMLAppletElement
noHTMLAppletElement = Nothing
{-# INLINE noHTMLAppletElement #-}

gTypeHTMLAppletElement :: JSM GType
gTypeHTMLAppletElement = GType . Object <$> jsg "HTMLAppletElement"

-- | Functions for this inteface are in "JSDOM.HTMLAreaElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
--     * "JSDOM.HTMLHyperlinkElementUtils"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAreaElement Mozilla HTMLAreaElement documentation>
newtype HTMLAreaElement = HTMLAreaElement { unHTMLAreaElement :: JSVal }

instance PToJSVal HTMLAreaElement where
  pToJSVal = unHTMLAreaElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAreaElement where
  pFromJSVal = HTMLAreaElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAreaElement where
  toJSVal = return . unHTMLAreaElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAreaElement where
  fromJSVal v = fmap HTMLAreaElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAreaElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAreaElement where
  makeObject = makeObject . unHTMLAreaElement

instance IsHTMLElement HTMLAreaElement
instance IsElement HTMLAreaElement
instance IsNode HTMLAreaElement
instance IsEventTarget HTMLAreaElement
instance IsSlotable HTMLAreaElement
instance IsParentNode HTMLAreaElement
instance IsNonDocumentTypeChildNode HTMLAreaElement
instance IsDocumentAndElementEventHandlers HTMLAreaElement
instance IsChildNode HTMLAreaElement
instance IsAnimatable HTMLAreaElement
instance IsGlobalEventHandlers HTMLAreaElement
instance IsElementCSSInlineStyle HTMLAreaElement
instance IsHTMLHyperlinkElementUtils HTMLAreaElement
instance IsGObject HTMLAreaElement where
  typeGType _ = gTypeHTMLAreaElement
  {-# INLINE typeGType #-}

noHTMLAreaElement :: Maybe HTMLAreaElement
noHTMLAreaElement = Nothing
{-# INLINE noHTMLAreaElement #-}

gTypeHTMLAreaElement :: JSM GType
gTypeHTMLAreaElement = GType . Object <$> jsg "HTMLAreaElement"

-- | Functions for this inteface are in "JSDOM.HTMLAttachmentElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAttachmentElement Mozilla HTMLAttachmentElement documentation>
newtype HTMLAttachmentElement = HTMLAttachmentElement { unHTMLAttachmentElement :: JSVal }

instance PToJSVal HTMLAttachmentElement where
  pToJSVal = unHTMLAttachmentElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAttachmentElement where
  pFromJSVal = HTMLAttachmentElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAttachmentElement where
  toJSVal = return . unHTMLAttachmentElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAttachmentElement where
  fromJSVal v = fmap HTMLAttachmentElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAttachmentElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAttachmentElement where
  makeObject = makeObject . unHTMLAttachmentElement

instance IsHTMLElement HTMLAttachmentElement
instance IsElement HTMLAttachmentElement
instance IsNode HTMLAttachmentElement
instance IsEventTarget HTMLAttachmentElement
instance IsSlotable HTMLAttachmentElement
instance IsParentNode HTMLAttachmentElement
instance IsNonDocumentTypeChildNode HTMLAttachmentElement
instance IsDocumentAndElementEventHandlers HTMLAttachmentElement
instance IsChildNode HTMLAttachmentElement
instance IsAnimatable HTMLAttachmentElement
instance IsGlobalEventHandlers HTMLAttachmentElement
instance IsElementCSSInlineStyle HTMLAttachmentElement
instance IsGObject HTMLAttachmentElement where
  typeGType _ = gTypeHTMLAttachmentElement
  {-# INLINE typeGType #-}

noHTMLAttachmentElement :: Maybe HTMLAttachmentElement
noHTMLAttachmentElement = Nothing
{-# INLINE noHTMLAttachmentElement #-}

gTypeHTMLAttachmentElement :: JSM GType
gTypeHTMLAttachmentElement = GType . Object <$> jsg "HTMLAttachmentElement"

-- | Functions for this inteface are in "JSDOM.HTMLAudioElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLMediaElement"
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLAudioElement Mozilla HTMLAudioElement documentation>
newtype HTMLAudioElement = HTMLAudioElement { unHTMLAudioElement :: JSVal }

instance PToJSVal HTMLAudioElement where
  pToJSVal = unHTMLAudioElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLAudioElement where
  pFromJSVal = HTMLAudioElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLAudioElement where
  toJSVal = return . unHTMLAudioElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLAudioElement where
  fromJSVal v = fmap HTMLAudioElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLAudioElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLAudioElement where
  makeObject = makeObject . unHTMLAudioElement

instance IsHTMLMediaElement HTMLAudioElement
instance IsHTMLElement HTMLAudioElement
instance IsElement HTMLAudioElement
instance IsNode HTMLAudioElement
instance IsEventTarget HTMLAudioElement
instance IsSlotable HTMLAudioElement
instance IsParentNode HTMLAudioElement
instance IsNonDocumentTypeChildNode HTMLAudioElement
instance IsDocumentAndElementEventHandlers HTMLAudioElement
instance IsChildNode HTMLAudioElement
instance IsAnimatable HTMLAudioElement
instance IsGlobalEventHandlers HTMLAudioElement
instance IsElementCSSInlineStyle HTMLAudioElement
instance IsGObject HTMLAudioElement where
  typeGType _ = gTypeHTMLAudioElement
  {-# INLINE typeGType #-}

noHTMLAudioElement :: Maybe HTMLAudioElement
noHTMLAudioElement = Nothing
{-# INLINE noHTMLAudioElement #-}

gTypeHTMLAudioElement :: JSM GType
gTypeHTMLAudioElement = GType . Object <$> jsg "HTMLAudioElement"

-- | Functions for this inteface are in "JSDOM.HTMLBRElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLBRElement Mozilla HTMLBRElement documentation>
newtype HTMLBRElement = HTMLBRElement { unHTMLBRElement :: JSVal }

instance PToJSVal HTMLBRElement where
  pToJSVal = unHTMLBRElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLBRElement where
  pFromJSVal = HTMLBRElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLBRElement where
  toJSVal = return . unHTMLBRElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLBRElement where
  fromJSVal v = fmap HTMLBRElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLBRElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLBRElement where
  makeObject = makeObject . unHTMLBRElement

instance IsHTMLElement HTMLBRElement
instance IsElement HTMLBRElement
instance IsNode HTMLBRElement
instance IsEventTarget HTMLBRElement
instance IsSlotable HTMLBRElement
instance IsParentNode HTMLBRElement
instance IsNonDocumentTypeChildNode HTMLBRElement
instance IsDocumentAndElementEventHandlers HTMLBRElement
instance IsChildNode HTMLBRElement
instance IsAnimatable HTMLBRElement
instance IsGlobalEventHandlers HTMLBRElement
instance IsElementCSSInlineStyle HTMLBRElement
instance IsGObject HTMLBRElement where
  typeGType _ = gTypeHTMLBRElement
  {-# INLINE typeGType #-}

noHTMLBRElement :: Maybe HTMLBRElement
noHTMLBRElement = Nothing
{-# INLINE noHTMLBRElement #-}

gTypeHTMLBRElement :: JSM GType
gTypeHTMLBRElement = GType . Object <$> jsg "HTMLBRElement"

-- | Functions for this inteface are in "JSDOM.HTMLBaseElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLBaseElement Mozilla HTMLBaseElement documentation>
newtype HTMLBaseElement = HTMLBaseElement { unHTMLBaseElement :: JSVal }

instance PToJSVal HTMLBaseElement where
  pToJSVal = unHTMLBaseElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLBaseElement where
  pFromJSVal = HTMLBaseElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLBaseElement where
  toJSVal = return . unHTMLBaseElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLBaseElement where
  fromJSVal v = fmap HTMLBaseElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLBaseElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLBaseElement where
  makeObject = makeObject . unHTMLBaseElement

instance IsHTMLElement HTMLBaseElement
instance IsElement HTMLBaseElement
instance IsNode HTMLBaseElement
instance IsEventTarget HTMLBaseElement
instance IsSlotable HTMLBaseElement
instance IsParentNode HTMLBaseElement
instance IsNonDocumentTypeChildNode HTMLBaseElement
instance IsDocumentAndElementEventHandlers HTMLBaseElement
instance IsChildNode HTMLBaseElement
instance IsAnimatable HTMLBaseElement
instance IsGlobalEventHandlers HTMLBaseElement
instance IsElementCSSInlineStyle HTMLBaseElement
instance IsGObject HTMLBaseElement where
  typeGType _ = gTypeHTMLBaseElement
  {-# INLINE typeGType #-}

noHTMLBaseElement :: Maybe HTMLBaseElement
noHTMLBaseElement = Nothing
{-# INLINE noHTMLBaseElement #-}

gTypeHTMLBaseElement :: JSM GType
gTypeHTMLBaseElement = GType . Object <$> jsg "HTMLBaseElement"

-- | Functions for this inteface are in "JSDOM.HTMLBodyElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
--     * "JSDOM.WindowEventHandlers"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLBodyElement Mozilla HTMLBodyElement documentation>
newtype HTMLBodyElement = HTMLBodyElement { unHTMLBodyElement :: JSVal }

instance PToJSVal HTMLBodyElement where
  pToJSVal = unHTMLBodyElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLBodyElement where
  pFromJSVal = HTMLBodyElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLBodyElement where
  toJSVal = return . unHTMLBodyElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLBodyElement where
  fromJSVal v = fmap HTMLBodyElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLBodyElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLBodyElement where
  makeObject = makeObject . unHTMLBodyElement

instance IsHTMLElement HTMLBodyElement
instance IsElement HTMLBodyElement
instance IsNode HTMLBodyElement
instance IsEventTarget HTMLBodyElement
instance IsSlotable HTMLBodyElement
instance IsParentNode HTMLBodyElement
instance IsNonDocumentTypeChildNode HTMLBodyElement
instance IsDocumentAndElementEventHandlers HTMLBodyElement
instance IsChildNode HTMLBodyElement
instance IsAnimatable HTMLBodyElement
instance IsGlobalEventHandlers HTMLBodyElement
instance IsElementCSSInlineStyle HTMLBodyElement
instance IsWindowEventHandlers HTMLBodyElement
instance IsGObject HTMLBodyElement where
  typeGType _ = gTypeHTMLBodyElement
  {-# INLINE typeGType #-}

noHTMLBodyElement :: Maybe HTMLBodyElement
noHTMLBodyElement = Nothing
{-# INLINE noHTMLBodyElement #-}

gTypeHTMLBodyElement :: JSM GType
gTypeHTMLBodyElement = GType . Object <$> jsg "HTMLBodyElement"

-- | Functions for this inteface are in "JSDOM.HTMLButtonElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLButtonElement Mozilla HTMLButtonElement documentation>
newtype HTMLButtonElement = HTMLButtonElement { unHTMLButtonElement :: JSVal }

instance PToJSVal HTMLButtonElement where
  pToJSVal = unHTMLButtonElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLButtonElement where
  pFromJSVal = HTMLButtonElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLButtonElement where
  toJSVal = return . unHTMLButtonElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLButtonElement where
  fromJSVal v = fmap HTMLButtonElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLButtonElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLButtonElement where
  makeObject = makeObject . unHTMLButtonElement

instance IsHTMLElement HTMLButtonElement
instance IsElement HTMLButtonElement
instance IsNode HTMLButtonElement
instance IsEventTarget HTMLButtonElement
instance IsSlotable HTMLButtonElement
instance IsParentNode HTMLButtonElement
instance IsNonDocumentTypeChildNode HTMLButtonElement
instance IsDocumentAndElementEventHandlers HTMLButtonElement
instance IsChildNode HTMLButtonElement
instance IsAnimatable HTMLButtonElement
instance IsGlobalEventHandlers HTMLButtonElement
instance IsElementCSSInlineStyle HTMLButtonElement
instance IsGObject HTMLButtonElement where
  typeGType _ = gTypeHTMLButtonElement
  {-# INLINE typeGType #-}

noHTMLButtonElement :: Maybe HTMLButtonElement
noHTMLButtonElement = Nothing
{-# INLINE noHTMLButtonElement #-}

gTypeHTMLButtonElement :: JSM GType
gTypeHTMLButtonElement = GType . Object <$> jsg "HTMLButtonElement"

-- | Functions for this inteface are in "JSDOM.HTMLCanvasElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement Mozilla HTMLCanvasElement documentation>
newtype HTMLCanvasElement = HTMLCanvasElement { unHTMLCanvasElement :: JSVal }

instance PToJSVal HTMLCanvasElement where
  pToJSVal = unHTMLCanvasElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLCanvasElement where
  pFromJSVal = HTMLCanvasElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLCanvasElement where
  toJSVal = return . unHTMLCanvasElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLCanvasElement where
  fromJSVal v = fmap HTMLCanvasElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLCanvasElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLCanvasElement where
  makeObject = makeObject . unHTMLCanvasElement

instance IsHTMLElement HTMLCanvasElement
instance IsElement HTMLCanvasElement
instance IsNode HTMLCanvasElement
instance IsEventTarget HTMLCanvasElement
instance IsSlotable HTMLCanvasElement
instance IsParentNode HTMLCanvasElement
instance IsNonDocumentTypeChildNode HTMLCanvasElement
instance IsDocumentAndElementEventHandlers HTMLCanvasElement
instance IsChildNode HTMLCanvasElement
instance IsAnimatable HTMLCanvasElement
instance IsGlobalEventHandlers HTMLCanvasElement
instance IsElementCSSInlineStyle HTMLCanvasElement
instance IsGObject HTMLCanvasElement where
  typeGType _ = gTypeHTMLCanvasElement
  {-# INLINE typeGType #-}

noHTMLCanvasElement :: Maybe HTMLCanvasElement
noHTMLCanvasElement = Nothing
{-# INLINE noHTMLCanvasElement #-}

gTypeHTMLCanvasElement :: JSM GType
gTypeHTMLCanvasElement = GType . Object <$> jsg "HTMLCanvasElement"

-- | Functions for this inteface are in "JSDOM.HTMLCollection".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLCollection Mozilla HTMLCollection documentation>
newtype HTMLCollection = HTMLCollection { unHTMLCollection :: JSVal }

instance PToJSVal HTMLCollection where
  pToJSVal = unHTMLCollection
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLCollection where
  pFromJSVal = HTMLCollection
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLCollection where
  toJSVal = return . unHTMLCollection
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLCollection where
  fromJSVal v = fmap HTMLCollection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLCollection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLCollection where
  makeObject = makeObject . unHTMLCollection

class (IsGObject o) => IsHTMLCollection o
toHTMLCollection :: IsHTMLCollection o => o -> HTMLCollection
toHTMLCollection = HTMLCollection . coerce

instance IsHTMLCollection HTMLCollection
instance IsGObject HTMLCollection where
  typeGType _ = gTypeHTMLCollection
  {-# INLINE typeGType #-}

noHTMLCollection :: Maybe HTMLCollection
noHTMLCollection = Nothing
{-# INLINE noHTMLCollection #-}

gTypeHTMLCollection :: JSM GType
gTypeHTMLCollection = GType . Object <$> jsg "HTMLCollection"

-- | Functions for this inteface are in "JSDOM.HTMLDListElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDListElement Mozilla HTMLDListElement documentation>
newtype HTMLDListElement = HTMLDListElement { unHTMLDListElement :: JSVal }

instance PToJSVal HTMLDListElement where
  pToJSVal = unHTMLDListElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDListElement where
  pFromJSVal = HTMLDListElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDListElement where
  toJSVal = return . unHTMLDListElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDListElement where
  fromJSVal v = fmap HTMLDListElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDListElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDListElement where
  makeObject = makeObject . unHTMLDListElement

instance IsHTMLElement HTMLDListElement
instance IsElement HTMLDListElement
instance IsNode HTMLDListElement
instance IsEventTarget HTMLDListElement
instance IsSlotable HTMLDListElement
instance IsParentNode HTMLDListElement
instance IsNonDocumentTypeChildNode HTMLDListElement
instance IsDocumentAndElementEventHandlers HTMLDListElement
instance IsChildNode HTMLDListElement
instance IsAnimatable HTMLDListElement
instance IsGlobalEventHandlers HTMLDListElement
instance IsElementCSSInlineStyle HTMLDListElement
instance IsGObject HTMLDListElement where
  typeGType _ = gTypeHTMLDListElement
  {-# INLINE typeGType #-}

noHTMLDListElement :: Maybe HTMLDListElement
noHTMLDListElement = Nothing
{-# INLINE noHTMLDListElement #-}

gTypeHTMLDListElement :: JSM GType
gTypeHTMLDListElement = GType . Object <$> jsg "HTMLDListElement"

-- | Functions for this inteface are in "JSDOM.HTMLDataElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDataElement Mozilla HTMLDataElement documentation>
newtype HTMLDataElement = HTMLDataElement { unHTMLDataElement :: JSVal }

instance PToJSVal HTMLDataElement where
  pToJSVal = unHTMLDataElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDataElement where
  pFromJSVal = HTMLDataElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDataElement where
  toJSVal = return . unHTMLDataElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDataElement where
  fromJSVal v = fmap HTMLDataElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDataElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDataElement where
  makeObject = makeObject . unHTMLDataElement

instance IsHTMLElement HTMLDataElement
instance IsElement HTMLDataElement
instance IsNode HTMLDataElement
instance IsEventTarget HTMLDataElement
instance IsSlotable HTMLDataElement
instance IsParentNode HTMLDataElement
instance IsNonDocumentTypeChildNode HTMLDataElement
instance IsDocumentAndElementEventHandlers HTMLDataElement
instance IsChildNode HTMLDataElement
instance IsAnimatable HTMLDataElement
instance IsGlobalEventHandlers HTMLDataElement
instance IsElementCSSInlineStyle HTMLDataElement
instance IsGObject HTMLDataElement where
  typeGType _ = gTypeHTMLDataElement
  {-# INLINE typeGType #-}

noHTMLDataElement :: Maybe HTMLDataElement
noHTMLDataElement = Nothing
{-# INLINE noHTMLDataElement #-}

gTypeHTMLDataElement :: JSM GType
gTypeHTMLDataElement = GType . Object <$> jsg "HTMLDataElement"

-- | Functions for this inteface are in "JSDOM.HTMLDataListElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDataListElement Mozilla HTMLDataListElement documentation>
newtype HTMLDataListElement = HTMLDataListElement { unHTMLDataListElement :: JSVal }

instance PToJSVal HTMLDataListElement where
  pToJSVal = unHTMLDataListElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDataListElement where
  pFromJSVal = HTMLDataListElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDataListElement where
  toJSVal = return . unHTMLDataListElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDataListElement where
  fromJSVal v = fmap HTMLDataListElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDataListElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDataListElement where
  makeObject = makeObject . unHTMLDataListElement

instance IsHTMLElement HTMLDataListElement
instance IsElement HTMLDataListElement
instance IsNode HTMLDataListElement
instance IsEventTarget HTMLDataListElement
instance IsSlotable HTMLDataListElement
instance IsParentNode HTMLDataListElement
instance IsNonDocumentTypeChildNode HTMLDataListElement
instance IsDocumentAndElementEventHandlers HTMLDataListElement
instance IsChildNode HTMLDataListElement
instance IsAnimatable HTMLDataListElement
instance IsGlobalEventHandlers HTMLDataListElement
instance IsElementCSSInlineStyle HTMLDataListElement
instance IsGObject HTMLDataListElement where
  typeGType _ = gTypeHTMLDataListElement
  {-# INLINE typeGType #-}

noHTMLDataListElement :: Maybe HTMLDataListElement
noHTMLDataListElement = Nothing
{-# INLINE noHTMLDataListElement #-}

gTypeHTMLDataListElement :: JSM GType
gTypeHTMLDataListElement = GType . Object <$> jsg "HTMLDataListElement"

-- | Functions for this inteface are in "JSDOM.HTMLDetailsElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDetailsElement Mozilla HTMLDetailsElement documentation>
newtype HTMLDetailsElement = HTMLDetailsElement { unHTMLDetailsElement :: JSVal }

instance PToJSVal HTMLDetailsElement where
  pToJSVal = unHTMLDetailsElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDetailsElement where
  pFromJSVal = HTMLDetailsElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDetailsElement where
  toJSVal = return . unHTMLDetailsElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDetailsElement where
  fromJSVal v = fmap HTMLDetailsElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDetailsElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDetailsElement where
  makeObject = makeObject . unHTMLDetailsElement

instance IsHTMLElement HTMLDetailsElement
instance IsElement HTMLDetailsElement
instance IsNode HTMLDetailsElement
instance IsEventTarget HTMLDetailsElement
instance IsSlotable HTMLDetailsElement
instance IsParentNode HTMLDetailsElement
instance IsNonDocumentTypeChildNode HTMLDetailsElement
instance IsDocumentAndElementEventHandlers HTMLDetailsElement
instance IsChildNode HTMLDetailsElement
instance IsAnimatable HTMLDetailsElement
instance IsGlobalEventHandlers HTMLDetailsElement
instance IsElementCSSInlineStyle HTMLDetailsElement
instance IsGObject HTMLDetailsElement where
  typeGType _ = gTypeHTMLDetailsElement
  {-# INLINE typeGType #-}

noHTMLDetailsElement :: Maybe HTMLDetailsElement
noHTMLDetailsElement = Nothing
{-# INLINE noHTMLDetailsElement #-}

gTypeHTMLDetailsElement :: JSM GType
gTypeHTMLDetailsElement = GType . Object <$> jsg "HTMLDetailsElement"

-- | Functions for this inteface are in "JSDOM.HTMLDirectoryElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDirectoryElement Mozilla HTMLDirectoryElement documentation>
newtype HTMLDirectoryElement = HTMLDirectoryElement { unHTMLDirectoryElement :: JSVal }

instance PToJSVal HTMLDirectoryElement where
  pToJSVal = unHTMLDirectoryElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDirectoryElement where
  pFromJSVal = HTMLDirectoryElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDirectoryElement where
  toJSVal = return . unHTMLDirectoryElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDirectoryElement where
  fromJSVal v = fmap HTMLDirectoryElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDirectoryElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDirectoryElement where
  makeObject = makeObject . unHTMLDirectoryElement

instance IsHTMLElement HTMLDirectoryElement
instance IsElement HTMLDirectoryElement
instance IsNode HTMLDirectoryElement
instance IsEventTarget HTMLDirectoryElement
instance IsSlotable HTMLDirectoryElement
instance IsParentNode HTMLDirectoryElement
instance IsNonDocumentTypeChildNode HTMLDirectoryElement
instance IsDocumentAndElementEventHandlers HTMLDirectoryElement
instance IsChildNode HTMLDirectoryElement
instance IsAnimatable HTMLDirectoryElement
instance IsGlobalEventHandlers HTMLDirectoryElement
instance IsElementCSSInlineStyle HTMLDirectoryElement
instance IsGObject HTMLDirectoryElement where
  typeGType _ = gTypeHTMLDirectoryElement
  {-# INLINE typeGType #-}

noHTMLDirectoryElement :: Maybe HTMLDirectoryElement
noHTMLDirectoryElement = Nothing
{-# INLINE noHTMLDirectoryElement #-}

gTypeHTMLDirectoryElement :: JSM GType
gTypeHTMLDirectoryElement = GType . Object <$> jsg "HTMLDirectoryElement"

-- | Functions for this inteface are in "JSDOM.HTMLDivElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDivElement Mozilla HTMLDivElement documentation>
newtype HTMLDivElement = HTMLDivElement { unHTMLDivElement :: JSVal }

instance PToJSVal HTMLDivElement where
  pToJSVal = unHTMLDivElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDivElement where
  pFromJSVal = HTMLDivElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDivElement where
  toJSVal = return . unHTMLDivElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDivElement where
  fromJSVal v = fmap HTMLDivElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDivElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDivElement where
  makeObject = makeObject . unHTMLDivElement

instance IsHTMLElement HTMLDivElement
instance IsElement HTMLDivElement
instance IsNode HTMLDivElement
instance IsEventTarget HTMLDivElement
instance IsSlotable HTMLDivElement
instance IsParentNode HTMLDivElement
instance IsNonDocumentTypeChildNode HTMLDivElement
instance IsDocumentAndElementEventHandlers HTMLDivElement
instance IsChildNode HTMLDivElement
instance IsAnimatable HTMLDivElement
instance IsGlobalEventHandlers HTMLDivElement
instance IsElementCSSInlineStyle HTMLDivElement
instance IsGObject HTMLDivElement where
  typeGType _ = gTypeHTMLDivElement
  {-# INLINE typeGType #-}

noHTMLDivElement :: Maybe HTMLDivElement
noHTMLDivElement = Nothing
{-# INLINE noHTMLDivElement #-}

gTypeHTMLDivElement :: JSM GType
gTypeHTMLDivElement = GType . Object <$> jsg "HTMLDivElement"

-- | Functions for this inteface are in "JSDOM.HTMLDocument".
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLDocument Mozilla HTMLDocument documentation>
newtype HTMLDocument = HTMLDocument { unHTMLDocument :: JSVal }

instance PToJSVal HTMLDocument where
  pToJSVal = unHTMLDocument
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLDocument where
  pFromJSVal = HTMLDocument
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLDocument where
  toJSVal = return . unHTMLDocument
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLDocument where
  fromJSVal v = fmap HTMLDocument <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLDocument
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLDocument where
  makeObject = makeObject . unHTMLDocument

instance IsDocument HTMLDocument
instance IsNode HTMLDocument
instance IsEventTarget HTMLDocument
instance IsGlobalEventHandlers HTMLDocument
instance IsDocumentOrShadowRoot HTMLDocument
instance IsNonElementParentNode HTMLDocument
instance IsParentNode HTMLDocument
instance IsDocumentAndElementEventHandlers HTMLDocument
instance IsGObject HTMLDocument where
  typeGType _ = gTypeHTMLDocument
  {-# INLINE typeGType #-}

noHTMLDocument :: Maybe HTMLDocument
noHTMLDocument = Nothing
{-# INLINE noHTMLDocument #-}

gTypeHTMLDocument :: JSM GType
gTypeHTMLDocument = GType . Object <$> jsg "HTMLDocument"

-- | Functions for this inteface are in "JSDOM.HTMLElement".
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement Mozilla HTMLElement documentation>
newtype HTMLElement = HTMLElement { unHTMLElement :: JSVal }

instance PToJSVal HTMLElement where
  pToJSVal = unHTMLElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLElement where
  pFromJSVal = HTMLElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLElement where
  toJSVal = return . unHTMLElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLElement where
  fromJSVal v = fmap HTMLElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLElement where
  makeObject = makeObject . unHTMLElement

class (IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsGObject o) => IsHTMLElement o
toHTMLElement :: IsHTMLElement o => o -> HTMLElement
toHTMLElement = HTMLElement . coerce

instance IsHTMLElement HTMLElement
instance IsElement HTMLElement
instance IsNode HTMLElement
instance IsEventTarget HTMLElement
instance IsSlotable HTMLElement
instance IsParentNode HTMLElement
instance IsNonDocumentTypeChildNode HTMLElement
instance IsDocumentAndElementEventHandlers HTMLElement
instance IsChildNode HTMLElement
instance IsAnimatable HTMLElement
instance IsGlobalEventHandlers HTMLElement
instance IsElementCSSInlineStyle HTMLElement
instance IsGObject HTMLElement where
  typeGType _ = gTypeHTMLElement
  {-# INLINE typeGType #-}

noHTMLElement :: Maybe HTMLElement
noHTMLElement = Nothing
{-# INLINE noHTMLElement #-}

gTypeHTMLElement :: JSM GType
gTypeHTMLElement = GType . Object <$> jsg "HTMLElement"

-- | Functions for this inteface are in "JSDOM.HTMLEmbedElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLEmbedElement Mozilla HTMLEmbedElement documentation>
newtype HTMLEmbedElement = HTMLEmbedElement { unHTMLEmbedElement :: JSVal }

instance PToJSVal HTMLEmbedElement where
  pToJSVal = unHTMLEmbedElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLEmbedElement where
  pFromJSVal = HTMLEmbedElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLEmbedElement where
  toJSVal = return . unHTMLEmbedElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLEmbedElement where
  fromJSVal v = fmap HTMLEmbedElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLEmbedElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLEmbedElement where
  makeObject = makeObject . unHTMLEmbedElement

instance IsHTMLElement HTMLEmbedElement
instance IsElement HTMLEmbedElement
instance IsNode HTMLEmbedElement
instance IsEventTarget HTMLEmbedElement
instance IsSlotable HTMLEmbedElement
instance IsParentNode HTMLEmbedElement
instance IsNonDocumentTypeChildNode HTMLEmbedElement
instance IsDocumentAndElementEventHandlers HTMLEmbedElement
instance IsChildNode HTMLEmbedElement
instance IsAnimatable HTMLEmbedElement
instance IsGlobalEventHandlers HTMLEmbedElement
instance IsElementCSSInlineStyle HTMLEmbedElement
instance IsGObject HTMLEmbedElement where
  typeGType _ = gTypeHTMLEmbedElement
  {-# INLINE typeGType #-}

noHTMLEmbedElement :: Maybe HTMLEmbedElement
noHTMLEmbedElement = Nothing
{-# INLINE noHTMLEmbedElement #-}

gTypeHTMLEmbedElement :: JSM GType
gTypeHTMLEmbedElement = GType . Object <$> jsg "HTMLEmbedElement"

-- | Functions for this inteface are in "JSDOM.HTMLFieldSetElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFieldSetElement Mozilla HTMLFieldSetElement documentation>
newtype HTMLFieldSetElement = HTMLFieldSetElement { unHTMLFieldSetElement :: JSVal }

instance PToJSVal HTMLFieldSetElement where
  pToJSVal = unHTMLFieldSetElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFieldSetElement where
  pFromJSVal = HTMLFieldSetElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFieldSetElement where
  toJSVal = return . unHTMLFieldSetElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFieldSetElement where
  fromJSVal v = fmap HTMLFieldSetElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFieldSetElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFieldSetElement where
  makeObject = makeObject . unHTMLFieldSetElement

instance IsHTMLElement HTMLFieldSetElement
instance IsElement HTMLFieldSetElement
instance IsNode HTMLFieldSetElement
instance IsEventTarget HTMLFieldSetElement
instance IsSlotable HTMLFieldSetElement
instance IsParentNode HTMLFieldSetElement
instance IsNonDocumentTypeChildNode HTMLFieldSetElement
instance IsDocumentAndElementEventHandlers HTMLFieldSetElement
instance IsChildNode HTMLFieldSetElement
instance IsAnimatable HTMLFieldSetElement
instance IsGlobalEventHandlers HTMLFieldSetElement
instance IsElementCSSInlineStyle HTMLFieldSetElement
instance IsGObject HTMLFieldSetElement where
  typeGType _ = gTypeHTMLFieldSetElement
  {-# INLINE typeGType #-}

noHTMLFieldSetElement :: Maybe HTMLFieldSetElement
noHTMLFieldSetElement = Nothing
{-# INLINE noHTMLFieldSetElement #-}

gTypeHTMLFieldSetElement :: JSM GType
gTypeHTMLFieldSetElement = GType . Object <$> jsg "HTMLFieldSetElement"

-- | Functions for this inteface are in "JSDOM.HTMLFontElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFontElement Mozilla HTMLFontElement documentation>
newtype HTMLFontElement = HTMLFontElement { unHTMLFontElement :: JSVal }

instance PToJSVal HTMLFontElement where
  pToJSVal = unHTMLFontElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFontElement where
  pFromJSVal = HTMLFontElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFontElement where
  toJSVal = return . unHTMLFontElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFontElement where
  fromJSVal v = fmap HTMLFontElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFontElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFontElement where
  makeObject = makeObject . unHTMLFontElement

instance IsHTMLElement HTMLFontElement
instance IsElement HTMLFontElement
instance IsNode HTMLFontElement
instance IsEventTarget HTMLFontElement
instance IsSlotable HTMLFontElement
instance IsParentNode HTMLFontElement
instance IsNonDocumentTypeChildNode HTMLFontElement
instance IsDocumentAndElementEventHandlers HTMLFontElement
instance IsChildNode HTMLFontElement
instance IsAnimatable HTMLFontElement
instance IsGlobalEventHandlers HTMLFontElement
instance IsElementCSSInlineStyle HTMLFontElement
instance IsGObject HTMLFontElement where
  typeGType _ = gTypeHTMLFontElement
  {-# INLINE typeGType #-}

noHTMLFontElement :: Maybe HTMLFontElement
noHTMLFontElement = Nothing
{-# INLINE noHTMLFontElement #-}

gTypeHTMLFontElement :: JSM GType
gTypeHTMLFontElement = GType . Object <$> jsg "HTMLFontElement"

-- | Functions for this inteface are in "JSDOM.HTMLFormControlsCollection".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLCollection"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormControlsCollection Mozilla HTMLFormControlsCollection documentation>
newtype HTMLFormControlsCollection = HTMLFormControlsCollection { unHTMLFormControlsCollection :: JSVal }

instance PToJSVal HTMLFormControlsCollection where
  pToJSVal = unHTMLFormControlsCollection
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFormControlsCollection where
  pFromJSVal = HTMLFormControlsCollection
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFormControlsCollection where
  toJSVal = return . unHTMLFormControlsCollection
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFormControlsCollection where
  fromJSVal v = fmap HTMLFormControlsCollection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFormControlsCollection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFormControlsCollection where
  makeObject = makeObject . unHTMLFormControlsCollection

instance IsHTMLCollection HTMLFormControlsCollection
instance IsGObject HTMLFormControlsCollection where
  typeGType _ = gTypeHTMLFormControlsCollection
  {-# INLINE typeGType #-}

noHTMLFormControlsCollection :: Maybe HTMLFormControlsCollection
noHTMLFormControlsCollection = Nothing
{-# INLINE noHTMLFormControlsCollection #-}

gTypeHTMLFormControlsCollection :: JSM GType
gTypeHTMLFormControlsCollection = GType . Object <$> jsg "HTMLFormControlsCollection"

-- | Functions for this inteface are in "JSDOM.HTMLFormElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement Mozilla HTMLFormElement documentation>
newtype HTMLFormElement = HTMLFormElement { unHTMLFormElement :: JSVal }

instance PToJSVal HTMLFormElement where
  pToJSVal = unHTMLFormElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFormElement where
  pFromJSVal = HTMLFormElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFormElement where
  toJSVal = return . unHTMLFormElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFormElement where
  fromJSVal v = fmap HTMLFormElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFormElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFormElement where
  makeObject = makeObject . unHTMLFormElement

instance IsHTMLElement HTMLFormElement
instance IsElement HTMLFormElement
instance IsNode HTMLFormElement
instance IsEventTarget HTMLFormElement
instance IsSlotable HTMLFormElement
instance IsParentNode HTMLFormElement
instance IsNonDocumentTypeChildNode HTMLFormElement
instance IsDocumentAndElementEventHandlers HTMLFormElement
instance IsChildNode HTMLFormElement
instance IsAnimatable HTMLFormElement
instance IsGlobalEventHandlers HTMLFormElement
instance IsElementCSSInlineStyle HTMLFormElement
instance IsGObject HTMLFormElement where
  typeGType _ = gTypeHTMLFormElement
  {-# INLINE typeGType #-}

noHTMLFormElement :: Maybe HTMLFormElement
noHTMLFormElement = Nothing
{-# INLINE noHTMLFormElement #-}

gTypeHTMLFormElement :: JSM GType
gTypeHTMLFormElement = GType . Object <$> jsg "HTMLFormElement"

-- | Functions for this inteface are in "JSDOM.HTMLFrameElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFrameElement Mozilla HTMLFrameElement documentation>
newtype HTMLFrameElement = HTMLFrameElement { unHTMLFrameElement :: JSVal }

instance PToJSVal HTMLFrameElement where
  pToJSVal = unHTMLFrameElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFrameElement where
  pFromJSVal = HTMLFrameElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFrameElement where
  toJSVal = return . unHTMLFrameElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFrameElement where
  fromJSVal v = fmap HTMLFrameElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFrameElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFrameElement where
  makeObject = makeObject . unHTMLFrameElement

instance IsHTMLElement HTMLFrameElement
instance IsElement HTMLFrameElement
instance IsNode HTMLFrameElement
instance IsEventTarget HTMLFrameElement
instance IsSlotable HTMLFrameElement
instance IsParentNode HTMLFrameElement
instance IsNonDocumentTypeChildNode HTMLFrameElement
instance IsDocumentAndElementEventHandlers HTMLFrameElement
instance IsChildNode HTMLFrameElement
instance IsAnimatable HTMLFrameElement
instance IsGlobalEventHandlers HTMLFrameElement
instance IsElementCSSInlineStyle HTMLFrameElement
instance IsGObject HTMLFrameElement where
  typeGType _ = gTypeHTMLFrameElement
  {-# INLINE typeGType #-}

noHTMLFrameElement :: Maybe HTMLFrameElement
noHTMLFrameElement = Nothing
{-# INLINE noHTMLFrameElement #-}

gTypeHTMLFrameElement :: JSM GType
gTypeHTMLFrameElement = GType . Object <$> jsg "HTMLFrameElement"

-- | Functions for this inteface are in "JSDOM.HTMLFrameSetElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
--     * "JSDOM.WindowEventHandlers"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLFrameSetElement Mozilla HTMLFrameSetElement documentation>
newtype HTMLFrameSetElement = HTMLFrameSetElement { unHTMLFrameSetElement :: JSVal }

instance PToJSVal HTMLFrameSetElement where
  pToJSVal = unHTMLFrameSetElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLFrameSetElement where
  pFromJSVal = HTMLFrameSetElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLFrameSetElement where
  toJSVal = return . unHTMLFrameSetElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLFrameSetElement where
  fromJSVal v = fmap HTMLFrameSetElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLFrameSetElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLFrameSetElement where
  makeObject = makeObject . unHTMLFrameSetElement

instance IsHTMLElement HTMLFrameSetElement
instance IsElement HTMLFrameSetElement
instance IsNode HTMLFrameSetElement
instance IsEventTarget HTMLFrameSetElement
instance IsSlotable HTMLFrameSetElement
instance IsParentNode HTMLFrameSetElement
instance IsNonDocumentTypeChildNode HTMLFrameSetElement
instance IsDocumentAndElementEventHandlers HTMLFrameSetElement
instance IsChildNode HTMLFrameSetElement
instance IsAnimatable HTMLFrameSetElement
instance IsGlobalEventHandlers HTMLFrameSetElement
instance IsElementCSSInlineStyle HTMLFrameSetElement
instance IsWindowEventHandlers HTMLFrameSetElement
instance IsGObject HTMLFrameSetElement where
  typeGType _ = gTypeHTMLFrameSetElement
  {-# INLINE typeGType #-}

noHTMLFrameSetElement :: Maybe HTMLFrameSetElement
noHTMLFrameSetElement = Nothing
{-# INLINE noHTMLFrameSetElement #-}

gTypeHTMLFrameSetElement :: JSM GType
gTypeHTMLFrameSetElement = GType . Object <$> jsg "HTMLFrameSetElement"

-- | Functions for this inteface are in "JSDOM.HTMLHRElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLHRElement Mozilla HTMLHRElement documentation>
newtype HTMLHRElement = HTMLHRElement { unHTMLHRElement :: JSVal }

instance PToJSVal HTMLHRElement where
  pToJSVal = unHTMLHRElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLHRElement where
  pFromJSVal = HTMLHRElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLHRElement where
  toJSVal = return . unHTMLHRElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLHRElement where
  fromJSVal v = fmap HTMLHRElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLHRElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLHRElement where
  makeObject = makeObject . unHTMLHRElement

instance IsHTMLElement HTMLHRElement
instance IsElement HTMLHRElement
instance IsNode HTMLHRElement
instance IsEventTarget HTMLHRElement
instance IsSlotable HTMLHRElement
instance IsParentNode HTMLHRElement
instance IsNonDocumentTypeChildNode HTMLHRElement
instance IsDocumentAndElementEventHandlers HTMLHRElement
instance IsChildNode HTMLHRElement
instance IsAnimatable HTMLHRElement
instance IsGlobalEventHandlers HTMLHRElement
instance IsElementCSSInlineStyle HTMLHRElement
instance IsGObject HTMLHRElement where
  typeGType _ = gTypeHTMLHRElement
  {-# INLINE typeGType #-}

noHTMLHRElement :: Maybe HTMLHRElement
noHTMLHRElement = Nothing
{-# INLINE noHTMLHRElement #-}

gTypeHTMLHRElement :: JSM GType
gTypeHTMLHRElement = GType . Object <$> jsg "HTMLHRElement"

-- | Functions for this inteface are in "JSDOM.HTMLHeadElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLHeadElement Mozilla HTMLHeadElement documentation>
newtype HTMLHeadElement = HTMLHeadElement { unHTMLHeadElement :: JSVal }

instance PToJSVal HTMLHeadElement where
  pToJSVal = unHTMLHeadElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLHeadElement where
  pFromJSVal = HTMLHeadElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLHeadElement where
  toJSVal = return . unHTMLHeadElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLHeadElement where
  fromJSVal v = fmap HTMLHeadElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLHeadElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLHeadElement where
  makeObject = makeObject . unHTMLHeadElement

instance IsHTMLElement HTMLHeadElement
instance IsElement HTMLHeadElement
instance IsNode HTMLHeadElement
instance IsEventTarget HTMLHeadElement
instance IsSlotable HTMLHeadElement
instance IsParentNode HTMLHeadElement
instance IsNonDocumentTypeChildNode HTMLHeadElement
instance IsDocumentAndElementEventHandlers HTMLHeadElement
instance IsChildNode HTMLHeadElement
instance IsAnimatable HTMLHeadElement
instance IsGlobalEventHandlers HTMLHeadElement
instance IsElementCSSInlineStyle HTMLHeadElement
instance IsGObject HTMLHeadElement where
  typeGType _ = gTypeHTMLHeadElement
  {-# INLINE typeGType #-}

noHTMLHeadElement :: Maybe HTMLHeadElement
noHTMLHeadElement = Nothing
{-# INLINE noHTMLHeadElement #-}

gTypeHTMLHeadElement :: JSM GType
gTypeHTMLHeadElement = GType . Object <$> jsg "HTMLHeadElement"

-- | Functions for this inteface are in "JSDOM.HTMLHeadingElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLHeadingElement Mozilla HTMLHeadingElement documentation>
newtype HTMLHeadingElement = HTMLHeadingElement { unHTMLHeadingElement :: JSVal }

instance PToJSVal HTMLHeadingElement where
  pToJSVal = unHTMLHeadingElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLHeadingElement where
  pFromJSVal = HTMLHeadingElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLHeadingElement where
  toJSVal = return . unHTMLHeadingElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLHeadingElement where
  fromJSVal v = fmap HTMLHeadingElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLHeadingElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLHeadingElement where
  makeObject = makeObject . unHTMLHeadingElement

instance IsHTMLElement HTMLHeadingElement
instance IsElement HTMLHeadingElement
instance IsNode HTMLHeadingElement
instance IsEventTarget HTMLHeadingElement
instance IsSlotable HTMLHeadingElement
instance IsParentNode HTMLHeadingElement
instance IsNonDocumentTypeChildNode HTMLHeadingElement
instance IsDocumentAndElementEventHandlers HTMLHeadingElement
instance IsChildNode HTMLHeadingElement
instance IsAnimatable HTMLHeadingElement
instance IsGlobalEventHandlers HTMLHeadingElement
instance IsElementCSSInlineStyle HTMLHeadingElement
instance IsGObject HTMLHeadingElement where
  typeGType _ = gTypeHTMLHeadingElement
  {-# INLINE typeGType #-}

noHTMLHeadingElement :: Maybe HTMLHeadingElement
noHTMLHeadingElement = Nothing
{-# INLINE noHTMLHeadingElement #-}

gTypeHTMLHeadingElement :: JSM GType
gTypeHTMLHeadingElement = GType . Object <$> jsg "HTMLHeadingElement"

-- | Functions for this inteface are in "JSDOM.HTMLHtmlElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLHtmlElement Mozilla HTMLHtmlElement documentation>
newtype HTMLHtmlElement = HTMLHtmlElement { unHTMLHtmlElement :: JSVal }

instance PToJSVal HTMLHtmlElement where
  pToJSVal = unHTMLHtmlElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLHtmlElement where
  pFromJSVal = HTMLHtmlElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLHtmlElement where
  toJSVal = return . unHTMLHtmlElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLHtmlElement where
  fromJSVal v = fmap HTMLHtmlElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLHtmlElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLHtmlElement where
  makeObject = makeObject . unHTMLHtmlElement

instance IsHTMLElement HTMLHtmlElement
instance IsElement HTMLHtmlElement
instance IsNode HTMLHtmlElement
instance IsEventTarget HTMLHtmlElement
instance IsSlotable HTMLHtmlElement
instance IsParentNode HTMLHtmlElement
instance IsNonDocumentTypeChildNode HTMLHtmlElement
instance IsDocumentAndElementEventHandlers HTMLHtmlElement
instance IsChildNode HTMLHtmlElement
instance IsAnimatable HTMLHtmlElement
instance IsGlobalEventHandlers HTMLHtmlElement
instance IsElementCSSInlineStyle HTMLHtmlElement
instance IsGObject HTMLHtmlElement where
  typeGType _ = gTypeHTMLHtmlElement
  {-# INLINE typeGType #-}

noHTMLHtmlElement :: Maybe HTMLHtmlElement
noHTMLHtmlElement = Nothing
{-# INLINE noHTMLHtmlElement #-}

gTypeHTMLHtmlElement :: JSM GType
gTypeHTMLHtmlElement = GType . Object <$> jsg "HTMLHtmlElement"

-- | Functions for this inteface are in "JSDOM.HTMLHyperlinkElementUtils".
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLHyperlinkElementUtils Mozilla HTMLHyperlinkElementUtils documentation>
newtype HTMLHyperlinkElementUtils = HTMLHyperlinkElementUtils { unHTMLHyperlinkElementUtils :: JSVal }

instance PToJSVal HTMLHyperlinkElementUtils where
  pToJSVal = unHTMLHyperlinkElementUtils
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLHyperlinkElementUtils where
  pFromJSVal = HTMLHyperlinkElementUtils
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLHyperlinkElementUtils where
  toJSVal = return . unHTMLHyperlinkElementUtils
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLHyperlinkElementUtils where
  fromJSVal v = fmap HTMLHyperlinkElementUtils <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLHyperlinkElementUtils
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLHyperlinkElementUtils where
  makeObject = makeObject . unHTMLHyperlinkElementUtils

class (IsGObject o) => IsHTMLHyperlinkElementUtils o
toHTMLHyperlinkElementUtils :: IsHTMLHyperlinkElementUtils o => o -> HTMLHyperlinkElementUtils
toHTMLHyperlinkElementUtils = HTMLHyperlinkElementUtils . coerce

instance IsHTMLHyperlinkElementUtils HTMLHyperlinkElementUtils
instance IsGObject HTMLHyperlinkElementUtils where
  typeGType _ = gTypeHTMLHyperlinkElementUtils
  {-# INLINE typeGType #-}

noHTMLHyperlinkElementUtils :: Maybe HTMLHyperlinkElementUtils
noHTMLHyperlinkElementUtils = Nothing
{-# INLINE noHTMLHyperlinkElementUtils #-}

gTypeHTMLHyperlinkElementUtils :: JSM GType
gTypeHTMLHyperlinkElementUtils = GType . Object <$> jsg "HTMLHyperlinkElementUtils"

-- | Functions for this inteface are in "JSDOM.HTMLIFrameElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLIFrameElement Mozilla HTMLIFrameElement documentation>
newtype HTMLIFrameElement = HTMLIFrameElement { unHTMLIFrameElement :: JSVal }

instance PToJSVal HTMLIFrameElement where
  pToJSVal = unHTMLIFrameElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLIFrameElement where
  pFromJSVal = HTMLIFrameElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLIFrameElement where
  toJSVal = return . unHTMLIFrameElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLIFrameElement where
  fromJSVal v = fmap HTMLIFrameElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLIFrameElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLIFrameElement where
  makeObject = makeObject . unHTMLIFrameElement

instance IsHTMLElement HTMLIFrameElement
instance IsElement HTMLIFrameElement
instance IsNode HTMLIFrameElement
instance IsEventTarget HTMLIFrameElement
instance IsSlotable HTMLIFrameElement
instance IsParentNode HTMLIFrameElement
instance IsNonDocumentTypeChildNode HTMLIFrameElement
instance IsDocumentAndElementEventHandlers HTMLIFrameElement
instance IsChildNode HTMLIFrameElement
instance IsAnimatable HTMLIFrameElement
instance IsGlobalEventHandlers HTMLIFrameElement
instance IsElementCSSInlineStyle HTMLIFrameElement
instance IsGObject HTMLIFrameElement where
  typeGType _ = gTypeHTMLIFrameElement
  {-# INLINE typeGType #-}

noHTMLIFrameElement :: Maybe HTMLIFrameElement
noHTMLIFrameElement = Nothing
{-# INLINE noHTMLIFrameElement #-}

gTypeHTMLIFrameElement :: JSM GType
gTypeHTMLIFrameElement = GType . Object <$> jsg "HTMLIFrameElement"

-- | Functions for this inteface are in "JSDOM.HTMLImageElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement Mozilla HTMLImageElement documentation>
newtype HTMLImageElement = HTMLImageElement { unHTMLImageElement :: JSVal }

instance PToJSVal HTMLImageElement where
  pToJSVal = unHTMLImageElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLImageElement where
  pFromJSVal = HTMLImageElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLImageElement where
  toJSVal = return . unHTMLImageElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLImageElement where
  fromJSVal v = fmap HTMLImageElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLImageElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLImageElement where
  makeObject = makeObject . unHTMLImageElement

instance IsHTMLElement HTMLImageElement
instance IsElement HTMLImageElement
instance IsNode HTMLImageElement
instance IsEventTarget HTMLImageElement
instance IsSlotable HTMLImageElement
instance IsParentNode HTMLImageElement
instance IsNonDocumentTypeChildNode HTMLImageElement
instance IsDocumentAndElementEventHandlers HTMLImageElement
instance IsChildNode HTMLImageElement
instance IsAnimatable HTMLImageElement
instance IsGlobalEventHandlers HTMLImageElement
instance IsElementCSSInlineStyle HTMLImageElement
instance IsGObject HTMLImageElement where
  typeGType _ = gTypeHTMLImageElement
  {-# INLINE typeGType #-}

noHTMLImageElement :: Maybe HTMLImageElement
noHTMLImageElement = Nothing
{-# INLINE noHTMLImageElement #-}

gTypeHTMLImageElement :: JSM GType
gTypeHTMLImageElement = GType . Object <$> jsg "HTMLImageElement"

-- | Functions for this inteface are in "JSDOM.HTMLInputElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement Mozilla HTMLInputElement documentation>
newtype HTMLInputElement = HTMLInputElement { unHTMLInputElement :: JSVal }

instance PToJSVal HTMLInputElement where
  pToJSVal = unHTMLInputElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLInputElement where
  pFromJSVal = HTMLInputElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLInputElement where
  toJSVal = return . unHTMLInputElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLInputElement where
  fromJSVal v = fmap HTMLInputElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLInputElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLInputElement where
  makeObject = makeObject . unHTMLInputElement

instance IsHTMLElement HTMLInputElement
instance IsElement HTMLInputElement
instance IsNode HTMLInputElement
instance IsEventTarget HTMLInputElement
instance IsSlotable HTMLInputElement
instance IsParentNode HTMLInputElement
instance IsNonDocumentTypeChildNode HTMLInputElement
instance IsDocumentAndElementEventHandlers HTMLInputElement
instance IsChildNode HTMLInputElement
instance IsAnimatable HTMLInputElement
instance IsGlobalEventHandlers HTMLInputElement
instance IsElementCSSInlineStyle HTMLInputElement
instance IsGObject HTMLInputElement where
  typeGType _ = gTypeHTMLInputElement
  {-# INLINE typeGType #-}

noHTMLInputElement :: Maybe HTMLInputElement
noHTMLInputElement = Nothing
{-# INLINE noHTMLInputElement #-}

gTypeHTMLInputElement :: JSM GType
gTypeHTMLInputElement = GType . Object <$> jsg "HTMLInputElement"

-- | Functions for this inteface are in "JSDOM.HTMLKeygenElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLKeygenElement Mozilla HTMLKeygenElement documentation>
newtype HTMLKeygenElement = HTMLKeygenElement { unHTMLKeygenElement :: JSVal }

instance PToJSVal HTMLKeygenElement where
  pToJSVal = unHTMLKeygenElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLKeygenElement where
  pFromJSVal = HTMLKeygenElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLKeygenElement where
  toJSVal = return . unHTMLKeygenElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLKeygenElement where
  fromJSVal v = fmap HTMLKeygenElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLKeygenElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLKeygenElement where
  makeObject = makeObject . unHTMLKeygenElement

instance IsHTMLElement HTMLKeygenElement
instance IsElement HTMLKeygenElement
instance IsNode HTMLKeygenElement
instance IsEventTarget HTMLKeygenElement
instance IsSlotable HTMLKeygenElement
instance IsParentNode HTMLKeygenElement
instance IsNonDocumentTypeChildNode HTMLKeygenElement
instance IsDocumentAndElementEventHandlers HTMLKeygenElement
instance IsChildNode HTMLKeygenElement
instance IsAnimatable HTMLKeygenElement
instance IsGlobalEventHandlers HTMLKeygenElement
instance IsElementCSSInlineStyle HTMLKeygenElement
instance IsGObject HTMLKeygenElement where
  typeGType _ = gTypeHTMLKeygenElement
  {-# INLINE typeGType #-}

noHTMLKeygenElement :: Maybe HTMLKeygenElement
noHTMLKeygenElement = Nothing
{-# INLINE noHTMLKeygenElement #-}

gTypeHTMLKeygenElement :: JSM GType
gTypeHTMLKeygenElement = GType . Object <$> jsg "HTMLKeygenElement"

-- | Functions for this inteface are in "JSDOM.HTMLLIElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLIElement Mozilla HTMLLIElement documentation>
newtype HTMLLIElement = HTMLLIElement { unHTMLLIElement :: JSVal }

instance PToJSVal HTMLLIElement where
  pToJSVal = unHTMLLIElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLLIElement where
  pFromJSVal = HTMLLIElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLLIElement where
  toJSVal = return . unHTMLLIElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLLIElement where
  fromJSVal v = fmap HTMLLIElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLLIElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLLIElement where
  makeObject = makeObject . unHTMLLIElement

instance IsHTMLElement HTMLLIElement
instance IsElement HTMLLIElement
instance IsNode HTMLLIElement
instance IsEventTarget HTMLLIElement
instance IsSlotable HTMLLIElement
instance IsParentNode HTMLLIElement
instance IsNonDocumentTypeChildNode HTMLLIElement
instance IsDocumentAndElementEventHandlers HTMLLIElement
instance IsChildNode HTMLLIElement
instance IsAnimatable HTMLLIElement
instance IsGlobalEventHandlers HTMLLIElement
instance IsElementCSSInlineStyle HTMLLIElement
instance IsGObject HTMLLIElement where
  typeGType _ = gTypeHTMLLIElement
  {-# INLINE typeGType #-}

noHTMLLIElement :: Maybe HTMLLIElement
noHTMLLIElement = Nothing
{-# INLINE noHTMLLIElement #-}

gTypeHTMLLIElement :: JSM GType
gTypeHTMLLIElement = GType . Object <$> jsg "HTMLLIElement"

-- | Functions for this inteface are in "JSDOM.HTMLLabelElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLabelElement Mozilla HTMLLabelElement documentation>
newtype HTMLLabelElement = HTMLLabelElement { unHTMLLabelElement :: JSVal }

instance PToJSVal HTMLLabelElement where
  pToJSVal = unHTMLLabelElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLLabelElement where
  pFromJSVal = HTMLLabelElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLLabelElement where
  toJSVal = return . unHTMLLabelElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLLabelElement where
  fromJSVal v = fmap HTMLLabelElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLLabelElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLLabelElement where
  makeObject = makeObject . unHTMLLabelElement

instance IsHTMLElement HTMLLabelElement
instance IsElement HTMLLabelElement
instance IsNode HTMLLabelElement
instance IsEventTarget HTMLLabelElement
instance IsSlotable HTMLLabelElement
instance IsParentNode HTMLLabelElement
instance IsNonDocumentTypeChildNode HTMLLabelElement
instance IsDocumentAndElementEventHandlers HTMLLabelElement
instance IsChildNode HTMLLabelElement
instance IsAnimatable HTMLLabelElement
instance IsGlobalEventHandlers HTMLLabelElement
instance IsElementCSSInlineStyle HTMLLabelElement
instance IsGObject HTMLLabelElement where
  typeGType _ = gTypeHTMLLabelElement
  {-# INLINE typeGType #-}

noHTMLLabelElement :: Maybe HTMLLabelElement
noHTMLLabelElement = Nothing
{-# INLINE noHTMLLabelElement #-}

gTypeHTMLLabelElement :: JSM GType
gTypeHTMLLabelElement = GType . Object <$> jsg "HTMLLabelElement"

-- | Functions for this inteface are in "JSDOM.HTMLLegendElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLegendElement Mozilla HTMLLegendElement documentation>
newtype HTMLLegendElement = HTMLLegendElement { unHTMLLegendElement :: JSVal }

instance PToJSVal HTMLLegendElement where
  pToJSVal = unHTMLLegendElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLLegendElement where
  pFromJSVal = HTMLLegendElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLLegendElement where
  toJSVal = return . unHTMLLegendElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLLegendElement where
  fromJSVal v = fmap HTMLLegendElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLLegendElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLLegendElement where
  makeObject = makeObject . unHTMLLegendElement

instance IsHTMLElement HTMLLegendElement
instance IsElement HTMLLegendElement
instance IsNode HTMLLegendElement
instance IsEventTarget HTMLLegendElement
instance IsSlotable HTMLLegendElement
instance IsParentNode HTMLLegendElement
instance IsNonDocumentTypeChildNode HTMLLegendElement
instance IsDocumentAndElementEventHandlers HTMLLegendElement
instance IsChildNode HTMLLegendElement
instance IsAnimatable HTMLLegendElement
instance IsGlobalEventHandlers HTMLLegendElement
instance IsElementCSSInlineStyle HTMLLegendElement
instance IsGObject HTMLLegendElement where
  typeGType _ = gTypeHTMLLegendElement
  {-# INLINE typeGType #-}

noHTMLLegendElement :: Maybe HTMLLegendElement
noHTMLLegendElement = Nothing
{-# INLINE noHTMLLegendElement #-}

gTypeHTMLLegendElement :: JSM GType
gTypeHTMLLegendElement = GType . Object <$> jsg "HTMLLegendElement"

-- | Functions for this inteface are in "JSDOM.HTMLLinkElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLLinkElement Mozilla HTMLLinkElement documentation>
newtype HTMLLinkElement = HTMLLinkElement { unHTMLLinkElement :: JSVal }

instance PToJSVal HTMLLinkElement where
  pToJSVal = unHTMLLinkElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLLinkElement where
  pFromJSVal = HTMLLinkElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLLinkElement where
  toJSVal = return . unHTMLLinkElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLLinkElement where
  fromJSVal v = fmap HTMLLinkElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLLinkElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLLinkElement where
  makeObject = makeObject . unHTMLLinkElement

instance IsHTMLElement HTMLLinkElement
instance IsElement HTMLLinkElement
instance IsNode HTMLLinkElement
instance IsEventTarget HTMLLinkElement
instance IsSlotable HTMLLinkElement
instance IsParentNode HTMLLinkElement
instance IsNonDocumentTypeChildNode HTMLLinkElement
instance IsDocumentAndElementEventHandlers HTMLLinkElement
instance IsChildNode HTMLLinkElement
instance IsAnimatable HTMLLinkElement
instance IsGlobalEventHandlers HTMLLinkElement
instance IsElementCSSInlineStyle HTMLLinkElement
instance IsGObject HTMLLinkElement where
  typeGType _ = gTypeHTMLLinkElement
  {-# INLINE typeGType #-}

noHTMLLinkElement :: Maybe HTMLLinkElement
noHTMLLinkElement = Nothing
{-# INLINE noHTMLLinkElement #-}

gTypeHTMLLinkElement :: JSM GType
gTypeHTMLLinkElement = GType . Object <$> jsg "HTMLLinkElement"

-- | Functions for this inteface are in "JSDOM.HTMLMapElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMapElement Mozilla HTMLMapElement documentation>
newtype HTMLMapElement = HTMLMapElement { unHTMLMapElement :: JSVal }

instance PToJSVal HTMLMapElement where
  pToJSVal = unHTMLMapElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMapElement where
  pFromJSVal = HTMLMapElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMapElement where
  toJSVal = return . unHTMLMapElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMapElement where
  fromJSVal v = fmap HTMLMapElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMapElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMapElement where
  makeObject = makeObject . unHTMLMapElement

instance IsHTMLElement HTMLMapElement
instance IsElement HTMLMapElement
instance IsNode HTMLMapElement
instance IsEventTarget HTMLMapElement
instance IsSlotable HTMLMapElement
instance IsParentNode HTMLMapElement
instance IsNonDocumentTypeChildNode HTMLMapElement
instance IsDocumentAndElementEventHandlers HTMLMapElement
instance IsChildNode HTMLMapElement
instance IsAnimatable HTMLMapElement
instance IsGlobalEventHandlers HTMLMapElement
instance IsElementCSSInlineStyle HTMLMapElement
instance IsGObject HTMLMapElement where
  typeGType _ = gTypeHTMLMapElement
  {-# INLINE typeGType #-}

noHTMLMapElement :: Maybe HTMLMapElement
noHTMLMapElement = Nothing
{-# INLINE noHTMLMapElement #-}

gTypeHTMLMapElement :: JSM GType
gTypeHTMLMapElement = GType . Object <$> jsg "HTMLMapElement"

-- | Functions for this inteface are in "JSDOM.HTMLMarqueeElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMarqueeElement Mozilla HTMLMarqueeElement documentation>
newtype HTMLMarqueeElement = HTMLMarqueeElement { unHTMLMarqueeElement :: JSVal }

instance PToJSVal HTMLMarqueeElement where
  pToJSVal = unHTMLMarqueeElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMarqueeElement where
  pFromJSVal = HTMLMarqueeElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMarqueeElement where
  toJSVal = return . unHTMLMarqueeElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMarqueeElement where
  fromJSVal v = fmap HTMLMarqueeElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMarqueeElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMarqueeElement where
  makeObject = makeObject . unHTMLMarqueeElement

instance IsHTMLElement HTMLMarqueeElement
instance IsElement HTMLMarqueeElement
instance IsNode HTMLMarqueeElement
instance IsEventTarget HTMLMarqueeElement
instance IsSlotable HTMLMarqueeElement
instance IsParentNode HTMLMarqueeElement
instance IsNonDocumentTypeChildNode HTMLMarqueeElement
instance IsDocumentAndElementEventHandlers HTMLMarqueeElement
instance IsChildNode HTMLMarqueeElement
instance IsAnimatable HTMLMarqueeElement
instance IsGlobalEventHandlers HTMLMarqueeElement
instance IsElementCSSInlineStyle HTMLMarqueeElement
instance IsGObject HTMLMarqueeElement where
  typeGType _ = gTypeHTMLMarqueeElement
  {-# INLINE typeGType #-}

noHTMLMarqueeElement :: Maybe HTMLMarqueeElement
noHTMLMarqueeElement = Nothing
{-# INLINE noHTMLMarqueeElement #-}

gTypeHTMLMarqueeElement :: JSM GType
gTypeHTMLMarqueeElement = GType . Object <$> jsg "HTMLMarqueeElement"

-- | Functions for this inteface are in "JSDOM.HTMLMediaElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMediaElement Mozilla HTMLMediaElement documentation>
newtype HTMLMediaElement = HTMLMediaElement { unHTMLMediaElement :: JSVal }

instance PToJSVal HTMLMediaElement where
  pToJSVal = unHTMLMediaElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMediaElement where
  pFromJSVal = HTMLMediaElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMediaElement where
  toJSVal = return . unHTMLMediaElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMediaElement where
  fromJSVal v = fmap HTMLMediaElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMediaElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMediaElement where
  makeObject = makeObject . unHTMLMediaElement

class (IsHTMLElement o, IsElement o, IsNode o, IsEventTarget o, IsSlotable o, IsParentNode o, IsNonDocumentTypeChildNode o, IsDocumentAndElementEventHandlers o, IsChildNode o, IsAnimatable o, IsGlobalEventHandlers o, IsElementCSSInlineStyle o, IsGObject o) => IsHTMLMediaElement o
toHTMLMediaElement :: IsHTMLMediaElement o => o -> HTMLMediaElement
toHTMLMediaElement = HTMLMediaElement . coerce

instance IsHTMLMediaElement HTMLMediaElement
instance IsHTMLElement HTMLMediaElement
instance IsElement HTMLMediaElement
instance IsNode HTMLMediaElement
instance IsEventTarget HTMLMediaElement
instance IsSlotable HTMLMediaElement
instance IsParentNode HTMLMediaElement
instance IsNonDocumentTypeChildNode HTMLMediaElement
instance IsDocumentAndElementEventHandlers HTMLMediaElement
instance IsChildNode HTMLMediaElement
instance IsAnimatable HTMLMediaElement
instance IsGlobalEventHandlers HTMLMediaElement
instance IsElementCSSInlineStyle HTMLMediaElement
instance IsGObject HTMLMediaElement where
  typeGType _ = gTypeHTMLMediaElement
  {-# INLINE typeGType #-}

noHTMLMediaElement :: Maybe HTMLMediaElement
noHTMLMediaElement = Nothing
{-# INLINE noHTMLMediaElement #-}

gTypeHTMLMediaElement :: JSM GType
gTypeHTMLMediaElement = GType . Object <$> jsg "HTMLMediaElement"

-- | Functions for this inteface are in "JSDOM.HTMLMenuElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMenuElement Mozilla HTMLMenuElement documentation>
newtype HTMLMenuElement = HTMLMenuElement { unHTMLMenuElement :: JSVal }

instance PToJSVal HTMLMenuElement where
  pToJSVal = unHTMLMenuElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMenuElement where
  pFromJSVal = HTMLMenuElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMenuElement where
  toJSVal = return . unHTMLMenuElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMenuElement where
  fromJSVal v = fmap HTMLMenuElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMenuElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMenuElement where
  makeObject = makeObject . unHTMLMenuElement

instance IsHTMLElement HTMLMenuElement
instance IsElement HTMLMenuElement
instance IsNode HTMLMenuElement
instance IsEventTarget HTMLMenuElement
instance IsSlotable HTMLMenuElement
instance IsParentNode HTMLMenuElement
instance IsNonDocumentTypeChildNode HTMLMenuElement
instance IsDocumentAndElementEventHandlers HTMLMenuElement
instance IsChildNode HTMLMenuElement
instance IsAnimatable HTMLMenuElement
instance IsGlobalEventHandlers HTMLMenuElement
instance IsElementCSSInlineStyle HTMLMenuElement
instance IsGObject HTMLMenuElement where
  typeGType _ = gTypeHTMLMenuElement
  {-# INLINE typeGType #-}

noHTMLMenuElement :: Maybe HTMLMenuElement
noHTMLMenuElement = Nothing
{-# INLINE noHTMLMenuElement #-}

gTypeHTMLMenuElement :: JSM GType
gTypeHTMLMenuElement = GType . Object <$> jsg "HTMLMenuElement"

-- | Functions for this inteface are in "JSDOM.HTMLMetaElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMetaElement Mozilla HTMLMetaElement documentation>
newtype HTMLMetaElement = HTMLMetaElement { unHTMLMetaElement :: JSVal }

instance PToJSVal HTMLMetaElement where
  pToJSVal = unHTMLMetaElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMetaElement where
  pFromJSVal = HTMLMetaElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMetaElement where
  toJSVal = return . unHTMLMetaElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMetaElement where
  fromJSVal v = fmap HTMLMetaElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMetaElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMetaElement where
  makeObject = makeObject . unHTMLMetaElement

instance IsHTMLElement HTMLMetaElement
instance IsElement HTMLMetaElement
instance IsNode HTMLMetaElement
instance IsEventTarget HTMLMetaElement
instance IsSlotable HTMLMetaElement
instance IsParentNode HTMLMetaElement
instance IsNonDocumentTypeChildNode HTMLMetaElement
instance IsDocumentAndElementEventHandlers HTMLMetaElement
instance IsChildNode HTMLMetaElement
instance IsAnimatable HTMLMetaElement
instance IsGlobalEventHandlers HTMLMetaElement
instance IsElementCSSInlineStyle HTMLMetaElement
instance IsGObject HTMLMetaElement where
  typeGType _ = gTypeHTMLMetaElement
  {-# INLINE typeGType #-}

noHTMLMetaElement :: Maybe HTMLMetaElement
noHTMLMetaElement = Nothing
{-# INLINE noHTMLMetaElement #-}

gTypeHTMLMetaElement :: JSM GType
gTypeHTMLMetaElement = GType . Object <$> jsg "HTMLMetaElement"

-- | Functions for this inteface are in "JSDOM.HTMLMeterElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLMeterElement Mozilla HTMLMeterElement documentation>
newtype HTMLMeterElement = HTMLMeterElement { unHTMLMeterElement :: JSVal }

instance PToJSVal HTMLMeterElement where
  pToJSVal = unHTMLMeterElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLMeterElement where
  pFromJSVal = HTMLMeterElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLMeterElement where
  toJSVal = return . unHTMLMeterElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLMeterElement where
  fromJSVal v = fmap HTMLMeterElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLMeterElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLMeterElement where
  makeObject = makeObject . unHTMLMeterElement

instance IsHTMLElement HTMLMeterElement
instance IsElement HTMLMeterElement
instance IsNode HTMLMeterElement
instance IsEventTarget HTMLMeterElement
instance IsSlotable HTMLMeterElement
instance IsParentNode HTMLMeterElement
instance IsNonDocumentTypeChildNode HTMLMeterElement
instance IsDocumentAndElementEventHandlers HTMLMeterElement
instance IsChildNode HTMLMeterElement
instance IsAnimatable HTMLMeterElement
instance IsGlobalEventHandlers HTMLMeterElement
instance IsElementCSSInlineStyle HTMLMeterElement
instance IsGObject HTMLMeterElement where
  typeGType _ = gTypeHTMLMeterElement
  {-# INLINE typeGType #-}

noHTMLMeterElement :: Maybe HTMLMeterElement
noHTMLMeterElement = Nothing
{-# INLINE noHTMLMeterElement #-}

gTypeHTMLMeterElement :: JSM GType
gTypeHTMLMeterElement = GType . Object <$> jsg "HTMLMeterElement"

-- | Functions for this inteface are in "JSDOM.HTMLModElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLModElement Mozilla HTMLModElement documentation>
newtype HTMLModElement = HTMLModElement { unHTMLModElement :: JSVal }

instance PToJSVal HTMLModElement where
  pToJSVal = unHTMLModElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLModElement where
  pFromJSVal = HTMLModElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLModElement where
  toJSVal = return . unHTMLModElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLModElement where
  fromJSVal v = fmap HTMLModElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLModElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLModElement where
  makeObject = makeObject . unHTMLModElement

instance IsHTMLElement HTMLModElement
instance IsElement HTMLModElement
instance IsNode HTMLModElement
instance IsEventTarget HTMLModElement
instance IsSlotable HTMLModElement
instance IsParentNode HTMLModElement
instance IsNonDocumentTypeChildNode HTMLModElement
instance IsDocumentAndElementEventHandlers HTMLModElement
instance IsChildNode HTMLModElement
instance IsAnimatable HTMLModElement
instance IsGlobalEventHandlers HTMLModElement
instance IsElementCSSInlineStyle HTMLModElement
instance IsGObject HTMLModElement where
  typeGType _ = gTypeHTMLModElement
  {-# INLINE typeGType #-}

noHTMLModElement :: Maybe HTMLModElement
noHTMLModElement = Nothing
{-# INLINE noHTMLModElement #-}

gTypeHTMLModElement :: JSM GType
gTypeHTMLModElement = GType . Object <$> jsg "HTMLModElement"

-- | Functions for this inteface are in "JSDOM.HTMLOListElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOListElement Mozilla HTMLOListElement documentation>
newtype HTMLOListElement = HTMLOListElement { unHTMLOListElement :: JSVal }

instance PToJSVal HTMLOListElement where
  pToJSVal = unHTMLOListElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOListElement where
  pFromJSVal = HTMLOListElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOListElement where
  toJSVal = return . unHTMLOListElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOListElement where
  fromJSVal v = fmap HTMLOListElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOListElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOListElement where
  makeObject = makeObject . unHTMLOListElement

instance IsHTMLElement HTMLOListElement
instance IsElement HTMLOListElement
instance IsNode HTMLOListElement
instance IsEventTarget HTMLOListElement
instance IsSlotable HTMLOListElement
instance IsParentNode HTMLOListElement
instance IsNonDocumentTypeChildNode HTMLOListElement
instance IsDocumentAndElementEventHandlers HTMLOListElement
instance IsChildNode HTMLOListElement
instance IsAnimatable HTMLOListElement
instance IsGlobalEventHandlers HTMLOListElement
instance IsElementCSSInlineStyle HTMLOListElement
instance IsGObject HTMLOListElement where
  typeGType _ = gTypeHTMLOListElement
  {-# INLINE typeGType #-}

noHTMLOListElement :: Maybe HTMLOListElement
noHTMLOListElement = Nothing
{-# INLINE noHTMLOListElement #-}

gTypeHTMLOListElement :: JSM GType
gTypeHTMLOListElement = GType . Object <$> jsg "HTMLOListElement"

-- | Functions for this inteface are in "JSDOM.HTMLObjectElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLObjectElement Mozilla HTMLObjectElement documentation>
newtype HTMLObjectElement = HTMLObjectElement { unHTMLObjectElement :: JSVal }

instance PToJSVal HTMLObjectElement where
  pToJSVal = unHTMLObjectElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLObjectElement where
  pFromJSVal = HTMLObjectElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLObjectElement where
  toJSVal = return . unHTMLObjectElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLObjectElement where
  fromJSVal v = fmap HTMLObjectElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLObjectElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLObjectElement where
  makeObject = makeObject . unHTMLObjectElement

instance IsHTMLElement HTMLObjectElement
instance IsElement HTMLObjectElement
instance IsNode HTMLObjectElement
instance IsEventTarget HTMLObjectElement
instance IsSlotable HTMLObjectElement
instance IsParentNode HTMLObjectElement
instance IsNonDocumentTypeChildNode HTMLObjectElement
instance IsDocumentAndElementEventHandlers HTMLObjectElement
instance IsChildNode HTMLObjectElement
instance IsAnimatable HTMLObjectElement
instance IsGlobalEventHandlers HTMLObjectElement
instance IsElementCSSInlineStyle HTMLObjectElement
instance IsGObject HTMLObjectElement where
  typeGType _ = gTypeHTMLObjectElement
  {-# INLINE typeGType #-}

noHTMLObjectElement :: Maybe HTMLObjectElement
noHTMLObjectElement = Nothing
{-# INLINE noHTMLObjectElement #-}

gTypeHTMLObjectElement :: JSM GType
gTypeHTMLObjectElement = GType . Object <$> jsg "HTMLObjectElement"

-- | Functions for this inteface are in "JSDOM.HTMLOptGroupElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOptGroupElement Mozilla HTMLOptGroupElement documentation>
newtype HTMLOptGroupElement = HTMLOptGroupElement { unHTMLOptGroupElement :: JSVal }

instance PToJSVal HTMLOptGroupElement where
  pToJSVal = unHTMLOptGroupElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOptGroupElement where
  pFromJSVal = HTMLOptGroupElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOptGroupElement where
  toJSVal = return . unHTMLOptGroupElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOptGroupElement where
  fromJSVal v = fmap HTMLOptGroupElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOptGroupElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOptGroupElement where
  makeObject = makeObject . unHTMLOptGroupElement

instance IsHTMLElement HTMLOptGroupElement
instance IsElement HTMLOptGroupElement
instance IsNode HTMLOptGroupElement
instance IsEventTarget HTMLOptGroupElement
instance IsSlotable HTMLOptGroupElement
instance IsParentNode HTMLOptGroupElement
instance IsNonDocumentTypeChildNode HTMLOptGroupElement
instance IsDocumentAndElementEventHandlers HTMLOptGroupElement
instance IsChildNode HTMLOptGroupElement
instance IsAnimatable HTMLOptGroupElement
instance IsGlobalEventHandlers HTMLOptGroupElement
instance IsElementCSSInlineStyle HTMLOptGroupElement
instance IsGObject HTMLOptGroupElement where
  typeGType _ = gTypeHTMLOptGroupElement
  {-# INLINE typeGType #-}

noHTMLOptGroupElement :: Maybe HTMLOptGroupElement
noHTMLOptGroupElement = Nothing
{-# INLINE noHTMLOptGroupElement #-}

gTypeHTMLOptGroupElement :: JSM GType
gTypeHTMLOptGroupElement = GType . Object <$> jsg "HTMLOptGroupElement"

-- | Functions for this inteface are in "JSDOM.HTMLOptionElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOptionElement Mozilla HTMLOptionElement documentation>
newtype HTMLOptionElement = HTMLOptionElement { unHTMLOptionElement :: JSVal }

instance PToJSVal HTMLOptionElement where
  pToJSVal = unHTMLOptionElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOptionElement where
  pFromJSVal = HTMLOptionElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOptionElement where
  toJSVal = return . unHTMLOptionElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOptionElement where
  fromJSVal v = fmap HTMLOptionElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOptionElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOptionElement where
  makeObject = makeObject . unHTMLOptionElement

instance IsHTMLElement HTMLOptionElement
instance IsElement HTMLOptionElement
instance IsNode HTMLOptionElement
instance IsEventTarget HTMLOptionElement
instance IsSlotable HTMLOptionElement
instance IsParentNode HTMLOptionElement
instance IsNonDocumentTypeChildNode HTMLOptionElement
instance IsDocumentAndElementEventHandlers HTMLOptionElement
instance IsChildNode HTMLOptionElement
instance IsAnimatable HTMLOptionElement
instance IsGlobalEventHandlers HTMLOptionElement
instance IsElementCSSInlineStyle HTMLOptionElement
instance IsGObject HTMLOptionElement where
  typeGType _ = gTypeHTMLOptionElement
  {-# INLINE typeGType #-}

noHTMLOptionElement :: Maybe HTMLOptionElement
noHTMLOptionElement = Nothing
{-# INLINE noHTMLOptionElement #-}

gTypeHTMLOptionElement :: JSM GType
gTypeHTMLOptionElement = GType . Object <$> jsg "HTMLOptionElement"

-- | Functions for this inteface are in "JSDOM.HTMLOptionsCollection".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLCollection"
--
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOptionsCollection Mozilla HTMLOptionsCollection documentation>
newtype HTMLOptionsCollection = HTMLOptionsCollection { unHTMLOptionsCollection :: JSVal }

instance PToJSVal HTMLOptionsCollection where
  pToJSVal = unHTMLOptionsCollection
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOptionsCollection where
  pFromJSVal = HTMLOptionsCollection
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOptionsCollection where
  toJSVal = return . unHTMLOptionsCollection
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOptionsCollection where
  fromJSVal v = fmap HTMLOptionsCollection <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOptionsCollection
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOptionsCollection where
  makeObject = makeObject . unHTMLOptionsCollection

instance IsHTMLCollection HTMLOptionsCollection
instance IsGObject HTMLOptionsCollection where
  typeGType _ = gTypeHTMLOptionsCollection
  {-# INLINE typeGType #-}

noHTMLOptionsCollection :: Maybe HTMLOptionsCollection
noHTMLOptionsCollection = Nothing
{-# INLINE noHTMLOptionsCollection #-}

gTypeHTMLOptionsCollection :: JSM GType
gTypeHTMLOptionsCollection = GType . Object <$> jsg "HTMLOptionsCollection"

-- | Functions for this inteface are in "JSDOM.HTMLOutputElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLOutputElement Mozilla HTMLOutputElement documentation>
newtype HTMLOutputElement = HTMLOutputElement { unHTMLOutputElement :: JSVal }

instance PToJSVal HTMLOutputElement where
  pToJSVal = unHTMLOutputElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLOutputElement where
  pFromJSVal = HTMLOutputElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLOutputElement where
  toJSVal = return . unHTMLOutputElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLOutputElement where
  fromJSVal v = fmap HTMLOutputElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLOutputElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLOutputElement where
  makeObject = makeObject . unHTMLOutputElement

instance IsHTMLElement HTMLOutputElement
instance IsElement HTMLOutputElement
instance IsNode HTMLOutputElement
instance IsEventTarget HTMLOutputElement
instance IsSlotable HTMLOutputElement
instance IsParentNode HTMLOutputElement
instance IsNonDocumentTypeChildNode HTMLOutputElement
instance IsDocumentAndElementEventHandlers HTMLOutputElement
instance IsChildNode HTMLOutputElement
instance IsAnimatable HTMLOutputElement
instance IsGlobalEventHandlers HTMLOutputElement
instance IsElementCSSInlineStyle HTMLOutputElement
instance IsGObject HTMLOutputElement where
  typeGType _ = gTypeHTMLOutputElement
  {-# INLINE typeGType #-}

noHTMLOutputElement :: Maybe HTMLOutputElement
noHTMLOutputElement = Nothing
{-# INLINE noHTMLOutputElement #-}

gTypeHTMLOutputElement :: JSM GType
gTypeHTMLOutputElement = GType . Object <$> jsg "HTMLOutputElement"

-- | Functions for this inteface are in "JSDOM.HTMLParagraphElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLParagraphElement Mozilla HTMLParagraphElement documentation>
newtype HTMLParagraphElement = HTMLParagraphElement { unHTMLParagraphElement :: JSVal }

instance PToJSVal HTMLParagraphElement where
  pToJSVal = unHTMLParagraphElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLParagraphElement where
  pFromJSVal = HTMLParagraphElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLParagraphElement where
  toJSVal = return . unHTMLParagraphElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLParagraphElement where
  fromJSVal v = fmap HTMLParagraphElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLParagraphElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLParagraphElement where
  makeObject = makeObject . unHTMLParagraphElement

instance IsHTMLElement HTMLParagraphElement
instance IsElement HTMLParagraphElement
instance IsNode HTMLParagraphElement
instance IsEventTarget HTMLParagraphElement
instance IsSlotable HTMLParagraphElement
instance IsParentNode HTMLParagraphElement
instance IsNonDocumentTypeChildNode HTMLParagraphElement
instance IsDocumentAndElementEventHandlers HTMLParagraphElement
instance IsChildNode HTMLParagraphElement
instance IsAnimatable HTMLParagraphElement
instance IsGlobalEventHandlers HTMLParagraphElement
instance IsElementCSSInlineStyle HTMLParagraphElement
instance IsGObject HTMLParagraphElement where
  typeGType _ = gTypeHTMLParagraphElement
  {-# INLINE typeGType #-}

noHTMLParagraphElement :: Maybe HTMLParagraphElement
noHTMLParagraphElement = Nothing
{-# INLINE noHTMLParagraphElement #-}

gTypeHTMLParagraphElement :: JSM GType
gTypeHTMLParagraphElement = GType . Object <$> jsg "HTMLParagraphElement"

-- | Functions for this inteface are in "JSDOM.HTMLParamElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLParamElement Mozilla HTMLParamElement documentation>
newtype HTMLParamElement = HTMLParamElement { unHTMLParamElement :: JSVal }

instance PToJSVal HTMLParamElement where
  pToJSVal = unHTMLParamElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLParamElement where
  pFromJSVal = HTMLParamElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLParamElement where
  toJSVal = return . unHTMLParamElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLParamElement where
  fromJSVal v = fmap HTMLParamElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLParamElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLParamElement where
  makeObject = makeObject . unHTMLParamElement

instance IsHTMLElement HTMLParamElement
instance IsElement HTMLParamElement
instance IsNode HTMLParamElement
instance IsEventTarget HTMLParamElement
instance IsSlotable HTMLParamElement
instance IsParentNode HTMLParamElement
instance IsNonDocumentTypeChildNode HTMLParamElement
instance IsDocumentAndElementEventHandlers HTMLParamElement
instance IsChildNode HTMLParamElement
instance IsAnimatable HTMLParamElement
instance IsGlobalEventHandlers HTMLParamElement
instance IsElementCSSInlineStyle HTMLParamElement
instance IsGObject HTMLParamElement where
  typeGType _ = gTypeHTMLParamElement
  {-# INLINE typeGType #-}

noHTMLParamElement :: Maybe HTMLParamElement
noHTMLParamElement = Nothing
{-# INLINE noHTMLParamElement #-}

gTypeHTMLParamElement :: JSM GType
gTypeHTMLParamElement = GType . Object <$> jsg "HTMLParamElement"

-- | Functions for this inteface are in "JSDOM.HTMLPictureElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLPictureElement Mozilla HTMLPictureElement documentation>
newtype HTMLPictureElement = HTMLPictureElement { unHTMLPictureElement :: JSVal }

instance PToJSVal HTMLPictureElement where
  pToJSVal = unHTMLPictureElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLPictureElement where
  pFromJSVal = HTMLPictureElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLPictureElement where
  toJSVal = return . unHTMLPictureElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLPictureElement where
  fromJSVal v = fmap HTMLPictureElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLPictureElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLPictureElement where
  makeObject = makeObject . unHTMLPictureElement

instance IsHTMLElement HTMLPictureElement
instance IsElement HTMLPictureElement
instance IsNode HTMLPictureElement
instance IsEventTarget HTMLPictureElement
instance IsSlotable HTMLPictureElement
instance IsParentNode HTMLPictureElement
instance IsNonDocumentTypeChildNode HTMLPictureElement
instance IsDocumentAndElementEventHandlers HTMLPictureElement
instance IsChildNode HTMLPictureElement
instance IsAnimatable HTMLPictureElement
instance IsGlobalEventHandlers HTMLPictureElement
instance IsElementCSSInlineStyle HTMLPictureElement
instance IsGObject HTMLPictureElement where
  typeGType _ = gTypeHTMLPictureElement
  {-# INLINE typeGType #-}

noHTMLPictureElement :: Maybe HTMLPictureElement
noHTMLPictureElement = Nothing
{-# INLINE noHTMLPictureElement #-}

gTypeHTMLPictureElement :: JSM GType
gTypeHTMLPictureElement = GType . Object <$> jsg "HTMLPictureElement"

-- | Functions for this inteface are in "JSDOM.HTMLPreElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLPreElement Mozilla HTMLPreElement documentation>
newtype HTMLPreElement = HTMLPreElement { unHTMLPreElement :: JSVal }

instance PToJSVal HTMLPreElement where
  pToJSVal = unHTMLPreElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLPreElement where
  pFromJSVal = HTMLPreElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLPreElement where
  toJSVal = return . unHTMLPreElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLPreElement where
  fromJSVal v = fmap HTMLPreElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLPreElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLPreElement where
  makeObject = makeObject . unHTMLPreElement

instance IsHTMLElement HTMLPreElement
instance IsElement HTMLPreElement
instance IsNode HTMLPreElement
instance IsEventTarget HTMLPreElement
instance IsSlotable HTMLPreElement
instance IsParentNode HTMLPreElement
instance IsNonDocumentTypeChildNode HTMLPreElement
instance IsDocumentAndElementEventHandlers HTMLPreElement
instance IsChildNode HTMLPreElement
instance IsAnimatable HTMLPreElement
instance IsGlobalEventHandlers HTMLPreElement
instance IsElementCSSInlineStyle HTMLPreElement
instance IsGObject HTMLPreElement where
  typeGType _ = gTypeHTMLPreElement
  {-# INLINE typeGType #-}

noHTMLPreElement :: Maybe HTMLPreElement
noHTMLPreElement = Nothing
{-# INLINE noHTMLPreElement #-}

gTypeHTMLPreElement :: JSM GType
gTypeHTMLPreElement = GType . Object <$> jsg "HTMLPreElement"

-- | Functions for this inteface are in "JSDOM.HTMLProgressElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLProgressElement Mozilla HTMLProgressElement documentation>
newtype HTMLProgressElement = HTMLProgressElement { unHTMLProgressElement :: JSVal }

instance PToJSVal HTMLProgressElement where
  pToJSVal = unHTMLProgressElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLProgressElement where
  pFromJSVal = HTMLProgressElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLProgressElement where
  toJSVal = return . unHTMLProgressElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLProgressElement where
  fromJSVal v = fmap HTMLProgressElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLProgressElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLProgressElement where
  makeObject = makeObject . unHTMLProgressElement

instance IsHTMLElement HTMLProgressElement
instance IsElement HTMLProgressElement
instance IsNode HTMLProgressElement
instance IsEventTarget HTMLProgressElement
instance IsSlotable HTMLProgressElement
instance IsParentNode HTMLProgressElement
instance IsNonDocumentTypeChildNode HTMLProgressElement
instance IsDocumentAndElementEventHandlers HTMLProgressElement
instance IsChildNode HTMLProgressElement
instance IsAnimatable HTMLProgressElement
instance IsGlobalEventHandlers HTMLProgressElement
instance IsElementCSSInlineStyle HTMLProgressElement
instance IsGObject HTMLProgressElement where
  typeGType _ = gTypeHTMLProgressElement
  {-# INLINE typeGType #-}

noHTMLProgressElement :: Maybe HTMLProgressElement
noHTMLProgressElement = Nothing
{-# INLINE noHTMLProgressElement #-}

gTypeHTMLProgressElement :: JSM GType
gTypeHTMLProgressElement = GType . Object <$> jsg "HTMLProgressElement"

-- | Functions for this inteface are in "JSDOM.HTMLQuoteElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLQuoteElement Mozilla HTMLQuoteElement documentation>
newtype HTMLQuoteElement = HTMLQuoteElement { unHTMLQuoteElement :: JSVal }

instance PToJSVal HTMLQuoteElement where
  pToJSVal = unHTMLQuoteElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLQuoteElement where
  pFromJSVal = HTMLQuoteElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLQuoteElement where
  toJSVal = return . unHTMLQuoteElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLQuoteElement where
  fromJSVal v = fmap HTMLQuoteElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLQuoteElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLQuoteElement where
  makeObject = makeObject . unHTMLQuoteElement

instance IsHTMLElement HTMLQuoteElement
instance IsElement HTMLQuoteElement
instance IsNode HTMLQuoteElement
instance IsEventTarget HTMLQuoteElement
instance IsSlotable HTMLQuoteElement
instance IsParentNode HTMLQuoteElement
instance IsNonDocumentTypeChildNode HTMLQuoteElement
instance IsDocumentAndElementEventHandlers HTMLQuoteElement
instance IsChildNode HTMLQuoteElement
instance IsAnimatable HTMLQuoteElement
instance IsGlobalEventHandlers HTMLQuoteElement
instance IsElementCSSInlineStyle HTMLQuoteElement
instance IsGObject HTMLQuoteElement where
  typeGType _ = gTypeHTMLQuoteElement
  {-# INLINE typeGType #-}

noHTMLQuoteElement :: Maybe HTMLQuoteElement
noHTMLQuoteElement = Nothing
{-# INLINE noHTMLQuoteElement #-}

gTypeHTMLQuoteElement :: JSM GType
gTypeHTMLQuoteElement = GType . Object <$> jsg "HTMLQuoteElement"

-- | Functions for this inteface are in "JSDOM.HTMLScriptElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLScriptElement Mozilla HTMLScriptElement documentation>
newtype HTMLScriptElement = HTMLScriptElement { unHTMLScriptElement :: JSVal }

instance PToJSVal HTMLScriptElement where
  pToJSVal = unHTMLScriptElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLScriptElement where
  pFromJSVal = HTMLScriptElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLScriptElement where
  toJSVal = return . unHTMLScriptElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLScriptElement where
  fromJSVal v = fmap HTMLScriptElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLScriptElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLScriptElement where
  makeObject = makeObject . unHTMLScriptElement

instance IsHTMLElement HTMLScriptElement
instance IsElement HTMLScriptElement
instance IsNode HTMLScriptElement
instance IsEventTarget HTMLScriptElement
instance IsSlotable HTMLScriptElement
instance IsParentNode HTMLScriptElement
instance IsNonDocumentTypeChildNode HTMLScriptElement
instance IsDocumentAndElementEventHandlers HTMLScriptElement
instance IsChildNode HTMLScriptElement
instance IsAnimatable HTMLScriptElement
instance IsGlobalEventHandlers HTMLScriptElement
instance IsElementCSSInlineStyle HTMLScriptElement
instance IsGObject HTMLScriptElement where
  typeGType _ = gTypeHTMLScriptElement
  {-# INLINE typeGType #-}

noHTMLScriptElement :: Maybe HTMLScriptElement
noHTMLScriptElement = Nothing
{-# INLINE noHTMLScriptElement #-}

gTypeHTMLScriptElement :: JSM GType
gTypeHTMLScriptElement = GType . Object <$> jsg "HTMLScriptElement"

-- | Functions for this inteface are in "JSDOM.HTMLSelectElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement Mozilla HTMLSelectElement documentation>
newtype HTMLSelectElement = HTMLSelectElement { unHTMLSelectElement :: JSVal }

instance PToJSVal HTMLSelectElement where
  pToJSVal = unHTMLSelectElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLSelectElement where
  pFromJSVal = HTMLSelectElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLSelectElement where
  toJSVal = return . unHTMLSelectElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLSelectElement where
  fromJSVal v = fmap HTMLSelectElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLSelectElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLSelectElement where
  makeObject = makeObject . unHTMLSelectElement

instance IsHTMLElement HTMLSelectElement
instance IsElement HTMLSelectElement
instance IsNode HTMLSelectElement
instance IsEventTarget HTMLSelectElement
instance IsSlotable HTMLSelectElement
instance IsParentNode HTMLSelectElement
instance IsNonDocumentTypeChildNode HTMLSelectElement
instance IsDocumentAndElementEventHandlers HTMLSelectElement
instance IsChildNode HTMLSelectElement
instance IsAnimatable HTMLSelectElement
instance IsGlobalEventHandlers HTMLSelectElement
instance IsElementCSSInlineStyle HTMLSelectElement
instance IsGObject HTMLSelectElement where
  typeGType _ = gTypeHTMLSelectElement
  {-# INLINE typeGType #-}

noHTMLSelectElement :: Maybe HTMLSelectElement
noHTMLSelectElement = Nothing
{-# INLINE noHTMLSelectElement #-}

gTypeHTMLSelectElement :: JSM GType
gTypeHTMLSelectElement = GType . Object <$> jsg "HTMLSelectElement"

-- | Functions for this inteface are in "JSDOM.HTMLSlotElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSlotElement Mozilla HTMLSlotElement documentation>
newtype HTMLSlotElement = HTMLSlotElement { unHTMLSlotElement :: JSVal }

instance PToJSVal HTMLSlotElement where
  pToJSVal = unHTMLSlotElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLSlotElement where
  pFromJSVal = HTMLSlotElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLSlotElement where
  toJSVal = return . unHTMLSlotElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLSlotElement where
  fromJSVal v = fmap HTMLSlotElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLSlotElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLSlotElement where
  makeObject = makeObject . unHTMLSlotElement

instance IsHTMLElement HTMLSlotElement
instance IsElement HTMLSlotElement
instance IsNode HTMLSlotElement
instance IsEventTarget HTMLSlotElement
instance IsSlotable HTMLSlotElement
instance IsParentNode HTMLSlotElement
instance IsNonDocumentTypeChildNode HTMLSlotElement
instance IsDocumentAndElementEventHandlers HTMLSlotElement
instance IsChildNode HTMLSlotElement
instance IsAnimatable HTMLSlotElement
instance IsGlobalEventHandlers HTMLSlotElement
instance IsElementCSSInlineStyle HTMLSlotElement
instance IsGObject HTMLSlotElement where
  typeGType _ = gTypeHTMLSlotElement
  {-# INLINE typeGType #-}

noHTMLSlotElement :: Maybe HTMLSlotElement
noHTMLSlotElement = Nothing
{-# INLINE noHTMLSlotElement #-}

gTypeHTMLSlotElement :: JSM GType
gTypeHTMLSlotElement = GType . Object <$> jsg "HTMLSlotElement"

-- | Functions for this inteface are in "JSDOM.HTMLSourceElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSourceElement Mozilla HTMLSourceElement documentation>
newtype HTMLSourceElement = HTMLSourceElement { unHTMLSourceElement :: JSVal }

instance PToJSVal HTMLSourceElement where
  pToJSVal = unHTMLSourceElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLSourceElement where
  pFromJSVal = HTMLSourceElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLSourceElement where
  toJSVal = return . unHTMLSourceElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLSourceElement where
  fromJSVal v = fmap HTMLSourceElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLSourceElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLSourceElement where
  makeObject = makeObject . unHTMLSourceElement

instance IsHTMLElement HTMLSourceElement
instance IsElement HTMLSourceElement
instance IsNode HTMLSourceElement
instance IsEventTarget HTMLSourceElement
instance IsSlotable HTMLSourceElement
instance IsParentNode HTMLSourceElement
instance IsNonDocumentTypeChildNode HTMLSourceElement
instance IsDocumentAndElementEventHandlers HTMLSourceElement
instance IsChildNode HTMLSourceElement
instance IsAnimatable HTMLSourceElement
instance IsGlobalEventHandlers HTMLSourceElement
instance IsElementCSSInlineStyle HTMLSourceElement
instance IsGObject HTMLSourceElement where
  typeGType _ = gTypeHTMLSourceElement
  {-# INLINE typeGType #-}

noHTMLSourceElement :: Maybe HTMLSourceElement
noHTMLSourceElement = Nothing
{-# INLINE noHTMLSourceElement #-}

gTypeHTMLSourceElement :: JSM GType
gTypeHTMLSourceElement = GType . Object <$> jsg "HTMLSourceElement"

-- | Functions for this inteface are in "JSDOM.HTMLSpanElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLSpanElement Mozilla HTMLSpanElement documentation>
newtype HTMLSpanElement = HTMLSpanElement { unHTMLSpanElement :: JSVal }

instance PToJSVal HTMLSpanElement where
  pToJSVal = unHTMLSpanElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLSpanElement where
  pFromJSVal = HTMLSpanElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLSpanElement where
  toJSVal = return . unHTMLSpanElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLSpanElement where
  fromJSVal v = fmap HTMLSpanElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLSpanElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLSpanElement where
  makeObject = makeObject . unHTMLSpanElement

instance IsHTMLElement HTMLSpanElement
instance IsElement HTMLSpanElement
instance IsNode HTMLSpanElement
instance IsEventTarget HTMLSpanElement
instance IsSlotable HTMLSpanElement
instance IsParentNode HTMLSpanElement
instance IsNonDocumentTypeChildNode HTMLSpanElement
instance IsDocumentAndElementEventHandlers HTMLSpanElement
instance IsChildNode HTMLSpanElement
instance IsAnimatable HTMLSpanElement
instance IsGlobalEventHandlers HTMLSpanElement
instance IsElementCSSInlineStyle HTMLSpanElement
instance IsGObject HTMLSpanElement where
  typeGType _ = gTypeHTMLSpanElement
  {-# INLINE typeGType #-}

noHTMLSpanElement :: Maybe HTMLSpanElement
noHTMLSpanElement = Nothing
{-# INLINE noHTMLSpanElement #-}

gTypeHTMLSpanElement :: JSM GType
gTypeHTMLSpanElement = GType . Object <$> jsg "HTMLSpanElement"

-- | Functions for this inteface are in "JSDOM.HTMLStyleElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLStyleElement Mozilla HTMLStyleElement documentation>
newtype HTMLStyleElement = HTMLStyleElement { unHTMLStyleElement :: JSVal }

instance PToJSVal HTMLStyleElement where
  pToJSVal = unHTMLStyleElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLStyleElement where
  pFromJSVal = HTMLStyleElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLStyleElement where
  toJSVal = return . unHTMLStyleElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLStyleElement where
  fromJSVal v = fmap HTMLStyleElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLStyleElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLStyleElement where
  makeObject = makeObject . unHTMLStyleElement

instance IsHTMLElement HTMLStyleElement
instance IsElement HTMLStyleElement
instance IsNode HTMLStyleElement
instance IsEventTarget HTMLStyleElement
instance IsSlotable HTMLStyleElement
instance IsParentNode HTMLStyleElement
instance IsNonDocumentTypeChildNode HTMLStyleElement
instance IsDocumentAndElementEventHandlers HTMLStyleElement
instance IsChildNode HTMLStyleElement
instance IsAnimatable HTMLStyleElement
instance IsGlobalEventHandlers HTMLStyleElement
instance IsElementCSSInlineStyle HTMLStyleElement
instance IsGObject HTMLStyleElement where
  typeGType _ = gTypeHTMLStyleElement
  {-# INLINE typeGType #-}

noHTMLStyleElement :: Maybe HTMLStyleElement
noHTMLStyleElement = Nothing
{-# INLINE noHTMLStyleElement #-}

gTypeHTMLStyleElement :: JSM GType
gTypeHTMLStyleElement = GType . Object <$> jsg "HTMLStyleElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableCaptionElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCaptionElement Mozilla HTMLTableCaptionElement documentation>
newtype HTMLTableCaptionElement = HTMLTableCaptionElement { unHTMLTableCaptionElement :: JSVal }

instance PToJSVal HTMLTableCaptionElement where
  pToJSVal = unHTMLTableCaptionElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableCaptionElement where
  pFromJSVal = HTMLTableCaptionElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableCaptionElement where
  toJSVal = return . unHTMLTableCaptionElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableCaptionElement where
  fromJSVal v = fmap HTMLTableCaptionElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableCaptionElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableCaptionElement where
  makeObject = makeObject . unHTMLTableCaptionElement

instance IsHTMLElement HTMLTableCaptionElement
instance IsElement HTMLTableCaptionElement
instance IsNode HTMLTableCaptionElement
instance IsEventTarget HTMLTableCaptionElement
instance IsSlotable HTMLTableCaptionElement
instance IsParentNode HTMLTableCaptionElement
instance IsNonDocumentTypeChildNode HTMLTableCaptionElement
instance IsDocumentAndElementEventHandlers HTMLTableCaptionElement
instance IsChildNode HTMLTableCaptionElement
instance IsAnimatable HTMLTableCaptionElement
instance IsGlobalEventHandlers HTMLTableCaptionElement
instance IsElementCSSInlineStyle HTMLTableCaptionElement
instance IsGObject HTMLTableCaptionElement where
  typeGType _ = gTypeHTMLTableCaptionElement
  {-# INLINE typeGType #-}

noHTMLTableCaptionElement :: Maybe HTMLTableCaptionElement
noHTMLTableCaptionElement = Nothing
{-# INLINE noHTMLTableCaptionElement #-}

gTypeHTMLTableCaptionElement :: JSM GType
gTypeHTMLTableCaptionElement = GType . Object <$> jsg "HTMLTableCaptionElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableCellElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableCellElement Mozilla HTMLTableCellElement documentation>
newtype HTMLTableCellElement = HTMLTableCellElement { unHTMLTableCellElement :: JSVal }

instance PToJSVal HTMLTableCellElement where
  pToJSVal = unHTMLTableCellElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableCellElement where
  pFromJSVal = HTMLTableCellElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableCellElement where
  toJSVal = return . unHTMLTableCellElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableCellElement where
  fromJSVal v = fmap HTMLTableCellElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableCellElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableCellElement where
  makeObject = makeObject . unHTMLTableCellElement

instance IsHTMLElement HTMLTableCellElement
instance IsElement HTMLTableCellElement
instance IsNode HTMLTableCellElement
instance IsEventTarget HTMLTableCellElement
instance IsSlotable HTMLTableCellElement
instance IsParentNode HTMLTableCellElement
instance IsNonDocumentTypeChildNode HTMLTableCellElement
instance IsDocumentAndElementEventHandlers HTMLTableCellElement
instance IsChildNode HTMLTableCellElement
instance IsAnimatable HTMLTableCellElement
instance IsGlobalEventHandlers HTMLTableCellElement
instance IsElementCSSInlineStyle HTMLTableCellElement
instance IsGObject HTMLTableCellElement where
  typeGType _ = gTypeHTMLTableCellElement
  {-# INLINE typeGType #-}

noHTMLTableCellElement :: Maybe HTMLTableCellElement
noHTMLTableCellElement = Nothing
{-# INLINE noHTMLTableCellElement #-}

gTypeHTMLTableCellElement :: JSM GType
gTypeHTMLTableCellElement = GType . Object <$> jsg "HTMLTableCellElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableColElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableColElement Mozilla HTMLTableColElement documentation>
newtype HTMLTableColElement = HTMLTableColElement { unHTMLTableColElement :: JSVal }

instance PToJSVal HTMLTableColElement where
  pToJSVal = unHTMLTableColElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableColElement where
  pFromJSVal = HTMLTableColElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableColElement where
  toJSVal = return . unHTMLTableColElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableColElement where
  fromJSVal v = fmap HTMLTableColElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableColElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableColElement where
  makeObject = makeObject . unHTMLTableColElement

instance IsHTMLElement HTMLTableColElement
instance IsElement HTMLTableColElement
instance IsNode HTMLTableColElement
instance IsEventTarget HTMLTableColElement
instance IsSlotable HTMLTableColElement
instance IsParentNode HTMLTableColElement
instance IsNonDocumentTypeChildNode HTMLTableColElement
instance IsDocumentAndElementEventHandlers HTMLTableColElement
instance IsChildNode HTMLTableColElement
instance IsAnimatable HTMLTableColElement
instance IsGlobalEventHandlers HTMLTableColElement
instance IsElementCSSInlineStyle HTMLTableColElement
instance IsGObject HTMLTableColElement where
  typeGType _ = gTypeHTMLTableColElement
  {-# INLINE typeGType #-}

noHTMLTableColElement :: Maybe HTMLTableColElement
noHTMLTableColElement = Nothing
{-# INLINE noHTMLTableColElement #-}

gTypeHTMLTableColElement :: JSM GType
gTypeHTMLTableColElement = GType . Object <$> jsg "HTMLTableColElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableElement Mozilla HTMLTableElement documentation>
newtype HTMLTableElement = HTMLTableElement { unHTMLTableElement :: JSVal }

instance PToJSVal HTMLTableElement where
  pToJSVal = unHTMLTableElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableElement where
  pFromJSVal = HTMLTableElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableElement where
  toJSVal = return . unHTMLTableElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableElement where
  fromJSVal v = fmap HTMLTableElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableElement where
  makeObject = makeObject . unHTMLTableElement

instance IsHTMLElement HTMLTableElement
instance IsElement HTMLTableElement
instance IsNode HTMLTableElement
instance IsEventTarget HTMLTableElement
instance IsSlotable HTMLTableElement
instance IsParentNode HTMLTableElement
instance IsNonDocumentTypeChildNode HTMLTableElement
instance IsDocumentAndElementEventHandlers HTMLTableElement
instance IsChildNode HTMLTableElement
instance IsAnimatable HTMLTableElement
instance IsGlobalEventHandlers HTMLTableElement
instance IsElementCSSInlineStyle HTMLTableElement
instance IsGObject HTMLTableElement where
  typeGType _ = gTypeHTMLTableElement
  {-# INLINE typeGType #-}

noHTMLTableElement :: Maybe HTMLTableElement
noHTMLTableElement = Nothing
{-# INLINE noHTMLTableElement #-}

gTypeHTMLTableElement :: JSM GType
gTypeHTMLTableElement = GType . Object <$> jsg "HTMLTableElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableRowElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableRowElement Mozilla HTMLTableRowElement documentation>
newtype HTMLTableRowElement = HTMLTableRowElement { unHTMLTableRowElement :: JSVal }

instance PToJSVal HTMLTableRowElement where
  pToJSVal = unHTMLTableRowElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableRowElement where
  pFromJSVal = HTMLTableRowElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableRowElement where
  toJSVal = return . unHTMLTableRowElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableRowElement where
  fromJSVal v = fmap HTMLTableRowElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableRowElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableRowElement where
  makeObject = makeObject . unHTMLTableRowElement

instance IsHTMLElement HTMLTableRowElement
instance IsElement HTMLTableRowElement
instance IsNode HTMLTableRowElement
instance IsEventTarget HTMLTableRowElement
instance IsSlotable HTMLTableRowElement
instance IsParentNode HTMLTableRowElement
instance IsNonDocumentTypeChildNode HTMLTableRowElement
instance IsDocumentAndElementEventHandlers HTMLTableRowElement
instance IsChildNode HTMLTableRowElement
instance IsAnimatable HTMLTableRowElement
instance IsGlobalEventHandlers HTMLTableRowElement
instance IsElementCSSInlineStyle HTMLTableRowElement
instance IsGObject HTMLTableRowElement where
  typeGType _ = gTypeHTMLTableRowElement
  {-# INLINE typeGType #-}

noHTMLTableRowElement :: Maybe HTMLTableRowElement
noHTMLTableRowElement = Nothing
{-# INLINE noHTMLTableRowElement #-}

gTypeHTMLTableRowElement :: JSM GType
gTypeHTMLTableRowElement = GType . Object <$> jsg "HTMLTableRowElement"

-- | Functions for this inteface are in "JSDOM.HTMLTableSectionElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTableSectionElement Mozilla HTMLTableSectionElement documentation>
newtype HTMLTableSectionElement = HTMLTableSectionElement { unHTMLTableSectionElement :: JSVal }

instance PToJSVal HTMLTableSectionElement where
  pToJSVal = unHTMLTableSectionElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTableSectionElement where
  pFromJSVal = HTMLTableSectionElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTableSectionElement where
  toJSVal = return . unHTMLTableSectionElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTableSectionElement where
  fromJSVal v = fmap HTMLTableSectionElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTableSectionElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTableSectionElement where
  makeObject = makeObject . unHTMLTableSectionElement

instance IsHTMLElement HTMLTableSectionElement
instance IsElement HTMLTableSectionElement
instance IsNode HTMLTableSectionElement
instance IsEventTarget HTMLTableSectionElement
instance IsSlotable HTMLTableSectionElement
instance IsParentNode HTMLTableSectionElement
instance IsNonDocumentTypeChildNode HTMLTableSectionElement
instance IsDocumentAndElementEventHandlers HTMLTableSectionElement
instance IsChildNode HTMLTableSectionElement
instance IsAnimatable HTMLTableSectionElement
instance IsGlobalEventHandlers HTMLTableSectionElement
instance IsElementCSSInlineStyle HTMLTableSectionElement
instance IsGObject HTMLTableSectionElement where
  typeGType _ = gTypeHTMLTableSectionElement
  {-# INLINE typeGType #-}

noHTMLTableSectionElement :: Maybe HTMLTableSectionElement
noHTMLTableSectionElement = Nothing
{-# INLINE noHTMLTableSectionElement #-}

gTypeHTMLTableSectionElement :: JSM GType
gTypeHTMLTableSectionElement = GType . Object <$> jsg "HTMLTableSectionElement"

-- | Functions for this inteface are in "JSDOM.HTMLTemplateElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTemplateElement Mozilla HTMLTemplateElement documentation>
newtype HTMLTemplateElement = HTMLTemplateElement { unHTMLTemplateElement :: JSVal }

instance PToJSVal HTMLTemplateElement where
  pToJSVal = unHTMLTemplateElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTemplateElement where
  pFromJSVal = HTMLTemplateElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTemplateElement where
  toJSVal = return . unHTMLTemplateElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTemplateElement where
  fromJSVal v = fmap HTMLTemplateElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTemplateElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTemplateElement where
  makeObject = makeObject . unHTMLTemplateElement

instance IsHTMLElement HTMLTemplateElement
instance IsElement HTMLTemplateElement
instance IsNode HTMLTemplateElement
instance IsEventTarget HTMLTemplateElement
instance IsSlotable HTMLTemplateElement
instance IsParentNode HTMLTemplateElement
instance IsNonDocumentTypeChildNode HTMLTemplateElement
instance IsDocumentAndElementEventHandlers HTMLTemplateElement
instance IsChildNode HTMLTemplateElement
instance IsAnimatable HTMLTemplateElement
instance IsGlobalEventHandlers HTMLTemplateElement
instance IsElementCSSInlineStyle HTMLTemplateElement
instance IsGObject HTMLTemplateElement where
  typeGType _ = gTypeHTMLTemplateElement
  {-# INLINE typeGType #-}

noHTMLTemplateElement :: Maybe HTMLTemplateElement
noHTMLTemplateElement = Nothing
{-# INLINE noHTMLTemplateElement #-}

gTypeHTMLTemplateElement :: JSM GType
gTypeHTMLTemplateElement = GType . Object <$> jsg "HTMLTemplateElement"

-- | Functions for this inteface are in "JSDOM.HTMLTextAreaElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement Mozilla HTMLTextAreaElement documentation>
newtype HTMLTextAreaElement = HTMLTextAreaElement { unHTMLTextAreaElement :: JSVal }

instance PToJSVal HTMLTextAreaElement where
  pToJSVal = unHTMLTextAreaElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTextAreaElement where
  pFromJSVal = HTMLTextAreaElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTextAreaElement where
  toJSVal = return . unHTMLTextAreaElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTextAreaElement where
  fromJSVal v = fmap HTMLTextAreaElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTextAreaElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTextAreaElement where
  makeObject = makeObject . unHTMLTextAreaElement

instance IsHTMLElement HTMLTextAreaElement
instance IsElement HTMLTextAreaElement
instance IsNode HTMLTextAreaElement
instance IsEventTarget HTMLTextAreaElement
instance IsSlotable HTMLTextAreaElement
instance IsParentNode HTMLTextAreaElement
instance IsNonDocumentTypeChildNode HTMLTextAreaElement
instance IsDocumentAndElementEventHandlers HTMLTextAreaElement
instance IsChildNode HTMLTextAreaElement
instance IsAnimatable HTMLTextAreaElement
instance IsGlobalEventHandlers HTMLTextAreaElement
instance IsElementCSSInlineStyle HTMLTextAreaElement
instance IsGObject HTMLTextAreaElement where
  typeGType _ = gTypeHTMLTextAreaElement
  {-# INLINE typeGType #-}

noHTMLTextAreaElement :: Maybe HTMLTextAreaElement
noHTMLTextAreaElement = Nothing
{-# INLINE noHTMLTextAreaElement #-}

gTypeHTMLTextAreaElement :: JSM GType
gTypeHTMLTextAreaElement = GType . Object <$> jsg "HTMLTextAreaElement"

-- | Functions for this inteface are in "JSDOM.HTMLTimeElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTimeElement Mozilla HTMLTimeElement documentation>
newtype HTMLTimeElement = HTMLTimeElement { unHTMLTimeElement :: JSVal }

instance PToJSVal HTMLTimeElement where
  pToJSVal = unHTMLTimeElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTimeElement where
  pFromJSVal = HTMLTimeElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTimeElement where
  toJSVal = return . unHTMLTimeElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTimeElement where
  fromJSVal v = fmap HTMLTimeElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTimeElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTimeElement where
  makeObject = makeObject . unHTMLTimeElement

instance IsHTMLElement HTMLTimeElement
instance IsElement HTMLTimeElement
instance IsNode HTMLTimeElement
instance IsEventTarget HTMLTimeElement
instance IsSlotable HTMLTimeElement
instance IsParentNode HTMLTimeElement
instance IsNonDocumentTypeChildNode HTMLTimeElement
instance IsDocumentAndElementEventHandlers HTMLTimeElement
instance IsChildNode HTMLTimeElement
instance IsAnimatable HTMLTimeElement
instance IsGlobalEventHandlers HTMLTimeElement
instance IsElementCSSInlineStyle HTMLTimeElement
instance IsGObject HTMLTimeElement where
  typeGType _ = gTypeHTMLTimeElement
  {-# INLINE typeGType #-}

noHTMLTimeElement :: Maybe HTMLTimeElement
noHTMLTimeElement = Nothing
{-# INLINE noHTMLTimeElement #-}

gTypeHTMLTimeElement :: JSM GType
gTypeHTMLTimeElement = GType . Object <$> jsg "HTMLTimeElement"

-- | Functions for this inteface are in "JSDOM.HTMLTitleElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTitleElement Mozilla HTMLTitleElement documentation>
newtype HTMLTitleElement = HTMLTitleElement { unHTMLTitleElement :: JSVal }

instance PToJSVal HTMLTitleElement where
  pToJSVal = unHTMLTitleElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTitleElement where
  pFromJSVal = HTMLTitleElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTitleElement where
  toJSVal = return . unHTMLTitleElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTitleElement where
  fromJSVal v = fmap HTMLTitleElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTitleElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTitleElement where
  makeObject = makeObject . unHTMLTitleElement

instance IsHTMLElement HTMLTitleElement
instance IsElement HTMLTitleElement
instance IsNode HTMLTitleElement
instance IsEventTarget HTMLTitleElement
instance IsSlotable HTMLTitleElement
instance IsParentNode HTMLTitleElement
instance IsNonDocumentTypeChildNode HTMLTitleElement
instance IsDocumentAndElementEventHandlers HTMLTitleElement
instance IsChildNode HTMLTitleElement
instance IsAnimatable HTMLTitleElement
instance IsGlobalEventHandlers HTMLTitleElement
instance IsElementCSSInlineStyle HTMLTitleElement
instance IsGObject HTMLTitleElement where
  typeGType _ = gTypeHTMLTitleElement
  {-# INLINE typeGType #-}

noHTMLTitleElement :: Maybe HTMLTitleElement
noHTMLTitleElement = Nothing
{-# INLINE noHTMLTitleElement #-}

gTypeHTMLTitleElement :: JSM GType
gTypeHTMLTitleElement = GType . Object <$> jsg "HTMLTitleElement"

-- | Functions for this inteface are in "JSDOM.HTMLTrackElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLTrackElement Mozilla HTMLTrackElement documentation>
newtype HTMLTrackElement = HTMLTrackElement { unHTMLTrackElement :: JSVal }

instance PToJSVal HTMLTrackElement where
  pToJSVal = unHTMLTrackElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLTrackElement where
  pFromJSVal = HTMLTrackElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLTrackElement where
  toJSVal = return . unHTMLTrackElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLTrackElement where
  fromJSVal v = fmap HTMLTrackElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLTrackElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLTrackElement where
  makeObject = makeObject . unHTMLTrackElement

instance IsHTMLElement HTMLTrackElement
instance IsElement HTMLTrackElement
instance IsNode HTMLTrackElement
instance IsEventTarget HTMLTrackElement
instance IsSlotable HTMLTrackElement
instance IsParentNode HTMLTrackElement
instance IsNonDocumentTypeChildNode HTMLTrackElement
instance IsDocumentAndElementEventHandlers HTMLTrackElement
instance IsChildNode HTMLTrackElement
instance IsAnimatable HTMLTrackElement
instance IsGlobalEventHandlers HTMLTrackElement
instance IsElementCSSInlineStyle HTMLTrackElement
instance IsGObject HTMLTrackElement where
  typeGType _ = gTypeHTMLTrackElement
  {-# INLINE typeGType #-}

noHTMLTrackElement :: Maybe HTMLTrackElement
noHTMLTrackElement = Nothing
{-# INLINE noHTMLTrackElement #-}

gTypeHTMLTrackElement :: JSM GType
gTypeHTMLTrackElement = GType . Object <$> jsg "HTMLTrackElement"

-- | Functions for this inteface are in "JSDOM.HTMLUListElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLUListElement Mozilla HTMLUListElement documentation>
newtype HTMLUListElement = HTMLUListElement { unHTMLUListElement :: JSVal }

instance PToJSVal HTMLUListElement where
  pToJSVal = unHTMLUListElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLUListElement where
  pFromJSVal = HTMLUListElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLUListElement where
  toJSVal = return . unHTMLUListElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLUListElement where
  fromJSVal v = fmap HTMLUListElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLUListElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLUListElement where
  makeObject = makeObject . unHTMLUListElement

instance IsHTMLElement HTMLUListElement
instance IsElement HTMLUListElement
instance IsNode HTMLUListElement
instance IsEventTarget HTMLUListElement
instance IsSlotable HTMLUListElement
instance IsParentNode HTMLUListElement
instance IsNonDocumentTypeChildNode HTMLUListElement
instance IsDocumentAndElementEventHandlers HTMLUListElement
instance IsChildNode HTMLUListElement
instance IsAnimatable HTMLUListElement
instance IsGlobalEventHandlers HTMLUListElement
instance IsElementCSSInlineStyle HTMLUListElement
instance IsGObject HTMLUListElement where
  typeGType _ = gTypeHTMLUListElement
  {-# INLINE typeGType #-}

noHTMLUListElement :: Maybe HTMLUListElement
noHTMLUListElement = Nothing
{-# INLINE noHTMLUListElement #-}

gTypeHTMLUListElement :: JSM GType
gTypeHTMLUListElement = GType . Object <$> jsg "HTMLUListElement"

-- | Functions for this inteface are in "JSDOM.HTMLUnknownElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLUnknownElement Mozilla HTMLUnknownElement documentation>
newtype HTMLUnknownElement = HTMLUnknownElement { unHTMLUnknownElement :: JSVal }

instance PToJSVal HTMLUnknownElement where
  pToJSVal = unHTMLUnknownElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLUnknownElement where
  pFromJSVal = HTMLUnknownElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLUnknownElement where
  toJSVal = return . unHTMLUnknownElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLUnknownElement where
  fromJSVal v = fmap HTMLUnknownElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLUnknownElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLUnknownElement where
  makeObject = makeObject . unHTMLUnknownElement

instance IsHTMLElement HTMLUnknownElement
instance IsElement HTMLUnknownElement
instance IsNode HTMLUnknownElement
instance IsEventTarget HTMLUnknownElement
instance IsSlotable HTMLUnknownElement
instance IsParentNode HTMLUnknownElement
instance IsNonDocumentTypeChildNode HTMLUnknownElement
instance IsDocumentAndElementEventHandlers HTMLUnknownElement
instance IsChildNode HTMLUnknownElement
instance IsAnimatable HTMLUnknownElement
instance IsGlobalEventHandlers HTMLUnknownElement
instance IsElementCSSInlineStyle HTMLUnknownElement
instance IsGObject HTMLUnknownElement where
  typeGType _ = gTypeHTMLUnknownElement
  {-# INLINE typeGType #-}

noHTMLUnknownElement :: Maybe HTMLUnknownElement
noHTMLUnknownElement = Nothing
{-# INLINE noHTMLUnknownElement #-}

gTypeHTMLUnknownElement :: JSM GType
gTypeHTMLUnknownElement = GType . Object <$> jsg "HTMLUnknownElement"

-- | Functions for this inteface are in "JSDOM.HTMLVideoElement".
-- Base interface functions are in:
--
--     * "JSDOM.HTMLMediaElement"
--     * "JSDOM.HTMLElement"
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
-- <https://developer.mozilla.org/en-US/docs/Web/API/HTMLVideoElement Mozilla HTMLVideoElement documentation>
newtype HTMLVideoElement = HTMLVideoElement { unHTMLVideoElement :: JSVal }

instance PToJSVal HTMLVideoElement where
  pToJSVal = unHTMLVideoElement
  {-# INLINE pToJSVal #-}

instance PFromJSVal HTMLVideoElement where
  pFromJSVal = HTMLVideoElement
  {-# INLINE pFromJSVal #-}

instance ToJSVal HTMLVideoElement where
  toJSVal = return . unHTMLVideoElement
  {-# INLINE toJSVal #-}

instance FromJSVal HTMLVideoElement where
  fromJSVal v = fmap HTMLVideoElement <$> maybeNullOrUndefined v
  {-# INLINE fromJSVal #-}
  fromJSValUnchecked = return . HTMLVideoElement
  {-# INLINE fromJSValUnchecked #-}

instance MakeObject HTMLVideoElement where
  makeObject = makeObject . unHTMLVideoElement

instance IsHTMLMediaElement HTMLVideoElement
instance IsHTMLElement HTMLVideoElement
instance IsElement HTMLVideoElement
instance IsNode HTMLVideoElement
instance IsEventTarget HTMLVideoElement
instance IsSlotable HTMLVideoElement
instance IsParentNode HTMLVideoElement
instance IsNonDocumentTypeChildNode HTMLVideoElement
instance IsDocumentAndElementEventHandlers HTMLVideoElement
instance IsChildNode HTMLVideoElement
instance IsAnimatable HTMLVideoElement
instance IsGlobalEventHandlers HTMLVideoElement
instance IsElementCSSInlineStyle HTMLVideoElement
instance IsGObject HTMLVideoElement where
  typeGType _ = gTypeHTMLVideoElement
  {-# INLINE typeGType #-}

noHTMLVideoElement :: Maybe HTMLVideoElement
noHTMLVideoElement = Nothing
{-# INLINE noHTMLVideoElement #-}

gTypeHTMLVideoElement :: JSM GType
gTypeHTMLVideoElement = GType . Object <$> jsg "HTMLVideoElement"
