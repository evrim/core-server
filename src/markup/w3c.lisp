;; Core Server: Web Application Server

;; Copyright (C) 2006-2008  Metin Evrim Ulu, Aycan iRiCAN

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :core-server)

;;+----------------------------------------------------------------------------
;;| W3C DOM Library
;;+----------------------------------------------------------------------------
;;
;; This file contains implementation of W3C DOM Standard.
;; http://www.w3.org/DOM/
;;
;; http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/ecma-script-binding.html
;;

;; Properties of the DOMException Constructor function:
;;     DOMException.INDEX_SIZE_ERR
;;         The value of the constant DOMException.INDEX_SIZE_ERR is 1.
;;     DOMException.DOMSTRING_SIZE_ERR
;;         The value of the constant DOMException.DOMSTRING_SIZE_ERR is 2.
;;     DOMException.HIERARCHY_REQUEST_ERR
;;         The value of the constant DOMException.HIERARCHY_REQUEST_ERR is 3.
;;     DOMException.WRONG_DOCUMENT_ERR
;;         The value of the constant DOMException.WRONG_DOCUMENT_ERR is 4.
;;     DOMException.INVALID_CHARACTER_ERR
;;         The value of the constant DOMException.INVALID_CHARACTER_ERR is 5.
;;     DOMException.NO_DATA_ALLOWED_ERR
;;         The value of the constant DOMException.NO_DATA_ALLOWED_ERR is 6.
;;     DOMException.NO_MODIFICATION_ALLOWED_ERR
;;         The value of the constant DOMException.NO_MODIFICATION_ALLOWED_ERR is 7.
;;     DOMException.NOT_FOUND_ERR
;;         The value of the constant DOMException.NOT_FOUND_ERR is 8.
;;     DOMException.NOT_SUPPORTED_ERR
;;         The value of the constant DOMException.NOT_SUPPORTED_ERR is 9.
;;     DOMException.INUSE_ATTRIBUTE_ERR
;;         The value of the constant DOMException.INUSE_ATTRIBUTE_ERR is 10.
;;     DOMException.INVALID_STATE_ERR
;;         The value of the constant DOMException.INVALID_STATE_ERR is 11.
;;     DOMException.SYNTAX_ERR
;;         The value of the constant DOMException.SYNTAX_ERR is 12.
;;     DOMException.INVALID_MODIFICATION_ERR
;;         The value of the constant DOMException.INVALID_MODIFICATION_ERR is 13.
;;     DOMException.NAMESPACE_ERR
;;         The value of the constant DOMException.NAMESPACE_ERR is 14.
;;     DOMException.INVALID_ACCESS_ERR
;;         The value of the constant DOMException.INVALID_ACCESS_ERR is 15.
;;     DOMException.VALIDATION_ERR
;;         The value of the constant DOMException.VALIDATION_ERR is 16.
;;     DOMException.TYPE_MISMATCH_ERR
;;         The value of the constant DOMException.TYPE_MISMATCH_ERR is 17. 
;;
(defvar *dom-exceptions*
  '((1 . INDEX_SIZE_ERR)
    (2 . DOMSTRING_SIZE_ERR)
    (3 . HIERARCHY_REQUEST_ERR)
    (4 . WRONG_DOCUmENT_ERR)
    (5 . INVALID_CHARACTER_ERR)
    (6 . NO_DATA_ALLOWED_ERR)
    (7 . NO_mODIFICATION_ALLOWED_ERR)
    (8 . NOT_FOUND_ERR)
    (9 . NOT_SUPPORTED_ERR)
    (10 . INUSE_ATTRIBUTE_ERR)
    (11 . INVALID_STATE_ERR)
    (12 . SYNTAX_ERR)
    (13 . INVALID_mODIFICATION_ERR)
    (14 . NAmESPACE_ERR)
    (15 . INVALID_ACCESS_ERR)
    (16 . VALIDATION_ERR)
    (17 . TYPE_mISmATCH_ERR)))

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMException interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMException interface:
;;         code
;;             This property is a Number.
(defclass *d-o-m-exception ()
  ((code :type integer :accessor code :initarg :code :initform 0)))

;;-----------------------------------------------------------------------------
;; Objects that implement the NodeList interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the NodeList interface:
;;         length
;;             This read-only property is a Number.
;;     Functions of objects that implement the NodeList interface:
;;         item(index)
;;             This function returns an object that implements the Node interface.
;;             The index parameter is a Number.
;;             Note: This object can also be dereferenced using square
;;             bracket notation (e.g. obj[1]). Dereferencing with an
;;             integer index is equivalent to invoking the item
;;             function with that index.
(defun item (list index)
  (nth index list))

;;-----------------------------------------------------------------------------
;; Objects that implement the NamedNodeMap interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the NamedNodeMap interface:
;;         length
;;             This read-only property is a Number.
;;
;;     Functions of objects that implement the NamedNodeMap interface:
;;         getNamedItem(name)
;;             This function returns an object that implements the Node interface.
;;             The name parameter is a String. 
;;         setNamedItem(arg)
;;             This function returns an object that implements the Node interface.
;;             The arg parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
;;         removeNamedItem(name)
;;             This function returns an object that implements the Node interface.
;;             The name parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
;;         item(index)
;;             This function returns an object that implements the Node interface.
;;             The index parameter is a Number.
;;             Note: This object can also be dereferenced using square
;;             bracket notation (e.g. obj[1]). Dereferencing with an
;;             integer index is equivalent to invoking the item
;;             function with that index.
;;         getNamedItemNS(namespaceURI, localName)
;;             This function returns an object that implements the Node interface.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
;;         setNamedItemNS(arg)
;;             This function returns an object that implements the Node interface.
;;             The arg parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
;;         removeNamedItemNS(namespaceURI, localName)
;;             This function returns an object that implements the Node interface.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the DOMException interface. 

;;-----------------------------------------------------------------------------
;; Objects that implement the CharacterData interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the CharacterData interface have all
;;     properties and functions of the Node interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the CharacterData interface:
;;         data
;;             This property is a String, can raise an object that
;;             implements the DOMException interface on setting and
;;             can raise an object that implements the DOMException
;;             interface on retrieval.
;;         length
;;             This read-only property is a Number. 
;;
;;     Functions of objects that implement the CharacterData interface:
;;         substringData(offset, count)
;;             This function returns a String.
;;             The offset parameter is a Number.
;;             The count parameter is a Number.
;;             This function can raise an object that implements the DOMException interface.
;;         appendData(arg)
;;             This function has no return value.
;;             The arg parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
;;         insertData(offset, arg)
;;             This function has no return value.
;;             The offset parameter is a Number.
;;             The arg parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
;;         deleteData(offset, count)
;;             This function has no return value.
;;             The offset parameter is a Number.
;;             The count parameter is a Number.
;;             This function can raise an object that implements the DOMException interface.
;;         replaceData(offset, count, arg)
;;             This function has no return value.
;;             The offset parameter is a Number.
;;             The count parameter is a Number.
;;             The arg parameter is a String.
;;             This function can raise an object that implements the DOMException interface. 


;; Properties of the Node Constructor function:
;;     Node.ELEMENT_NODE
;;         The value of the constant Node.ELEMENT_NODE is 1.
;;     Node.ATTRIBUTE_NODE
;;         The value of the constant Node.ATTRIBUTE_NODE is 2.
;;     Node.TEXT_NODE
;;         The value of the constant Node.TEXT_NODE is 3.
;;     Node.CDATA_SECTION_NODE
;;         The value of the constant Node.CDATA_SECTION_NODE is 4.
;;     Node.ENTITY_REFERENCE_NODE
;;         The value of the constant Node.ENTITY_REFERENCE_NODE is 5.
;;     Node.ENTITY_NODE
;;         The value of the constant Node.ENTITY_NODE is 6.
;;     Node.PROCESSING_INSTRUCTION_NODE
;;         The value of the constant Node.PROCESSING_INSTRUCTION_NODE is 7.
;;     Node.COMMENT_NODE
;;         The value of the constant Node.COMMENT_NODE is 8.
;;     Node.DOCUMENT_NODE
;;         The value of the constant Node.DOCUMENT_NODE is 9.
;;     Node.DOCUMENT_TYPE_NODE
;;         The value of the constant Node.DOCUMENT_TYPE_NODE is 10.
;;     Node.DOCUMENT_FRAGMENT_NODE
;;         The value of the constant Node.DOCUMENT_FRAGMENT_NODE is 11.
;;     Node.NOTATION_NODE
;;         The value of the constant Node.NOTATION_NODE is 12.
;;     Node.DOCUMENT_POSITION_DISCONNECTED
;;         The value of the constant Node.DOCUMENT_POSITION_DISCONNECTED is 0x01.
;;     Node.DOCUMENT_POSITION_PRECEDING
;;         The value of the constant Node.DOCUMENT_POSITION_PRECEDING is 0x02.
;;     Node.DOCUMENT_POSITION_FOLLOWING
;;         The value of the constant Node.DOCUMENT_POSITION_FOLLOWING is 0x04.
;;     Node.DOCUMENT_POSITION_CONTAINS
;;         The value of the constant Node.DOCUMENT_POSITION_CONTAINS is 0x08.
;;     Node.DOCUMENT_POSITION_CONTAINED_BY
;;         The value of the constant Node.DOCUMENT_POSITION_CONTAINED_BY is 0x10.
;;     Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC
;;         The value of the constant Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC is 0x20. 

;;-----------------------------------------------------------------------------
;; Objects that implement the Node interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the Node interface:
;;         nodeName
;;             This read-only property is a String.
;;         nodeValue
;;             This property is a String, can raise an object that
;;             implements the DOMException interface on setting and
;;             can raise an object that implements the DOMException
;;             interface on retrieval.
;;         nodeType
;;             This read-only property is a Number.
;;         parentNode
;;             This read-only property is an object that implements the Node interface.
;;         childNodes
;;             This read-only property is an object that implements the NodeList interface.
;;         firstChild
;;             This read-only property is an object that implements the Node interface.
;;         lastChild
;;             This read-only property is an object that implements the Node interface.
;;         previousSibling
;;             This read-only property is an object that implements the Node interface.
;;         nextSibling
;;             This read-only property is an object that implements the Node interface.
;;         attributes
;;             This read-only property is an object that implements the NamedNodeMap interface.
;;         ownerDocument
;;             This read-only property is an object that implements the Document interface.
;;         namespaceURI
;;             This read-only property is a String.
;;         prefix
;;             This property is a String and can raise an object that
;;             implements the DOMException interface on setting.
;;         localName
;;             This read-only property is a String.
;;         baseURI
;;             This read-only property is a String.
;;         textContent
;;             This property is a String, can raise an object that
;;             implements the DOMException interface on setting and
;;             can raise an object that implements the DOMException
;;             interface on retrieval.
(defclass *node ()
  ((node-name :reader node-name :type string)
   (node-value :accessor node-value :type string)
   (node-type :reader node-type :type integer)
   (parent-node :reader parent-node :type *node)
   (child-nodes :reader child-nodes :type list)
   (first-child :reader first-child :type *node)
   (last-child :reader last-child :type *node)
   (previous-sibling :reader previous-sibling :type *node)
   (next-sibling :reader next-sibling :type *node)
   (attributes :reader attributes :type *named-node-map)
   (owner-document :reader owner-document :type *document)
   (namespace-u-r-i :reader namespace-u-r-i :type string)
   (prefix :accessor prefix :type string)
   (local-name :reader local-name :type string)
   (base-u-r-i :reader base-u-r-i :type string)
   (text-content :accessor text-content :type string)))

;;     Functions of objects that implement the Node interface:
;;         insertBefore(newChild, refChild)
;;             This function returns an object that implements the Node interface.
;;             The newChild parameter is an object that implements the Node interface.
;;             The refChild parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod insert-before ((node *node) (new-child *node) (ref-child *node))
  )

;;         replaceChild(newChild, oldChild)
;;             This function returns an object that implements the Node interface.
;;             The newChild parameter is an object that implements the Node interface.
;;             The oldChild parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod replace-child ((node *node) (new-child *node) (ref-child *node))
  )

;;         removeChild(oldChild)
;;             This function returns an object that implements the Node interface.
;;             The oldChild parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod remove-child ((node *node) (old-child *node))
  )

;;         appendChild(newChild)
;;             This function returns an object that implements the Node interface.
;;             The newChild parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod append-child ((node *node) (new-child *node))
  )

;;         hasChildNodes()
;;             This function returns a Boolean.
(defmethod has-child-nodes ((node *node))
  )

;;         cloneNode(deep)
;;             This function returns an object that implements the Node interface.
;;             The deep parameter is a Boolean. 
(defmethod clone-node ((node *node) (deep boolean))
  )

;;         normalize()
;;             This function has no return value. 
(defmethod normalize ((node *node))
  )

;;         isSupported(feature, version)
;;             This function returns a Boolean.
;;             The feature parameter is a String.
;;             The version parameter is a String. 
(defmethod is-supported ((node *node) (feature string) (version string))
  )

;;         hasAttributes()
;;             This function returns a Boolean.
(defmethod has-attributes ((node *node))
  )

;;         compareDocumentPosition(other)
;;             This function returns a Number.
;;             The other parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod compare-document-position ((node *node) (other *node))
  )

;;         isSameNode(other)
;;             This function returns a Boolean.
;;             The other parameter is an object that implements the Node interface. 
(defmethod is-same-node ((node *node) (other *node))
  )

;;         lookupPrefix(namespaceURI)
;;             This function returns a String.
;;             The namespaceURI parameter is a String. 
(defmethod lookup-prefix ((node *node) (namespace-u-r-i string))
  )

;;         isDefaultNamespace(namespaceURI)
;;             This function returns a Boolean.
;;             The namespaceURI parameter is a String. 
(defmethod is-default-namespace ((node *node) (namespace-u-r-i string))
  )

;;         lookupNamespaceURI(prefix)
;;             This function returns a String.
;;             The prefix parameter is a String. 
(defmethod lookup-namespace-u-r-i ((node *node) (prefix string))
  )

;;         isEqualNode(arg)
;;             This function returns a Boolean.
;;             The arg parameter is an object that implements the Node interface. 
(defmethod is-equal-node ((node *node) (arg *node))
  )

;;         getFeature(feature, version)
;;             This function returns an object that implements the Object interface.
;;             The feature parameter is a String.
;;             The version parameter is a String. 
(defmethod get-feature ((node *node) (feature string) (version string))
  )

;;         setUserData(key, data, handler)
;;             This function returns an object that implements the any type interface.
;;             The key parameter is a String.
;;             The data parameter is an object that implements the any type interface.
;;             The handler parameter is an object that implements the UserDataHandler interface. 
(defmethod set-user-data ((node *node) (key string) data handler)
  )

;;         getUserData(key)
;;             This function returns an object that implements the any type interface.
;;             The key parameter is a String.
(defmethod get-user-data ((node *node) (key string))
  )

;;-----------------------------------------------------------------------------
;; Objects that implement the DocumentFragment interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the DocumentFragment interface have all
;;     properties and functions of the Node interface.
(defclass *document-fragment (*node)
  ())

;;-----------------------------------------------------------------------------
;; Objects that implement the Document interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Document interface have all
;;     properties and functions of the Node interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the Document interface:
;;         doctype
;;             This read-only property is an object that implements
;;             the DocumentType interface.
;;         implementation
;;             This read-only property is an object that implements
;;             the DOMImplementation interface.
;;         documentElement
;;             This read-only property is an object that implements
;;             the Element interface.
;;         inputEncoding
;;             This read-only property is a String.
;;         xmlEncoding
;;             This read-only property is a String.
;;         xmlStandalone
;;             This property is a Boolean and can raise an object that
;;             implements the DOMException interface on setting.
;;         xmlVersion
;;             This property is a String and can raise an object that
;;             implements the DOMException interface on setting.
;;         strictErrorChecking
;;             This property is a Boolean.
;;         documentURI
;;             This property is a String.
;;         domConfig
;;             This read-only property is an object that implements
;;             the DOMConfiguration interface.
(defclass *document (*node)
  ((doctype :reader doctype)
   (implementation :reader implementation)
   (document-element :reader document-element)
   (input-encoding :reader input-encoding :type string)
   (xml-encoding :reader xml-encoding :type string)
   (xml-standalone :accessor xml-standalone :type boolean)
   (xml-version :accessor xml-version :type string)
   (strict-error-checking :accessor strict-error-checking :type boolean)
   (document-u-r-i :accessor document-u-r-i :type string)
   (dom-config :reader dom-config)))

;;     Functions of objects that implement the Document interface:
;;         createElement(tagName)
;;             This function returns an object that implements the
;;             Element interface.
;;             The tagName parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod create-element ((self *document) (tagname string))
  (make-*element :tagname tagname :parent self))

;;         createDocumentFragment()
;;             This function returns an object that implements the
;;             DocumentFragment interface.
(defmethod create-document-fragment ((self *document))
  (make-instance '*document-fragment :parent self))

;;         createTextNode(data)
;;             This function returns an object that implements the
;;             Text interface.
;;             The data parameter is a String.
(defmethod create-text-node ((self *document) (data string))
  (make-instance '*text :data data))

;;         createComment(data)
;;             This function returns an object that implements the
;;             Comment interface.
;;             The data parameter is a String.
(defmethod create-comment ((self *document) (data string))
  (make-instance '*comment :data data :parent self))

;;         createCDATASection(data)
;;             This function returns an object that implements the CDATASection interface.
;;             The data parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-c-d-a-t-a-section ((self *document) (data string))
  (make-instance '*c-d-a-t-a-section :parent self :data data))

;;         createProcessingInstruction(target, data)
;;             This function returns an object that implements the ProcessingInstruction interface.
;;             The target parameter is a String.
;;             The data parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-processing-instruction ((self *document)
					  (target string) (data string))
  (error "implement me"))

;;         createAttribute(name)
;;             This function returns an object that implements the Attr interface.
;;             The name parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-attribute ((self *document) (name string))
  (make-instance *attr :parent self :name name))

;;         createEntityReference(name)
;;             This function returns an object that implements the EntityReference interface.
;;             The name parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-entity-reference ((self *document) (name string))
  (make-instance '*entity-reference :name name :parent self))

;;         getElementsByTagName(tagname)
;;             This function returns an object that implements the NodeList interface.
;;             The tagname parameter is a String. 
(defmethod get-elements-by-tag-name ((document *document) (tagname string))
  (let (acc)
    (core-search (list document)
		 #'(lambda (a)
		     (push a acc)
		     nil)
		 #'dom-successor
		 #'append)
    acc))

;;         importNode(importedNode, deep)
;;             This function returns an object that implements the Node interface.
;;             The importedNode parameter is an object that implements the Node interface.
;;             The deep parameter is a Boolean.
;;             This function can raise an object that implements the DOMException interface.
(defmethod import-node ((document *document)
			(imported-node *node) (deep boolean))
  )

;;         createElementNS(namespaceURI, qualifiedName)
;;             This function returns an object that implements the Element interface.
;;             The namespaceURI parameter is a String.
;;             The qualifiedName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-element-n-s ((document *document)
			       (namespace-u-r-i string) (qualified-name string))
  )

;;         createAttributeNS(namespaceURI, qualifiedName)
;;             This function returns an object that implements the Attr interface.
;;             The namespaceURI parameter is a String.
;;             The qualifiedName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod create-attribute-n-s ((document *document)
				 (namespace-u-r-i string) (qualified-name string))
  )

;;         getElementsByTagNameNS(namespaceURI, localName)
;;             This function returns an object that implements the NodeList interface.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
(defmethod get-elements-by-tag-name-n-s ((document *document)
					 (namespace-u-r-i string) (local-name string))
  )

;;         getElementById(elementId)
;;             This function returns an object that implements the Element interface.
;;             The elementId parameter is a String.
(defmethod get-element-by-id ((document *document) (element-id string))
  )

;;         adoptNode(source)
;;             This function returns an object that implements the Node interface.
;;             The source parameter is an object that implements the Node interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod adopt-node ((document *document) (source *node))
  )

;;         normalizeDocument()
;;             This function has no return value. 
(defmethod normalize-document ((document *document))
  nil)

;;         renameNode(n, namespaceURI, qualifiedName)
;;             This function returns an object that implements the Node interface.
;;             The n parameter is an object that implements the Node interface.
;;             The namespaceURI parameter is a String.
;;             The qualifiedName parameter is a String.
;;             This function can raise an object that implements the DOMException interface. 
(defmethod rename-node ((document *document) (n *node)
			(namespace-u-r-i string) (qualified-name string))
  )

;;-----------------------------------------------------------------------------
;; Objects that implement the Attr interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Attr interface have all properties
;;     and functions of the Node interface as well as the properties
;;     and functions defined below.
;;
;;     Properties of objects that implement the Attr interface:
;;         name
;;             This read-only property is a String.
;;         specified
;;             This read-only property is a Boolean.
;;         value
;;             This property is a String and can raise an object that
;;             implements the DOMException interface on setting.
;;         ownerElement
;;             This read-only property is an object that implements
;;             the Element interface.
;;         schemaTypeInfo
;;             This read-only property is an object that implements
;;             the TypeInfo interface.
;;         isId
;;             This read-only property is a Boolean. 
(defclass *attr (*node)
  ((name :reader name :type string)
   (specified :reader specified :type boolean)
   (value :accessor value :type string)
   (owner-element :reader owner-element :type *element)
   (schema-type-info :reader schema-type-info :type *type-info)
   (is-id :reader is-id :type boolean)))

;;-----------------------------------------------------------------------------
;; Objects that implement the Element interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Element interface have all
;;     properties and functions of the Node interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the Element interface:
;;         tagName
;;             This read-only property is a String.
;;         schemaTypeInfo
;;             This read-only property is an object that implements
;;             the TypeInfo interface.
(defclass *element (*node)
  ((tag-name :reader tag-name :type string)
   (schema-type-info :reader schema-type-info :type *type-info)))

;;     Functions of objects that implement the Element interface:
;;         getAttribute(name)
;;             This function returns a String.
;;             The name parameter is a String. 
(defmethod get-attribute ((element *element) (name string))
  )

;;         setAttribute(name, value)
;;             This function has no return value.
;;             The name parameter is a String.
;;             The value parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod set-attribute ((element *element) (name string) (value string))
  )

;;         removeAttribute(name)
;;             This function has no return value.
;;             The name parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod remove-attribute ((element *element) (name string))
  )

;;         getAttributeNode(name)
;;             This function returns an object that implements the
;;             Attr interface.
;;             The name parameter is a String. 
(defmethod get-attribute-node ((element *element) (name string))
  )

;;         setAttributeNode(newAttr)
;;             This function returns an object that implements the
;;             Attr interface.
;;             The newAttr parameter is an object that implements the
;;             Attr interface.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod set-attribute-node ((element *element) (new-attr *attr))
  )

;;         removeAttributeNode(oldAttr)
;;             This function returns an object that implements the
;;             Attr interface.
;;             The oldAttr parameter is an object that implements the
;;             Attr interface.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod remove-attribute-node ((element *element) (old-attr *attr))
  )

;;         getElementsByTagName(name)
;;             This function returns an object that implements the
;;             NodeList interface.
;;             The name parameter is a String.
(defmethod get-elements-by-tag-name ((element *element) (name string))
  )

;;         getAttributeNS(namespaceURI, localName)
;;             This function returns a String.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod get-attribute-n-s ((element *element) (namespace-u-r-i string)
			      (local-name string))
  )

;;         setAttributeNS(namespaceURI, qualifiedName, value)
;;             This function has no return value.
;;             The namespaceURI parameter is a String.
;;             The qualifiedName parameter is a String.
;;             The value parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod set-attribute-n-s ((element *element) (namespace-u-r-i string)
			      (qualified-name string) (value string))
  )

;;         removeAttributeNS(namespaceURI, localName)
;;             This function has no return value.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the
;;             DOMException interface.
(defmethod remove-attribute-n-s ((element *element) (namespace-u-r-i string)
				 (local-name string))
  )

;;         getAttributeNodeNS(namespaceURI, localName)
;;             This function returns an object that implements the Attr interface.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod get-attribute-node-n-s ((element *element) (namespace-u-r-i string)
				   (local-name string))
  )

;;         setAttributeNodeNS(newAttr)
;;             This function returns an object that implements the Attr interface.
;;             The newAttr parameter is an object that implements the Attr interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod set-attribute-node-n-s ((element *element) (new-attr *attr))
  )

;;         getElementsByTagNameNS(namespaceURI, localName)
;;             This function returns an object that implements the NodeList interface.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod get-elements-by-tag-name-n-s ((element *element) (namespace-u-r-i string)
					 (local-name string))
  )

;;         hasAttribute(name)
;;             This function returns a Boolean.
;;             The name parameter is a String. 
(defmethod has-attribute ((element *element) (name string))
  )

;;         hasAttributeNS(namespaceURI, localName)
;;             This function returns a Boolean.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod has-attribute-n-s ((element *element) (namespace-u-r-i string)
			      (local-name string))
  )

;;         setIdAttribute(name, isId)
;;             This function has no return value.
;;             The name parameter is a String.
;;             The isId parameter is a Boolean.
;;             This function can raise an object that implements the DOMException interface.
(defmethod set-id-attribute ((element *element) (name string) (is-id boolean))
  )

;;         setIdAttributeNS(namespaceURI, localName, isId)
;;             This function has no return value.
;;             The namespaceURI parameter is a String.
;;             The localName parameter is a String.
;;             The isId parameter is a Boolean.
;;             This function can raise an object that implements the DOMException interface.
(defmethod set-id-attribute-n-s ((element *element) (namespace-u-r-i string)
				 (local-name string) (is-id boolean))
  )

;;         setIdAttributeNode(idAttr, isId)
;;             This function has no return value.
;;             The idAttr parameter is an object that implements the Attr interface.
;;             The isId parameter is a Boolean.
;;             This function can raise an object that implements the DOMException interface. 
(defmethod set-id-attribute-node ((element *element) (id-attr *attr) (is-id boolean))
  )

;;-----------------------------------------------------------------------------
;; Objects that implement the Text interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Text interface have all properties
;;     and functions of the CharacterData interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the Text interface:
;;         isElementContentWhitespace
;;             This read-only property is a Boolean.
;;         wholeText
;;             This read-only property is a String. 
(defclass *text ()
  ((is-element-content-whitespace :reader is-element-content-whitespace :type boolean)
   (whole-text :reader whole-text :type string)))

;;     Functions of objects that implement the Text interface:
;;         splitText(offset)
;;             This function returns an object that implements the Text interface.
;;             The offset parameter is a Number.
;;             This function can raise an object that implements the DOMException interface.
(defmethod split-text ((text *text) (offset integer))
  )

;;         replaceWholeText(content)
;;             This function returns an object that implements the Text interface.
;;             The content parameter is a String.
;;             This function can raise an object that implements the DOMException interface. 
(defmethod replace-whole-text ((text *text) (content string))
  )

;;-----------------------------------------------------------------------------
;; Objects that implement the Comment interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Comment interface have all
;;     properties and functions of the CharacterData interface.
;;
(defclass *comment ()
  ())

;; Properties of the TypeInfo Constructor function:
;;
;;     TypeInfo.DERIVATION_RESTRICTION
;;         The value of the constant TypeInfo.DERIVATION_RESTRICTION is 0x00000001.
;;     TypeInfo.DERIVATION_EXTENSION
;;         The value of the constant TypeInfo.DERIVATION_EXTENSION is 0x00000002.
;;     TypeInfo.DERIVATION_UNION
;;         The value of the constant TypeInfo.DERIVATION_UNION is 0x00000004.
;;     TypeInfo.DERIVATION_LIST
;;         The value of the constant TypeInfo.DERIVATION_LIST is 0x00000008. 

;;-----------------------------------------------------------------------------
;; Objects that implement the TypeInfo interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the TypeInfo interface:
;;         typeName
;;             This read-only property is a String.
;;         typeNamespace
;;             This read-only property is a String. 
(defclass *type-info ()
  ((type-name :reader type-name :type string)
   (type-namespace :reader type-namespace :type string)))

;;     Functions of objects that implement the TypeInfo interface:
;;         isDerivedFrom(typeNamespaceArg, typeNameArg, derivationMethod)
;;             This function returns a Boolean.
;;             The typeNamespaceArg parameter is a String.
;;             The typeNameArg parameter is a String.
;;             The derivationMethod parameter is a Number. 
(defmethod is-derived-from ((type-info *type-info) (type-namespace-arg string)
			    (type-name-arg string) (derivation-method integer))
  )

;; Properties of the UserDataHandler Constructor function:
;;     UserDataHandler.NODE_CLONED
;;         The value of the constant UserDataHandler.NODE_CLONED is 1.
(defvar *user-data-handler.*n-o-d-e_-c-l-o-n-e-d 1)
;;     UserDataHandler.NODE_IMPORTED
;;         The value of the constant UserDataHandler.NODE_IMPORTED is 2.
(defvar *user-data-handler.*n-o-d-e_-i-m-p-o-r-t-e-d 2)
;;     UserDataHandler.NODE_DELETED
;;         The value of the constant UserDataHandler.NODE_DELETED is 3.
(defvar *user-data-handler.*n-o-d-e_-d-e-l-e-t-e-d 3)
;;     UserDataHandler.NODE_RENAMED
;;         The value of the constant UserDataHandler.NODE_RENAMED is 4.
(defvar *user-data-handler.*n-o-d-e_-r-e-n-a-m-e-d 4)
;;     UserDataHandler.NODE_ADOPTED
;;         The value of the constant UserDataHandler.NODE_ADOPTED is 5. 
(defvar *user-data-handler.*n-o-d-e_-a-d-o-p-t-e-d 5)

;; UserDataHandler function:
;;     This function has no return value. The first parameter is a
;;     Number. The second parameter is a String. The third parameter
;;     is an object that implements the any type interface. The fourth
;;     parameter is an object that implements the Node interface. The
;;     fifth parameter is an object that implements the Node
;;     interface.

;; Properties of the DOMError Constructor function:
;;     DOMError.SEVERITY_WARNING
;;         The value of the constant DOMError.SEVERITY_WARNING is 1.
(defvar *d-o-m-error.*s-e-v-e-r-i-t-y_-w-a-r-n-i-n-g 1)
;;     DOMError.SEVERITY_ERROR
;;         The value of the constant DOMError.SEVERITY_ERROR is 2.
(defvar *d-o-m-error.*s-e-v-e-r-i-t-y_-e-r-r-o-r 2)
;;     DOMError.SEVERITY_FATAL_ERROR
;;         The value of the constant DOMError.SEVERITY_FATAL_ERROR is 3. 
(defvar *d-o-m-error.*s-e-v-e-r-i-t-y_-f-a-t-al_-e-r-r-o-r 2)

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMError interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMError interface:
;;         severity
;;             This read-only property is a Number.
;;         message
;;             This read-only property is a String.
;;         type
;;             This read-only property is a String.
;;         relatedException
;;             This read-only property is an object that implements
;;             the Object interface.
;;         relatedData
;;             This read-only property is an object that implements
;;             the Object interface.
;;         location
;;             This read-only property is an object that implements
;;             the DOMLocator interface.
(defclass *d-o-m-error ()
  ((severity :reader severity :type number)
   (message :reader message :type string)
   (type :reader %type :type string)
   (related-exception :reader related-exception)
   (related-data :reader related-data)
   (location :reader location)))

;; DOMErrorHandler function:
;;     This function returns a Boolean. The parameter is an object
;;     that implements the DOMError interface.


;;-----------------------------------------------------------------------------
;; Objects that implement the DOMLocator interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMLocator interface:
;;         lineNumber
;;             This read-only property is a Number.
;;         columnNumber
;;             This read-only property is a Number.
;;         byteOffset
;;             This read-only property is a Number.
;;         utf16Offset
;;             This read-only property is a Number.
;;         relatedNode
;;             This read-only property is an object that implements
;;             the Node interface.
;;         uri
;;             This read-only property is a String. 
(defclass *d-o-m-locator ()
  ((line-number :reader line-number :type number)
   (column-number :reader column-number :type number)
   (byte-offset :reader byte-offset :type number)
   (utf16-offset :reader utf16-offset :type number)
   (related-node :reader related-node :type *node)
   (uri :reader uri :type string)))

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMConfiguration interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMConfiguration interface:
;;         parameterNames
;;             This read-only property is an object that implements the DOMStringList interface. 
(defclass *d-o-m-configuration ()
  ((parameter-names :reader parameter-names)))

;;     Functions of objects that implement the DOMConfiguration interface:
;;         setParameter(name, value)
;;             This function has no return value.
;;             The name parameter is a String.
;;             The value parameter is an object that implements the any type interface.
;;             This function can raise an object that implements the DOMException interface.
(defmethod set-parameter ((dom-configuration *d-o-m-configuration)
			  (name string) value)
  )

;;         getParameter(name)
;;             This function returns an object that implements the any type interface.
;;             The name parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
(defmethod get-parameter ((dom-configuration *d-o-m-configuration) (name string))
  )

;;         canSetParameter(name, value)
;;             This function returns a Boolean.
;;             The name parameter is a String.
;;             The value parameter is an object that implements the any type interface. 
(defmethod can-set-parameter ((dom-configuration *d-o-m-configuration)
			      (name string) value)
  )

;;-----------------------------------------------------------------------------
;; Objects that implement the CDATASection interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the CDATASection interface have all
;;     properties and functions of the Text interface.
(defclass *c-d-a-t-a-section (*text)
  ())

;;-----------------------------------------------------------------------------
;; Objects that implement the DocumentType interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the DocumentType interface have all
;;     properties and functions of the Node interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the DocumentType interface:
;;         name
;;             This read-only property is a String.
;;         entities
;;             This read-only property is an object that implements
;;             the NamedNodeMap interface.
;;         notations
;;             This read-only property is an object that implements
;;             the NamedNodeMap interface.
;;         publicId
;;             This read-only property is a String.
;;         systemId
;;             This read-only property is a String.
;;         internalSubset
;;             This read-only property is a String. 
(defclass *document-type (*node)
  ((name :reader name :type string)
   (entities :reader entities)
   (notation :reader notations)
   (public-id :reader public-id :type string)
   (system-id :reader system-id :type string)
   (internal-subset :reader internal-subset :type string)))

;;-----------------------------------------------------------------------------
;; Objects that implement the Notation interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Notation interface have all
;;     properties and functions of the Node interface as well as the
;;     properties and functions defined below.
;;
;;     Properties of objects that implement the Notation interface:
;;         publicId
;;             This read-only property is a String.
;;         systemId
;;             This read-only property is a String. 
(defclass *notation (*node)
  ((public-id :reader public-id :type string)
   (system-id :reader system-id :type string)))

;;-----------------------------------------------------------------------------
;; Objects that implement the Entity interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the Entity interface have all properties
;;     and functions of the Node interface as well as the properties
;;     and functions defined below.
;;
;;     Properties of objects that implement the Entity interface:
;;         publicId
;;             This read-only property is a String.
;;         systemId
;;             This read-only property is a String.
;;         notationName
;;             This read-only property is a String.
;;         inputEncoding
;;             This read-only property is a String.
;;         xmlEncoding
;;             This read-only property is a String.
;;         xmlVersion
;;             This read-only property is a String. 
(defclass *entity (*node)
  ((public-id :reader public-id :type string)
   (system-id :reader system-id :type string)
   (notation-name :reader notation-name :type string)
   (input-encoding :reader input-encoding :type string)
   (xml-encoding :reader xml-encoding :type string)
   (xml-version :reader xml-version :type string)))

;;-----------------------------------------------------------------------------
;; Objects that implement the EntityReference interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the EntityReference interface have all
;;     properties and functions of the Node interface.
(defclass *entity-reference (*node)
  ())

;;-----------------------------------------------------------------------------
;; Objects that implement the ProcessingInstruction interface:
;;-----------------------------------------------------------------------------
;;     Objects that implement the ProcessingInstruction interface have
;;     all properties and functions of the Node interface as well as
;;     the properties and functions defined below.
;;
;;     Properties of objects that implement the ProcessingInstruction interface:
;;         target
;;             This read-only property is a String.
;;         data
;;             This property is a String and can raise an object that
;;             implements the DOMException interface on setting.
(defclass *processing-instruction (*node)
  ((target :reader target :type string)
   (data :accessor data :type string)))


;;-----------------------------------------------------------------------------
;; Objects that implement the DOMStringList interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMStringList interface:
;;         length
;;             This read-only property is a Number. 
;;     Functions of objects that implement the DOMStringList interface:
;;         item(index)
;;             This function returns a String.
;;             The index parameter is a Number.
;;             Note: This object can also be dereferenced using square
;;             bracket notation (e.g. obj[1]). Dereferencing with an
;;             integer index is equivalent to invoking the item
;;             function with that index.
;;         contains(str)
;;             This function returns a Boolean.
;;             The str parameter is a String. 

;; TODO: Implement domstring-list (gee, what the hell w3c ppl think?)

;;-----------------------------------------------------------------------------
;; Objects that implement the NameList interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the NameList interface:
;;         length
;;             This read-only property is a Number. 
;;     Functions of objects that implement the NameList interface:
;;         getName(index)
;;             This function returns a String.
;;             The index parameter is a Number. 
;;         getNamespaceURI(index)
;;             This function returns a String.
;;             The index parameter is a Number. 
;;         contains(str)
;;             This function returns a Boolean.
;;             The str parameter is a String. 
;;         containsNS(namespaceURI, name)
;;             This function returns a Boolean.
;;             The namespaceURI parameter is a String.
;;             The name parameter is a String.

;; TODO: Implement namelist

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMImplementationList interface:
;;-----------------------------------------------------------------------------
;;     Properties of objects that implement the DOMImplementationList interface:
;;         length
;;             This read-only property is a Number. 
;;     Functions of objects that implement the DOMImplementationList interface:
;;         item(index)
;;             This function returns an object that implements the
;;             DOMImplementation interface.
;;             The index parameter is a Number.
;;             Note: This object can also be dereferenced using square
;;             bracket notation (e.g. obj[1]). Dereferencing with an
;;             integer index is equivalent to invoking the item
;;             function with that index.

;; TODO: Implement domimplementationlist

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMImplementationSource interface:
;;-----------------------------------------------------------------------------
;;     Functions of objects that implement the DOMImplementationSource interface:
;;         getDOMImplementation(features)
;;             This function returns an object that implements the
;;             DOMImplementation interface.
;;             The features parameter is a String. 
;;         getDOMImplementationList(features)
;;             This function returns an object that implements the
;;             DOMImplementationList interface.
;;             The features parameter is a String.

;; TODO: Implement Domimplementationsource

;;-----------------------------------------------------------------------------
;; Objects that implement the DOMImplementation interface:
;;-----------------------------------------------------------------------------
;;     Functions of objects that implement the DOMImplementation interface:
;;         hasFeature(feature, version)
;;             This function returns a Boolean.
;;             The feature parameter is a String.
;;             The version parameter is a String. 
;;         createDocumentType(qualifiedName, publicId, systemId)
;;             This function returns an object that implements the DocumentType interface.
;;             The qualifiedName parameter is a String.
;;             The publicId parameter is a String.
;;             The systemId parameter is a String.
;;             This function can raise an object that implements the DOMException interface.
;;         createDocument(namespaceURI, qualifiedName, doctype)
;;             This function returns an object that implements the Document interface.
;;             The namespaceURI parameter is a String.
;;             The qualifiedName parameter is a String.
;;             The doctype parameter is an object that implements the DocumentType interface.
;;             This function can raise an object that implements the DOMException interface.
;;         getFeature(feature, version)
;;             This function returns an object that implements the Object interface.
;;             The feature parameter is a String.
;;             The version parameter is a String. 

;; TODO: Implement domimplementaiton intirfeyz
