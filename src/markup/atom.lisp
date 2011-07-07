;; +-------------------------------------------------------------------------
;; | RFC 4287 - The Atom Syndication Format
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011
;;
;; Specification: http://tools.ietf.org/html/rfc4287
;;
(in-package :core-server)

;; -------------------------------------------------------------------------
;; Atom Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass atom+ (xml+)
    ()
    (:default-initargs
     :namespace "atom"
     :schema "http://www.w3.org/2005/Atom")))

(defclass+ atom-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | Atom Object Definition: defatom-tag
;; +------------------------------------------------------------------------
(defmacro defatom-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (atom-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  (remove 'id attributes)))
       (:metaclass atom+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defatom-tag <atom:feed gphoto media opensearch)
(defatom-tag <atom:author)
(defatom-tag <atom:category term scheme label)
(defatom-tag <atom:contributor)
(defatom-tag <atom:generator version uri)
(defatom-tag <atom:icon)
(defatom-tag <atom:id)
(defatom-tag <atom:link rel href)
(defatom-tag <atom:logo)
(defatom-tag <atom:rights)
(defatom-tag <atom:subtitle)
(defatom-tag <atom:title)
(defatom-tag <atom:updated)

(defatom-tag <atom:name)
(defatom-tag <atom:uri)
(defatom-tag <atom:email)
(defatom-tag <atom:entry)
(defatom-tag <atom:published)
(defatom-tag <atom:summary)

(defparameter +atom-feed+
  "<?xml version= \"1.0\" encoding= \"utf-8\"?>
<feed xmlns=\"http://www.w3.org/2005/Atom\">
        <title>Example Feed</title>
        <subtitle>A subtitle.</subtitle>
        <link href=\"http://example.org/feed/\" rel=\"self\" />
        <link href=\"http://example.org/\" />
        <id>urn:uuid:60a76c80-d399-11d9-b91C-0003939e0af6</id>
        <updated>2003-12-13T18:30:02Z</updated>
        <author>
                <name>John Doe</name>
                <email>johndoe@example.com</email>
        </author>
        <entry>
                <title>Atom-Powered Robots Run Amok</title>
                <link href=\"http://example.org/2003/12/13/atom03\" />
                <link rel=\"alternate\" type=\"text/html\"
                      href=\"http://example.org/2003/12/13/atom03.html\"/>
                <link rel=\"edit\"
                      href=\"http://example.org/2003/12/13/atom03/edit\"/>
                <id>urn:uuid:1225c695-cfb8-4ebb-aaaa-80da344efa6a</id>
                <updated>2003-12-13T18:30:02Z</updated>
                <summary>Some text.</summary>
        </entry>
</feed>")

;; SERVER> (parse-xml (xml-lexer? (make-core-stream +atom-feed+)))
;; #<TR.GEN.CORE.SERVER.ATOM:FEED  {10038A63C1}>

;; or

;; SERVER> (read-stream (make-xml-stream (make-core-stream +atom-feed+)))
;; #<TR.GEN.CORE.SERVER.ATOM:FEED  {10038A63C1}>