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

(in-package :tr.gen.core.server)

(defparameter +hxpath+
  (merge-pathnames #P"bin/HXPath"
		   (pathname (sb-posix:getenv "CORESERVER_HOME"))))

(defrule hxpath? (expr c (acc (core-server::make-accumulator)))
  (:checkpoint
   (:seq "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
   (:commit))
  (:lwsp?)
  (:seq "<xpath-result")
  (:lwsp?)
  (:zom (:not #\>)
	(:or (:and (:seq "/>") (:return nil))
	     (:and (:seq "expr=") (:quoted? expr)
		   (:lwsp?))
	     (:and (:seq "source=") 
		   (:quoted? expr)
		   (:lwsp?))))
  (:lwsp?)
  (:zom (:or (:and (:seq "</xpath-result>") (:return acc))
	     (:and (:type octet? c) (:collect c acc))))
  (:return nil))

;; core@node5 ~/core-server/bin $ ./HXPath --help
;; HXPath - XPath Evaluator of the Haskell XML Toolbox (Arrow Version)
;; Usage: HXPath [OPTION...] <XPath expr> <URL or FILE>
;;   -v          --verbose                   verbose output
;;   -h, -?      --help                      this message
;;   -t[LEVEL]   --trace[=LEVEL]             trace level (0-4), default 1
;;   -p PROXY    --proxy=PROXY               proxy for http access (e.g. "www-cache:3128")
;;               --use-curl                  HTTP access via external program "curl", more functionality, supports HTTP/1.0, less efficient
;;               --do-not-use-curl           HTTP access via built in HTTP/1.1 module (default)
;;               --options-curl=STR          additional curl options, e.g. for timeout, ...
;;               --default-base-URI=URI      default base URI, default: "file:///<cwd>/"
;;   -e CHARSET  --encoding=CHARSET          default document encoding (UTF-8, ISO-8859-1, US-ASCII, ...)
;;               --issue-errors              issue all errorr messages on stderr (default)
;;               --do-not-issue-errors       ignore all error messages
;;   -H          --parse-html                parse input as HTML, try to interprete everything as HTML, no validation
;;               --issue-warnings            issue warnings, when parsing HTML (default)
;;   -Q          --do-not-issue-warnings     ignore warnings, when parsing HTML
;;               --parse-xml                 parse input as XML (default)
;;               --validate                  document validation when parsing XML (default)
;;   -w          --do-not-validate           only wellformed check, no validation
;;               --canonicalize              canonicalize document, remove DTD, comment, transform CDATA, CharRef's, ... (default)
;;   -c          --do-not-canonicalize       do not canonicalize document, don't remove DTD, comment, don't transform CDATA, CharRef's, ...
;;   -C          --preserve-comment          don't remove comments during canonicalisation
;;               --do-not-preserve-comment   remove comments during canonicalisation (default)
;;   -n          --check-namespaces          tag tree with namespace information and check namespaces
;;               --do-not-check-namespaces   ignore namespaces (default)
;;   -r          --remove-whitespace         remove redundant whitespace, simplifies tree and processing
;;               --do-not-remove-whitespace  don't remove redundant whitespace (default)
;;   -i          --indent                    indent XML output for readability
;;   -o CHARSET  --output-encoding=CHARSET   encoding of output (UTF-8, ISO-8859-1, US-ASCII)
;;   -f FILE     --output-file=FILE          output file for resulting document (default: stdout)
;;               --output-html               output of none ASCII chars as HTMl entity references
;;               --no-xml-pi                 output without <?xml ...?> processing instruction, useful in combination with --"output-html"
;;
;; EXAMPLE: ./HXPath -Hw "//div[@id='content']/" /tmp/zeben.html
;;
(defcommand hxpath (shell)
  ((xquery :host local :initform (error "Specify query") )
   (uri :host local :initform (error "specify uri")))
  (:default-initargs :cmd +hxpath+
    :args '( "--use-curl" "--output-html" "--no-xml-pi" "-Hw" "--indent") :verbose nil))

(defmethod render-arguments ((self hxpath))
  (append 
   (args self)
   (list (s-v 'xquery) (s-v 'uri))))

(defmethod run-command ((self hxpath) args)
  (handler-bind ((error
		  #'(lambda (condition) (declare (ignore condition))
		      (return-from run-command nil))))
    (call-next-method))
  (hxpath? (make-core-stream (command.output-stream self))))

(defparameter +xmltr+ (merge-pathnames #P"bin/xmltr"
				       (pathname (sb-posix:getenv "CORESERVER_HOME"))))
					
;; core@node5 ~/core-server/bin $ echo "hobaaa" |./xmltr content /tmp/ge.html /tmp/zeben.html   
(defcommand xmltr (shell)
  ((id :host local)
   (in :host local)
   (out :host local)
   (text :host local))
  (:default-initargs :cmd +xmltr+ :verbose nil :wait nil))

(defmethod render-arguments ((self xmltr))
  (append 
   (args self)
   (list (s-v 'id) (s-v 'in) (s-v 'out))))

(defmethod run-command ((self xmltr) args)
  (call-next-method)
  (format (command.input-stream self) "~A" (s-v 'text))
  (close (command.input-stream self))
;;   (let ((cs (core-server::make-core-stream (command.input-stream self))))
;;     (core-server::string! cs (text self))
;;     (core-server::close-stream cs))
  (wait-process self))
