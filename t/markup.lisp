(in-package :core-server.test)

;;-----------------------------------------------------------------------------
;; Html markup tests
;;-----------------------------------------------------------------------------
(defparameter *html1*
  "<html><head><title>Test Title</title></head><body><div id=\"XX\">XX Content</div></body></html>")

(deftest html-parse-1
    (with-core-stream (s *html1*)
      (let ((html (html? s)))
	(car (dom.children (car (dom.children (car (dom.children html))))))))
  "Test Title")

(deftest html-parse-2
    (with-core-stream (s *html1*)
      (let ((html (html? s)))
	(block nil
	  (core-search (list (html? (make-core-stream *html1*)))
		       (lambda (a)
			 (if (and (typep a 'dom-element)
				  (equal "XX" (cdr (assoc "id" (dom.attributes a) :test #'equal))))
			     (return (car (dom.children a)))))
		       #'core-server::dom-successor
		       #'append))))
  "XX Content")

(deftest html-render-1
    (with-core-stream (s "")
      (html! s (html? (make-core-stream *html1*)))
      (return-stream s))
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\"
   \"http://www.w3.org/TR/html4/frameset.dtd\">
<html>
  <head>
    <title>Test Title</title></head>
  <body>
    <div id=\"XX\">XX Content</div></body>
</html>")

;;-----------------------------------------------------------------------------
;; RSS markup tests (Thanks to Fazlamesai.net for RSS Feed)
;;-----------------------------------------------------------------------------
(defvar *rss* "<?xml version=\"1.0\" encoding=\"ISO-8859-9\"?>

<!DOCTYPE rss PUBLIC \"-//Netscape Communications//DTD RSS 0.91//EN\"
 \"http://my.netscape.com/publish/formats/rss-0.91.dtd\">

<rss version=\"0.91\">

<channel>
<title>Fazlamesai.net</title>
<link>http://www.fazlamesai.net</link>
<description>fazlamesai.net</description>
<language>tr</language>

<item>

<title>e-bergi Temmuz SayÄ±sÄ± ÃÄ±ktÄ±!</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5046</link>
<description>ODTÃ Bilgisayar TopluluÄu olarak, bilgisayar bilimi alanÄ±ndaki TÃ¼rkÃ§e kaynak sÄ±kÄ±ntÄ±sÄ±nÄ± gidermek amacÄ±yla yayÄ±nlamakta olduÄumuz elektronik dergi &lt;a href='http://e-bergi.com'&gt;e-bergi &lt;/a&gt;, Temmuz 2008 sayÄ±sÄ±yla bu ay da sizlerle...</description>
</item>

<item>
<title>KOffice 2.0 Windows, Linux ve MacOSX'de Ã§alÄ±ÅÄ±yor</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5045</link>

<description>Yeni duyurulan son alpha8 sÃ¼rÃ¼mÃ¼ ile bÃ¼tÃ¼n iÅletim sistemleri iÃ§in hazÄ±r kurulum araÃ§larÄ± geliyor. KOffice2.0'Ä±n Ã§Ä±kmasÄ±nÄ± sabÄ±rsÄ±zlÄ±kla bekliyoruz. 
&lt;br&gt;&lt;br&gt;
&lt;a href=http://www.koffice.org/announcements/announce-2.0alpha8.php&gt;http://www.koffice.org/announcements/&lt;/a&gt;
</description>
</item>


</channel>
</rss>
")

(deftest rss-parse-1
    (with-core-stream (s "")
      (type-of (rss? (make-core-stream *rss*))))
  <rss:rss)

(deftest rss-render-1
    (with-core-stream (s "")
      (rss! s (rss? (make-core-stream *rss*)))
      (return-stream s))
  "<rss version=\"0.91\">
  <channel>
    <title>Fazlamesai.net</title>
    <link>http://www.fazlamesai.net</link>
    <description>fazlamesai.net</description>
    <language>tr</language>
    <item>
      <title>e-bergi Temmuz SayÃÂ±sÃÂ± ÃÂÃÂ±ktÃÂ±!</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5046</link>
      <description>ODTÃÂ Bilgisayar TopluluÃÂu olarak, bilgisayar bilimi alanÃÂ±ndaki TÃÂ¼rkÃÂ§e kaynak sÃÂ±kÃÂ±ntÃÂ±sÃÂ±nÃÂ± gidermek amacÃÂ±yla yayÃÂ±nlamakta olduÃÂumuz elektronik dergi &lt;a href='http://e-bergi.com'&gt;e-bergi &lt;/a&gt;, Temmuz 2008 sayÃÂ±sÃÂ±yla bu ay da sizlerle...</description>
    </item>
    <item>
      <title>KOffice 2.0 Windows, Linux ve MacOSX'de ÃÂ§alÃÂ±ÃÂÃÂ±yor</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5045</link>
      <description>Yeni duyurulan son alpha8 sÃÂ¼rÃÂ¼mÃÂ¼ ile bÃÂ¼tÃÂ¼n iÃÂletim sistemleri iÃÂ§in hazÃÂ±r kurulum araÃÂ§larÃÂ± geliyor. KOffice2.0'ÃÂ±n ÃÂ§ÃÂ±kmasÃÂ±nÃÂ± sabÃÂ±rsÃÂ±zlÃÂ±kla bekliyoruz. 
&lt;br&gt;&lt;br&gt;
&lt;a href=http://www.koffice.org/announcements/announce-2.0alpha8.php&gt;http://www.koffice.org/announcements/&lt;/a&gt;
</description>
    </item>
  </channel></rss>")

;;-----------------------------------------------------------------------------
;; CSS Tests
;;-----------------------------------------------------------------------------
;; (deftest css-render-1
;;     (with-core-stream (s "")
;;       (css! s (css "html" :background-color "#000" :color "#FFF"))
;;       (return-stream s))
;;   "html {
;; background-color: #000;
;; color: #FFF;
;; };

;; ")

;; (defmacro defcss-test (name fun string &body result)
;;   `(deftest ,(intern (format nil "CSS-~A" name))       
;;        (funcall (function ,(intern (symbol-name fun) :core-server))
;; 		(make-core-stream ,string))
;;      ,@result))

;; (defcss-test value-1 css-value? "moo#gee" "moo")

;; (defcss-test value-2 css-value? "-gee_" "-gee")

;; (defcss-test universal-selector css-universal-selector?
;;     "*" (core-server::universal-selector))

;; (defcss-test type-selector css-type-selector? "div#moo"
;;   (core-server::type-selector "div"))

;; (defcss-test descendant-selector css-descendant-selector? "div span"
;;   (core-server::descendant-selector (core-server::type-selector "div")
;; 				    (core-server::type-selector "span")))

;; (defcss-test child-selector css-child-selector? "div > span"
;;   (core-server::child-selector (core-server::type-selector "div")
;; 			       (core-server::type-selector "span")))

;; (defcss-test first-child-selector css-first-child-selector? "div:first-child"
;;   (core-server::first-child-selector (core-server::type-selector "div")))

;; (defcss-test link-selector-1 css-link-selector? "a:link"
;;   (core-server::link-selector (core-server::type-selector "a") core-server::link))

;; (defcss-test link-selector-2 css-link-selector? "a:visited"
;;   (core-server::link-selector (core-server::type-selector "a") core-server::visited))

;; (defcss-test dynamic-selector css-dynamic-selector? "a:active"
;;   (core-server::dynamic-selector (core-server::type-selector "a")))

;; (defcss-test lang-selector css-lang-selector? "div:lang(turkish)"
;;   (core-server::lang-selector (core-server::type-selector "div") "turkish"))

;; (defcss-test adjacent-selector css-adjacent-selector? "div + span"
;;   (core-server::adjacent-selector (core-server::type-selector "div")
;; 				  (core-server::type-selector "span")))

;; (defcss-test attribute-selector-1 css-attribute-selector? "div[foo]"
;;   (core-server::attribute-selector (core-server::type-selector "div")
;; 				   ((core-server::exists "foo" nil))))

;; (defcss-test attribute-selector-2 css-attribute-selector? "div[foo|=\"oof\"][moo=\"zoo\"][poo~=\"oop\"]"
;;   (core-server::attribute-selector (core-server::type-selector "div")
;; 				   ((core-server::hypen-member "foo" "oof")
;; 				    (core-server::equal "moo" "zoo")
;; 				    (core-server::space-member "poo" "oop"))))

;; (defcss-test class-selector css-class-selector? "moo.warning"
;;   (core-server::attribute-selector (core-server::type-selector "moo")
;; 				   ((equal "class" "warning"))))

;; (defcss-test id-selector css-id-selector? "div#gee"
;;   (core-server::id-selector (core-server::type-selector "div") "gee"))

