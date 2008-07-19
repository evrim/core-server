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
(deftest css-render-1
    (with-core-stream (s "")
      (css! s (css "html" :background-color "#000" :color "#FFF"))
      (return-stream s))
  "html {
background-color: #000;
color: #FFF;
};

")