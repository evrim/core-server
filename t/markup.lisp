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

<item>
<title>IBM TÃ¼rk ÃalÄ±ÅanlarÄ±nÄ±n Sendikal ÃrgÃ¼tlenmesi</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5044</link>

<description>Bizler 258 kiÅiyle Tez-koop-iÅ sendikasÄ±na Ã¼ye IBM TÃ¼rk Ã§alÄ±ÅanlarÄ±yÄ±z.
&lt;br&gt;&lt;br&gt;
SendikamÄ±z, toplu sÃ¶zleÅme yapma hakkÄ±mÄ±zÄ± almak iÃ§in 26 Mart 2008 tarihinde ÃalÄ±Åma BakanlÄ±ÄÄ±na yetki baÅvurusunda bulundu.


</description>
</item>

<item>
<title>IBM YazÄ±lÄ±m Akademisi ilk mezunlarÄ±nÄ± verdi</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5043</link>
<description>&lt;p&gt;Akademi kapsamÄ±nda yapÄ±lan yazÄ±lÄ±m yarÄ±ÅmasÄ±nda en iyi 3 proje, Galatasaray, Dokuz EylÃ¼l ve SabancÄ± Ãniversitelerinden Ã§Ä±ktÄ±.&lt;/p&gt;

&lt;p&gt;IBM TÃ¼rkï¿½Ã¼n geleceÄin yazÄ±lÄ±mcÄ±larÄ±nÄ± yetiÅtirmek Ã¼zere Ekim 2007ï¿½de baÅlattÄ±ÄÄ± ï¿½IBM YazÄ±lÄ±m Akademisiï¿½nin 2007-2008 Ã¶Ärenim yÄ±lÄ±, kapsamlÄ± eÄitim, proje geliÅtirme ve yazÄ±lÄ±m yarÄ±ÅmasÄ± aÅamalarÄ±ndan sonra sona erdi.&lt;/p&gt;</description>
</item>

<item>
<title>Diablo III Resmen Duyuruldu!</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5042</link>
<description>Blizzard, geÃ§tiÄimiz aylarda belki de bilgisayar oyunlarÄ± tarihinin gelmiÅ geÃ§miÅ en Ã§ok beklenen iki oyunundan biri olan Starcraft II 'yi duyurmuÅtu.
&lt;br&gt;&lt;br&gt;

Åimdi de yÄ±llardÄ±r beklenen, Diablo nun III. oyunu sonunda blizzard tarafÄ±ndan resmen duyuruldu!
&lt;a href=&quot;http://www.blizzard.com/diablo3/&quot;&gt;Diablo III&lt;/a&gt;</description>
</item>

<item>
<title>Pardus 2008</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5041</link>
<description>&lt;a href=http://www.pardus.org.tr&gt;Pardus 2008&lt;/a&gt; nihayet duyuruldu.&lt;br&gt;

&lt;a href=http://www.ozgurlukicin.com/haber/pardus-2008-yenilikleri/&gt;Pardus 2008 Yenilikleri&lt;/a&gt;&lt;br&gt;
&lt;a href=http://www.pardus.org.tr/indir.html&gt;Ä°ndirme sayfasÄ± (ftp ve torrent)&lt;/a&gt;&lt;br&gt;
&lt;a href=http://distrowatch.com/?newsid=04967&gt;distrowatch.com duyuru sayfasÄ±&lt;/a&gt;</description>
</item>

<item>

<title>TÃ¼rk GÃ¼neÅ Teknesi Muavenet DÃ¼nya 2.si</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5040</link>
<description>GÃ¼neÅ enerjisi ile Ã§alÄ±Åan teknelerin yarÄ±ÅtÄ±ÄÄ± &lt;a href=&quot;http://www.solarsplash.com&quot;&gt;Solar Splash&lt;/a&gt; yarÄ±ÅmasÄ±nda geÃ§en sene 3. olan &lt;a href=&quot;http://www.solarsplash.itu.edu.tr&quot;&gt;TÃ¼rk takÄ±mÄ±&lt;/a&gt;, bu sene yeni tekneleri Muavenet ile DÃ¼nya 2.si oldular. Ä°TÃ'lÃ¼ takÄ±m ayrÄ±ca; En Ä°yi Sistem DizaynÄ±, (Ãnceki Seneye GÃ¶re) En GeliÅmiÅ TakÄ±m, En Seksi Tekne (Hottest Looking :P), Ticari DeÄeri En YÃ¼ksek Tekne Ã¶dÃ¼llerinin de sahibi oldu. AyrÄ±ntÄ±lÄ± sonuÃ§lar iÃ§in &lt;a href=&quot;http://www.solarsplash.com/splash/results/event08.html#HEADING5&quot;&gt;tÄ±klayÄ±nÄ±z&lt;/a&gt;. 
Seneye de 1.lik haberlerini duyururum umarÄ±m :) </description>

</item>

<item>
<title>AÃ§Ä±k Akademi YayÄ±nlarÄ±nda en az %50 indirim</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5039</link>
<description>Daha Ã¶nce tasviye sÃ¼recine gireceÄi bildirilen AÃ§Ä±k Akademi
yayÄ±nlarÄ±nda Ã§ok yakÄ±n zamanda son noktaya gelinecektir.
YayÄ±nevi stoklarÄ±nÄ± tÃ¼ketmek iÃ§in kitaplarda son bir indirim daha
yaparak, indirimleri %50 ile %60 seviyesine Ã§Ä±karmÄ±ÅtÄ±r.  &lt;br&gt;</description>
</item>

<item>
<title>Ultra taÅÄ±nabilir dizÃ¼stÃ¼ler iÃ§in Ubuntu</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5038</link>

<description>GÃ¼nÃ¼mÃ¼zÃ¼n en gÃ¶zde trendi, palmdan bÃ¼yÃ¼k, laptoptan kÃ¼Ã§Ã¼k sistemler. BaÅta Asus'un &lt;a href=http://distrowatch.com/images/other/eeepc-compared.png&gt;EEE PC&lt;/a&gt;'si olmak Ã¼zere, &lt;a href=http://distrowatch.com/images/other/aspire-one.png&gt;Acer Aspire One&lt;/a&gt; ve Samsung'un &lt;a href=http://www.ultramobilegeek.com/2008/04/q1-ultra-ubuntu-linux-experiment.html&gt;Q1 Ultra&lt;/a&gt;'sÄ± bu alanda sivrilen Ã¶rnekler.&lt;br&gt;&lt;br&gt; 


Bu geliÅmelerde ÅaÅÄ±rtÄ±cÄ± olan belki de bu bilgisayarlarÄ±n iÅletim sistem seÃ§imleri. Asus gibi bazÄ±larÄ± Linux ile baÅlayÄ±p daha sonra Åirketin genel politikalarÄ± yÃ¼zÃ¼nden olsa gerek &quot;Better with Windows&quot; mottosuyla devam &lt;a href=http://www.ultramobilegeek.com/2008/06/when-is-netbook-not-netbook-when-its.html&gt;etmekte&lt;/a&gt;, bazÄ±larÄ± ise Ã§eÅitli Linux versiyonlarÄ± denemekte.&lt;br&gt;&lt;br&gt;

Buradan yola Ã§Ä±kan Canonical ise Ubuntu ailesinin en yeni Ã¼yesi &lt;a href=http://www.ubuntu.com/news/netbook-remix&gt;Netbook Remix&lt;/a&gt;'i bizle tanÄ±ÅtÄ±rdÄ±.</description>

</item>

<item>
<title>Kor BiliÅim'den DOM Programlama AraÃ§larÄ±</title>
<link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5037</link>
<description>DOM modelinin kÄ±smi bir gerÃ§ekleÅtirimi artÄ±k Core Server projesi ile beraber geliyor. Bu araÃ§la, birinci dereceden DOM nesneleri yaratabiliyor, parametrik DOM Ã¼reten fonksiyonlar tanÄ±mlayabiliyorsunuz. Ãyle ki, tanÄ±mladÄ±ÄÄ±nÄ±z DOM yapÄ±sÄ±nÄ± javascript betiÄine Ã§evirip, web tarayÄ±cÄ±nÄ±n kullanabileceÄi bir hale getirebiliyorsunuz.

Ãrnekler iÃ§in &lt;a href=&quot;http://labs.core.gen.tr/#domprogramming&quot;&gt;Dom Programming&lt;/a&gt; belgesine bakabilirsiniz.</description>
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
    <item>
      <title>IBM TÃÂ¼rk ÃÂalÃÂ±ÃÂanlarÃÂ±nÃÂ±n Sendikal ÃÂrgÃÂ¼tlenmesi</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5044</link>
      <description>Bizler 258 kiÃÂiyle Tez-koop-iÃÂ sendikasÃÂ±na ÃÂ¼ye IBM TÃÂ¼rk ÃÂ§alÃÂ±ÃÂanlarÃÂ±yÃÂ±z.
&lt;br&gt;&lt;br&gt;
SendikamÃÂ±z, toplu sÃÂ¶zleÃÂme yapma hakkÃÂ±mÃÂ±zÃÂ± almak iÃÂ§in 26 Mart 2008 tarihinde ÃÂalÃÂ±ÃÂma BakanlÃÂ±ÃÂÃÂ±na yetki baÃÂvurusunda bulundu.


</description>
    </item>
    <item>
      <title>IBM YazÃÂ±lÃÂ±m Akademisi ilk mezunlarÃÂ±nÃÂ± verdi</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5043</link>
      <description>&lt;p&gt;Akademi kapsamÃÂ±nda yapÃÂ±lan yazÃÂ±lÃÂ±m yarÃÂ±ÃÂmasÃÂ±nda en iyi 3 proje, Galatasaray, Dokuz EylÃÂ¼l ve SabancÃÂ± ÃÂniversitelerinden ÃÂ§ÃÂ±ktÃÂ±.&lt;/p&gt;

&lt;p&gt;IBM TÃÂ¼rkÃ¯Â¿Â½ÃÂ¼n geleceÃÂin yazÃÂ±lÃÂ±mcÃÂ±larÃÂ±nÃÂ± yetiÃÂtirmek ÃÂ¼zere Ekim 2007Ã¯Â¿Â½de baÃÂlattÃÂ±ÃÂÃÂ± Ã¯Â¿Â½IBM YazÃÂ±lÃÂ±m AkademisiÃ¯Â¿Â½nin 2007-2008 ÃÂ¶ÃÂrenim yÃÂ±lÃÂ±, kapsamlÃÂ± eÃÂitim, proje geliÃÂtirme ve yazÃÂ±lÃÂ±m yarÃÂ±ÃÂmasÃÂ± aÃÂamalarÃÂ±ndan sonra sona erdi.&lt;/p&gt;</description>
    </item>
    <item>
      <title>Diablo III Resmen Duyuruldu!</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5042</link>
      <description>Blizzard, geÃÂ§tiÃÂimiz aylarda belki de bilgisayar oyunlarÃÂ± tarihinin gelmiÃÂ geÃÂ§miÃÂ en ÃÂ§ok beklenen iki oyunundan biri olan Starcraft II 'yi duyurmuÃÂtu.
&lt;br&gt;&lt;br&gt;

ÃÂimdi de yÃÂ±llardÃÂ±r beklenen, Diablo nun III. oyunu sonunda blizzard tarafÃÂ±ndan resmen duyuruldu!
&lt;a href=&quot;http://www.blizzard.com/diablo3/&quot;&gt;Diablo III&lt;/a&gt;</description>
    </item>
    <item>
      <title>Pardus 2008</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5041</link>
      <description>&lt;a href=http://www.pardus.org.tr&gt;Pardus 2008&lt;/a&gt; nihayet duyuruldu.&lt;br&gt;

&lt;a href=http://www.ozgurlukicin.com/haber/pardus-2008-yenilikleri/&gt;Pardus 2008 Yenilikleri&lt;/a&gt;&lt;br&gt;
&lt;a href=http://www.pardus.org.tr/indir.html&gt;ÃÂ°ndirme sayfasÃÂ± (ftp ve torrent)&lt;/a&gt;&lt;br&gt;
&lt;a href=http://distrowatch.com/?newsid=04967&gt;distrowatch.com duyuru sayfasÃÂ±&lt;/a&gt;</description>
    </item>
    <item>
      <title>TÃÂ¼rk GÃÂ¼neÃÂ Teknesi Muavenet DÃÂ¼nya 2.si</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5040</link>
      <description>GÃÂ¼neÃÂ enerjisi ile ÃÂ§alÃÂ±ÃÂan teknelerin yarÃÂ±ÃÂtÃÂ±ÃÂÃÂ± &lt;a href=&quot;http://www.solarsplash.com&quot;&gt;Solar Splash&lt;/a&gt; yarÃÂ±ÃÂmasÃÂ±nda geÃÂ§en sene 3. olan &lt;a href=&quot;http://www.solarsplash.itu.edu.tr&quot;&gt;TÃÂ¼rk takÃÂ±mÃÂ±&lt;/a&gt;, bu sene yeni tekneleri Muavenet ile DÃÂ¼nya 2.si oldular. ÃÂ°TÃÂ'lÃÂ¼ takÃÂ±m ayrÃÂ±ca; En ÃÂ°yi Sistem DizaynÃÂ±, (ÃÂnceki Seneye GÃÂ¶re) En GeliÃÂmiÃÂ TakÃÂ±m, En Seksi Tekne (Hottest Looking :P), Ticari DeÃÂeri En YÃÂ¼ksek Tekne ÃÂ¶dÃÂ¼llerinin de sahibi oldu. AyrÃÂ±ntÃÂ±lÃÂ± sonuÃÂ§lar iÃÂ§in &lt;a href=&quot;http://www.solarsplash.com/splash/results/event08.html#HEADING5&quot;&gt;tÃÂ±klayÃÂ±nÃÂ±z&lt;/a&gt;. 
Seneye de 1.lik haberlerini duyururum umarÃÂ±m :) </description>
    </item>
    <item>
      <title>AÃÂ§ÃÂ±k Akademi YayÃÂ±nlarÃÂ±nda en az %50 indirim</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5039</link>
      <description>Daha ÃÂ¶nce tasviye sÃÂ¼recine gireceÃÂi bildirilen AÃÂ§ÃÂ±k Akademi
yayÃÂ±nlarÃÂ±nda ÃÂ§ok yakÃÂ±n zamanda son noktaya gelinecektir.
YayÃÂ±nevi stoklarÃÂ±nÃÂ± tÃÂ¼ketmek iÃÂ§in kitaplarda son bir indirim daha
yaparak, indirimleri %50 ile %60 seviyesine ÃÂ§ÃÂ±karmÃÂ±ÃÂtÃÂ±r.  &lt;br&gt;</description>
    </item>
    <item>
      <title>Ultra taÃÂÃÂ±nabilir dizÃÂ¼stÃÂ¼ler iÃÂ§in Ubuntu</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5038</link>
      <description>GÃÂ¼nÃÂ¼mÃÂ¼zÃÂ¼n en gÃÂ¶zde trendi, palmdan bÃÂ¼yÃÂ¼k, laptoptan kÃÂ¼ÃÂ§ÃÂ¼k sistemler. BaÃÂta Asus'un &lt;a href=http://distrowatch.com/images/other/eeepc-compared.png&gt;EEE PC&lt;/a&gt;'si olmak ÃÂ¼zere, &lt;a href=http://distrowatch.com/images/other/aspire-one.png&gt;Acer Aspire One&lt;/a&gt; ve Samsung'un &lt;a href=http://www.ultramobilegeek.com/2008/04/q1-ultra-ubuntu-linux-experiment.html&gt;Q1 Ultra&lt;/a&gt;'sÃÂ± bu alanda sivrilen ÃÂ¶rnekler.&lt;br&gt;&lt;br&gt; 


Bu geliÃÂmelerde ÃÂaÃÂÃÂ±rtÃÂ±cÃÂ± olan belki de bu bilgisayarlarÃÂ±n iÃÂletim sistem seÃÂ§imleri. Asus gibi bazÃÂ±larÃÂ± Linux ile baÃÂlayÃÂ±p daha sonra ÃÂirketin genel politikalarÃÂ± yÃÂ¼zÃÂ¼nden olsa gerek &quot;Better with Windows&quot; mottosuyla devam &lt;a href=http://www.ultramobilegeek.com/2008/06/when-is-netbook-not-netbook-when-its.html&gt;etmekte&lt;/a&gt;, bazÃÂ±larÃÂ± ise ÃÂ§eÃÂitli Linux versiyonlarÃÂ± denemekte.&lt;br&gt;&lt;br&gt;

Buradan yola ÃÂ§ÃÂ±kan Canonical ise Ubuntu ailesinin en yeni ÃÂ¼yesi &lt;a href=http://www.ubuntu.com/news/netbook-remix&gt;Netbook Remix&lt;/a&gt;'i bizle tanÃÂ±ÃÂtÃÂ±rdÃÂ±.</description>
    </item>
    <item>
      <title>Kor BiliÃÂim'den DOM Programlama AraÃÂ§larÃÂ±</title>
      <link>http://www.fazlamesai.net/index.php?a=article&amp;sid=5037</link>
      <description>DOM modelinin kÃÂ±smi bir gerÃÂ§ekleÃÂtirimi artÃÂ±k Core Server projesi ile beraber geliyor. Bu araÃÂ§la, birinci dereceden DOM nesneleri yaratabiliyor, parametrik DOM ÃÂ¼reten fonksiyonlar tanÃÂ±mlayabiliyorsunuz. ÃÂyle ki, tanÃÂ±mladÃÂ±ÃÂÃÂ±nÃÂ±z DOM yapÃÂ±sÃÂ±nÃÂ± javascript betiÃÂine ÃÂ§evirip, web tarayÃÂ±cÃÂ±nÃÂ±n kullanabileceÃÂi bir hale getirebiliyorsunuz.

ÃÂrnekler iÃÂ§in &lt;a href=&quot;http://labs.core.gen.tr/#domprogramming&quot;&gt;Dom Programming&lt;/a&gt; belgesine bakabilirsiniz.</description>
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