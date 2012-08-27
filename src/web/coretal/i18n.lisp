(in-package :core-server)

;; -------------------------------------------------------------------------
;; Language Pack
;; -------------------------------------------------------------------------
(defcomponent <core:language-pack ()
  ((pack :host remote :initform nil)
   (name :host remote :initform "en" :print t))
  (:ctor <core:language-pack))

(defmethod/remote init ((self <core:language-pack))
  (when (null (slot-value +language-data+ (name self)))
    (setf (slot-value +language-data+ (name self)) (jobject)))
  
  (let ((data (slot-value +language-data+ (name self))))
    (mapcar (lambda (a) (setf (slot-value data (car a)) (car (cdr a))))
	    (pack self))))

;; -------------------------------------------------------------------------
;; Turkish Language Pack
;; -------------------------------------------------------------------------
(defparameter +tr-data+
  (list (cons "About Coretal.net" "Coretal.net Hakkında")
	(cons "Learn more about our services"
	      "Hizmetlerimiz hakkında bilgi edinin")
	(cons "Close" "Kapat")
	(cons "Hide this taskbar" "Bu paneli gizle")
	(cons "Console" "Konsol")
	(cons "Show log console" "Kayıtları göster")
	(cons "Log Console" "Kayıtlar geçmişi")
	(cons "close" "kapat")

	;; Months
	(cons "January" "Ocak")
	(cons "February" "Şubat")
	(cons "March" "Mart")
	(cons "April" "Nisan")
	(cons "May" "Mayıs")
	(cons "June" "Haziran")
	(cons "July" "Temmuz")
	(cons "August" "Ağustos")
	(cons "September" "Eylül")
	(cons "October" "Ekim")
	(cons "November" "Kasım")
	(cons "December" "Aralık")
	
	;; UI/Sidebar
	(cons "Do you want to discard changes?"
	      "Değişiklikleri göz ardı etmek istyor musunuz?")
	(cons "close this sidebar" "bu paneli kapat")
	
	;; Demo Plugin
	(cons (concatenate 'string
			   "This demo requires password of the site owner. "
			   "Please enter password in order to "
			   "load the demo.")
	      (concatenate 'string
			   "Bu uygulama test aşamasındadır. "
			   "Eğer sitenin sahibi iseniz, parolanızı girip"
			   " test işlemlerini gerçekleştirebilirsiniz."))
	;; Authentication Plugin
	(cons "Login or Register" "Kayıt ve Giriş")
	(cons "Open a new session" "Sistemde oturum açın")
	(cons "Welcome" "Hoşgeldin")
	(cons "you are now logged-in" "sisteme giriş onaylandı")
	(cons "Change Password" "Parolamı Değiştir")
	(cons "Change your password" "Parolanızı güncelleyin")
	(cons "Logout" "Çıkış")
	(cons "End your session" "Oturumu kapat")
	(cons "Username" "İsim, Soyisim")
	(cons "Email" "Eposta")
	(cons "Enter your full name" "İsminizi ve soyisminizi girin")
	(cons "Enter your email address" "Eposta adresinizi girin")
	(cons "Enter your password" "Parolanızı girin")
	(cons "Re-enter your password" "Parolanızı tekrar girin")
	(cons "agreement" "Sözleşme")
	(cons "I accept the terms in the" "metnindeki hususları kabul ediyorum")
	(cons "Register" "Kayıt ol")
	(cons "Login" "Oturum Aç")
	(cons "Cancel" "İptal")
	(cons "forgot password?" "unuttunuz mu?")
	(cons "Send my password" "Parolamı gönder")
	(cons "register" "kayıt ol")
	(cons "login" "oturum aç")
	(cons "Sign in with" "Sosyal medya ile oturum açın")
	(cons "Old Password" "Eski Parola")
	(cons "New Password" "Yeni Parola")
	(cons "New Password (verify)" "Yeni Parola (onay)")
	(cons "Change" "Güncelle")
	(cons "retry" "hata")
	(cons "Two passwords do not match." "Parolalar birbirini tutmuyor.")
	(cons "Do you want to retry?" "Tekrar denemek istiyor musunuz?")
	(cons "An error occurred." "Üzgünüm, sistemde bir hata oluştu.")
	(cons "thank you" "teşekkürler")
	(cons "Your credentials has been updated." "Parolanız güncellendi.")
	(cons "Thank you" "Teşekkürler")
	(cons "Yes" "Evet")
	(cons "No" "Hayır")
	(cons "Your password is too short." "Parolanız çok kısa.")
	(cons "Your email is invalid." "Eposta adresiniz geçersiz.")
	(cons "This field is required." "Bu alan zorunlu.")
	(cons "This box must be checked." "Bu kutu işaretli olmalı.")
	(cons "This field is required." "Bu alan zorunludur.")
	(cons "%1 is not a number." "%1 değeri sayı olmalıdır.")
	(cons "Enter a number" "Bir sayı girin")
	(cons "Enter a date" "Tarih seçin")

	;; Authentication Manager
	(cons "Two passwords do not match."
	      "Girdiğiniz iki parola birbirinden farklı.")
	(cons "Do you want to retry?" "Tekrar denemek ister misiniz?")
	(cons "Your credentials has been updated." "Parolanız güncellendi.")
	(cons "Thank you." "Teşekkür ederiz.")
	(cons "Authentication failed." "Giriş reddedildi.")
	(cons "Sorry, this email is already registered on our system."
	      "Üzgünüm, bu eposta sistemde kayıtlı.")
	(cons "error" "hata")
	
	;; Feedback Plugin
	(cons "Feedback" "Geri bildirim")
	(cons "Give us feedback or report a bug"
	      "Öneri ve/veya şikayetlerinizi bildirin")
	(cons "Your feedback has been sent" "İstekleriniz kaydedildi")
	(cons "Thank you for your feedback" "Teşekkür ederiz")
	(cons "Feedback Console" "Geri Bildirim Paneli")
	(cons "Enter your name" "İsminiz")
	(cons "Enter your email" "Eposta adresiniz")
	(cons "Enter feedback subject" "İstek konusu")
	(cons "Date" "Tarih")
	(cons "Current URL" "URL")
	(cons "Browser" "İstemci yazılımı")
	(cons "Name" "İsim")	
	(cons "Subject" "Konu")
	(cons "Feedback" "Geri Bildirim")
	(cons "Send" "Kaydet")
	(cons "Please fill the form below."
	      "Lütfen aşağıdaki formu doldurun.")
	(cons "Thank you for your patience and cooperation."
	      "Yardımlarınız ve işbiriliğiniz için teşekkür ederiz.")
	(cons "Please enter your feedback, question, or problem and please be descriptive."
	      "Lütfen istek ve/veya probleminizi açıklayıcı olarak giriniz.")
	(cons "Remember that we read all inquiries and it takes time to resolve an issue."
	      "Her iletiyi büyük bir titizlikle değerlendiriyoruz. Unutmayınız ki, değerlendirme süreci zaman alıyor. ")

	;; Page Plugin
	;; Plugin
	(cons "Page %1 is loaded." "%1 sayfası yüklendi.")
	(cons "Sorry, page %1 is not found." "Üzgünüm, %1 sayfası bulunamadı.")
	(cons "Configure Template" "Şablonu Düzenle")
	(cons "Configure common widgets" "Şablon ayaları, genel widgetlar")
	(cons "Page %1 is not found." "%1 sayfası bulunamadı.")
	(cons "Do you want to create?" "Yenisini oluşturmak ister misiniz?")
	(cons "new page" "yeni sayfa")
	(cons "Please enter a name for the new page:"
	      "Yeni sayfanın ismini girin:")
	(cons "New Page" "Yeni Sayfa")
	(cons "Create a new page" "Yeni sayfalar oluşturun")
	(cons "Pages" "Sayfalar")
	(cons "Open a page on this site" "Sitedeki tüm sayfalar")
	;; Page
	(cons "Last modified on" "Son değişiklik")
	(cons "Page" "Sayfa")
	(cons "copy" "kopya")
	(cons "Please enter a new name for this page:"
	      "Sayfanın yeni ismini girin:")
	(cons "Page %1 is renamed to %2."
	      "%1 sayfası, %2 olarak adlandırıldı.")
	(cons "delete" "sil")
	(cons "Do you really want to delete page %1?"
	      "%1 sayfasını silmek üzeresiniz. Devam etmek istiyor musunuz?")
	(cons "Page %1 is deleted." "%1 sayfası silindi.")
	(cons "Please enter a name for the copy of this page:"
	      "Sayfanın kopyası için yeni isim girin:")
	(cons "Page %1 is copied to %2."
	      "%1 sayfası %2 sayfası olarak kopyalandı.")
	(cons "This page is" "Bu sayfa")
	(cons "published" "yayımda")
	(cons "not published" "yayımda değil")
	(cons "Rename Page" "Yeniden Adlandır")
	(cons "Rename this page" "Bu sayfayı yeniden adlandırın")
	(cons "Copy Page" "Kopyala")
	(cons "Make a new copy of this page" "Sayfanın kopyasını oluşturun")
	(cons "Delete Page" "Sayfayı Sil")
	(cons "Delete this page" "Bu sayfayı tümüyle kaldırın")
	(cons "Page %1 is published now." "%1 sayfası şu an yayımda.")
	(cons "Page %1 is unpublished." "%1 sayfası yayımdan kaldırıldı.")
	(cons "Add Widget" "Widget Ekle")
	(cons "Add a widget to the current page"
	      "Sayfaya uygulamalar ekleyin")
	(cons "Publish/Unpublish" "Yayıma Al/Kaldır")
	(cons "Publish or unpublish current page"
	      "Sayfayı yayıma alın veya yayımdan kaldırın")
	(cons "All Revisions" "Tüm Revizyonlar")
	(cons "Revisions of this page" "Sayfaya ait revizyonların listesi")
	(cons "Published" "Yayımda")
	;; Template
	(cons "Template" "Şablon")
	(cons "Add a widget to this template"
	      "Şablona yeni uygulama ekleyin")
	(cons "Return back to page editing" "Sayfa görünümüne geri dönün")
	(cons "Revisions of this template"
	      "Şablona ait tüm revizyonların listesi")
	;; Widget Mapping
	(cons "Relocate" "Konumlandır")
	(cons "Change visual place of this widget"
	      "Uygulamayı sayfada başka yere taşıyın")
	(cons "This widget is disabled temporarily"
	      "Bu uygulama geçici olarak devre dışı")
	(cons "please relocate or remove."
	      "uygulamayı yeniden konumlandırın veya kaldırın.")
	(cons "remove" "kaldır")
	(cons "Do you really want to remove this widget?"
	      "Bu uygulamayı kaldırmak istiyor musunuz?")
	(cons "Remove" "Kaldır")
	(cons "Remove this widget from the current page"
	      "Bu uygulamayı sayfadan kaldırın")
	(cons "Stick/Unstick" "Yapıştır/Sök")
	(cons "Make this widget global (template) or local (page)"
	      "Bu uygulama tüm sayfalarda olsun yada olmasın")
	(cons "Security Configuration" "Güvenlik Yapılandırması")
	(cons "Configure security rules of this widget"
	      "Bu uygulamanin güvenlik ayarlarını oluşturun")
	;; Revision
	(cons "Revision" "Revizyon")
	(cons "revert" "geri al")
	(cons "Do you really want to revert to this revision?"
	      "Bu sayfanın eski sürümüne dönem istiyor musunuz?")
	(cons "Warning: All data in the [Latest Revision] will be lost."
	      "Uyarı: Son sürümdeki değişiklikler kaybolabilir.")
	(cons "Revert" "Geri Al")
	(cons "Revert page to this revision"
	      "Sayfanin bu sürümüne geri dönün")
	(cons "Publish" "Yayımla")
	(cons "Publish a snapshot of this page"
	      "Bu sayfanın yeni sürümünü yayıma alın")
	(cons "snapshot" "yeni revizyon")
	(cons "Please enter a name for the snapshot of this page."
	      "Yeni revizyon için isim girin.")
	(cons "publish" "yayıma al")
	(cons "Publish a snapshot of this page"
	      "Sayfanın yeni revizyonunu alıp yayımlayın")
	(cons "Snapshot" "Yeni Revizyon")
	(cons "Create a new snapshot"
	      "Sayfanın anlık yeni revizyonunu oluşturun")
	(cons "Add a widget to this revision"
	      "Bu revizyona yeni uygulamalar ekleyin")

	;; Example
	(cons "Basic" "Temel")
	(cons "Basic example pages" "Temel sayfa örnekleri")
	(cons "Front Page" "Ana Sayfa")
	(cons "Front page examples for your site"
	      "Sitenizin ana sayfası için örnekler")

	;; Widget
	(cons "Basic Widgets" "Temel Widgetlar")
	(cons "Basic building blocks of a site"
	      "Sitenizin temel yapıtaşları")
	(cons "Widgets for your front page"
	      "Ana sayfanız için widgetlar")
	(cons "Navigation" "Navigasyon")
	(cons "Widgets designed for site navigation"
	      "Site navigasyonu için geliştirilmiş widgetlar")
	(cons "Data Collection" "Veri Toplama")
	(cons "Widgets providing forms" "Formlar, oylama vb.")
	(cons "Social Networks" "Sosyal Medya")
	(cons "Integrate your site with social networks"
	      "Sosyal medya ile entegre olun")
	(cons "Other" "Diğer")
	(cons "More Coretal.net widget"
	      "Siteniz için geliştirilmiş diğer widgetlar")
	
	;; UI/Widget Node Selector
	(cons "Widget Location" "Widget Konumu")
	(cons "Please select a location from below."
	      "Lütfen aşağıdan widget için konum seçin.")
	;; UI/Security
	(cons "Object Ownership" "Nesnenin Sahipliği")
	(cons "Owner" "Sahip")
	(cons "Group" "Grubu")
	(cons "Permissions" "Hak ve Hukuk")
	(cons "Owner Permissions" "Sahip Hakları")
	(cons "Group Permissions" "Grup Hakları")
	(cons "Other Permissions" "Kayıtlı Kullanıcı Hakları")
	(cons "Anonymous Permissions" "Anonim Kullanıcı Hakları")
	(cons "Save" "Kaydet")
	(cons "Object Security Configuration"
	      "Güvenli Nesne Yapılandırması")
	(cons "Please select the options from below."
	      "Lütfen aşağıdaki seçenekleri değerlendirin.")
	;; UI/Widget Selector
	(cons "No screenshots are available."
	      "Üzgünüm, bu widget'a ait ekran görüntüsü bulunmuyor.")
	(cons "Widget Documentation" "Widget Belgelendirmesi")
	(cons "Add this widget" "Bu widgetı ekle")
	(cons "Description" "Açıklama ve Tanımlar")
	(cons "Widgets" "Widgetlar")
	(cons "Popular Widgets" "Popüler Widgetlar")
	(cons "add widget" "widget ekle")
	(cons "Screenshots" "Ekran Görüntüleri")
	(cons "References" "İlgili Bağlantılar")
	;; UI/New Page Dialog
	(cons "create a new page" "yeni sayfa oluşturun")
	(cons "Create this page" "Bu sayfadan oluştur")
	(cons "Examples" "Örnekler")
	(cons "A New Page" "Yeni Sayfalar Oluşturun")
	(cons "Hello" "Merhaba")
	(cons "In this section, you can create new page on your site."
	      "Bu bölümde, sitenizde yeni sayfalar oluşturabilirsiniz.")
	(cons "We have prepared example pages for you to build your site quickly."
	      "Sayfalarınızı kolayca oluşturabilmeniz için, çeşitli örnekler hazırladık.")
	(cons "They are listed on the left column of this dialog."
	      "Hazırladığımız örnekler, sayfanın sol tarafında yer almakta.")
	(cons "Most of the example page that we have prepared for you, have built-in widgets for your sites specific needs."
	      "Örneklerimiz, ihtiyaçlarınızı karşılamak amacıyla bir araya getirilmiş widgetlardan oluşmaktadır.")
	(cons "You can read more about them by clicking on the specific example."
	      "Detaylara ulaşmak için sol kolondaki örneklere tıklayabilirsiniz.")
	(cons "Moreover, you will be able to add more widgets after you create a new page."
	      "Unutmayınız ki, yeni sayfanızı örneklerden oluşturduktan sonra, kalan ihtiyaçlarınız için yeni widgetlar ekleyebilirsiniz.")
	(cons "If you are unfamiliar with managing pages on your site, please refer to "
	      "Eğer sayfaları nasıl yönetebileceğinizden emin değiseniz, ilgili belgelere göz atabilirsiniz, ")
	(cons "Coretal.net Site Editor's Guide"
	      "Coretal.net Site Editörü Klavuzu")
	;; Page Plugin/Widgets/Content
	(cons "Content" "İçerik")
	(cons "Cancel preview" "Ön-izlemeyi kapat")
	(cons "Return back to editing" "Düzenlemeye geri dönün")
	(cons "Cancel Editing" "Düzenlemeleri iptal et")
	(cons "Cancel without saving changes"
	      "Değişiklikleri göz ardı edin")
	(cons "Save Content" "İçeriği Kaydet")
	(cons "Save contents of this widget" "Değişiklikleri kaydedin")
	(cons "Preview Changes" "Önizle")
	(cons "Preview changes uptill now" "Değişiklikleri gözleyin")
	(cons "Edit Content" "İçeriği Düzenle")
	(cons "Edit contents of this widget"
	      "İçeriği güncelleyin, değiştirin")
	;; Page Plugin/Widgets/Comment
	(cons "Anonymous" "Anonim")
	(cons "Do you really want to delete this comment?"
	      "Bu yorumu silmek istiyor musunuz?")
	(cons "( %1 ) said on %2" "( %1 ) %2 tarihinde demiş ki:")
	(cons "Delete" "Sil")
	(cons "Comment(s)" "Yorum")
	(cons "Thank you. Your post is waiting to be approved."
	      "Teşekkürler, yorumunuz onay için bekliyor.")
	(cons "Thank you for your post."
	      "Teşekkürler, yorumunuz kaydedildi.")
	(cons "Type your comment here." "Yorumunuzu buraya yazın.")
	(cons "Post Comment" "Yorum gönderin")
	(cons "or" "yada")
	(cons "Login with" "Sosyal medya ile bağlan:")
	(cons "Please login to subscribe" "Takip etmek için bağlanın")
	(cons "Thank you, you are now subscribed."
	      "Teşekkürler, artık takip etmeye başladınız.")
	(cons "Thank you, you are now unsubscribed."
	      "Teşekkürler, takip listesinden çıkarıldınız.")
	(cons "Subscribe by email" "Eposta ile takip edeyim")
	(cons "Unsubscribe" "Artık takip etmek istemiyorum")
	(cons "Thank you." "Teşekkürler.")
	(cons "Post" "Gönder")
	(cons "Configure Comments" "Yorumları yapılandır")
	(cons "Configure this comment widget"
	      "Yorum ayarlarını, onayları düzenleyin")
	(cons "Comments Configuration" "Yorum Yapılandırması")
	(cons "Please configure the options from below."
	      "Lütfen aşağıdaki yapılandırmaları düzenleyin.")
	(cons "Show comments by default" "Yorumları açılışta göster")
	(cons "Increases page load time" "Sayfa geç yüklenebilir")
	(cons "Only authenticated users can comment"
	      "Sadece kayıtllı kullanıcılar yorumlasın")
	(cons "Otherwise, everybody can comment"
	      "Aksi takdirde, herkes yorum yapabilir")
	(cons "Comments are moderated" "Yorumlar onaylansın")
	(cons "Editors need to approve them"
	      "Editörlerin her yorumu onaylamasını gerektirir")
	;; Plugin/Page/Widgets/Form
	(cons "Your form has been submitted, thank you."
	      "İletiniz kaydedildi, teşekkür ederiz.")
	(cons "An error occured while completing your request. Sorry for inconvenience."
	      "Özür dileriz, isteğinizi gerçekleştirirken bir hata oluştu.")
	(cons "Edit Validations" "Geçerlilik Yapılandırması")
	(cons "Edit form element validations"
	      "Form alanlarının geçerliliğini kontrol edin")
	(cons "Form Actions" "Form İşlevleri")
	(cons "Configure form action" "Formun işlevini belirleyin")
	(cons "Email address" "Eposta adresi")
	(cons "Enter an email" "Eposta adresi girin")
	(cons "Action" "İşlev")
	(cons "No action" "İşlevsiz")
	(cons "Email this form" "Bu formu eposta ile ilet")
	(cons "Form Action Configuration" "Form İşlev Yapılandırması")
	(cons "Field Configuration" "Alan Yapılandırmaları")
	(cons "Validation" "Geçerlilik testi")
	(cons "none" "hiçbiri")
	(cons "email" "eposta")
	(cons "password" "parola")
	(cons "required" "zorunlu")
	(cons "Validation Configuration" "Geçerlilik Yapılandırmaları")
	(cons "Fields" "Alanlar")
	(cons "Click to configure a field."
	      "Alanın geçerlilik testini yapılandırmak için üzerine tıklayın.")
	;; Plugin/Page/Widgets/Filter
	(cons "Filters" "Filtreler")
	(cons "Configure filters" "Filtrelerini ayarlayın")
	(cons "Filter by" "Aşağıdaki kritere göre filtre uygula")
	(cons " none (all items)" "Filtre yok, hepsini göster")
	(cons " query strings" "Sadece bu kelimeleri içerenler")
	(cons "Filter Configuration" "Filtreler")
	
	;; Plugin/Page/Widgets/List
	(cons "A %1 widget" "Bir %1 widgetı")
	(cons "No %1(s) found." "Hiç %1 bulunamadı.")
	(cons "More %1(s)" "Daha fazla %1 göster")
	(cons "List Configuration" "Liste Ayarları")
	(cons "Listing mode, max items etc."
	      "Listeleme türü, eleman sayısı vb.")
	(cons "All list items" "Tüm liste")
	(cons "sep. by spaces" "boşluklarla ayrılmış")
	(cons "Number of items" "eleman sayısı")
	(cons "Continuous" "Devamı yüklenen")
	(cons "Page View" "Sayfalara bölünmüş halde")
	(cons "Number of items to show" "Bu kadar eleman göster")
	(cons "Listing mode" "Listeleme türü")
	
	;; Plugin/Page/Widgets/Map
	;; empty
	
	;; Plugin/Page/Widgets/Menu
	(cons "Configure Navigation" "Menü Yapılandırması")
	(cons "Configure & order links on this navigation"
	      "Bağlantıları düzenleyin, sıralayın")
	(cons "Link Configuration" "Yönlendirme Yapılandırması")
	(cons "Link text" "Bağlantı yazısı")
	(cons "Map to.." "Tıklandığında..")
	(cons " an existing document" " mevcut sayfaya yönlensin")
	(cons " a page on internet" " internetteki bir sayfaya yönlensin")
	(cons "Load.." "Tıklandığında..")
	(cons "in current page" "mevcut pencerede açılsın")
	(cons "in new page" "yeni pencerede açılsın")
	(cons "Remove this link" "Bu bağlantıyı kaldır")
	(cons "Please click a link to configure."
	      "Yapılandırmak için bağlantıya tıklayın.")
	(cons "Page1" "Sayfa1")
	(cons "Page2" "Sayfa2")
	(cons "Menu Configuration" "Menü Yapılandırması")
	(cons "New Menu Item" "Yeni Bağlantı")
	(cons "Menu highlight class name"
	      "Menü vurgu sınıfı ismi (Hilight className)")
	(cons "Drag and drop to re-order menu items."
	      "Sıralamak için sürükleyip bırakmayı kullanın.")
	(cons "Click on a link to change properties."
	      "Özelliklerini düzenlemek için üzerine tıklayın.")

	;; Plugin/Page/Widgets/Picasa
	(cons "Picasa image gallery at %1 is loaded"
	      "Picasa resim galerisi yüklendi. (%1)")
	(cons "Please configure to enable this image gallery."
	      "Resim galerisini çalıştırmak için lütfen yapılandırın.")
	(cons "Configure" "Yapılandır")
	(cons "Configure this Picasa Image Gallery"
	      "Picasa resim galerisini yapılandırın")
	(cons "Enter username" "Kullanıcı adı")
	(cons "Small" "Küçük")
	(cons "Medium" "Orta")
	(cons "Large" "Büyük")
	(cons "Picasa Configuration" "Picasa Yapılandırması")
	(cons "Picasa Username:" "Picasa Kullanıcı Adı:")
	(cons "Picasa Album:" "Picasa Albümü:")
	(cons "Thumbnail size:" "Ufak resim boyutu:")

	;; Plugin/Page/Widgets/Picasa Carousel
	(cons "Carousel Configuration" "Carousel Yapılandırması")
	(cons "Configure carousel of this widget" "Bu carouseli yapılandırın")
	(cons "Configure Carousel" "Carousel Yapılandırması")
	(cons "# of items" "Resim adedi")
	(cons "# of scrolls" "Geçişlerdeki resim adedi")
	(cons "Number of items:" "Gösterilen resim adedi:")
	(cons "Number of scrolls:" "Geçişlerdeki resim adedi:")
	(cons "Scroll speed:" "Geçiş hızı:")
	(cons "Orientation:" "Yerleşim planı:")
	(cons "Top - Horizontal" "Tepede - Yatay")
	(cons "Bottom - Horizontal" "Altta - Yatay")
	(cons "Left - Vertical" "Solda - Dikey")
	(cons "Right - Vertical" "Sağda - Dikey")
	(cons "Fast" "Hızlı")
	(cons "Normal" "Normal")
	(cons "Slow" "Yavaş")
	
	;; Plugin/Page/Widgets/Slider
	(cons "Configure this Orbit Slider"
	      "Orbit slider widgetı yapılandırın")
	(cons "Orbit Configuration" "Orbit Yapılandırması")
	(cons "Animation type:" "Animasyon türü:")
	(cons "Animation speed:" "Animasyon hızı:")
	(cons "Advance speed:" "Geçiş hızı:")
	(cons "Pause on hover?" "Üzerine geldiğinde dursun")
	(cons "Directional navigation?" "Geçiş düğmeleri gözüksün")

	;; Plugin/Page/Widgets/Social
	(cons "Configure this Facebook Share widget"
	      "Facebook Paylaşım widgetını yapılandırın")
	(cons "Facebook Configuration" "Facebook Yapılandırması")
	(cons "Configure this Twitter Share widget"
	      "Twitter Paylaşım widgetını yapılandırın")
	(cons "Twitter Configuration" "Twitter Yapılandırması")
	(cons "Social Configuration" "Sosyal Paylaşım Yapılandırması")
	(cons "When clicked.." "Tıklandığında..")
	(cons " share the current page" "buludunğu sayfayı paylaşsın")
	(cons " share the following page:"
	      " işaret ettiğim sayfayı paylaşsın")
	(cons "Share this page on %1" "Bu sayfayı %1 üzerinde paylaş")
	(cons "Configure social sharing settings"
	      "Sosyal paylaşım yapılandırmasını düzenleyin")
	(cons "Social Share Configuration"
	      "Sosyal Paylaşım Yapılandırması")
	(cons "Widget title:" "Başlık yazısı:")
	(cons "Select networks from below."
	      "Kullanmak istediğiniz sosyal ağları seçin.")

	;; Plugin/Page/Widgets/Tab
	(cons "Configure this Tab Widget" "Tab widgetı yapılandırın")
	(cons "Tab Widget Configuration" "Tab Widget Yapılandırması")
	(cons "Tab position:" "Tab pozisyonu")
	(cons "Hilight class:" "Vurgu sınıfı ismi (Hilight className):")
	(cons "Switch event:" "Geçiş türü:")
	(cons "onmouseover" "üzerine gelindiğinde")
	(cons "onclick" "tıklandığında")
	(cons "on the left" "sağda")
	(cons "on the right" "solda")
	(cons "at the top" "tepede")
	(cons "at the bottom" "altta")

	;; Plugin/Page/Widgets/Template
	(cons "An error occurred while saving." "Kaydederken hata oluştu.")
	(cons "Sorry, an error occurred." "Üzgünüm, hata oluştu.")
	(cons "Save Template" "Şablonu Kaydet")
	(cons "Save template of this widget" "Widget şablonunu kaydedin")
	(cons "Widget Template" "Widget Şablonu")
	(cons "Edit Widget Template" "Şablonu Düzenle")
	(cons "Edit HTML template of this widget"
	      "HTML şablonu düzenleyin")

	;; Plugin/Page/Widgets/Ticker
	(cons "Sorry, ticker does not have any ticks yet."
	      "Üzgünüm, ticker şu anda boş.")
	(cons "Configure this Ticker widget"
	      "Ticker widgetıni yapılandırın")
	(cons "Ticker Configuration" "Ticker Yapılandırması")
	(cons "Ticker Rate (ms):" "Hız (ms):")
	(cons "Start Delay (ms):" "Başlangıç Duraksaması (ms):")
	(cons "Loop Delay (ms):" "Turlar arası Duraksama (ms):")
	(cons "Place Holder 1:" "Yardımcı karakter 1:")
	(cons "Place Holder 2:" "Yarcımdı karakter 2:")

	;; Plugin/Page/Widgets/Twitter
	(cons "Configure Twitter settings"
	      "Twitter ayarlarini yapilandirin")
	(cons "Show All Twits" "Tüm Twit'leri Göster")
	(cons "Twitter Configuration" "Twitter Yapılandırması")
	(cons "Twitter Username:" "Twitter Kullanıcı Adı:")
	(cons "List:" "Liste:")
	(cons "Poll for new results"
	      "Yeni sonuçları otomatik çeksin")
	(cons "Include scrollbar" "Kaydırma çubuğu gözüksün")
	(cons "Show Timestamps" "Tarihleri göstersin")
	(cons "Show Avatars" "Kişi resimlerini göstersin")
	(cons "Show Hash Tags" "Konuları göstersin (Hash Tags)")
	(cons "Shell background color" "Kutu arka plan rengi")
	(cons "Shell text color" "Kutu yazı rengi")
	(cons "Tweet background color" "Twit arka plan rengi")
	(cons "Tweet text color" "Twit yazı rengi")
	(cons "Link color" "Bağlantı yazı rengi")
	(cons "Height" "Yükseklik")
	(cons "Width" "Genişlik")

	;; Plugin/Blog/Plugin
	(cons "Please enter a name for the new blog:" "Yeni blog isim girin")
	(cons "New Blog" "Yeni Blog")
	(cons "Blog" "Blog")
	(cons "Manage blogs, add a new one" "Yeni blog ekleyin, diğerlerini yönetin")
	(cons "Create a new blog" "Yeni Blog Ekle")
	;; Plugin/Blog/Example
	(cons "Blog plugin examples" "Blog Plugin Örnekleri")
	(cons "A Blog Page" "Blog Sayfası")
	(cons "Blog Index Page" "Blog Ana Sayfası")
	(cons "An index page for your blog" "BLogunuz için ana sayfa")

	;; Plugin/Blog/Archive
	(cons "Blog Archive" "Bloglar Arşivi")
	;; Plugin/Blog/Blog
	(cons "Enter a title for this blog" "Bu blog için başlık girin")
	(cons "Enter tags seperated by spaces" "Etiketleri boşluklu yazın")
	(cons "Edit Blog" "Düzenle")
	(cons "Edit this blog" "Blog'u düzenleyin, güncelleyin")
	;; !Plugin/Blog/List
	(cons "Blog List" "Bloglar")
	(cons "Configure list settings" "Liste yapılandırması")
	(cons "Filters, number of items, etc."
	      "Filtreler, gösterilen sayısı vb.")
	(cons "Edit blog template" "Blog Şablonu")
	(cons "Make blogs look pretty or awesome"
	      "Blogların görünümünü düzenleyin")
	(cons "Blog List Configuration" "Blog Listesi Ayarları")
	(cons "Enter tags sep. by spaces" "Etiketleri boşluklu yazın")
	(cons "Enter search string to query" "Arama terimi")
	;; Plugin/Blog/Search
	(cons "Blog Search" "Blog Arama")
	(cons "Enter search string" "Arama kriterini girin")
	;; PLugin/Blog/Stats
	(cons "Blog Stats" "Blog İst.")
	(cons "Serving %1 blogs from %2 authors."
	      "%2 yazardan toplam %1 blog sunulmakta.")
	(cons "Latest" "Son")
	(cons "is written by %1 on %2."
	      "%1 tarafindan %2 tarihinde yazıldı.")

	;; Plugin/Blog/Tags
	(cons "Blog Tags" "Blog Etkiketleri")
	(cons "No tags found." "Hiç etiket yok.")

	;; Plugin/Blog/Filter
	(cons "Blog Filter Configuration" "Blog Filtreleri")
	(cons "blog tags" "blog etiketleri")
	;; Plugin/Profile/Picture
	(cons "Picture" "Resim")
	))

(defparameter +language-packs+
  (list (<core:language-pack :name "en")
	(<core:language-pack :name "tr" :pack +tr-data+)))

(defparameter +default-language-pack+ (car +language-packs+))

(defun find-language-pack (lang)
  (find lang +language-packs+ :test #'string= :key #'name))