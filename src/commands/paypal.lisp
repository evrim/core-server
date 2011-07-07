(in-package :core-server)

;; +-------------------------------------------------------------------------
;; | PayPal Interface
;; +-------------------------------------------------------------------------
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: July 2011

;; Reference:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_NVPAPIOverview

;; API Servers for API Signature Security
;; If you use an API signature, post the request to one of these servers:

;; Sandbox: https://api-3t.sandbox.paypal.com/nvp
;; Live: https://api-3t.paypal.com/nvp

;; API Servers for API Certificate Security
;; If you use an API certificate, post the request to one of these servers:

;; Sandbox: https://api.sandbox.paypal.com/nvp
;; Live: https://api.paypal.com/nvp

;; Test AccountDate Created
;; Jun. 13, 2011 14:35:36 PDT

;; Test Account:seller_1308000922_biz@core.gen.tr
;; API Username:seller_1308000922_biz_api1.core.gen.tr
;; API Password:1308000936
;;    Signature:ASN4WxQZ7F3NHI77BpZO.40yTYXrACLrN89a7RJHuhI3K.7Cce1LVPbC

;; -------------------------------------------------------------------------
;; Base PayPal Command
;; -------------------------------------------------------------------------
(defcommand paypal (http)
  ((username :host local :initform (error "Please provide :username"))
   (password :host local :initform (error "Please provide :password"))
   (signature :host local :initform (error "Please provide :signature"))
   (api-method :host local :initform (error "Please provide :api-method")))
  (:default-initargs :url "https://api-3t.sandbox.paypal.com/nvp"
    :username "seller_1308000922_biz_api1.core.gen.tr"
    :password "1308000936"
    :signature "ASN4WxQZ7F3NHI77BpZO.40yTYXrACLrN89a7RJHuhI3K.7Cce1LVPbC"
    :method 'post))

(defmethod paypal.add-parameter ((self paypal) key val)
  (setf (s-v 'post-data)
	(concatenate 'string (s-v 'post-data)
		     (format nil "~A=~A&" (escape-as-uri key)
			     (escape-as-uri val)))))

(defmethod paypal.add-parameter ((self paypal) key (val symbol))
  (paypal.add-parameter self key (symbol-name val)))

(defmethod paypal.add-parameter ((self paypal) key (val integer))
  (paypal.add-parameter self key (format nil "~D" val)))

;; 100.00
(defmethod paypal.to-currency ((self paypal) amount)
  (format nil "~,2,,F" amount))

;; 2011-07-05T01:26:40Z
(defmethod paypal.to-date ((self paypal) timestamp)
  (multiple-value-bind
	(second minute hour day month year day-of-week dst-p timezone)
      (decode-universal-time timestamp)
    (declare (ignore timezone dst-p day-of-week))
    (format nil "~a-~2,'0d-~2,'0dT~2,'0d:~,'0d:~2,'0dZ"
	    year month day hour minute second)))

(defmethod run ((self paypal))
  (paypal.add-parameter self "USER" (s-v 'username))
  (paypal.add-parameter self "PWD" (s-v 'password))
  (paypal.add-parameter self "SIGNATURE" (s-v 'signature))
  (paypal.add-parameter self "VERSION" "74.0")
  (paypal.add-parameter self "METHOD" (s-v 'api-method))
  
  (let* ((result (call-next-method))
	 (result (aif (slot-value result 'entities)
		      (uri.queries
		       (uri? (make-core-stream
			      (format nil "/foo.core?~A"
				      (octets-to-string (car it) :utf-8)))))
		      (error "Could not parse PayPal Http result"))))
    result))

;; -------------------------------------------------------------------------
;; Set Express Checkout
;; -------------------------------------------------------------------------
;; API:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_SetExpressCheckout
(defcommand paypal-set-express-checkout (paypal)
  ((amount :host local :initform (error "Please provide :amount"))
   (return-url :host local
	       :initform "http://www.coretal.net/sales.html?page:success")
   (cancel-url :host local
	       :initform "http://www.coretal.net/sales.html?page:cancel")
   (action :host local :initform "Sale"))
  (:default-initargs :api-method "SetExpressCheckout"))

(defmethod run ((self paypal-set-express-checkout))
  (with-slots (amount return-url cancel-url action) self
    (paypal.add-parameter self "PAYMENTREQUEST_0_AMT"
			  (paypal.to-currency self amount))
    (paypal.add-parameter self "RETURNURL" return-url)
    (paypal.add-parameter self "CANCELURL" cancel-url)
    (paypal.add-parameter self "PAYMENTREQUEST_0_PAYMENTACTION" action))
  (call-next-method self))

;; SERVER> (paypal-set-express-checkout :amount 10)
;; (("TOKEN" . "EC-3XS050383E9627310")
;;  ("TIMESTAMP" . "2011-07-04T23:19:48Z")
;;  ("CORRELATIONID" . "47bce484e6e64")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1936884"))

;; After SetExpressCheckout, redirect to:
;; https://www.sandbox.paypal.com/cgi-bin/webscr?cmd=_express-checkout&token=EC-3XS050383E9627310

;; Returns back to:
;; http://www.coretal.net/sales.html?page:success&token=EC-3XS050383E9627310&PayerID=FK6S5PJBHZ6JJ

;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_GetExpressCheckoutDetails
(defcommand paypal-get-express-checkout-details (paypal)
  ((token :host local :initform (error "Please provide :token")))
  (:default-initargs :api-method "GetExpressCheckoutDetails"))

(defmethod run ((self paypal-get-express-checkout-details))
  (paypal.add-parameter self "TOKEN" (s-v 'token))
  (call-next-method self))

;; SERVER> (paypal-get-express-checkout-details 
;; 	 :token "EC-3XS050383E9627310")
;; (("TOKEN" . "EC-3XS050383E9627310")
;;  ("CHECKOUTSTATUS" . "PaymentActionNotInitiated")
;;  ("TIMESTAMP" . "2011-07-04T23:50:04Z")
;;  ("CORRELATIONID" . "ce875ca9b0e12")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1936884")
;;  ("EMAIL" . "buyer_1308001708_per@core.gen.tr")
;;  ("PAYERID" . "FK6S5PJBHZ6JJ")
;;  ("PAYERSTATUS" . "verified")
;;  ("FIRSTNAME" . "Test")
;;  ("LASTNAME" . "User")
;;  ("COUNTRYCODE" . "US")
;;  ("SHIPTONAME" . "Test User")
;;  ("SHIPTOSTREET" . "1 Main St")
;;  ("SHIPTOCITY" . "San Jose")
;;  ("SHIPTOSTATE" . "CA")
;;  ("SHIPTOZIP" . "95131")
;;  ("SHIPTOCOUNTRYCODE" . "US")
;;  ("SHIPTOCOUNTRYNAME" . "United States")
;;  ("ADDRESSSTATUS" . "Confirmed")
;;  ("CURRENCYCODE" . "USD")
;;  ("AMT" . "10.00")
;;  ("SHIPPINGAMT" . "0.00")
;;  ("HANDLINGAMT" . "0.00")
;;  ("TAXAMT" . "0.00")
;;  ("INSURANCEAMT" . "0.00")
;;  ("SHIPDISCAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_CURRENCYCODE" . "USD")
;;  ("PAYMENTREQUEST_0_AMT" . "10.00")
;;  ("PAYMENTREQUEST_0_SHIPPINGAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_HANDLINGAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_TAXAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_INSURANCEAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_SHIPDISCAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_INSURANCEOPTIONOFFERED" . "false")
;;  ("PAYMENTREQUEST_0_SHIPTONAME" . "Test User")
;;  ("PAYMENTREQUEST_0_SHIPTOSTREET" . "1 Main St")
;;  ("PAYMENTREQUEST_0_SHIPTOCITY" . "San Jose")
;;  ("PAYMENTREQUEST_0_SHIPTOSTATE" . "CA")
;;  ("PAYMENTREQUEST_0_SHIPTOZIP" . "95131")
;;  ("PAYMENTREQUEST_0_SHIPTOCOUNTRYCODE" . "US")
;;  ("PAYMENTREQUEST_0_SHIPTOCOUNTRYNAME" . "United States")
;;  ("PAYMENTREQUESTINFO_0_ERRORCODE" . "0"))

;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_DoExpressCheckoutPayment
(defcommand paypal-do-express-checkout-payment (paypal)
  ((token :host local :initform (error "Please provide :token"))
   (payer-id :host local :initform (error "Please provide :payer-id"))
   (amount :host local :initform (error "Please provide :amount"))
   (currency :host local :initform "USD")
   (action :host local :initform "Sale")   
   (fraud-filter-details :host local :initform t))
  (:defualt-initargs :api-method "DoExpressCheckoutPayment"))

(defmethod run ((self paypal-do-express-checkout-payment))
  (with-slots (token amount currency payer-id action) self
    (paypal.add-parameter self "TOKEN" token)
    (paypal.add-parameter self "PAYMENTREQUEST_0_AMT"
			  (paypal.to-currency self amount))
    (paypal.add-parameter self "PAYMENTREQUEST_0_CURRENCYCODE" currency)
    (paypal.add-parameter self "PAYERID" (s-v 'payer-id))
    (paypal.add-parameter self "PAYMENTREQUEST_0_PAYMENTACTION" action))

  (if (s-v 'fraud-filter-details)
      (paypal.add-parameter self "RETURNFMFDETAILS" "1"))
  
  (call-next-method self))

;; SERVER> (paypal-do-express-checkout-payment 
;; 	 :token "EC-3XS050383E9627310"
;; 	 :payer-id "FK6S5PJBHZ6JJ"
;; 	 :amount 10)

;; (("TOKEN" . "EC-3XS050383E9627310")
;;  ("SUCCESSPAGEREDIRECTREQUESTED" . "false")
;;  ("TIMESTAMP" . "2011-07-05T01:26:40Z")
;;  ("CORRELATIONID" . "a3d8df9d3aa60")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1936884")
;;  ("INSURANCEOPTIONSELECTED" . "false")
;;  ("SHIPPINGOPTIONISDEFAULT" . "false")
;;  ("PAYMENTINFO_0_TRANSACTIONID" . "92W25351B27670106")
;;  ("PAYMENTINFO_0_TRANSACTIONTYPE" . "expresscheckout")
;;  ("PAYMENTINFO_0_PAYMENTTYPE" . "instant")
;;  ("PAYMENTINFO_0_ORDERTIME" . "2011-07-05T01:26:38Z")
;;  ("PAYMENTINFO_0_AMT" . "10.00")
;;  ("PAYMENTINFO_0_FEEAMT" . "0.59")
;;  ("PAYMENTINFO_0_TAXAMT" . "0.00")
;;  ("PAYMENTINFO_0_CURRENCYCODE" . "USD")
;;  ("PAYMENTINFO_0_PAYMENTSTATUS" . "Completed")
;;  ("PAYMENTINFO_0_PENDINGREASON" . "None")
;;  ("PAYMENTINFO_0_REASONCODE" . "None")
;;  ("PAYMENTINFO_0_PROTECTIONELIGIBILITY" . "Eligible")
;;  ("PAYMENTINFO_0_PROTECTIONELIGIBILITYTYPE" . "ItemNotReceivedEligible,UnauthorizedPaymentEligible")
;;  ("PAYMENTINFO_0_SECUREMERCHANTACCOUNTID" . "44RZP6LPAMSQL")
;;  ("PAYMENTINFO_0_ERRORCODE" . "0")
;;  ("PAYMENTINFO_0_ACK" . "Success"))


;; +-------------------------------------------------------------------------
;; | Recurring Payments
;; +-------------------------------------------------------------------------

;; Documentation:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_ECRecurringPayments

;; API:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_SetExpressCheckout

;; -------------------------------------------------------------------------
;; Paypal Recurring Set Checkout
;; -------------------------------------------------------------------------
(defcommand paypal-recurring-set-express-checkout (paypal)
  ((return-url :host local
	       :initform "http://www.coretal.net/sales.html?page:success")
   (cancel-url :host local
	       :initform "http://www.coretal.net/sales.html?page:cancel")
   (description :host local :initform (error "Provide :description"))
   (max-amount :host local :initform 25))
  (:default-initargs :api-method "SetExpressCheckout"))

(defmethod run ((self paypal-recurring-set-express-checkout))
  (with-slots (return-url cancel-url description max-amount) self
    (paypal.add-parameter self "RETURNURL" return-url)
    (paypal.add-parameter self "CANCELURL" cancel-url)
    (paypal.add-parameter self "L_BILLINGTYPE0" "RecurringPayments")
    (paypal.add-parameter self "L_BILLINGAGREEMENTDESCRIPTION0" description)
    (paypal.add-parameter self "MAXAMT" (paypal.to-currency self max-amount))
    (paypal.add-parameter self "AMT" (paypal.to-currency self 0)))
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Paypal Create Recurring Payments Profile
;; -------------------------------------------------------------------------
(defvar +paypal-billing-period+ '("Month" "Day" "Week" "SemiMonth" "Year"))

;; API:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_CreateRecurringPayments
(defcommand paypal-create-recurring-payments-profile (paypal)
  ((token :host local :initform (error "Please provide :token"))
   (amount :host local :initform (error "Please provide :amount"))
   (description :host local :initform (error "Please provide :description"))
   (start-date :host local :initform (get-universal-time))
   (currency :host local :initform "USD")
   (billing-period :host local :initform (car +paypal-billing-period+))
   (billing-frequency :host local :initform 1)
   (trial-billing-period :host local :initform (car +paypal-billing-period+))
   (trial-billing-frequency :host local :initform 1)
   (trial-amount :host local :initform nil))
  (:default-initargs :api-method "CreateRecurringPaymentsProfile"))

(defmethod run ((self paypal-create-recurring-payments-profile))
  (with-slots (token start-date billing-period
		     billing-frequency amount currency description) self
    (paypal.add-parameter self "TOKEN" token)
    (paypal.add-parameter self "PROFILESTARTDATE"
			  (paypal.to-date self start-date))
    (paypal.add-parameter self "BILLINGPERIOD" billing-period)
    (paypal.add-parameter self "BILLINGFREQUENCY" billing-frequency)
    (paypal.add-parameter self "CURRENCYCODE" currency)
    (paypal.add-parameter self "AMT" (paypal.to-currency self amount))
    (paypal.add-parameter self "DESC" description))

  (when (s-v 'trial-amount)
    (with-slots (trial-billing-period trial-billing-frequency
				      trial-amount) self
      (paypal.add-parameter self "TRIALBILLINGPERIOD" trial-billing-period)
      (paypal.add-parameter self "TRIALBILLINGFREQUENCY"
			    trial-billing-frequency)
      (paypal.add-parameter self "TRIALAMT"
			    (paypal.to-currency self trial-amount))))
  
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Paypal Get Recurring Payments Profile Details
;; -------------------------------------------------------------------------
;; API:
;; https://cms.paypal.com/us/cgi-bin/?cmd=_render-content&content_ID=developer/e_howto_api_nvp_r_GetRecurringPaymentsProfileDetails

(defcommand paypal-get-recurring-payments-profile (paypal)
  ((profile-id :host local :initform (error "Provide :profile-id")))
  (:default-initargs :api-method "GetRecurringPaymentsProfileDetails"))

(defmethod run ((self paypal-get-recurring-payments-profile))
  (paypal.add-parameter self "PROFILEID" (s-v 'profile-id))
  (call-next-method self))

;; -------------------------------------------------------------------------
;; Recurring Payment Flow
;; -------------------------------------------------------------------------

;; SERVER> (paypal-recurring-set-express-checkout
;;           :description "Time Magazine Subscription")
;; (("TOKEN" . "EC-3P199454L29273808")
;;  ("TIMESTAMP" . "2011-07-05T21:44:01Z")
;;  ("CORRELATIONID" . "9e9d2e5210f4b")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1936884"))
;;
;; Redirect to:
;; https://www.sandbox.paypal.com/cgi-bin/webscr?cmd=_express-checkout&token=<value_from_SetExpressCheckoutResponse>&address_override=1

;; Returns back to:
;; http://www.coretal.net/sales.html?page:success&token=EC-3P199454L29273808

;; SERVER> (paypal-get-express-checkout-details
;;           :token "EC-3P199454L29273808")
;; (("TOKEN" . "EC-3P199454L29273808")
;;  ("BILLINGAGREEMENTACCEPTEDSTATUS" . "1")
;;  ("CHECKOUTSTATUS" . "PaymentActionNotInitiated")
;;  ("TIMESTAMP" . "2011-07-05T22:00:14Z")
;;  ("CORRELATIONID" . "741d53f91608a")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1936884")
;;  ("EMAIL" . "buyer_1308001708_per@core.gen.tr")
;;  ("PAYERID" . "FK6S5PJBHZ6JJ")
;;  ("PAYERSTATUS" . "verified")
;;  ("FIRSTNAME" . "Test")
;;  ("LASTNAME" . "User")
;;  ("COUNTRYCODE" . "US")
;;  ("SHIPTONAME" . "Test User")
;;  ("SHIPTOSTREET" . "1 Main St")
;;  ("SHIPTOCITY" . "San Jose")
;;  ("SHIPTOSTATE" . "CA")
;;  ("SHIPTOZIP" . "95131")
;;  ("SHIPTOCOUNTRYCODE" . "US")
;;  ("SHIPTOCOUNTRYNAME" . "United States")
;;  ("ADDRESSSTATUS" . "Confirmed")
;;  ("CURRENCYCODE" . "USD")
;;  ("AMT" . "0.00")
;;  ("SHIPPINGAMT" . "0.00")
;;  ("HANDLINGAMT" . "0.00")
;;  ("TAXAMT" . "0.00")
;;  ("INSURANCEAMT" . "0.00")
;;  ("SHIPDISCAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_CURRENCYCODE" . "USD")
;;  ("PAYMENTREQUEST_0_AMT" . "0.00")
;;  ("PAYMENTREQUEST_0_SHIPPINGAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_HANDLINGAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_TAXAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_INSURANCEAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_SHIPDISCAMT" . "0.00")
;;  ("PAYMENTREQUEST_0_INSURANCEOPTIONOFFERED" . "false")
;;  ("PAYMENTREQUEST_0_SHIPTONAME" . "Test User")
;;  ("PAYMENTREQUEST_0_SHIPTOSTREET" . "1 Main St")
;;  ("PAYMENTREQUEST_0_SHIPTOCITY" . "San Jose")
;;  ("PAYMENTREQUEST_0_SHIPTOSTATE" . "CA")
;;  ("PAYMENTREQUEST_0_SHIPTOZIP" . "95131")
;;  ("PAYMENTREQUEST_0_SHIPTOCOUNTRYCODE" . "US")
;;  ("PAYMENTREQUEST_0_SHIPTOCOUNTRYNAME" . "United States")
;;  ("PAYMENTREQUESTINFO_0_ERRORCODE" . "10001")
;;  ("PAYMENTREQUESTINFO_0_SHORTMESSAGE" . "Internal Error")
;;  ("PAYMENTREQUESTINFO_0_LONGMESSAGE" . "Internal Error")
;;  ("PAYMENTREQUESTINFO_0_SEVERITYCODE" . "Error"))

;; SERVER> (paypal-create-recurring-payments-profile
;; 	     :token "EC-3P199454L29273808"
;; 	     :description "Time Magazine Subscription"
;; 	     :amount 99)
;; (("PROFILEID" . "I-1KTXAD75YWUW")
;;  ("PROFILESTATUS" . "ActiveProfile")
;;  ("TIMESTAMP" . "2011-07-05T22:05:52Z")
;;  ("CORRELATIONID" . "3d703d750b23")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1907759"))

;; SERVER> (paypal-get-recurring-payments-profile 
;;             :profile-id "I-1KTXAD75YWUW")
;; (("PROFILEID" . "I-1KTXAD75YWUW")
;;  ("STATUS" . "Active")
;;  ("AUTOBILLOUTAMT" . "NoAutoBill")
;;  ("DESC" . "Time Magazine Subscription")
;;  ("MAXFAILEDPAYMENTS" . "0")
;;  ("SUBSCRIBERNAME" . "Test User")
;;  ("PROFILESTARTDATE" . "2011-07-06T07:00:00Z")
;;  ("NEXTBILLINGDATE" . "2011-07-06T10:00:00Z")
;;  ("NUMCYCLESCOMPLETED" . "0")
;;  ("NUMCYCLESREMAINING" . "0")
;;  ("OUTSTANDINGBALANCE" . "0.00")
;;  ("FAILEDPAYMENTCOUNT" . "0")
;;  ("TRIALAMTPAID" . "0.00")
;;  ("REGULARAMTPAID" . "0.00")
;;  ("AGGREGATEAMT" . "0.00")
;;  ("AGGREGATEOPTIONALAMT" . "0.00")
;;  ("FINALPAYMENTDUEDATE" . "1970-01-01T00:00:00Z")
;;  ("TIMESTAMP" . "2011-07-05T22:17:56Z")
;;  ("CORRELATIONID" . "9c5b1112c0ffe")
;;  ("ACK" . "Success")
;;  ("VERSION" . "74.0")
;;  ("BUILD" . "1907759")
;;  ("SHIPTOSTREET" . "1 Main St")
;;  ("SHIPTOCITY" . "San Jose")
;;  ("SHIPTOSTATE" . "CA")
;;  ("SHIPTOZIP" . "95131")
;;  ("SHIPTOCOUNTRYCODE" . "US")
;;  ("SHIPTOCOUNTRY" . "US")
;;  ("SHIPTOCOUNTRYNAME" . "United States")
;;  ("SHIPADDRESSOWNER" . "PayPal")
;;  ("SHIPADDRESSSTATUS" . "Unconfirmed")
;;  ("BILLINGPERIOD" . "Month")
;;  ("BILLINGFREQUENCY" . "1")
;;  ("TOTALBILLINGCYCLES" . "0")
;;  ("CURRENCYCODE" . "USD")
;;  ("AMT" . "99.00")
;;  ("SHIPPINGAMT" . "0.00")
;;  ("TAXAMT" . "0.00")
;;  ("REGULARBILLINGPERIOD" . "Month")
;;  ("REGULARBILLINGFREQUENCY" . "1")
;;  ("REGULARTOTALBILLINGCYCLES" . "0")
;;  ("REGULARCURRENCYCODE" . "USD")
;;  ("REGULARAMT" . "99.00")
;;  ("REGULARSHIPPINGAMT" . "0.00")
;;  ("REGULARTAXAMT" . "0.00"))


;; -------------------------------------------------------------------------
;; IPN Request
;; -------------------------------------------------------------------------

;; POST /ipn.core HTTP/1.0
;; Content-Type: application/x-www-form-urlencoded
;; Host: aycan.dyndns.org
;; Content-Length: 846

;; test_ipn=1&payment_type=instant&payment_date=03%3A58%3A43+Jul+06%2C+2011+PDT&payment_status=Completed&address_status=confirmed&payer_status=unverified&first_name=John&last_name=Smith&payer_email=buyer%40paypalsandbox.com&payer_id=TESTBUYERID01&address_name=John+Smith&address_country=United+States&address_country_code=US&address_zip=95131&address_state=CA&address_city=San+Jose&address_street=123%2C+any+street&receiver_email=seller%40paypalsandbox.com&receiver_id=TESTSELLERID1&residence_country=US&item_name1=something&item_number1=AK-1234&quantity1=1&tax=2.02&mc_currency=USD&mc_fee=0.44&mc_gross_1=9.34&mc_handling=2.06&mc_handling1=1.67&mc_shipping=3.02&mc_shipping1=1.02&txn_type=cart&txn_id=43761058&notify_version=2.4&custom=xyz123&invoice=abc1234&charset=windows-1252&verify_sign=AFcWxV21C7fd0v3bYYYRCpSSRl31AXgz8rE83b1Uq60X2t2wHFjYn7xj