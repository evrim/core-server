;; +-------------------------------------------------------------------------
;; | Wordpress
;; +-------------------------------------------------------------------------
;;
;; Author: Evrim Ulu <evrim@core.gen.tr>
;; Date: October 2011
;;
;; Namespace: http://wordpress.org/export/1.0/
;;
(in-package :core-server)

;; -------------------------------------------------------------------------
;; WordPress Metaclass
;; -------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass wordpress+ (xml+)
    ()
    (:default-initargs
     :namespace "wp"
     :schema "http://wordpress.org/export/1.0/")))

(defclass+ wordpress-element (xml)
  ((type :host remote :print nil)))

;; +------------------------------------------------------------------------
;; | WordPress Object Definition: defwp-tag
;; +------------------------------------------------------------------------
(defmacro defwp-tag (name &rest attributes)  
  `(progn
     (defclass+ ,name (atom-element)
       (,@(mapcar (lambda (attr) (list attr :print nil :host 'remote))
		  attributes))
       (:metaclass wordpress+)
       (:tag ,@(string-downcase (symbol-name name)))
       (:attributes ,@attributes))
     (find-class+ ',name)))

(defwp-tag <wp:wxr_version)
(defwp-tag <wp:base_site_url)
(defwp-tag <wp:base_blog_url)
(defwp-tag <wp:author)
(defwp-tag <wp:author_id)
(defwp-tag <wp:author_email)
(defwp-tag <wp:author_login)
(defwp-tag <wp:author_display_name)
(defwp-tag <wp:author_first_name)
(defwp-tag <wp:author_last_name)
(defwp-tag <wp:category)
(defwp-tag <wp:category_nicename)
(defwp-tag <wp:category_parent)
(defwp-tag <wp:category_description)
(defwp-tag <wp:cat_name)
(defwp-tag <wp:tag)
(defwp-tag <wp:tag_slug)
(defwp-tag <wp:tag_name)
(defwp-tag <wp:term)
(defwp-tag <wp:term_id)
(defwp-tag <wp:term_taxonomy)
(defwp-tag <wp:term_slug)
(defwp-tag <wp:term_name)
(defwp-tag <wp:term_parent)
(defwp-tag <wp:post_id)
(defwp-tag <wp:post_name)
(defwp-tag <wp:post_parent)
(defwp-tag <wp:post_date)
(defwp-tag <wp:post_date_gmt)
(defwp-tag <wp:post_type)
(defwp-tag <wp:post_password)
(defwp-tag <wp:comment_status)
(defwp-tag <wp:ping_status)
(defwp-tag <wp:status)
(defwp-tag <wp:menu_order)
(defwp-tag <wp:is_sticky)
(defwp-tag <wp:postmeta)
(defwp-tag <wp:meta_key)
(defwp-tag <wp:meta_value)
(defwp-tag <wp:comment)
(defwp-tag <wp:comment_id)
(defwp-tag <wp:comment_author)
(defwp-tag <wp:comment_author_email)
(defwp-tag <wp:comment_author_url)
(defwp-tag <wp:comment_author_-I-P)
(defwp-tag <wp:comment_user_id)
(defwp-tag <wp:comment_date)
(defwp-tag <wp:comment_date_gmt)
(defwp-tag <wp:comment_content)
(defwp-tag <wp:comment_approved)
(defwp-tag <wp:comment_type)
(defwp-tag <wp:comment_parent)
(defwp-tag <wp:attachment_url)