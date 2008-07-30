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


;; Core-serveR Emacs Configuration
;;(setenv "LANG" "tr_TR.UTF-8")
;;(setenv "LC_ALL" "tr_TR.UTF-8")

(if (null (getenv "CORESERVER_HOME"))
    (error "Environment variable CORESERVER_HOME is not set."))

(defun load-el (str)
  "Load the el file in the core-server base directory"
  (let ((file (concat (concat (getenv "CORESERVER_HOME") "etc/emacs/") str)))
    (load file)))

; IBUFFER
(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(column-number-mode)

;; PAREDIT
;;(load-el "paredit-beta.el")
(load-el "paredit-7.0b4.el")

(autoload 'enable-paredit-mode "paredit" 
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)

;; Highlights Parenthesis
(show-paren-mode t)

;; SLIME
(add-to-list 'load-path (concat (getenv "CORESERVER_HOME") "lib/slime/"))
(add-to-list 'load-path (concat (getenv "CORESERVER_HOME") "lib/slime/contrib/"))
(require 'slime)
(add-hook 'slime-load-hook (lambda () 
			     (require 'slime-fuzzy)
			     (slime-fuzzy-init)))

(setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 1024"
      lisp-indent-function 'common-lisp-indent-function
      slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-startup-animation nil
      slime-net-coding-system 'utf-8-unix
      slime-multiprocessing t)

(global-set-key "\C-cs" 'slime-selector)
(defun save-and-load-and-compile () 
  (interactive) (save-buffer)
  (interactive) (slime-compile-and-load-file))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (slime-define-key "\C-c\C-k" 
			      'save-and-load-and-compile)))

(defun slime-run-rt-test ()
  (interactive)
  (slime-display-output-buffer)
  (slime-interactive-eval (format "%s" `(rt:do-test ',(cadr (read (slime-last-expression)))))))

(add-hook 'slime-mode-hook
	  (lambda ()
	    (slime-define-key "\C-x\C-a"
			      'slime-run-rt-test)))

(slime-setup '(slime-fancy slime-asdf))
;;(setq (get 'defmethod/cc 'common-lisp-indent-function) 'lisp-indent-defmethod
;;      (get 'defmethod/unit 'common-lisp-indent-function) 'lisp-indent-defmethod)

;; DARCSUM
(load-el "darcsum.el")

(defun core ()
  (interactive)
  (slime-connect "127.0.0.1" 4005)
  (slime-repl-set-package "core-server"))

(setq speedbar-track-mouse-flag t)

; MOUSE SCROLL
(mouse-wheel-mode 1)

; SLOPPY FOCUS
(setq mouse-autoselect-window t)

;; proper indentation
(setf *core-server-methods* '(defmethod/cc
			      defmethod/unit
			      defmethod/local
			      defmethod/remote))

(setf *core-server-functions* '(defun/cc <:div <:form <:a <:select <:css))

(defun cl-indent (sym indent) ;; by Pierpaolo Bernardi
  (put sym 'common-lisp-indent-function
       (if (symbolp indent)
	   (get indent 'common-lisp-indent-function)
	   indent)))

(dolist (i *core-server-methods*)
  (cl-indent i 'defmethod))

(dolist (i *core-server-functions*)
  (cl-indent i 'defun))

; Function to run Tidy HTML parser on buffer
; NOTE: this requires external Tidy program
(defun tidy-buffer ()
  "Run Tidy HTML parser on current buffer."
  (interactive)
  (if (get-buffer "tidy-errs") (kill-buffer "tidy-errs"))
  (shell-command-on-region (point-min) (point-max)
                           ;;"tidy -f /tmp/tidy-errs -asxhtml -q -utf8 -i -wrap 72 -c"
                           "tidy -f /tmp/tidy-errs -asxhtml --doctype transitional --char-encoding utf8 --output-encoding utf8 --add-xml-decl y --indent-attributes y --gnu-emacs y --tidy-mark n -q -utf8 -i -w 0" t)
  (find-file-other-window "/tmp/tidy-errs")
  (other-window 1)
  (delete-file "/tmp/tidy-errs")
  (message "buffer tidy'ed"))

(define-skeleton coretal-insert-link
  "Make a dynamic link"
  "Link text: "
  '(progn 
     (setq anchor (skeleton-read "Page anchor:"))
     (setq page-name (buffer-name (current-buffer))))
  "<a href=\"" page-name "#" anchor "\" onclick=\"return coretal.loadPage('" anchor "');\">" str "</a>")

(defun make-backup-file-name (file)
  "Create the non-numeric backup file name for FILE.
Normally this will just be the file's name with `~' appended.
Customization hooks are provided as follows.

If the variable `make-backup-file-name-function' is non-nil, its value
should be a function which will be called with FILE as its argument;
the resulting name is used.

Otherwise a match for FILE is sought in `backup-directory-alist'; see
the documentation of that variable.  If the directory for the backup
doesn't exist, it is created."
  (if make-backup-file-name-function
      (funcall make-backup-file-name-function file)
      (if (and (eq system-type 'ms-dos) (not (msdos-long-file-names)))
	  (let ((fn (file-name-nondirectory file)))
	    (concat (file-name-directory file)
		    "."
		    (or (and (string-match "\\`[^.]+\\'" fn)
			     (concat (match-string 0 fn) ".~"))
			(and (string-match "\\`[^.]+\\.\\(..?\\)?" fn)
			     (concat (match-string 0 fn) "~")))))
	  (let ((fname (make-backup-file-name-1 file)))
	    (concat (file-name-directory fname) "." (file-name-nondirectory fname) "~")))))

;; irc.core.gen.tr developer connection
(when (locate-library "erc")
  (require 'erc)
  (defun core-irc ()
    (interactive)
    (erc-tls :server "irc.core.gen.tr" 
	     :port 8994
	     :nick (getenv "USERNAME")
	     :full-name (getenv "USER"))
    (view-buffer "#core"))
  ;; timestamp on left etc...
  (setq 
   erc-insert-timestamp-function 'erc-insert-timestamp-left
   erc-timestamp-format "%H:%M "
   erc-timestamp-only-if-changed-flag nil
   erc-hide-timestamps nil)

  ;; diğer yapılandırma değerleri
  (custom-set-variables
   '(erc-prompt-for-nickserv-password nil)
   '(erc-auto-query (quote window-noselect))
   '(erc-track-mode t)
   '(erc-log-mode t)
   '(erc-server-auto-reconnect nil)
   '(erc-timestamp-mode t)
   '(erc-echo-timestamps t)
   '(erc-auto-discard-away t)
   '(erc-autoaway-idle-seconds 3600)
   '(erc-auto-set-away t)
   '(erc-enable-logging (quote erc-log-all-but-server-buffers))
   '(erc-log-insert-log-on-open nil)
   '(erc-dcc-get-default-directory "~/dcc/")
   '(erc-log-channels-directory "~/.irclogs/")
   '(erc-modules '(autoaway autojoin button capab-identify completion fill irccontrols log match netsplit stamp notify page ring scrolltobottom services stamp track truncate))
   '(erc-kill-queries-on-quit t)
   '(erc-kill-server-buffer-on-quit t)))

(core-irc)
