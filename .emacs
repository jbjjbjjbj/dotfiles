
(add-to-list 'load-path "~/.emacs.d/load")
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(ac-config-default)

(setq c-default-style "k&r"
      c-basic-offset 4)
(setq evil-want-integration nil) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(evil-collection-init)

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(require 'evil)
(evil-mode 1)


(require 'evil-surround)
(global-evil-surround-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-collection notmuch company-c-headers org jedi evil-surround buffer-move auto-complete)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

;; Auto-complete (AC)
(require 'auto-complete)

(require 'auto-complete-config)
(ac-config-default)

(require 'company)
(add-hook 'global-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-c-headers)

;
;(require 'mu4e)
;(setq mu4e-maildir "~/Mail/mailbox")
;(setq mu4e-drafts-folder "/Drafts")
;(setq mu4e-sent-folder   "/Sent")
;(setq mu4e-trash-folder   "/Trash")
;(setq mu4e-refile-folder   "/Archive")
;
;(setq mu4e-get-mail-command "offlineimap")
;;; shortcuts
;(setq mu4e-maildir-shortcuts
    ;'( ("/INBOX"               . ?i)
       ;("/Archive"   . ?a)))
;
;;; something about ourselves
;(setq
   ;user-mail-address "julian@jtle.dk"
   ;user-full-name  "Julian T"
   ;)
;
;;; show images
;(setq mu4e-show-images t)
;
;;; use imagemagick, if available
;(when (fboundp 'imagemagick-register-types)
  ;(imagemagick-register-types))
;
;;; convert html emails properly
;;; Possible options:
;;;   - html2text -utf8 -width 72
;;;   - textutil -stdin -format html -convert txt -stdout
;;;   - html2markdown | grep -v '&nbsp_place_holder;' (Requires html2text pypi)
;;;   - w3m -dump -cols 80 -T text/html
;;;   - view in browser (provided below)
;(setq mu4e-html2text-command "html2text")
;
;;; add option to view html message in a browser
;;; `aV` in view to activate
;(add-to-list 'mu4e-view-actions
			   ;'("ViewInBrowser" . mu4e-action-view-in-browser) t)
;
;;; fetch mail every 10 mins
;(setq mu4e-update-interval 600)
;
;
;(setf initial-buffer-choice (lambda () (mu4e)))
;
;(setq message-send-mail-function 'smtpmail-send-it)
;
;(setq smtpmail-default-smtp-server "smtp.mailbox.org"
      ;smtpmail-smtp-server "smtp.mailbox.org"
      ;smtpmail-smtp-service 465)
;
