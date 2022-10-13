(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages"))
(package-initialize)
; (package-refresh-contents)

(mapcar (lambda (p)
       (unless (package-installed-p p)
	 (package-install p))
       ) (list 'evil 'haskell-mode 'bbdb))

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

; (require 'evil-collection)
; (evil-collection-init)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(show-paren-mode 1)
(setq show-paren-when-point-inside-paren 1)

(setq backup-directory-alist '(("." . "~/.emacs_savefiles")))

(setq indent-tabs-mode nil)

(tool-bar-mode -1)
(toggle-scroll-bar -1)

(setq org-agenda-files (list "~/Common/org"))

(add-hook 'org-mode-hook
  (lambda ()
    (evil-local-set-key 'normal (kbd "<SPC>") 'org-cycle)))

;; Contact book
(setq bbdb-file "~/Common/bbdb")
(require 'bbdb)
(bbdb-initialize 'gnus 'message)

(setq org-icalendar-combined-agenda-file "org.ics")

;; Load email configuration
(defun load-if-exists (file)
  (if (file-exists-p file) (load file) nil))

(load-if-exists "~/Common/gnus.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages '(bbdb haskell-mode evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
