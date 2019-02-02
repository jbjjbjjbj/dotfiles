
(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'load-path "~/.emacs.d/load")
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize )

(eval-when-compile
  (require 'use-package))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t))

;;
;; Evil mode
;;
(defun save-and-quit ()
  "Save the document and exit buffer"
  (interactive)
  (save-buffer (current-buffer))
  (kill-this-buffer))

					; TODO find out what this is
(setq evil-want-integration nil)
(setq evil-want-keybinding nil)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-ex-define-cmd "b[utterfly]" 'bufferfly)
  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
  (evil-ex-define-cmd "wq" 'save-and-quit)
  (evil-ex-define-cmd "describe-function" 'describe-function)
  (evil-mode 1))

(use-package evil-collection
			 :config
			 (evil-collection-init))

(use-package org-evil
  :ensure t
  )

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package nlinum-relative
  :ensure
  :config
  ;; something else you want
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

(use-package evil-magit
  :ensure t)

;;
;; General config
;;

(setq initial-buffer-choice nil)
(setq org-support-shift-select t)

(setq c-default-style "k&r"
      c-basic-offset 4)


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)                 ; optional

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-c-headers-path-user
   (quote
    ("." "~/Documents/aausat6/software/lib/libcsp/include" "~/Documents/aausat6/software/stm32f/libopencm3/include" "~/Documents/aausat6/software/lib/libfreertosv10/Source/include")))
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages
   (quote
    (company-ycmd ycmd company-c-headers evil-magit magit company auto-complete-config auto-complete evil-collection evil use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun show-file-name ()
  "Show the current filename"
  (interactive)
  (message (buffer-file-name)) )

(defun goto-startup-screen ()
  "Show the startup screen"
  (interactive)
  (display-startup-screen) )

(setq org-directory "~/Syncthing/OrgNotes")
(setq org-default-notes-file (concat org-directory "/notes.org"))

(use-package magit
  :ensure t )

;;
;; Auto-complete (AC)
;;
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default)
)
(require 'auto-complete-config)

(use-package company-c-headers
  :ensure t
  )
(use-package company
  :ensure t)

(add-hook 'global-init-hook 'global-company-mode)

(add-to-list 'company-backends 'company-c-headers)

;;
;; Keybindings
;;

;; Leader Keybinding
(defvar leader-map (make-sparse-keymap)
  "Keymap for evil leader key")

(define-key evil-normal-state-map (kbd "SPC") leader-map)

(define-key leader-map "b" 'switch-to-buffer)
(define-key leader-map "t" 'term)
(define-key leader-map "o" 'org-capture)
