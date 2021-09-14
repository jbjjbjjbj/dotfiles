;; Package init and install
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(defvar packages (list
		  'use-package
		  'geiser-chez
		  'geiser-racket
		  'company
		  'nix-mode
		  'lsp-mode
		  'magit
		  ))
(defun install-stuff () (interactive)
       (mapc (lambda (pack)
	       (unless (package-installed-p pack)
		 (package-install pack))
	       ) packages)
       )

(require 'use-package)
(require 'nix-mode)
(require 'magit)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config

  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t

  :config
  (evil-collection-init))

;; General settings
;; (global-hl-line-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(show-paren-mode 1)
(setq show-paren-when-point-inside-paren 1)

(setq backup-directory-alist '(("." . "~/.emacs_savefiles")))

(defvar color-theme-state 'dark)
(defun toggle-dark-mode ()
  (interactive)
  (let ((switch (lambda (theme state)
		  (progn
		    (setq color-theme-state state)
		    (load-theme theme))
		  )))
    (if (eq color-theme-state 'dark)
	(funcall switch 'deeper-blue 'light)
      (funcall switch 'adwaita 'dark))))
(toggle-dark-mode)


;; Keyboard bindings
(defun split-and-follow-horizontal ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key(kbd "C-x 2") 'split-and-follow-horizontal)
(defun split-and-follow-vertical ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key(kbd "C-x 3") 'split-and-follow-vertical)

(keyboard-translate ?\C-h ?\C-x)
(keyboard-translate ?\C-x ?\C-h)

(define-key evil-normal-state-map (kbd "SPC") 'execute-extended-command)

(global-set-key (kbd "C-x C-d") 'toggle-dark-mode)

;; Completion

(require 'lsp-mode)
(mapc (lambda (mode) (add-hook mode #'lsp)) (list
					     'c-mode-hook
					     'python-mode-hook
					     ))

(mapc (lambda (mode) (add-hook mode 'company-mode)) (list
						     'emacs-lisp-mode-hook
						     ))

;; Custom keybinding
(define-key (current-global-map) [remap list-buffers] 'ibuffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(package-selected-packages '(magit use-package evil-surround evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
