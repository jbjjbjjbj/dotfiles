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
		  'auctex
		  'better-jumper
		  'lsp-haskell
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

(setq indent-tabs-mode nil)

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

(setq-default TeX-master 'shared)

(better-jumper-mode +1)
(with-eval-after-load 'evil-maps
                      (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
                      (define-key evil-motion-state-map (kbd "<tab>") 'better-jumper-jump-forward))

;; The following add mappings to 40j and 20k to add it to the jump list.
;; It was stolen from [1]
(defun my-jump-advice (oldfun &rest args)
  (let ((old-pos (point)))
    (apply oldfun args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
	     1)
      (save-excursion
	(goto-char old-pos)
	(evil-set-jump)))))

(advice-add 'evil-next-line :around #'my-jump-advice)
(advice-add 'evil-previous-line :around #'my-jump-advice)

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

(add-hook 'direc-mode-hook 'auto-revert-mode)

;; Completion

(require 'lsp-mode)
(require 'lsp-haskell)
(mapc (lambda (mode) (add-hook mode #'lsp)) (list
					     'c-mode-hook
					     'python-mode-hook
					     'haskell-mode-hook
					     'haskell-literate-mode-hook
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
 '(package-selected-packages
   '(better-jumper auctex lsp-haskell haskell-mode geiser-racket magit use-package evil-surround evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; [1] https://www.reddit.com/r/emacs/comments/8flkrg/evil_add_numbered_movement_to_the_jump_list/dy6b8qq/
