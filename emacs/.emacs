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

(mapc (lambda (pack)
        (unless (package-installed-p pack)
          (package-install pack))
        ) packages)

(require 'use-package)
(require 'nix-mode)

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
(global-hl-line-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Completion
;; (add-hook 'after-init-hook 'global-company-mode)

(require 'lsp-mode)
(mapc (lambda (mode) (add-hook mode #'lsp)) (list
					     'c-mode-hook
					     'python-mode-hook
					     ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes '(deeper-blue))
 '(package-selected-packages '(magit use-package evil-surround evil-collection)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
