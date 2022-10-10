(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages"))
; (package-initialize)
; (package-refresh-contents)

(mapcar (lambda (p)
       (unless (package-installed-p p)
	 (package-install p))
       ) (list 'evil 'haskell-mode))

(require 'evil)
(evil-mode 1)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq display-line-numbers 'relative)
(global-display-line-numbers-mode)

(add-hook 'org-mode-hook
	  (lambda ()
	    (evil-local-set-key 'normal (kbd "<SPC>") 'org-cycle)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tango-dark))
 '(package-selected-packages '(haskell-mode evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
