(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
; (package-refresh-contents)

(mapcar (lambda (p)
       (unless (package-installed-p p)
	 (package-install p))
       ) (list 'auctex 'magit 'undo-tree 'haskell-mode 'evil 'bbdb 'company))

(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

; (require 'evil-collection)
; (evil-collection-init)

(require 'tex-site)

(evil-set-initial-state 'haskell-interactive-mode 'emacs)
(evil-set-initial-state 'haskell-error-mode 'emacs)

;; Windmove
(defvar vim-move-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-w") nil)
    (define-key map (kbd "C-w l") 'windmove-right)
    (define-key map (kbd "C-w h") 'windmove-left)
    (define-key map (kbd "C-w j") 'windmove-down)
    (define-key map (kbd "C-w k") 'windmove-up)
    (define-key map (kbd "C-w C-w") 'other-window)
    map) "vim-move-mode keymap.")

(define-minor-mode vim-move-mode
  "A minor mode where C-w works somewhat like what im used to."
  :init-value t
  :lighter " vim-move")
(vim-move-mode 1)

(require 'magit)

(require 'undo-tree)
(global-undo-tree-mode 1)
(evil-set-undo-system 'undo-tree)
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(winner-mode 1)
(global-set-key (kbd "C-c u") #'winner-undo)

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

;; Capture
(setq ord-default-notes-file "~/Common/org/todo.org")
(setq org-capture-templates
      '(("l" "Linked todo" entry (file+headline "~/Common/org/todo.org" "Captures")
	 "* TODO %?\n %i \n %a")
	("t" "Todo" entry (file+headline "~/Common/org/todo.org" "Captures"))
	("k" "Kalender" entry (file+headline "~/Common/org/kalender.org" "Captures"))
	))

;; Contact book
(setq bbdb-file "~/Common/bbdb")
(require 'bbdb)
(bbdb-initialize 'gnus 'message)

(setq icalendar-export-sexp-enumerate-all t)
(setq org-icalendar-combined-agenda-file "~/org.ics")

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Load email configuration
(defun load-if-exists (file)
  (if (file-exists-p file) (load file) nil))

(load-if-exists "~/Common/gnus.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "8455a572e70665b512a068f64b19716b79a0ead6")
 '(custom-enabled-themes '(modus-vivendi))
 '(display-line-numbers-type 'relative)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(auctex go-mode magit undo-tree 'flycheck 'flycheck bbdb haskell-mode evil))
 '(tool-bar-mode nil)
 '(warning-suppress-types '((org))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ubuntu Mono" :foundry "DAMA" :slant normal :weight normal :height 127 :width normal)))))
