
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#757575" "#CD5542" "#4A8F30" "#7D7C21" "#4170B3" "#9B55C3" "#68A5E9" "gray43"])
 '(beacon-color "#cc6666")
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
    ("cec19046bfe0bd715cb30d3e8e841411f4e59042d8538a8c9da4bf66b08664d9" "e297f54d0dc0575a9271bb0b64dad2c05cff50b510a518f5144925f627bb5832" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "4e63466756c7dbd78b49ce86f5f0954b92bf70b30c01c494b37c586639fa3f6f" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(electric-pair-mode t)
 '(fci-rule-color "#373b41")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(org-agenda-time-grid
   (quote
    ((daily today require-timed)
     "----------------"
     (800 1000 1200 1400 1600 1800 2000))))
 '(org-hide-leading-stars t)
 '(org-highlight-latex-and-related (quote (latex)))
 '(package-selected-packages
   (quote
    (projectile ample-theme tangotango-theme cyberpunk-theme color-theme-sanityinc-tomorrow)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 113 :width normal))))
 '(org-agenda-structure ((t (:inherit default :underline nil :slant normal :weight normal :height 2.0 :width normal :foundry "MS  " :family "Source Code Pro")))))



(setq org-log-done 'time)


(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %e-%b-%Y>" . "<%a %e-%b-%Y %H:%M>"))

(setq org-agenda-files '("~/Dropbox/Noter"))



(ac-config-default)

(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(setq c-default-style "k&r"
      c-basic-offset 4)

;;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))


(setq backup-directory-alist
          `(("." . ,(concat user-emacs-directory "backups"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )
(defun hsplit-last-buffer ()
  (interactive)
   (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)


(global-set-key (kbd "C-å C-b")  'windmove-left)
(global-set-key (kbd "C-å C-f") 'windmove-right)
(global-set-key (kbd "C-å C-p")    'windmove-up)
(global-set-key (kbd "C-å C-n")  'Windmove-down)


(windmove-default-keybindings)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)



(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-<backspace>") 'switch-to-last-buffer)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'eval-and-replace)


(require 'iy-go-to-char)
(global-set-key (kbd "M-m") 'iy-go-to-char)


(require 'expand-region)
(global-set-key (kbd "M-n") 'er/expand-region)

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    
(require 'multiple-cursors)

(global-set-key (kbd "M-p e") 'mc/edit-lines)
(global-set-key (kbd "M-p f") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p b") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-p h") 'mc/mark-all-like-this)


(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'fortune)
(setq fortune-dir "/usr/share/games/fortunes")
(setq fortune-file "/usr/share/games/fortunes/fortunes")

(defun dashboard-fortune (hej)
  (insert "Fortune:\n")
  (insert (with-temp-buffer
	    (shell-command "fortune" t)
	    (buffer-string)))
  )


(add-to-list 'dashboard-item-generators  '(custom . dashboard-fortune))

(setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(custom . 3)
			))

(show-paren-mode 1)

<<<<<<< HEAD
(require 'dired-single)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
         loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer "..")))))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))



(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (sh . t)
   (haskell . t)
   (js . t)
   (latex . t)
   (C . t)
   ))

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

(display-time-mode 1)
(display-battery-mode 1)
