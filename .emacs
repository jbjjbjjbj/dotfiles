
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



(setq org-log-done 'time)


(setq-default org-display-custom-times t)
(setq org-time-stamp-custom-formats '("<%a %e-%b-%Y>" . "<%a %e-%b-%Y %H:%M>"))

;;(setq org-agenda-files '("~/Dropbox/Noter"))



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
