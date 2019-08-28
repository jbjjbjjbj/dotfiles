(require 'package)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defvar my-packages '( auto-complete
					   evil
					   evil-surround
					   org
					   org-evil
					   go-mode
					   dired-narrow
					   ace-window
					   magit
					   yaml-mode))

(setq evil-want-C-u-scroll t)

(dolist (p my-packages)
  (unless (package-installed-p p)
	(package-refresh-contents)
	(package-install p))
  (add-to-list 'package-selected-packages p))


;; Use up to 20mb before running garbage collector
(setq gc-cons-threshold 20000000)

;; Use tmp as backup dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t)
(setq confirm-kill-emacs 'y-or-n-p)

;; Use a to goto dired directory
(put 'dired-find-alternate-file 'disabled nil)
;; Direc fuzzy searching, use g to go back to file listing
(require 'dired)
(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(display-time-mode t)
(tool-bar-mode 0)

(ac-config-default)

(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(evil-mode t)
(global-set-key (kbd "M-x") 'execute-extended-command)

(require 'evil-surround)
(global-evil-surround-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(package-selected-packages (quote (yaml-mode magit evil-surround evil auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun save-and-quit ()
  "Save the document and exit buffer"
  (interactive)
  (save-buffer (current-buffer))
  (kill-this-buffer))

(evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
(evil-ex-define-cmd "wq" 'save-and-quit)
(evil-ex-define-cmd "describe-function" 'describe-function)

(defun my-jump-advice (oldfun &rest args)
  (let ((old-pos (point)))
    (apply oldfun args)
    (when (> (abs (- (line-number-at-pos old-pos) (line-number-at-pos (point))))
             1)
      (evil-set-jump old-pos))))

(advice-add 'evil-next-line :around #'my-jump-advice)
(advice-add 'evil-previous-line :around #'my-jump-advice)


(global-set-key (kbd "M-w") 'ace-window)

(evil-add-command-properties #'evil-scroll-down :jump t)
(evil-add-command-properties #'evil-scroll-up :jump t)


;; Go mode

(add-hook 'before-save-hook 'gofmt-before-save)
