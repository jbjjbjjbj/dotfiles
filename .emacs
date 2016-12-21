(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/") 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default default default italic underline success warning error])
 '(ansi-color-names-vector
	 ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(global-auto-revert-mode t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (magit powerline-evil spaceline js2-mode jedi auto-complete undo-tree dashboard neotree)))
 '(tab-stop-list (quote (2)))
 '(tab-width 2)
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(global-linum-mode t)
(setq python-shell-interpreter "/usr/bin/python")


(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))



    (setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)



(global-unset-key "\C-z")
;;(global-set-key "\C-z" 'advertised-undo)

(global-set-key [mouse-5]
    (lambda () (interactive) (next-line 3)))

(global-set-key [mouse-4]
    (lambda () (interactive) (previous-line 3)))



(setq-default tab-width 2)


;; scroll one line at a time (less "jumpy" than defaults)
    
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    
    (setq scroll-step 1) ;; keyboard scroll one line at a time


(defun copy-line (arg)
      "Copy lines (as many as prefix argument) in the kill ring"
      (interactive "p")
      (kill-ring-save (line-beginning-position)
                      (line-beginning-position (+ 1 arg)))
      (message "%d line%s copied" arg (if (= 1 arg) "" "s")))


(global-set-key "\C-c\C-k" 'copy-line)


(defun revert-all-buffers ()
          "Refreshes all open buffers from their respective files"
          (interactive)
          (let* ((list (buffer-list))
                 (buffer (car list)))
            (while buffer
              (when (and (buffer-file-name buffer) 
                         (not (buffer-modified-p buffer)))
                (set-buffer buffer)
                (revert-buffer t t t))
              (setq list (cdr list))
              (setq buffer (car list))))
          (message "Refreshed open files"))


(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'powerline)
(powerline-center-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)


(global-set-key (kbd "C-x C-b") 'bs-show)
(when (fboundp 'winner-mode)
      (winner-mode 1))



;; Julians cool keybindings
(global-unset-key (kbd "C-d"))
(global-set-key (kbd "C-d u") 'undo-tree-visualize)
(global-set-key (kbd "C-d r") '(lambda() (interactive) (load-file "~/.emacs")))
(global-set-key (kbd "C-d i") 'indent-region)
(global-set-key (kbd "C-d e") 'eshell)
(global-set-key (kbd "C-d n") 'neotree)


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)





(display-time-mode 1)

