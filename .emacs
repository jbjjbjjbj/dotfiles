
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

(ac-config-default)

(setq c-default-style "k&r"
      c-basic-offset 4)


(add-to-list 'auto-mode-alist '("\\.ino\\'" . c++-mode))

(require 'evil)
(evil-mode 1)

(require 'evil-surround)
(global-evil-surround-mode 1)

