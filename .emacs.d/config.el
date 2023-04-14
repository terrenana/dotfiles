;; default is 800 kb - measured in bytes
(setq gc-cons-threshold (* 50 1000 1000))

(server-start)

;; profile startup
(add-hook 'emacs-startup-hook (lambda () (message "*** Emacs loaded in %s seconds with %d garbage collections." (emacs-init-time "%.2f") gcs-done)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;; uncomment this if packages arent working
;; (package-refresh-contents)

(eval-when-compile
  (require 'use-package))

;; change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
 url-history-file (expand-file-name "url/history" user-emacs-directory))

;; use no-littering to set common junk files to be in the new user-emacs-directory
(use-package no-littering)

;; tab width is 2
(setq tab-width 2)
(setq evil-shift-width tab-width)
;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
 :config (general-evil-setup t))
(general-create-definer defkey/default
 :states '(normal emacs)
 :prefix "/")
(general-create-definer defkey/org
 :states '(normal emacs)
 :keymaps 'org-mode-map
 :prefix "/")
(general-create-definer defkey/rust
 :states '(normal emacs)
 :keymaps 'rustic-mode-map
 :prefix "/")
(general-create-definer defkey/python
 :states '(normal emacs)
 :keymaps 'python-mode-map
 :prefix "/")

;; define functions we use later
(defun open-config () (interactive) (find-file "~/.emacs.d/config.org") (parinfer-rust-mode))
(defun reload-config () (interactive)(load-file "~/.emacs.d/init.el"))
 ;; autocomment lines
(use-package evil-nerd-commenter)
(use-package evil
 :config (evil-mode 1))
(defkey/default
 "w" 'save-buffer
 "f" 'find-file
 "c" 'open-config
 "r" 'reload-config
 "q" 'evilnc-comment-or-uncomment-lines
 "z" 'ibuffer)

(evil-define-key 'insert 'global (kbd "C-v") 'yank)

(use-package which-key
      :config (which-key-mode))

; remove visual clutter
(setq inhibit-startup-message t)
(setq visible-bell t
                              ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(setq display-line-numbers-type 'relative)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-wilmersdorf t))

;; TODO

; display minor mode menu on the modeline
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

; use the doom-modeline
(use-package doom-modeline
  :after eshell
  :hook (after-init . doom-modeline-init)
  :custom-face
  (mode-line ((t (:height 0.85))))
  (mode-line-inactive ((t (:height 0.85))))
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))
:config
(doom-modeline-mode)

(use-package ws-butler
 :hook text-mode prog-mode)

(use-package parinfer
 :disabled
 :hook clojure-mode
 emacs-lisp-mode
 common-lisp-mode
 scheme-mode
 lisp-mode)
(setq parinfer-extensions
 '(defaults
   pretty-parens
   evil
   smart-tab
   smart-yank))
(setq parinfer-rust-library (expand-file-name "~/.emacs.d/parinfer-rust/libparinfer_rust.so"))

(defkey/default
 "tp" 'parinfer-rust-mode)

(use-package rainbow-delimiters
 :init
 (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package company
   :config
   (setq company-idle-delay 0)
   (setq company-minimum-prefix-length 1)
   (setq company-selection-wrap-around t)
   (company-tng-configure-default))


  (add-hook 'after-init-hook 'global-company-mode)
(evil-define-key 'insert company-active-map (kbd "<ret>") nil)

(use-package ivy
 :config
 (ivy-mode))
(use-package counsel
 :config
 (counsel-mode))
(global-set-key "\C-s" 'swiper)

(use-package yafolding)
(add-hook 'prog-mode-hook (lambda () (yafolding-mode)))
  ;; make yafolding work using <TAB> in all modes
(evil-define-key 'normal global-map
 (kbd "<tab>") #'yafolding-toggle-element)
(evil-define-key 'insert org-mode-map
 (kbd "<tab>") #'yafolding-toggle-element)

;; org mode leader keys
(defkey/org
   "s" 'org-insert-structure-template)

(use-package rustic
 :config
 (setq rustic-format-on-save t)
 (add-hook 'rustic-mode-hook 'rust-lsp-mode))

(defun rust-lsp-mode ()
 ;; called when in rust-mode
 ;; configures rust-specific LSP mode features
 (setq lsp-rust-analyzer-cargo-watch-command "clippy")
 (setq lsp-eldoc-render-all t)
 (setq lsp-idle-delay 0.6))
(defkey/rust
 "r" 'rustic-cargo-run)

(add-hook 'python-mode-hook 'lsp-mode)
(add-hook 'python-mode-hook 'python-lsp-mode)
(defun python-lsp-mode ()
 ;; called when editing a python file
 ;; configures python-specific LSP mode features
 (setq lsp-pyls-plugins-flake8-enabled t)
 (add-to-list 'company-backends 'company-jedi))
(defkey/python
 "r" 'python-shell-send-buffer)

(use-package lsp-mode
 :config
 (add-hook 'lsp-mode-hook 'lsp-ui-mode))
   
(use-package lsp-ui
 :ensure)