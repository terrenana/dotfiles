(defun cl/init-org-mode ()
  (org-indent-mode)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . cl/init-org-mode)
  :config

(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font (case system-type
                                             ('gnu/linux "LiberationMono")
                                             ('darwin "Liberation Mono"))
                      :weight 'regular :height (cdr face))))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(use-package org-bullets :ensure t :demand t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package poly-org :ensure t :demand t
  :after org
  :hook (org-mode . poly-org-mode))

(setq display-line-numbers-type 'relative)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(setq inhibit-startup-message t)
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)

(set-frame-font (case system-type
                  ('darwin "Liberation Mono")
                  ('gnu/linux "LiberationMono"))
                nil t)
(use-package nerd-icons :ensure t :demand t)

(load-file (expand-file-name "~/.emacs.d/noctilux-theme.el"))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                   (load-theme 'noctilux t))))
  (load-theme 'noctilux t))

(use-package doom-modeline :ensure t :demand t
  :custom
  (doom-modeline-height 30)
  (doom-modeline-bar-width 7)
  (doom-modeline-support-imenu t)
  (doom-modeline-icon nil)
  (doom-modeline-enable-word-count t)
  (doom-modeline-buffer-encoding nil)
 (doom-modeline-minor-modes t)
 :config
 (setq inhibit-compacting-font-caches t)
 (doom-modeline-mode))

(use-package evil :ensure t :demand t
  :config
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

(use-package parinfer-rust-mode :ensure t :demand t
  :hook emacs-lisp-mode
  :config
  (setq parinfer-rust-check-before-enable nil)

  (setq parinfer-rust-library (expand-file-name "~/.emacs.d/parinfer-rust/parinfer-rust.so")))

(setq-default indent-tabs-mode nil)

(use-package rainbow-delimiters :ensure t :demand t
   :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general :ensure t :demand t
  :after evil
  :config (general-evil-setup t)
  (general-create-definer defkey/leader
   :states '(normal emacs)
   :prefix "/")
  (general-create-definer defkey/haskell
    :states '(normal emacs)
    :keymaps 'haskell-mode-map
    :prefix "/")
 (defkey/leader
  "s" 'save-buffer
  "w" 'save-buffers-kill-emacs
  "f" 'find-file))

(use-package ivy :ensure t :demand t
  :config
  (ivy-mode))

(use-package counsel :ensure t :demand t
  :config
  (counsel-mode))

(global-set-key "\C-s" 'swiper)

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

(use-package no-littering :ensure t :demand t)

(setq auto-save-file-name-transforms
      '((".*" "~/.cache/emacs/backup/" t)))

(defun reload-config () (interactive)(load-file "~/.emacs.d/init.el"))
 (defun cl/tangle-config () 
   (when (string-equal (buffer-file-name)
                       (expand-file-name "~/dotfiles/.emacs.d/config.org"))
     (let ((org-confirm-babel-evaluate nil))
       (org-babel-tangle-file "~/.emacs.d/config.org"))))
  
(advice-add #'cl/tangle-config :around #'polymode-with-current-base-buffer)

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cl/tangle-config)))

(use-package haskell-mode :ensure t :demand t
  :config
  (defkey/haskell
    "r" 'haskell-compile))
