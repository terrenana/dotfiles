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
  (set-face-attribute (car face) nil :font "LiberationMono" :weight 'regular :height (cdr face))))

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

(set-frame-font "LiberationMono" nil t)
(use-package unicode-fonts :ensure t :demand t
 :config
 (unicode-fonts-setup))

(load-file (expand-file-name "~/.emacs.d/noctilux-theme.el"))
(load-theme 'noctilux t)

(use-package evil :ensure t :demand t
  :config
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

(use-package parinfer-rust-mode :ensure t :demand t
  :hook emacs-lisp-mode
  :config
  (setq parinfer-rust-check-before-enable nil))

(setq-default indent-tabs-mode nil)

(use-package rainbow-delimiters :ensure t :demand t
   :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(defun reload-config () (interactive)(load-file "~/.emacs.d/init.el"))
(defun cl/tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle-file (buffer-file-name)))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cl/tangle-config)))
