(when (file-readable-p "~/.emacs.d/config.org")
 (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "adaf421037f4ae6725aa9f5654a2ed49e2cd2765f71e19a7d26a454491b486eb" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "2e05569868dc11a52b08926b4c1a27da77580daa9321773d92822f7a639956ce" default))
 '(package-selected-packages
   '(rustic lsp-ui yasnippet-snippets nim-mode company-jedi elpy swiper-helm counsel ivy evil-nerd-commenter which-key yasnippet yafolding use-package underwater-theme swiper smooth-scroll rainbow-delimiters parinfer-rust-mode org-bullets no-littering magit-popup magit ivy-prescient general flymake-python-pyflakes flycheck flx evil doom-themes doom-modeline company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
