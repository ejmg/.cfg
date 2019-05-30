;;; package --- Summary
;;; Commentary:
;; This is the initialization file.  It gets everything I need ready.  Most
;; custom configs occur I set in config.org which will be processed after base
;; settings are configured by init.el
;;
;; author: elias garcia
;; version: 08.11.17

;;; Code:

;; Don't load anything till we tell emacs where to pull its packages from.
(setq package-enable-at-startup nil)

(setq user-emacs-directory (file-truename "~/.emacs.d/"))


;; Note: Sometimes https gives problems. Unfortunately, switching to http is the solution here.
;; note: Marmalade is a mess, avoid. Elpa is also no longer updated, avoid.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Load packages
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

;; Bootstrap `org-mode'
(unless (package-installed-p 'org) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'org))


;; TODO MAKE THESE THREE INTO A LIST COME ON
(unless (package-installed-p 'cl) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'cl))

(require 'cl)

(org-babel-load-file "~/.emacs.d/config.org")

;; Other way of doing this that I'm abandoning but don't want to forget.
;; It is possible to byte-compile your emacs configs which makes execution faster.
;; How beneficial this is vs the compile time required is ???
;; find the config file and load it
;; (find-file  "/home/spook/.myemacs.d/my-configs/my-init.org")
;; extract all the code blocks from my-init.org
;; (org-babel-tangle)
;; convert it into my-init.el
;; (load-file  "/home/spook/.myemacs.d/my-configs/my-init.el")
;; byte compilation
;; (byte-compile-file "/home/spook/.myemacs.d/my-configs/my-init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (pomidor google-translate google-this fireplace smart-mode-line mips-mode csv-mode gnuplot minimap writeroom-mode dracula-theme dark-mint-theme cyberpunk-theme gandalf-theme seti-theme org-journal htmlize org-download org-pomodoro org-bullets typo auctex-latexmk auctex rake rvm ruby-tools rubocop rspec-mode rbenv chruby bundler emmet-mode pug-mode less-css-mode web-mode slim-mode scss-mode sass-mode haml-mode coffee-mode web-beautify json-snatcher json-mode js2-refactor js2-mode js-doc pytest hy-mode pyenv-mode anaconda-mode hlint-refactor hindent haskell-snippets haskell-mode clojure-snippets clojure-mode clj-refactor cider-eval-sexp-fu cider racket-mode slime rust-mode racer cargo cmake-mode clang-format yaml-mode toml-mode markdown-mode magit git-gutter-fringe company-statistics flycheck-haskell company-ghci enh-ruby-mode flycheck-rust company-ycmd slime-company company-tern company-anaconda company-auctex company-math elisp-slime-nav company-c-headers flycheck-irony auto-yasnippet yasnippet flycheck-tip flycheck company expand-region ggtags smartparens rainbow-delimiters diminish projectile paredit ace-window undo-tree golden-ratio volatile-highlights fill-column-indicator counsel swiper ivy which-key general use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)

;;; init ends here
(put 'narrow-to-region 'disabled nil)
