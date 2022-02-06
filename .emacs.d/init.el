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


;; Note: Sometimes https gives problems. Unfortunately, switching to http is the solution here.
;; note: Marmalade is a mess, avoid. Elpa is also no longer updated, avoid.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; Load packages
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive. should only have to do this one time.
  (package-install 'use-package)) ; and install the most recent version of use-package

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

(require 'use-package-ensure)
;; TODO: Remove unnecessary `:ensure t` in config.org
(setq use-package-always-ensure t)

;; Bootstrap `quelpa'
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

;; ;; Bootstrap `quelpa-use-package'
;; (unless (package-installed-p 'quelpa-use-package)
;;   (package-install 'quelpa-use-package))

;; Bootstrap `org-mode'
(unless (package-installed-p 'org) ; unless it is already installed
  (package-install 'org))

;; TODO MAKE THESE THREE INTO A LIST COME ON
(unless (package-installed-p 'cl-lib) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'cl-lib))

(require 'cl-lib)
(require 'cl)

(let ((normal-gc-cons-threshold (* 150 1024 1024))
      (init-gc-cons-threshold (* 250 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(org-babel-load-file "~/.emacs.d/config.org")
(byte-compile-file (concat user-emacs-directory "config.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "d71f6c718dab453b625c407adc50479867a557668d5c21599a1ebea204d9e4f3" "5c85b6f7f76fe0e0979da4c650dee525ae5185b134cb0fdfb12eeb580ea4dafb" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "36c86cb6c648b9a15d849026c90bd6a4ae76e4d482f7bcd138dedd4707ff26a5" "6f01d5233bf31b3606972faca955b943eb934c896b9a205cdbf7105b917ce290" "5cdc1832748ae451c19a1546a4bc200750557a924f6124428272f114b6d28ac1" "a12ec87ff9e72a9561314c7ae2c82a373e1b7c80d0fe15579e282080c8d5aef2" "0990a0b1f0b473858c1ae6b73b8d9c3b804cc1251430f54dc080d82cc1e26e24" "2f524d307a2df470825718e27b8e3b81c0112dad112ad126805c043d7c1305c6" "462d6915a7eac1c6f00d5acd8b08ae379e12db2341e7d3eac44ff7f984a5e579" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "26d613485834c8498d96a664d970e19b7d5286c39a78452f492ae5572cf1bd21" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "8ba0a9fc75f2e3b4c254183e814b8b7b8bcb1ad6ca049fde50e338e1c61a12a0" "a9ab62408cda1e1758d913734527a8fdbe6f22e1c06a104375456107063aff9c" "fc65950aacea13c96940a2065ef9b8faefe7a4da44331adf22ea46f8c9b34cdd" "9e8b9cbfe2f4b3510f77fb456b52f8fae38e9103070a2f6155b9bbc1c789da74" "be327a6a477b07f76081480fb93a61fffaa8ddc2acc18030e725da75342b2c2e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "efefb69e7781fcfe62f3d0b573701f56e45e29afbe9e378a422025fd767ea246" "0bff60fb779498e69ea705825a2ca1a5497a4fccef93bf3275705c2d27528f2f" "4ad63526fc7cc542a258977a4b92c9709611720f7207164aa9df416dc0024be8" "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b" "f9f2ea69700b0c660f1a6507bbd0aec13e213b7618336ff20852f617991ae369" default)))
 '(diredp-hide-details-initially-flag nil t)
 '(package-selected-packages
   (quote
    (quelpa diff-hl define-word minimal-theme dockerfile-mode docker lorem-ipsum pandoc-mode vue-mode xterm-color kaolin-themes alarm-clock unfill workgroups toc-org emojify pomidor google-translate google-this fireplace smart-mode-line mips-mode csv-mode gnuplot minimap writeroom-mode dracula-theme dark-mint-theme cyberpunk-theme gandalf-theme seti-theme org-journal htmlize org-download org-pomodoro org-bullets typo auctex-latexmk rake rvm ruby-tools rubocop rspec-mode rbenv chruby bundler emmet-mode pug-mode less-css-mode web-mode slim-mode scss-mode sass-mode haml-mode coffee-mode web-beautify json-snatcher json-mode js2-refactor js2-mode js-doc pytest hy-mode pyenv-mode anaconda-mode hlint-refactor hindent haskell-snippets haskell-mode clojure-snippets clojure-mode clj-refactor cider-eval-sexp-fu cider racket-mode slime rust-mode racer cargo cmake-mode clang-format yaml-mode toml-mode markdown-mode magit git-gutter-fringe company-statistics flycheck-haskell company-ghci enh-ruby-mode flycheck-rust company-ycmd slime-company company-tern company-anaconda company-auctex company-math elisp-slime-nav company-c-headers flycheck-irony auto-yasnippet yasnippet flycheck-tip flycheck company expand-region ggtags smartparens rainbow-delimiters diminish projectile paredit ace-window undo-tree golden-ratio volatile-highlights fill-column-indicator counsel swiper ivy which-key general use-package)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 )
(provide 'init)

;;; init ends here
