;;; package --- Summary
;;; Commentary:
;; This is the initialization file. It gets everything I need ready. Most custom configs occur I set
;; in my-init.org which will be processed after base settings are configured by init.el
;;
;; author: elias garcia
;; version: 08.11.17

;;; Code:

;; Don't load anything till we tell emacs where to pull its packages from.
(setq package-enable-at-startup nil)


;; Note: Sometimes https gives problems. Unfortunately, switching to http is the solution here.
(setq package-archives '(("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
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

(org-babel-load-file "~/.myemacs.d/config.org")

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
