(let* ((package--builtins nil)
       (packages
        '(
          ;; utilities
          general which-key ivy swiper counsel fill-column-indicator volatile-highlights
          golden-ratio undo-tree ace-window paredit projectile diminish rainbow-delimiters
          bookmark+ smartparens ggtags dired+


          ;; auto-complete
          company flycheck hippie-exp yasnippet auto-yasnippet flycheck-irony company-c-headers
          elisp-slime-nav company-math company-auctex company-anaconda company-tern
          slime-company company-ycmd flycheck-rust enh-ruby-mode
          company-ghci company-ghc flycheck-haskell


          ;; git
          git-gutter-fringe magit

          ;; markdown
          markdown-mode toml-mode yaml-mode

          ;; c/cpp
          clang-format cmake-mode

          ;; rust
          cargo company racer rust-mode

          ;; lisp
          slime racket-mode

          ;; clojure
          cider cider-eval-sexp-fu clj-refactor clojure-mode clojure-snippets

          ;; haskell
          haskell-mode haskell-snippets hindent hlint-refactor 

          ;; python
          anaconda-mode pyenv-mode hy-mode;eldoc 

          ;; js
          js-doc js2-mode js2-refactor json-mode json-snatcher web-beautify coffee-mode

          ;; webprogramming
          haml-mode sass-mode scss-mode slim-mode web-mode less-css-mode pug-mode emmet-mode
          ;company-web-html company-web-jade company-web-slim                  
          ;css-mode

          ;; ruby
          bundler chruby rbenv rspec-mode rubocop ruby-tools rvm rake 

          ;; tex
          auctex auctex-latexmk typo

          ;; org 
          org-bullets org-pomodoro org-download htmlize

          ;; themes
          seti-theme gandalf-theme cyberpunk-theme dark-mint-theme 

          ;; other
          writeroom-mode minimap gnuplot csv-mode mips-mode

          )))
  (let ((packages (remove-if 'package-installed-p packages)))
    (when packages
      ;; Install uninstalled packages
      (package-refresh-contents)
      (mapc 'package-install packages))))

;; no splash screen, sorry Stallman
(setq inhibit-splash-screen t)

;; don't use the tool bar, thx
(tool-bar-mode -1)

;; do like menu bar
(menu-bar-mode 1)

;; UTF-8 is our friend in a world of shitty programming standards
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; speaking of text, unify emacs clipboard with linux
(setq x-select-enable-clipboard t)

;; make sure it attempts utf-8 first when pasting text into emacs
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; always tell me the column and row of where i am please, i am already lost enough as is.
(line-number-mode 1)
(column-number-mode 1)

;; do you seriously think i'm good at typing
(setq read-file-name-completion-ignore-case t)

;; life is too short for fully authenticating bad decisions
(defalias 'yes-or-no-p 'y-or-n-p)

;; fix bad escape sequence weirdness
(setq system-uses-terminfo nil)

;; stronk encryption good
(setq gnutls-min-prime-bits 4096)

;; don't let me be moronic and kill emacs w/o warning. GUI only.
(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

;; move through lines based on visual appearance rather than literal. Very useful for long, single lines.
(setq line-move-visual t)

;; differentiate buffers with identical names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; use newer files rather than old compiled files, .elc
(setq load-prefer-newer t)

;; if emacs crashes w/o warning, we want a valid list of recent files, don't we?
(run-at-time nil (* 5 60) 'recentf-save-list)

;; finally, set emacs to display the path directory in the menu bar
;; displays current working directory at all times in emacs
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; lisp and haskell got me used to working with 2 space indents, idk man
(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 2)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; for dark: seti; for light: gandalf
;; when i feel like a hacker: cyberpunk or dark-mint
(load-theme 'seti t)

;; C-c LEFT to undo window change, which i need often
(use-package winner
  :init (winner-mode 1))

;; re-opens file at last place edited
(use-package saveplace
  :defer t
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; enables recent-files to be re-opened
(recentf-mode 1)
(setq recentf-max-menu-items 50)

;; do not need spam in recents list
(setq recentf-exclude '("/auto-install/" ".recentf" "/repos/" "/elpa/"
                        "\\.mime-example" "\\.ido.last" "COMMIT_EDITMSG"
                        ".gz"
                        "~$" "/tmp/" "/ssh:" "/sudo:" "/scp:"))
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(defun my/dired-mode-hook ()
  (my/turn-on-hl-line-mode)
  (toggle-truncate-lines 1))

(use-package dired
  :bind ("C-x C-j" . dired-jump)
  :config
  (progn
    (use-package dired-x
      :init (setq-default dired-omit-files-p t)
      :config
      (add-to-list 'dired-omit-extensions ".DS_Store"))
    (customize-set-variable 'diredp-hide-details-initially-flag nil)
    (use-package dired+)
    (use-package dired-aux
      :init (use-package dired-async))
    (put 'dired-find-alternate-file 'disabled nil)
    (setq ls-lisp-dirs-first t
          dired-recursive-copies 'always
          dired-recursive-deletes 'always
          dired-dwim-target t
          ;; -F marks links with @
          dired-ls-F-marks-symlinks t
          delete-by-moving-to-trash t
          ;; Auto refresh dired
          global-auto-revert-non-file-buffers t
          wdired-allow-to-change-permissions t)
    (add-hook 'dired-mode-hook #'my/dired-mode-hook)))

;; make ispell fast and make it only look at 3 char+ words
(setq ispell-extra-args
      (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
            "--lang=en_US"
            "--ignore=3"))

;; whitespace checker starts at 80
(setq whitespace-line-column 80)

;; what whitespace looks for
(setq whitespace-style '(tabs newline space-mark
                   tab-mark newline-mark
                   face lines-tail))

;; special visual market up for non-whitespace
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      ;; 32 SPACE, 183 MIDDLE DOT
      '((space-mark nil)
        ;; 10 LINE FEED
        ;;(newline-mark 10 [172 10])
        (newline-mark nil)
        ;; 9 TAB, MIDDLE DOT
        (tab-mark 9 [183 9] [92 9])))

;; disabled for modes that it doesn't make sense
(setq whitespace-global-modes '(not org-mode
                                   eshell-mode
                                   shell-mode
                                   web-mode
                                   log4j-mode
                                   "Web"
                                   dired-mode
                                   emacs-lisp-mode
                                   clojure-mode
                                   lisp-mode))

(use-package company
  :defer t
  :bind ("C-." . company-complete)
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-selection-wrap-around t
          company-dabbrev-downcase nil
          company-transformers '(company-sort-by-occurrence))
    (bind-keys :map company-active-map
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)
               ("C-d" . company-show-doc-buffer)
               ("<tab>" . company-complete))))

;; haskell


;; python
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

;; (use-package haskell-mode
;;   :defer t
;;   :init
;;   (progn
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;     (add-hook 'haskell-mode-hook 'turn-on-haskell-indent))
;;   :config
;;   (setq haskell-process-type 'stack-ghci))


(use-package haskell-mode
  :defer t
  :config
  (defun my-haskell-setup()
    (interactive)
    (haskell-doc-mode)
    (haskell-indent-mode)
    (setq haskell-process-type 'stack-ghci))
  (add-hook 'haskell-mode-hook 'my-haskell-setup))

(use-package python
  :defer t
  :config
  (defun my-python-setup ()
    (interactive)
    (anaconda-mode)
    (anaconda-eldoc-mode)
    (pyenv-mode)
    (yapf-mode))
  (add-hook 'python-mode-hook 'my-python-setup))



(find-file  "/home/spook/.myemacs.d/my-configs/test.org")

(org-babel-tangle)
(load-file  "/home/spook/.myemacs.d/my-configs/test.el")

;; byte compilation
(byte-compile-file "/home/spook/.myemacs.d/my-configs/test.el")
