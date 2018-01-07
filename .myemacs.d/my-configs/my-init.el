(let* ((package--builtins nil)
       (packages
        '(
          ;; utilities
          general which-key ivy swiper counsel fill-column-indicator volatile-highlights
          golden-ratio undo-tree ace-window paredit projectile diminish rainbow-delimiters
          bookmark+ smartparens ggtags


          ;; auto-complete
          company flycheck hippie-exp yasnippet auto-yasnippet flycheck-irony company-c-headers
          elisp-slime-nav company-math company-auctex company-anaconda
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
          anaconda-mode pyenv-mode ;eldoc 

          ;; js

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
          writeroom-mode minimap gnuplot

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

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

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

;; make ispell fast and make it only look at 3 char+ words
(setq ispell-extra-args
      (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
            "--lang=en_US"
            "--ignore=3"))

(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(find-file  "/home/spook/.myemacs.d/my-configs/test.org")

(org-babel-tangle)
(load-file  "/home/spook/.myemacs.d/my-configs/test.el")

;; byte compilation
(byte-compile-file "/home/spook/.myemacs.d/my-configs/test.el")
