
(let* ((package--builtins nil)
       (packages
        '(
          ;; utilities
          general which-key ivy swiper counsel fill-column-indicator volatile-highlights
          golden-ratio undo-tree ace-window paredit projectile diminish rainbow-delimiters
          smartparens ggtags expand-region ;;bookmarks+ dired+ no longer on melpa 


          ;; auto-complete
          company flycheck flycheck-tip hippie-exp yasnippet auto-yasnippet flycheck-irony company-c-headers
          elisp-slime-nav company-math company-auctex company-anaconda company-tern
          slime-company company-ycmd flycheck-rust enh-ruby-mode
          company-ghci flycheck-haskell company-statistics

          ;; git
          git-gutter-fringe magit

          ;; markdown
          markdown-mode toml-mode yaml-mode

          ;; c/cpp
          clang-format cmake-mode

          ;; rust
          cargo racer rust-mode

          ;; lisp
          slime racket-mode

          ;; clojure
          cider cider-eval-sexp-fu clj-refactor clojure-mode clojure-snippets

          ;; haskell
          haskell-mode haskell-snippets hindent hlint-refactor

          ;; python
          anaconda-mode pyenv-mode hy-mode pytest

          ;; js
          js-doc js2-mode js2-refactor json-mode json-snatcher web-beautify coffee-mode

          ;; webprogramming
          haml-mode sass-mode scss-mode slim-mode web-mode less-css-mode pug-mode emmet-mode


          ;; ruby
          bundler chruby rbenv rspec-mode rubocop ruby-tools rvm rake

          ;; tex
          auctex auctex-latexmk typo

          ;; org
          org-bullets org-pomodoro org-download htmlize org-journal

          ;; themes
          seti-theme gandalf-theme cyberpunk-theme dark-mint-theme dracula-theme

          ;; other
          writeroom-mode minimap gnuplot csv-mode mips-mode smart-mode-line fireplace

          ;; google
          google-this google-translate

          )))
  (let ((packages (remove-if 'package-installed-p packages)))
    (when packages
      ;; Install uninstalled packages
      (package-refresh-contents)
      (mapc 'package-install packages))))

;; no splash screen, sorry Stallman
(setq inhibit-splash-screen t)

;; don't use the tool or scroll bar, thx
(tool-bar-mode -1)
(scroll-bar-mode -1)

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

;; global hl line mode
(global-hl-line-mode t)

;; can't get flycheck mode to enable itself otherwise
(global-flycheck-mode)

;; make our mode line prettier
(use-package smart-mode-line)
(setq sml/no-confirm-load-theme t)
(sml/setup)

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
(add-hook 'prog-mode-hook 'linum-mode)

;; Set transparency of emacs
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; for dark: seti; for light: gandalf
;; when i feel like a hacker: cyberpunk or dark-mint
(load-theme 'dracula t)

;; pretty symboles
(prettify-symbols-mode t)
(setq-default prettify-symbols-alist '(("lambda" . ?λ)
                                       ("delta" . ?Δ)
                                       ("gamma" . ?Γ)
                                       ("phi" . ?φ)
                                       ("psi" . ?ψ)))

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
(diminish 'ivy-mode)
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
    ;; (use-package dired+) no longer available via melpa
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

(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

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
  :diminish ""
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
               ("<tab>" . company-complete)))
  (add-hook 'after-init-hook 'company-statistics-mode))

;; haskell
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-ghci))

;; python
(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package expand-region
  :bind ("M-/" . er/expand-region))

(defun my-flycheck-customize ()
  (interactive)
  (global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
  (global-set-key (kbd "C-c C-p") 'flycheck-tip-cycle-reverse))

(use-package flycheck
  :defer t
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :diminish ""
  :config
  (use-package flycheck-tip
    :config (add-hook 'flycheck-mode-hook 'my-flycheck-customize)))

(use-package undo-tree
  :init (global-undo-tree-mode t)
  :defer t
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)
    (define-key undo-tree-map (kbd "C-M-/") 'undo-tree-redo)))

(defun generic-org-minor-modes ()
  (interactive)
  (org-bullets-mode 1))
(setq org-journal-dir "/home/spook/Documents/.journal/")
(add-hook 'org-mode-hook 'generic-org-minor-modes)

(defun my-add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook 'my-add-watchwords)

(use-package haskell-mode
  :defer t
  :config
  (defun my-haskell-setup()
    (interactive)
    (haskell-doc-mode)
    (haskell-indent-mode)
    (flycheck-haskell-setup)
    ;;(hindent-mode) ;; must install with stack
    (haskell-snippets-initialize)
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
    ;; (pytest-mode)
    ;; (semantic-mode 0)
    (yapf-mode))
  (add-hook 'python-mode-hook 'my-python-setup))

(defun activate-slime-helper ()
  (when (file-exists-p "~/.quicklisp/slime-helper.el")
    (load (expand-file-name "~/.quicklisp/slime-helper.el"))
    (define-key slime-repl-mode-map (kbd "C-l")
      'slime-repl-clear-buffer))
  (remove-hook 'lisp-mode-hook #'activate-slime-helper))

(add-hook 'lisp-mode-hook #'activate-slime-helper)

(setq inferior-lisp-program "sbcl")

(setq lisp-loop-forms-indentation   6
      lisp-simple-loop-indentation  2
      lisp-loop-keyword-indentation 6)

(defun my/helpful-lisp-modes ()
  (interactive)
  (paredit-mode 1)
  (rainbow-delimiters-mode 2)
  (show-paren-mode 1)
  (eldoc-mode 1))

(add-hook 'lisp-mode-hook #'my/helpful-lisp-modes)

(defun my/turn-on-paredit-and-eldoc ()
  (interactive)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (show-paren-mode 1)
  (eldoc-mode 1))

(add-hook 'emacs-lisp-mode-hook #'my/turn-on-paredit-and-eldoc)
(add-hook 'ielm-mode-hook #'my/turn-on-paredit-and-eldoc)

(use-package eldoc
  :config
  (progn
    (use-package diminish
      :init
      (progn (diminish 'eldoc-mode "")))
    (setq eldoc-idle-delay 0.3)
    (set-face-attribute 'eldoc-highlight-function-argument nil
                        :underline t :foreground "green"
                        :weight 'bold)))

(defun ielm-other-window ()
  "Run ielm on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*ielm*"))
  (call-interactively 'ielm))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'ielm-other-window)
(define-key lisp-interaction-mode-map (kbd "C-c C-z") 'ielm-other-window)

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(use-package rust-mode
  :defer t
  :config
  (defun my-rust-setup ()
    (interactive)
    (racer-mode)
    (cargo-minor-mode)
    (flycheck-rust-setup))
  (add-hook 'rust-mode-hook 'my-rust-setup))

(use-package web-mode
  :defer t
  :config
  (defun my-web-mode-setup ()
    (interactive)
    (emmet-mode)
    (compan)
    ()))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))
(use-package tex
  :ensure auctex)

