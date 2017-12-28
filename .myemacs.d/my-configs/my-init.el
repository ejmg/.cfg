(let* ((package--builtins nil)
       (packages
        '(
          ;; utilities
          general which-key ivy swiper counsel fill-column-indicator volatile-highlights
          golden-ratio undo-tree ace-window paredit projectile diminish rainbow-delimiters
          bookmark+


          ;; auto-complete
          company flycheck hippie-exp yasnippet auto-yasnippet flycheck-irony company-c-headers
          eldoc elisp-slime-nav company-math company-auctex company-jedi company-web-html
          company-web-jade company-web-slim slime-company company-ycmd flycheck-rust


          ;; git
          git-gutter-fringe magit

          ;; markdown
          markdown-mode

          ;; c/cpp
          cc-mode clang-format cmake-mode

          ;; rust
          cargo company racer rust-mode

          ;; haskell

          ;; python

          ;; js

          ;; webprogramming
          css-mode haml-mode sass-mode scss-mode slim-mode web-mode less-css-mode pug-mode emmet-mode

          ;; ruby

          ;; tex
          auctex auctex-latexmk

          )))
  (let ((packages (remove-if 'package-installed-p packages)))
    (when packages
      ;; Install uninstalled packages
      (package-refresh-contents)
      (mapc 'package-install packages))))







(find-file  "/home/spook/.myemacs.d/my-configs/test.org")

(org-babel-tangle)
(load-file  "/home/spook/.myemacs.d/my-configs/test.el")

;; byte compilation
(byte-compile-file "/home/spook/.myemacs.d/my-configs/test.el")
