(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(use-package ivy :ensure t
:diminish (ivy-mode . "")
:init (ivy-mode t))

(find-file  "/home/spook/.myemacs.d/my-configs/test.org")

(org-babel-tangle)
(load-file  "/home/spook/.myemacs.d/my-configs/test.el")

;; byte compilation
(byte-compile-file "/home/spook/.myemacs.d/my-configs/test.el")
