
;; open my configuration file
(find-file (concat default-directory "my-init.org"))

;; "tangle it", extracting my source code blocks
(org-babel-tangle)

;; load it, goodbye this file!
(load-file (concat default-directory "my-init.el"))

;; byte compilation
(byte-compile-file (concat default-directory "my-init.el"))
