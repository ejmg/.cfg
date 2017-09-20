;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
   "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
   (setq-default
      ;; Base distribution to use. This is a layer contained in the directory
      ;; `+distribution'. For now available distributions are `spacemhttps://github.com/syl20bnr/spacemacs/blob/master/layers/+fun/selectric/README.orgacs-base'
      ;; or `spacemacs'. (default 'spacemacs)
      dotspacemacs-distribution 'spacemacs
      ;; Lazy installation of layers (i.e. layers are installed only when a file
      ;; with a supported type is opened). Possible values are `all', `unused'
      ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
      ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
      ;; lazy install any layer that support lazy installation even the layers
      ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
      ;; installation feature and you have to explicitly list a layer in the
      ;; variable `dotspacemacs-configuration-layers' to install it.
      ;; (default 'unused)
      dotspacemacs-enable-lazy-installation 'unused
      ;; If non-nil then Spacemacs will ask for confirmation before installing
      ;; a layer lazily. (default t)
      dotspacemacs-ask-for-lazy-installation t
      ;; If non-nil layers with lazy install support are lazy installed.
      ;; List of additional paths where to look for configuration layers.
      ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
      dotspacemacs-configuration-layer-path '()
      ;; List of configuration layers to load.
      dotspacemacs-configuration-layers
      '(
        octave
          ruby
          helm
          (auto-completion :variables
             auto-completion-enable-help-tooltip t
             auto-completion-enable-snippets-in-popup t
             auto-completion-enable-sort-by-usage t)
          ;;semantic
          gtags
          spell-checking
          syntax-checking
          csv
          python
          ;; ----------------------------------------------------------------
          ;; Example of useful layers you may want to use right away.
          ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
          ;; <M-m f e R> (Emacs style) to install them.
          ;; ----------------------------------------------------------------
          (c-c++ :variables
                 c-c++-enable-clang-support t
                 c-c++-default-mode-for-headers 'c++-mode
                 )
          rust
          (common-lisp :variables
                       common-lisp-style-default 'sbcl)
          javascript
          better-defaults
          emacs-lisp
          git
          markdown
          html
          java
          org
          latex
          bibtex
          yaml
          nginx
          pdf-tools
          ipython-notebook
          (erc :variables
               erc-server-list
               '(("irc.mozilla.org"
                 :port "6697"
                 :ssl t
                 :nick "[spook]"
                 )))
          (shell :variables
                  shell-default-height 30
                  shell-default-position 'bottom)
          version-control
          )
      ;; List of additional packages that will be installed without being
      ;; wrapped in a layer. If you need some configuration for these
      ;; packages, then consider creating a layer. You can also put the
      ;; configuration in `dotspacemacs/user-config'.
      dotspacemacs-additional-packages '(
                                           dark-mint-theme
                                           android-mode
                                           gandalf-theme
                                           seti-theme
                                           (mips-mode
                                            :mode "\\.s$")
                                           )
      ;; A list of packages that cannot be updated.
      dotspacemacs-frozen-packages '()
      ;; A list of packages that will not be installed and loaded.
      dotspacemacs-excluded-packages '()
      ;; Defines the behaviour of Spacemacs when installing packages.
      ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
      ;; `used-only' installs only explicitly used packages and uninstall any
      ;; unused packages as well as their unused dependencies.
      ;; `used-but-keep-unused' installs only the used packages but won't uninstall
      ;; them if they become unused. `all' installs *all* packages supported by
      ;; Spacemacs and never uninstall them. (default is `used-only')
      dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
   "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
   ;; This setq-default sexp is an exhaustive list of all the supported
   ;; spacemacs settings.
   (setq-default
      ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
      ;; possible. Set it to nil if you have no way to use HTTPS in your
      ;; environment, otherwise it is strongly recommended to let it set to t.
      ;; This variable has no effect if Emacs is launched with the parameter
      ;; `--insecure' which forces the value of this variable to nil.
      ;; (default t)
      dotspacemacs-elpa-https t
      ;; Maximum allowed time in seconds to contact an ELPA repository.
      dotspacemacs-elpa-timeout 5
      ;; If non nil then spacemacs will check for updates at startup
      ;; when the current branch is not `develop'. Note that checking for
      ;; new versions works via git commands, thus it calls GitHub services
      ;; whenever you start Emacs. (default nil)
      dotspacemacs-check-for-update nil
      ;; If non-nil, a form that evaluates to a package directory. For example, to
      ;; use different package directories for different Emacs versions, set this
      ;; to `emacs-version'.
      dotspacemacs-elpa-subdirectory nil
      ;; One of `vim', `emacs' or `hybrid'.
      ;; `hybrid' is like `vim' except that `insert state' is replaced by the
      ;; `hybrid state' with `emacs' key bindings. The value can also be a list
      ;; with `:variables' keyword (similar to layers). Check the editing styles
      ;; section of the documentation for details on available variables.
      ;; (default 'vim)
      dotspacemacs-editing-style 'emacs
      ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
      dotspacemacs-verbose-loading nil
      ;; Specify the startup banner. Default value is `official', it displays
      ;; the official spacemacs logo. An integer value is the index of text
      ;; banner, `random' chooses a random text banner in `core/banners'
      ;; directory. A string value must be a path to an image format supported
      ;; by your Emacs build.
      ;; If the value is nil then no banner is displayed. (default 'official)
      dotspacemacs-startup-banner 'official
      ;; List of items to show in startup buffer or an association list of
      ;; the form `(list-type . list-size)`. If nil then it is disabled.
      ;; Possible values for list-type are:
      ;; `recents' `bookmarks' `projects' `agenda' `todos'."
      ;; List sizes may be nil, in which case
      ;; `spacemacs-buffer-startup-lists-length' takes effect.
      dotspacemacs-startup-lists '((recents . 5)
                                     (projects . 7))
      ;; True if the home buffer should respond to resize events.
      dotspacemacs-startup-buffer-responsive t
      ;; Default major mode of the scratch buffer (default `text-mode')
      dotspacemacs-scratch-mode 'text-mode
      ;; List of themes, the first of the list is loaded when spacemacs starts.
      ;; Press <SPC> T n to cycle to the next theme in the list (works great
      ;; with 2 themes variants, one dark and one light)
      dotspacemacs-themes '(
                            seti
                            gandalf
                            dark-mint
                              )
      ;; If non nil the cursor color matches the state color in GUI Emacs.
      dotspacemacs-colorize-cursor-according-to-state t
      ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
      ;; quickly tweak the mode-line size to make separators look not too crappy.
      dotspacemacs-default-font '("Source Code Pro"
                                    :size 13
                                    :weight normal
                                    :width normal
                                    :powerline-scale 1.1)
      ;; The leader key
      dotspacemacs-leader-key "SPC"
      ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
      ;; (default "SPC")
      dotspacemacs-emacs-command-key "SPC"
      ;; The key used for Vim Ex commands (default ":")
      dotspacemacs-ex-command-key ":"
      ;; The leader key accessible in `emacs state' and `insert state'
      ;; (default "M-m")
      dotspacemacs-emacs-leader-key "M-m"
      ;; Major mode leader key is a shortcut key which is the equivalent of
      ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
      dotspacemacs-major-mode-leader-key ","
      ;; Major mode leader key accessible in `emacs state' and `insert state'.
      ;; (default "C-M-m")
      dotspacemacs-major-mode-emacs-leader-key "C-M-m"
      ;; These variables control whether separate commands are bound in the GUI to
      ;; the key pairs C-i, TAB and C-m, RET.
      ;; Setting it to a non-nil value, allows for separate commands under <C-i>
      ;; and TAB or <C-m> and RET.
      ;; In the terminal, these pairs are generally indistinguishable, so this only
      ;; works in the GUI. (default nil)
      dotspacemacs-distinguish-gui-tab nil
      ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
      dotspacemacs-remap-Y-to-y$ nil
      ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
      ;; there. (default t)
      dotspacemacs-retain-visual-state-on-shift t
      ;; If non-nil, J and K move lines up and down when in visual mode.
      ;; (default nil)
      dotspacemacs-visual-line-move-text nil
      ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
      ;; (default nil)
      dotspacemacs-ex-substitute-global nil
      ;; Name of the default layout (default "Default")
      dotspacemacs-default-layout-name "Default"
      ;; If non nil the default layout name is displayed in the mode-line.
      ;; (default nil)
      dotspacemacs-display-default-layout nil
      ;; If non nil then the last auto saved layouts are resume automatically upon
      ;; start. (default nil)
      dotspacemacs-auto-resume-layouts nil
      ;; Size (in MB) above which spacemacs will prompt to open the large file
      ;; literally to avoid performance issues. Opening a file literally means that
      ;; no major mode or minor modes are active. (default is 1)
      dotspacemacs-large-file-size 1
      ;; Location where to auto-save files. Possible values are `original' to
      ;; auto-save the file in-place, `cache' to auto-save the file to another
      ;; file stored in the cache directory and `nil' to disable auto-saving.
      ;; (default 'cache)
      dotspacemacs-auto-save-file-location 'cache
      ;; Maximum number of rollback slots to keep in the cache. (default 5)
      dotspacemacs-max-rollback-slots 5
      ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
      dotspacemacs-helm-resize nil
      ;; if non nil, the helm header is hidden when there is only one source.
      ;; (default nil)
      dotspacemacs-helm-no-header nil
      ;; define the position to display `helm', options are `bottom', `top',
      ;; `left', or `right'. (default 'bottom)
      dotspacemacs-helm-position 'bottom
      ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
      ;; in all non-asynchronous sources. If set to `source', preserve individual
      ;; source settings. Else, disable fuzzy matching in all sources.
      ;; (default 'always)
      dotspacemacs-helm-use-fuzzy 'always
      ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
      ;; several times cycle between the kill ring content. (default nil)
      dotspacemacs-enable-paste-transient-state nil
      ;; Which-key delay in seconds. The which-key buffer is the popup listing
      ;; the commands bound to the current keystroke sequence. (default 0.4)
      dotspacemacs-which-key-delay 0.4
      ;; Which-key frame position. Possible values are `right', `bottom' and
      ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
      ;; right; if there is insufficient space it displays it at the bottom.
      ;; (default 'bottom)
      dotspacemacs-which-key-position 'bottom
      ;; If non nil a progress bar is displayed when spacemacs is loading. This
      ;; may increase the boot time on some systems and emacs builds, set it to
      ;; nil to boost the loading time. (default t)
      dotspacemacs-loading-progress-bar t
      ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
      ;; (Emacs 24.4+ only)
      dotspacemacs-fullscreen-at-startup nil
      ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
      ;; Use to disable fullscreen animations in OSX. (default nil)
      dotspacemacs-fullscreen-use-non-native nil
      ;; If non nil the frame is maximized when Emacs starts up.
      ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
      ;; (default nil) (Emacs 24.4+ only)
      dotspacemacs-maximized-at-startup nil
      ;; A value from the range (0..100), in increasing opacity, which describes
      ;; the transparency level of a frame when it's active or selected.
      ;; Transparency can be toggled through `toggle-transparency'. (default 90)
      dotspacemacs-active-transparency 85
      ;; A value from the range (0..100), in increasing opacity, which describes
      ;; the transparency level of a frame when it's inactive or deselected.
      ;; Transparency can be toggled through `toggle-transparency'. (default 90)
      dotspacemacs-inactive-transparency 90
      ;; If non nil show the titles of transient states. (default t)
      dotspacemacs-show-transient-state-title t
      ;; If non nil show the color guide hint for transient state keys. (default t)
      dotspacemacs-show-transient-state-color-guide t
      ;; If non nil unicode symbols are displayed in the mode line. (default t)
      dotspacemacs-mode-line-unicode-symbols t
      ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
      ;; scrolling overrides the default behavior of Emacs which recenters point
      ;; when it reaches the top or bottom of the screen. (default t)
      dotspacemacs-smooth-scrolling t
      ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
      ;; derivatives. If set to `relative', also turns on relative line numbers.
      ;; (default nil)
      dotspacemacs-line-numbers nil
      ;; Code folding method. Possible values are `evil' and `origami'.
      ;; (default 'evil)
      dotspacemacs-folding-method 'evil
      ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
      ;; (default nil)
      dotspacemacs-smartparens-strict-mode nil
      ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
      ;; over any automatically added closing parenthesis, bracket, quote, etc…
      ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
      dotspacemacs-smart-closing-parenthesis nil
      ;; Select a scope to highlight delimiters. Possible values are `any',
      ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
      ;; emphasis the current one). (default 'all)
      dotspacemacs-highlight-delimiters 'all
      ;; If non nil, advise quit functions to keep server open when quitting.
      ;; (default nil)
      dotspacemacs-persistent-server nil
      ;; List of search tool executable names. Spacemacs uses the first installed
      ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
      ;; (default '("ag" "pt" "ack" "grep"))
      dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
      ;; The default package repository used if no explicit repository has been
      ;; specified with an installed package.
      ;; Not used for now. (default nil)
      dotspacemacs-default-package-repository nil
      ;; Delete whitespace while saving buffer. Possible values are `all'
      ;; to aggressively delete empty line and long sequences of whitespace,
      ;; `trailing' to delete only the whitespace at end of lines, `changed'to
      ;; delete only whitespace for changed lines or `nil' to disable cleanup.
      ;; (default nil)
      dotspacemacs-whitespace-cleanup nil
      ))

(defun dotspacemacs/user-init ()
   "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

   (set-face-background 'default nil)

   ;;PACKAGES
   ;;-------------------------------
   ;;HOON-MODE FOR URBIT
   ;;has to be a more graceful way of installing it
   (load-file "~/.emacs.d/private/local/hoon-mode.el")
   (require 'hoon-mode)

   ;; GPG ENCRYPTION SUPPORT
   ;; for files like .org, just add the extra extension '.gpg'
   (require 'epa-file)
   (epa-file-enable)

   ;; POMIDOR
   ;; taken from https://github.com/TatriX/pomidor
   ;; commands: 'm-x pomidor' to start, 'enter' to start new pomodoro, 'space' start break, 'R' reset, 'q' quite pomodoro buffer, 'Q' turn off
   (add-to-list 'load-path "~/.emacs.d/private/local/pomidor/")
   (require 'pomidor)

   ;;C++ Configurations
   ; still not sure why these need to be set here? Maybe layer configs override otherwise?
   (setq-default
      c-default-style "bsd"
      c-basic-offset 3
      c-basic-indent 3)
   )

(defun dotspacemacs/user-config ()
   "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

   ;;GENERAL CONFIGURATIONS
   ;;-------------------------------

   (setenv "PATH" (shell-command-to-string "echo -n $PATH"))
   (custom-set-variables '(android-mode-sdk-dir "~/Android/Sdk"))
   (global-company-mode)

   (global-linum-mode t) ;; enable line numbers globally
   (setq column-number-mode t);;enable column numbers everywhere
   (setq-default fill-column 100)
   ;; displays current working directory at all times in Emacs
   (setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
         '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

   ;;KEY BINDINGS AND EDITS
   ;;------------------------------
   (global-set-key (kbd "C-~") 'ace-window)

   ;; enables c-a/e to go to beginning of header rather than line in org
   (setq org-special-ctrl-a/e 'reverse)

   ;; disables ido-mode while writing file
   (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)

   (setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
   (epa-file-name-regexp-update)

   ;;SPACING, TABS, AND LANG CONFIGURATIONS
   ;;-------------------------------

   ;; MIPS
   ;; allows mips to override asm and assembly mode for .asm files
   (add-to-list 'auto-mode-alist '("\\.asm$" . mips-mode))
   (add-hook 'mips-mode-hook 'fci-mode)

   ;;C AND C++
   ;;No tabs, set to 3 spaces when found
   (setq-default indent-tabs-mode nil)
   (setq-default tab-width 4)
   (setq c-guess-current-offset nil)
   (add-hook 'c++-mode-hook 'fci-mode)
   ;;(setq company-backends (delete 'company-semantic company-backends))
   ;;(add-to-list 'company-backends '(company-clang company-dabbrev))

   ;;PYTHON
   ;; attempt to fix stupid inline error for shell process
   (with-eval-after-load 'python
      (defun python-shell-completion-native-try ()
         "Return non-nil if can trigger native completion."
         (let ((python-shell-completion-native-enable t)
                 (python-shell-completion-native-output-timeout
                    python-shell-completion-native-try-output-timeout))
            (python-shell-completion-native-get-completions
               (get-buffer-process (current-buffer))
               nil "_"))))

   (setq-default py-indent-tabs-mode nil)
   (setq python-indent 4)
   (setq python-indent-offset 4)
   (setq python-guess-indent nil)
   (setenv "WORKON_HOME" "~/.pyenv/versions/")
   (add-hook 'python-mode-hook 'fci-mode)

   ;; if quotes act up again in python mode, try this:
   ;;(add-hook 'python-mode-hook (lambda() (smartparens-mode 0)))

   ;;JAVA
   (setq eclim-eclipse-dirs "~/eclipse")
   (setq eclim-executable "~/eclipse/eclim")
   (add-hook 'java-mode-hook 'fci-mode)

   ;;LISP
   ;;(setq slime-contribs '(slime-cl-indent))
   ;;(setq lisp-indent-function 'common-lisp-indent-function)
   ;;(setq common-lisp-style "sbcl")
   ;;(setq-default common-lisp-style "sbcl")
   ;;(setq-default lisp-indent-offset 4)

   ;;ORG MODE
   (add-hook 'org-mode-hook (lambda () (linum-mode 0)))
   ;; I am childish with my configs
   (setq org-bullets-bullet-list (quote ("◉" "▣" "●" "◼" "►" "▷" "◆")))
   (setq org-hide-emphasis-markers t)

   ;;PDF TOOLS
   (add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))

   ;;POMIDOR
   (add-hook 'pomidor-mode-hook (lambda () (linum-mode 0)))

   ;; Weird Theme edits
   (custom-theme-set-faces
    'gandalf
    '(underline ((t (:underline t))))
    '(italic ((t (:slant italic)))))

   )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(android-mode-sdk-dir "~/Android/Sdk")
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(compilation-message-face (quote default))
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "5999e12c8070b9090a2a1bbcd02ec28906e150bb2cdce5ace4f965c76cf30476" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "5e2dc1360a92bb73dafa11c46ba0f30fa5f49df887a8ede4e3533c3ab6270e08" "c1390663960169cd92f58aad44ba3253227d8f715c026438303c09b9fb66cdfb" "a632c5ce9bd5bcdbb7e22bf278d802711074413fd5f681f39f21d340064ff292" "1b1e54d9e0b607010937d697556cd5ea66ec9c01e555bb7acea776471da59055" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2d16f85f22f1841390dfc1234bd5acfcce202d9bb1512aa8eabd0068051ac8c3" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "steel blue" t)
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" . 0)
     ("#63de5d" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#424748" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (mips-mode x86-lookup nasm-mode org-category-capture request-deferred deferred erc-yt erc-view-log erc-social-graph erc-image erc-hl-nicks toml-mode racer flycheck-rust seq cargo rust-mode slime-company slime common-lisp-snippets rvm ruby-tools ruby-test-mode rubocop rspec-mode robe rbenv rake minitest chruby bundler inf-ruby company-quickhelp xterm-color shell-pop multi-term eshell-z eshell-prompt-extras esh-help helm-gtags ggtags android-mode csv-mode list-packages-ext dark-mint-theme zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme yaml-mode web-mode web-beautify tagedit stickyfunc-enhance srefactor slim-mode scss-mode sass-mode pug-mode pdf-tools tablist org-ref key-chord ivy nginx-mode livid-mode skewer-mode simple-httpd less-css-mode json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc helm-css-scss helm-bibtex parsebib haml-mode git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter emmet-mode ein websocket diff-hl company-web web-completion-data company-tern dash-functional tern company-emacs-eclim eclim company-auctex coffee-mode biblio biblio-core auctex-latexmk auctex mwim disaster company-c-headers cmake-mode clang-format smeargle orgit org-projectile org-present org org-pomodoro alert log4e gntp org-download mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor company-statistics company-anaconda company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic ws-butler window-numbering which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide ido-vertical-mode hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async quelpa package-build spacemacs-theme)))
 '(pomidor-seconds 1800)
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(safe-local-variable-values
   (quote
    ((clang-format-style . c++11)
     (company-clang-arguments . "-I/home/spook/projects/classes/cs310/223/")
     (company-clang-arguments . "-I/home/spook/projects/classes/cs310/217"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff0066")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#63de5d")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#53f2dc")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#06d8ff"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(org-level-4 ((t (:foreground "dodger blue"))))
 )
