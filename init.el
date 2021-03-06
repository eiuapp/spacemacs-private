;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
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
   dotspacemacs-enable-lazy-installation nil
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation nil
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     (ivy :variables ivy-enable-advanced-buffer-information nil)
     better-defaults
     ranger
     emoji
     (plantuml :variables plantuml-jar-path "~/.spacemacs.d/plantuml.jar")
     lsp
     dap
     colors
     prodigy
     ;; github
     search-engine
     graphviz
     (haskell :variables haskell-enable-hindent t
              haskell-completion-backend 'intero)
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; (vinegar :variables vinegar-reuse-dired-buffer t)
     (spacemacs-layouts :variables layouts-enable-autosave nil
                        layouts-autosave-delay 300)
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     (ibuffer :variables ibuffer-group-buffers-by 'projects)
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-tab-key-behavior 'cycle
                      :disabled-for org markdown)
     (osx :variables osx-dictionary-dictionary-choice "Simplified Chinese - English"
          osx-command-as 'super)
     restclient
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (shell :variables shell-default-shell 'eshell)
     ;; (shell :variables shell-default-shell 'ansi-term
     ;;       shell-default-term-shell "/bin/zsh")
     ;; docker
     latex
     deft
     markdown
     (org :variables org-want-todo-bindings t
          org-enable-hugo-support t)
     gpu
     yaml
     react
     (python :variables
             python-test-runner '(nose pytest))
             ;; python-test-runner '(nose pytest)
             ;; python-backend 'lsp
             ;; python-lsp-server 'mspyls
             ;; python-lsp-git-root "~/Github/python-language-server")
     ;; (ruby :variables ruby-version-manager 'chruby)
     ;; ruby-on-rails
     lua
     html
     (javascript :variables
                 node-add-modules-path t
                 javascript-backend 'nil)
     ;; (javascript :variables javascript-backend 'lsp)
     (typescript :variables
                 typescript-fmt-on-save nil
                 typescript-fmt-tool 'typescript-formatter
		 ;; typescript-backend 'lsp
		 )
     emacs-lisp
     (clojure :variables clojure-enable-fancify-symbols t)
     racket
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
            ;; c-c++-default-mode-for-headers 'c++-mode
            ;; c-c++-backend 'lsp-ccls
            ;; c-c++-lsp-executable (file-truename "/usr/local/bin/ccls"))
     zilongshanren
     zilongshanren-tomtsang             ;; because git submodule, so can't rename to "tomtsang"
     (chinese :packages youdao-dictionary fcitx
              :variables chinese-enable-fcitx nil
              chinese-enable-youdao-dict t)
     ;; (chinese :variables chinese-default-input-method 'pinyin
             ;; chinese-enable-youdao-dict t)
     (go :variables
         go-use-gometalinter t
         gofmt-command "goimports"
         go-tab-width 4)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(sicp ssh-agency anki-editor
                                      company-tern
                                      prettier-js
                                      add-node-modules-path
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages
   '(org-projectile org-brain magit-gh-pulls magit-gitflow  evil-mc realgud tern company-tern
                    evil-args evil-ediff evil-exchange evil-unimpaired
                    evil-indent-plus volatile-highlights smartparens
                    spaceline holy-mode skewer-mode rainbow-delimiters
                    highlight-indentation vi-tilde-fringe eyebrowse ws-butler
                    org-bullets smooth-scrolling org-repo-todo org-download org-timer
                    livid-mode git-gutter git-gutter-fringe  evil-escape
                    leuven-theme gh-md evil-lisp-state spray lorem-ipsum symon
                    ac-ispell ace-jump-mode auto-complete auto-dictionary
                    clang-format define-word google-translate disaster epic
                    fancy-battery org-present orgit orglue spacemacs-theme
                    helm-flyspell flyspell-correct-helm clean-aindent-mode
                    helm-c-yasnippet ace-jump-helm-line helm-make magithub
                    helm-themes helm-swoop helm-spacemacs-help smeargle
                    ido-vertical-mode flx-ido company-quickhelp ivy-rich helm-purpose
                    ;; window-purpose ivy-purpose helm-purpose spacemacs-purpose-popwin
		    ;;         clojure-cheatsheet
                    )
   dotspacemacs-install-packages 'used-only
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 300

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner '"~/Pictures/tom/google-logo.png"
   ;; dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         monokai
                         solarized-dark
                         solarized-light
                         spacemacs-light)
   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
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
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

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
   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'origami

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   ;; dotspacemacs-whitespace-cleanup nil
   dotspacemacs-whitespace-cleanup 'changed

  ;; dotspacemacs-frame-title-format "%t"
   dotspacemacs-icon-title-format nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-init ()
   (setq-default configuration-layer-elpa-archives
                 '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
                   ("org-cn"   . "http://elpa.emacs-china.org/org/")
                  ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))


  (setq term-char-mode-point-at-process-mark nil)

  ;; https://github.com/syl20bnr/spacemacs/issues/2705
  ;; (setq tramp-mode nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; start ss prxoy
  ;; ss proxy. But it will cause anacond-mode failed.
  ;; (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (setq evil-shift-round nil)
  (setq byte-compile-warnings '(not obsolete))
  (setq warning-minimum-level :error)
  ;; (setq url-proxy-services
  ;;     '(("http"     . "http://127.0.0.1:8118")
  ;;       ("https"    . "http://127.0.0.1:8118")
	;;     ("ftp"      . "127.0.0.1:8118")
	;;     ("no_proxy" . "^.*github.com")))
  ;; end ss proxy

  ;; https://github.com/syl20bnr/spacemacs/issues/8901
  (setq-default quelpa-build-tar-executable "/usr/local/bin/gtar")
  ;; hack for remove purpose mode
  ;; (setq purpose-mode nil)
  ;; (setq js2-include-node-externs t)

  ;; Turn off js2 mode errors & warnings (we lean on eslint/standard)
  ;; (setq js2-mode-show-parse-errors nil)
  ;; (setq js2-mode-show-strict-warnings nil)
  )

(defun dotspacemacs/user-config ()

  ;; start 配置一下 eww
  (with-eval-after-load 'eww
    (custom-set-variables
     ;; '(eww-search-prefix "https://www.google.com.hk/search?q=")
     '(eww-search-prefix "https://cn.bing.com/search?q=")
     ;; '(eww-search-prefix "https://www.baidu.com/s?wd=hello")
     )

    ;; (define-key eww-mode-map (kbd "h") 'backward-char)
    ;; (define-key eww-mode-map (kbd "n") 'next-line)
    ;; (define-key eww-mode-map (kbd "s") 'forward-char)
    ;; (define-key eww-mode-map (kbd "t") 'previous-line)

    ;; (define-key eww-mode-map (kbd "H") 'eww-back-url)
    ;; (define-key eww-mode-map (kbd "S") 'eww-forward-url)

    ;; (define-key eww-mode-map (kbd "b") 'eww-history-browse)
    ;; (define-key eww-mode-map (kbd "c") 'eww-browse-with-external-browser)
    ;; (define-key eww-mode-map (kbd "i") 'eww)
    ;; (define-key eww-mode-map (kbd "m") 'eww-lnum-follow)
    ;; (define-key eww-mode-map (kbd "z") 'eww-lnum-universal)

    ;; (define-key eww-mode-map (kbd "M-n") 'nil)
    ;; (define-key eww-mode-map (kbd "M-p") 'nil)

    ;; (define-key eww-mode-map (kbd "<C-S-iso-lefttab>") 'eww-previous-buffer)
    ;; (define-key eww-mode-map (kbd "<C-tab>")           'eww-next-buffer)
    )
  ;; end 配置一下 eww

  ;; start 配置一下 Emacs Application Framework
  ;; https://github.com/manateelazycat/emacs-application-framework
  ;; https://github.com/Linusp/emacs_config/commit/9b99efa16e8d89ae4fc9563d12a76c7c90613699
  ;; ATTENTION: mac, wsl, can not worked
  ;;  (add-to-list 'load-path "~/.spacemacs.d/layers/zilongshanren-tomtsang/utils/emacs-application-framework/")
  ;;  (require 'eaf)
  ;; end 配置一下 Emacs Application Framework

  ;; https://github.com/pierre-lecocq/emacs4developers/blob/master/chapters/03-building-your-own-editor.org
  ;; define your name and mail address
  (setq user-full-name "yunlong zeng")
  (setq user-mail-address "273412935@qq.com")
  ;; Highlight tabulations
  (setq-default highlight-tabs t)

  ;; Show trailing white spaces
  (setq-default show-trailing-whitespace t)

  ;; Remove useless whitespace before saving a file
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

  ;; Remove all backup files
  (setq make-backup-files nil)
  (setq backup-inhibited t)
  (setq auto-save-default nil)

  ;; Set locale to UTF8
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;; https://blog.csdn.net/u010654583/article/details/73920206
  ;; I. 显示时间
  ;; .emacs加上：
  (display-time-mode 1)              ;; 常显
  (setq display-time-24hr-format t)  ;;格式
  (setq display-time-day-and-date t) ;;显示时间、星期、日期

  ;; II. 隐藏菜单栏工具栏滚动条
  ;; .emacs加上：
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-bar-mode 0)
  ;;注: 新版改成使用0，旧版使用nil的做法已失效，但 (set-scroll-bar-mode nil) 仍可使用

  ;; III. 关闭启动画面
  ;; .emacs加上：
  (setq inhibit-startup-message t)

  ;; IV. highlight当前行
  ;; .emacs加上：
  (global-hl-line-mode 1)

  (defconst macp (eq system-type 'darwin))
  (message "system type is mac : %s" macp)

  ;; ;; emacspeak mac
  ;; (require 'cl)
  ;; (setq load-path (cons "~/emacs/emacspeak/lisp" load-path))
  ;; (setq emacspeak-directory "~/emacs/emacspeak")
  ;; (setq dtk-program "mac")
  ;; (require 'emacspeak-setup)
  ;; (require 'mac-voices)
  ;; (emacspeak-tts-startup-hook)
  ;; (dtk-set-rate 300 t)

  ;; ;; emacspeak outloud
  ;; (setenv "DTK_PROGRAM" "outloud")
  ;; (load-file "/mnt/c/Users/a/emacs/emacspeak-src/emacspeak_voxin_install-49.0-6/build/emacspeak-49.0/lisp/emacspeak-setup.el")

  ;; ;; emacspeak espeak
  ;; (setenv "DTK_PROGRAM" "espeak")

  (kill-buffer "*spacemacs*")

  ;; (global-flycheck-mode t)
  ;; stop default linter - use ESLint linter as part of FlyCheck
  ;;  (setq js2-mode-show-parse-errors nil js2-mode-show-strict-warnings nil)
  ;;  (eval-after-load 'js2-mode
  ;;    '(progn
  ;;      (add-hook 'js2-mode-hook 'flycheck-mode) ;;  'eslintd-fix-mode #'add-node-modules-path
  ;;      (add-hook 'js2-mode-hook 'eslintd-fix-mode)
  ;;      (add-hook 'js2-mode-hook #'add-node-modules-path)))

  ;; stop default linter - use ESLint linter as part of FlyCheck
  ;; (setq js2-mode-show-parse-errors nil js2-mode-show-strict-warnings nil)
  (eval-after-load 'js2-mode
    '(add-hook 'js2-mode-hook #'add-node-modules-path))

  ;; prettier
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)))

  (require 'prettier-js)
  (setq prettier-js-command "prettier-eslint_d")

  ;; (require 'company-tern)
  ;; (require 'company-mode)
  ;; (add-to-list 'company-backends 'company-tern)

  (require 'auto-complete)
  (global-auto-complete-mode t)
  ;; (add-to-list 'ac-modes (js2-mode company-mode tern-mode))
  (add-to-list 'load-path "~/emacs/tern/emacs/")
  (autoload 'tern-mode "tern.el" nil t)
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  ;;解决org表格里面中英文对齐的问题
  (when (configuration-layer/layer-usedp 'chinese)
    (when (and (spacemacs/system-is-mac) window-system)
      (spacemacs//set-monospaced-font "Source Code Pro" "Hiragino Sans GB" 14 16)))

  ;; Setting Chinese Font
  (when (and (spacemacs/system-is-mswindows) window-system)
    (setq ispell-program-name "aspell")
    (setq w32-pass-alt-to-system nil)
    (setq w32-apps-modifier 'super)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family "Microsoft Yahei" :size 14))))

  (fset 'evil-visual-update-x-selection 'ignore)

  ;; force horizontal split window
  (setq split-width-threshold 120)
  ;; (linum-relative-on)

  (spacemacs|add-company-backends :modes text-mode)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  ;; temp fix for ivy-switch-buffer
  ;; (spacemacs/set-leader-keys "bb" 'helm-mini)

  (global-hungry-delete-mode t)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
  (spacemacs|diminish counsel-mode)

  (evilified-state-evilify-map special-mode-map :mode special-mode)

  (add-to-list 'auto-mode-alist
               '("Capstanfile\\'" . yaml-mode))

  (defun js-indent-line ()
    "Indent the current line as JavaScript."
    (interactive)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (point) (save-excursion (back-to-indentation) (point)))))
      (if (nth 3 parse-status)
          'noindent
        (indent-line-to (js--proper-indentation parse-status))
        (when (> offset 0) (forward-char offset)))))

  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at (concat "^" (make-string tab-width ?\ )))
          (replace-match "")))))

  (defun zilongshanren/toggle-major-mode ()
    (interactive)
    (if (eq major-mode 'fundamental-mode)
        (set-auto-mode)
      (fundamental-mode)))
  (spacemacs/set-leader-keys "otm" 'zilongshanren/toggle-major-mode)

  (setq inhibit-compacting-font-caches t)
  (global-display-line-numbers-mode -1)

  (defun moon-override-yank-pop (&optional arg)
    "Delete the region before inserting poped string."
    (when (and evil-mode (eq 'visual evil-state))
      (kill-region (region-beginning) (region-end))))

  (advice-add 'counsel-yank-pop :before #'moon-override-yank-pop)
  (setq ivy-more-chars-alist '((counsel-ag . 2)
                               (counsel-grep .2)
                               (t . 3)))

  ;; boost find file and load saved persp layout  performance
  ;; which will break some function on windows platform
  ;; eg. known issues: magit related buffer color, reopen will fix it
  (when (spacemacs/system-is-mswindows)
    (progn (setq find-file-hook nil)
           (setq vc-handled-backends nil)
           (setq magit-refresh-status-buffer nil)
           (add-hook 'find-file-hook 'spacemacs/check-large-file)

           ;; emax.7z in not under pdumper release
           ;; https://github.com/m-parashar/emax64/releases/tag/pdumper-20180619
           (defvar emax-root (concat (expand-file-name "~") "/emax"))

           (when (file-exists-p emax-root)
             (progn
               (defvar emax-root (concat (expand-file-name "~") "/emax"))
               (defvar emax-bin64 (concat emax-root "/bin64"))
               (defvar emax-mingw64 (concat emax-root "/mingw64/bin"))
               (defvar emax-lisp (concat emax-root "/lisp"))

               (setq exec-path (cons emax-bin64 exec-path))
               (setenv "PATH" (concat emax-bin64 ";" (getenv "PATH")))

               (setq exec-path (cons emax-mingw64 exec-path))
               (setenv "PATH" (concat emax-mingw64 ";" (getenv "PATH")))
               ))

           (add-hook 'projectile-mode-hook '(lambda () (remove-hook 'find-file-hook #'projectile-find-file-hook-function)))))

  ;; (setq exec-path (cons "/Users/lionqu/.nvm/versions/node/v10.16.0/bin/" exec-path))
  (setq exec-path (cons "~/.nvm/versions/node/v11.14.0/bin/" exec-path))
  ;; (setenv "PATH" (concat "/Users/lionqu/.nvm/versions/node/v10.16.0/bin:" (getenv "PATH")))
  (setenv "PATH" (concat "~/.nvm/versions/node/v11.14.0/bin:" (getenv "PATH")))

  (defun counsel-locate-cmd-es (input)
    "Return a shell command based on INPUT."
    (counsel-require-program "es.exe")
    (encode-coding-string (format "es.exe -i -r -p %s"
                                  (counsel-unquote-regex-parens
                                   (ivy--regex input t)))
                          'gbk))
  ;; (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)

  (add-hook 'org-mode-hook 'emojify-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  ;; https://emacs-china.org/t/ox-hugo-auto-fill-mode-markdown/9547/4
  (defadvice org-hugo-paragraph (before org-hugo-paragraph-advice
                                        (paragraph contents info) activate)
    "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to hugo markdown."
    (let* ((origin-contents (ad-get-arg 1))
           (fix-regexp "[[:multibyte:]]")
           (fixed-contents
            (replace-regexp-in-string
             (concat
              "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
      (ad-set-arg 1 fixed-contents)))

  ;; fix for the magit popup doesn't have a q keybindings
  (with-eval-after-load 'transient
    (transient-bind-q-to-quit))

  ;; fix for the lsp error
  (defvar spacemacs-jump-handlers-fundamental-mode nil)

  ;; ;; start 配置一下 eslint-auto.el
  ;; ;; https://gist.github.com/ustun/73321bfcb01a8657e5b8
  ;; (defun eslint-fix-file ()
  ;;   (interactive)
  ;;   (message "eslint --fixing the file" (buffer-file-name))
  ;;   (shell-command-to-string (concat "eslint --fix " (buffer-file-name))))

  ;; (defun eslint-fix-file-and-revert ()
  ;;   (interactive)
  ;;   (eslint-fix-file)
  ;;   (revert-buffer t t))

  ;; (add-hook 'js2-mode-hook
  ;;   (lambda ()
  ;;     (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))
  ;; ;; end 配置一下 eslint-auto.el

  ;; (require 'flycheck)
  ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  ;; (eval-after-load 'vue-mode
  ;;   '(add-hook 'vue-mode-hook #'add-node-modules-path))
  ;; (flycheck-add-mode 'javascript-eslint 'vue-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  ;; (flycheck-add-mode 'javascript-eslint 'css-mode)
  ;; (add-hook 'vue-mode-hook 'flycheck-mode)

  ;;  (eval-after-load 'js2-mode
  ;;    '(add-hook 'js2-mode-hook #'add-node-modules-path))
  ;;  (eval-after-load 'js2-jsx-mode
  ;;    '(add-hook 'js2-jsx-mode-hook #'add-node-modules-path))
  ;;  (eval-after-load 'web-mode
  ;;    '(add-hook 'web-mode-hook #'add-node-modules-path))

  ;;  (setq flycheck-eslintrc "~/.eslintrc")

  ;; start clipboard
  ;; (setq x-select-enable-clipboard t)
  ;; http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
  ;; I prefer using the "clipboard" selection (the one the
  ;; typically is used by c-c/c-v) before the primary selection
  ;; (that uses mouse-select/middle-button-click)
  (setq x-select-enable-clipboard t)
  ;; after mouse selection in X11, you can paste by `yank' in emacs
  (setq x-select-enable-primary t)

  ;; If emacs is run in a terminal, the clipboard- functions have no
  ;; effect. Instead, we use of xsel, see
  ;; http://www.vergenet.net/~conrad/software/xsel/ -- "a command-line
  ;; program for getting and setting the contents of the X selection"
  (unless window-system
    (when (getenv "DISPLAY")
      ;; Callback for when user cuts
      (defun xsel-cut-function (text &optional push)
        ;; Insert text to temp-buffer, and "send" content to xsel stdin
        (with-temp-buffer
          (insert text)
          ;; I prefer using the "clipboard" selection (the one the
          ;; typically is used by c-c/c-v) before the primary selection
          ;; (that uses mouse-select/middle-button-click)
          (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
      ;; Call back for when user pastes
      (defun xsel-paste-function()
        ;; Find out what is current selection by xsel. If it is different
        ;; from the top of the kill-ring (car kill-ring), then return
        ;; it. Else, nil is returned, so whatever is in the top of the
        ;; kill-ring will be used.
        (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
          (unless (string= (car kill-ring) xsel-output)
	        xsel-output )))
      ;; Attach callbacks to hooks
      (setq interprogram-cut-function 'xsel-cut-function)
      (setq interprogram-paste-function 'xsel-paste-function)
      ;; Idea from
      ;; http://shreevatsa.wordpress.com/2006/10/22/emacs-copypaste-and-x/
      ;; http://www.mail-archive.com/help-gnu-emacs@gnu.org/msg03577.html
      ))

  ;; end clipboard

  ;; start my layout
  ;; (zilongshanren/load-my-layout)
  ;; (add-hook 'emacs-startup-hook zilongshanren/load-my-layout)
  ;; end my layout

  ;; start open shell when startup
  ;; (shell)
  ;; end open shell when startup

  ;; start Skip HTTP Version
  ;;  (skip-chars-forward "HTTP/")        ; Skip HTTP Version
  ;;  (skip-chars-forward "http/")        ; Skip HTTP Version
  ;; end Skip HTTP Version

  ;; start highlight-it

  (defun highlite-it ()
    "Highlight certain lines…"
    (interactive)
    (if (equal "log" (file-name-extension (buffer-file-name)))
        (progn
          (highlight-lines-matching-regexp "ERROR:" 'hi-red-b)
          (highlight-lines-matching-regexp "NOTE:" 'hi-blue-b))))

  (add-hook 'find-file-hook 'highlite-it)
  ;; end highlight-it

  ;; start irc
  (setq rcirc-default-nick "tomtsang")
  ;; end irc

  ;; start winner-mode
  ;; restore split pane config, winner-undo, winner-redo
  (winner-mode t)                       ; in GNU emacs 23.2
  ;; end winner-mode

  ;; start desktop-save-mode
                                        ; save/restore opened files and windows config
  (desktop-save-mode 1)                 ; 0 for off
  ;; end desktop-save-mode

  ;; start use Shift+arrow_keys to move cursor around split panes
  (progn
    (require 'windmove)
    ;; use Shift+arrow_keys to move cursor around split panes
    (windmove-default-keybindings)
    ;; when cursor is on edge, move to the other side, as in a torus space
    (setq windmove-wrap-around t )
    )
  ;; end use Shift+arrow_keys to move cursor around split panes

  ;; start save bookmarks
  ;; everytime bookmark is changed, automatically save it
  (setq bookmark-save-flag 1)
  ;; Set Emacs to Open Bookmark File on Start
  (require 'bookmark)
  (setq inhibit-splash-screen t)
  (bookmark-bmenu-list)
  (switch-to-buffer "*Bookmark List*")
  ;; end save bookmarks

  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
