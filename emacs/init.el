;;; init.el -*- lexical-binding: t; -*-
(setq gc-cons-threshold 300000000)
(setq read-process-output-max (* 3(* 1024 1024))) ;; 1mb
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Ensure straight.el is installed
(defvar straight-use-package-by-default t)
(defvar straight-recipes-repo-clone-depth 1)
(defvar straight-enable-github-repos t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Configure straight.el
(straight-use-package 'use-package)
(setq display-time-load-average nil
      idle-update-delay 0.01
      grep-command "grep --color=auto -nHr --null -e "
      visible-bell t
      create-lockfiles nil
      use-short-answers t
      ring-bell-function nil
      make-backup-files t
      backup-by-copying t
      inhibit-startup-message t
      inhibit-compacting-font-caches t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      scroll-margin 8
      mode-line-end-spaces nil
      blink-paren-function nil
      blink-matching-paren nil
      set-language-environment "UTF-8")

(add-hook 'window-setup-hook 'toggle-frame-maximized t)
;; (set-frame-parameter nil 'alpha-background 70)


;; Package management
(use-package straight
  :custom
  (straight-check-for-modifications nil))

(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(when IS-WINDOWS
  (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
  (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
  (setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
  )

(when IS-LINUX
  (add-to-list 'default-frame-alist '(alpha-background . 97))
  (setq backup-directory-alist '(("." . "~/.config/emacs/backup"))))

(setq  backup-by-copying t    ; Don't delink hardlinks
       version-control t      ; Use version numbers on backups
       delete-old-versions t  ; Automatically delete excess backups
       kept-new-versions 20   ; how many of the newest versions to keep
       kept-old-versions 5    ; and how many of the old
       )
;; Basic UI setup
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(setq-default display-line-numbers-type 'relative)
(set-face-attribute 'default nil :font "Lilex Nerd Font-12:weight=medium")
(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-set-key [remap lookup-definition] #'xref-find-definitions)
(global-set-key [remap lookup-reference] #'xref-find-references)
(global-set-key [remap sp/format-buffer] #'format-all-buffer)

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package s)
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))
(use-package dtrt-indent)
(use-package smartparens)

(use-package which-key
  :init (which-key-mode))

(use-package no-littering)
;; Evil mode
(use-package evil
  :config
  (defun sp/evil-yank-advice (orig-fn beg end &rest args)
    (require 'pulsar)
    (pulsar--pulse nil nil beg end)
    (apply orig-fn beg end args))

  (advice-add 'evil-yank :around 'sp/evil-yank-advice)
  (evil-global-set-key 'normal (kbd "g d") 'lookup-definition)
  (evil-global-set-key 'normal (kbd "g i") 'lookup-implementation)
  (evil-global-set-key 'normal (kbd "g r r") 'lookup-reference)
  (evil-global-set-key 'normal (kbd "g t") 'lookup-type-definition)
  (evil-global-set-key 'normal (kbd "g c c") 'comment-line)
  (evil-global-set-key 'visual (kbd "g c") 'comment-or-uncomment-region)
  (evil-global-set-key 'insert (kbd "C-p") nil)
  (evil-global-set-key 'insert (kbd "C-j") nil)
  (evil-global-set-key 'insert (kbd "C-k") nil)
  (evil-global-set-key 'insert (kbd "C-h") nil)
  (evil-global-set-key 'insert (kbd "C-l") nil)
  (evil-global-set-key 'normal (kbd "C-p") nil)
  (evil-global-set-key 'normal (kbd "K") nil)
  (evil-global-set-key 'normal (kbd "J") nil)
  (evil-global-set-key 'normal (kbd "C-f") nil)
  (evil-global-set-key 'normal (kbd "C-j") 'windmove-down)
  (evil-global-set-key 'normal (kbd "C-k") 'windmove-up)
  (evil-global-set-key 'normal (kbd "C-h") 'windmove-left)
  (evil-global-set-key 'normal (kbd "C-l") 'windmove-right)
  (evil-global-set-key 'normal "-" 'dired-jump)
  (evil-global-set-key 'normal (kbd "C-f") 'projectile-persp-switch-project)
  (evil-global-set-key 'normal (kbd "M-.") 'consult-project-extra-find)
  (evil-global-set-key 'normal (kbd "\\") 'evil-window-vsplit)
  (evil-global-set-key 'normal (kbd "C-+") 'text-scale-increase)
  (evil-global-set-key 'normal (kbd "C--") 'text-scale-decrease)
  :init      ;; tweak evil's configuration before loading it
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package posframe)
;; LSP mode
(use-package lsp-mode
  :hook ((typescript-ts-mode . lsp-deferred)
         (html-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (csharp-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (lsp-mode . lsp-optimization-mode)
         (lsp-mode . lsp-signature-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         )
  :commands lsp-deferred
  :custom
  (read-process-output-max (* 3(* 1024 1024)))
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefic "C-c")
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-lens-enable nil)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	      '(flex))) ;; Configure flex
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (setq lsp-signature-render-documentation t)
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lookup-implementation] #'lsp-goto-implementation)
  (define-key lsp-mode-map [remap lookup-declaration] #'lsp-find-declaration)
  (define-key lsp-mode-map [remap lookup-reference] #'lsp-find-references)
  (define-key lsp-mode-map [remap lookup-definition] #'lsp-find-definition)
  (define-key lsp-mode-map [remap lookup-type-definition] #'lsp-goto-type-definition)
  (define-key lsp-mode-map [remap lookup-doc] #'lsp-ui-doc-glance)
  (define-key lsp-mode-map [remap sp/format-buffer] #'lsp-format-buffer)
  (evil-define-key 'normal lsp-mode-map (kbd "SPC c a") 'lsp-execute-code-action)
  (evil-global-set-key 'normal (kbd "C-SPC") 'lsp-execute-code-action)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
  )

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-n" . flycheck-next-error)
              ("C-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-checker-error-threshold 2000)
  )

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :init
  (evil-define-key 'normal lsp-ui-mode-map (kbd "K") 'lsp-ui-doc-glance)
  (evil-define-key 'normal lsp-ui-mode-map (kbd "TAB") 'lsp-ui-doc-focus-frame)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "<escape>") 'lsp-ui-doc-hide)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "q") 'lsp-ui-doc-hide)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-include-signature t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-ui-peek-enable t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)

  (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))


(use-package consult-lsp
  :defer t)

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
	     ("C-c p t" . complete-tag)        ;; etags
	     ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
	     ("C-c p h" . cape-history)
	     ("C-c p f" . cape-file)
	     ("C-c p k" . cape-keyword)
	     ("C-c p s" . cape-elisp-symbol)
	     ("C-c p e" . cape-elisp-block)
	     ("C-c p a" . cape-abbrev)
	     ("C-c p l" . cape-line)
	     ("C-c p w" . cape-dict)
	     ("C-c p :" . cape-emoji)
	     ("C-c p \\" . cape-tex)
	     ("C-c p _" . cape-tex)
	     ("C-c p ^" . cape-tex)
	     ("C-c p &" . cape-sgml)
	     ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)

  (setq-local completion-at-point-functions
		      (list (cape-capf-buster #'some-caching-capf)))
  )
;; Vertico
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package vertico-posframe
  :init
  (vertico-posframe-mode)
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (top-fringe . 8)
          (bottom-fringe . 8)
          (right-fringe . 8)))
  )

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setq consult-narrow-key "<"))

(use-package consult-project-extra
  :straight t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package embark
  :bind
  (("C-q" . embark-act))) ;; Bind C-q to embark-act for acting on results

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode)))


(use-package corfu
  :bind (:map corfu-map
              ("C-y" . corfu-insert)
              ("TAB" . nil)
              ("<tab>" . nil))
  :custom
  (corfu-auto t)
  (corfu-preselect 'insert)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-auto-delay 0)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Doom themes
(use-package doom-themes
  :init
  (load-theme 'gruvbox-sp t))

;; Doom modeline
(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

;; Nerd Fonts
(use-package unicode-fonts
  :init
  (unicode-fonts-setup))
;; This assumes you've installed the package via MELPA.
(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package emojify)
(use-package nerd-icons)


(use-package windmove
  :config
  (setq windmove-wrap-around t)
  )

;; Keybindings
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer sp/leader-keys
    :prefix "SPC"
    )
  (general-create-definer sp/leader-keys-local
    :prefix "SPC c"
    :wk "Local Leader"
    )
  (sp/leader-keys-local
    :states 'normal
    :keymaps 'html-ts-mode-map
    "n" '(sgml-skip-tag-forward :wk "Find Closing Tag")
    "p" '(sgml-skip-tag-backward :wk "Find Opening Tag")
    )
  (sp/leader-keys-local
    :states 'normal
    :keymaps 'csharp-ts-mode-map
    "s" '(sharper-main-transient :wk "[O]pen [S]harper")
    )
  (sp/leader-keys
    :keymaps 'visual
    "ae" '(copilot-chat-explain :wk "[A]i [E]xplain")
    "ar" '(copilot-chat-review :wk "[A]i [R]eview")
    )
  (sp/leader-keys
    :keymaps 'normal
    ;; single use keymaps
    "." '(find-file :wk "find files")
    ;; "SPC" '(consult-projectile :wk "find files")
    "f" '(sp/format-buffer :wk "format buffer")
    "w" '(save-buffer :wk "save")
    ;; ai
    "a" '(:ignore t :wk "[A]I")
    "aa" '(copilot-chat :wk "[A]I [A]sk")
    "at" '(copilot-chat-transient :wk "[A]I [A]sk")
    ;; buffers
    "b" '(:ignore t :wk "buffer")
    "bb" '(consult-project-buffer :wk "Switch buffer")
    "bd" '(kill-this-buffer :wk "Switch buffer")
    "bB" '(consult-buffer :wk "all buffers")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer")
    ;; delete
    "d" '(:ignore t :wk "[D]elete")
    "db" '(kill-buffer :wk "[D]elete [B]uffer")
    "dw" '(delete-window :wk "[D]elete [W]indow")
    "dp" '(+popup/close :wk "[D]elete [W]indow")
    "pp" '(+popup/toggle :wk "[D]elete [W]indow")
    ;; git
    "g" '(:ignore t :wk "[G]it")
    "gs" '(magit-status :wk "[G]it [S]tatus")
    ;; open things
    "o" '(:ignore t :wk "[O]pen")
    "ot" '(projectile-run-shell :wk "[O]pen [T]erminal")
    "oe" '(projectile-run-eshell :wk "[O]pen [T]erminal")
    ;; projects
    "p" '(:ignore t :wk "[P]erspective")
    "ps" '(persp-switch :wk "[P]erspective [S]witch")
    "pt" '(projectile-run-shell :wk "[O]pen [T]erminal")
    "pe" '(projectile-run-eshell :wk "[O]pen [T]erminal")
    ;; search
    "s" '(:ignore t :wk "[S]earch")
    "sd" '(consult-lsp-diagnostics :wk "[S]earch [D]iagnostics")
    "sg" '(consult-ripgrep :wk "[S]earch [G]rep")
    "ss" '(consult-lsp-symbols :wk "[S]earch [G]rep")
    )
  (general-define-key
   "<f5>" 'projectile-compile-project
   "C-f" '(project-switch-project :wk "switch project")
   "C-+" 'text-scale-increase
   (kbd "C--") 'text-scale-increase
   "C-h" 'windmove-left
   "C-l" 'windmove-right
   "C-k" 'windmove-up
   "C-j" 'windmove-down))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(go gomod json markdown c-sharp javascript typescript tsx css html))
  (setq treesit-font-lock-level 4)
  (treesit-auto-add-to-auto-mode-alist '(go gomod json markdown c-sharp javascript typescript tsx css html))
  (global-treesit-auto-mode))

(use-package all-the-icons
  :straight (all-the-icons :fetcher github :repo "domtronn/all-the-icons.el")
  )

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (all-the-icons-completion-mode))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :defer t)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (setq kind-icon-default-style
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.6 :scale 1.0 :background
                   nil)) ;; hack to fix overflowing icons on corfu

  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
;; (let* ((kind-func (lambda (cand) (company-call-backend 'kind cand)))
;;        (formatter (kind-icon-margin-formatter `((company-kind . ,kind-func)))))
;;   (defun my-company-kind-icon-margin (cand _selected)
;;     (funcall formatter cand))
;;   (setq company-format-margin-function #'my-company-kind-icon-margin)))

(use-package pulsar
  :init (pulsar-global-mode +1))

(use-package drag-stuff
  :defer t
  :config
  (evil-global-set-key 'visual (kbd "J") (lambda (arg) (interactive "p") (drag-stuff-down arg)
                                           (if (bound-and-true-p lsp-mode)
                                               (lsp-format-buffer)
                                             (format-all-buffer))))
  (evil-global-set-key 'visual (kbd "K") (lambda (arg) (interactive "p") (drag-stuff-up arg)
                                           (if (bound-and-true-p lsp-mode)
                                               (lsp-format-buffer)
                                             (format-all-buffer))))
  :init
  (drag-stuff-global-mode +1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package format-all)

(use-package popup-mode
  :demand t
  :straight (popup-mode :host github :repo "aaronjensen/emacs-popup-mode")
  :hook (after-init . +popup-mode)
  :config
  (defun my-windmove-ignore-popup-and-minibuffer (original-fn &rest args)
    "Advice to make windmove ignore popup and minibuffer windows."
    (let ((windmove-wrap-around t)
          (ignore-window-parameters t))
      (cl-letf (((symbol-function 'windmove-find-other-window)
                 (lambda (dir &optional arg window)
                   (let ((other-window (window-in-direction dir window ignore-window-parameters)))
                     (while (and other-window
                                 (or (window-minibuffer-p other-window)
                                     (string-match-p "\\*popup\\*" (buffer-name (window-buffer other-window)))))
                       (setq other-window (window-in-direction dir other-window ignore-window-parameters)))
                     other-window))))
        (apply original-fn args))))

  ;; Add advice to windmove commands
  (advice-add 'windmove-up :around #'my-windmove-ignore-popup-and-minibuffer)
  (advice-add 'windmove-down :around #'my-windmove-ignore-popup-and-minibuffer)
  (advice-add 'windmove-left :around #'my-windmove-ignore-popup-and-minibuffer)
  (advice-add 'windmove-right :around #'my-windmove-ignore-popup-and-minibuffer)
  (set-popup-rules! '(("^\\*Process List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Buffer List\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Embark Actions\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*Occur\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\*command-log\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :size +popup-shrink-to-fit)
                      ("^\\(?:\\*magit\\|magit:\\|\\*Embark\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*eshell\\|eshell:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*term\\|term:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*Copilot Chat\\|Copilot Chat:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*vterm\\|vterm:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*shell\\|shell:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*org-brain\\|org-brain:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Warnings\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\*Help\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("Output\\*$"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.4 :ttl nil)
                      ("^\\(?:\\*Messages\\|Messages:\\| \\*transient\\*\\)"
                       :side bottom :select t :slot -1 :vslot -1 :height 0.3 :ttl nil)))
  ;; (setq popup-mode-enable-hacks t)
  )


(use-package golden-ratio
  :init
  (golden-ratio-mode +1))

(use-package projectile
  :defer t
  :bind (:map projectile-mode-map
              ("C-x p" . projectile-command-map ))
  :init
  (when IS-WINDOWS
    (setq project-dirs '(
                         ("D:/work" . 3)
                         ("D:/projects" . 3)
                         ("C:/Users/sam/AppData/Roaming/" . 1)
                         )))
  (when IS-LINUX
    (setq project-dirs '(("~/work/" . 3)
                         ("~/projects/" . 3))))
  (setq projectile-enable-caching (not noninteractive)
        projectile-indexing-method 'hybrid
        projectile-auto-discover nil
        projectile-project-search-path project-dirs)

  (add-hook 'kill-emacs-hook 'projectile-discover-projects-in-search-path)
  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)
  )

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (setq persp-modestring-dividers '(" "))
  (add-hook 'persp-created-hook (lambda () (split-window-horizontally)))
  (setq persp-nil-name "main"
        persp-modestring-short t
        persp-set-last-persp-for-new-frames t)

  (persp-mode))

(use-package consult-projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile"))

(use-package persp-projectile)
(consult-customize consult--source-buffer :hidden t :default nil)
(add-to-list 'consult-buffer-sources persp-consult-source)

(use-package magit
  :defer t
  :hook (magit-mode . (lambda ()
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-k") nil)
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-j") nil)
                        ))
  :config
  (when IS-WINDOWS
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
    )
  (evil-collection-magit-setup)
  :commands (magit-status magit-get-current-branch))

(use-package org
  :config
  (evil-define-key 'normal org-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal org-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal org-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package org-modern
  :hook((org-mode . org-modern-mode)
        (org-agenda-finilize . org-modern-agenda))
  :config
  (setq org-modern-star 'replace))

(use-package org-appear
  :straight (org-appear :type git :fetcher github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autoentities t
        org-appear-autokeywords t
        org-appear-autosubmarkers t))

(use-package toc-org
  :hook (org-mode . toc-org-mode))

(use-package org-fancy-priorities
  :hook ((org-mode org-agenda-mode) . org-fancy-priorities-mode))

(use-package evil-org
  :hook (org-mode . evil-org-mode))

(use-package copilot-chat
  :after org
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode evil)
  :config
  (evil-define-key 'normal copilot-chat-org-prompt-mode-map "@" 'copilot-chat-add-file)
  (evil-define-key 'normal copilot-chat-org-prompt-mode-map "@" 'copilot-chat-add-file)
  (evil-define-key 'normal org-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal org-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal org-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (fundamental-mode . hl-todo-mode)
         (org-mode . hl-todo-mode)
         (git-commit-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo--regexp "\\(\\<\\(FIX\\|fix\\|FEAT\\|feat\\|TODO\\|todo\\|FIXME\\|fixme\\|HACK\\|hack\\|REVIEW\\|review\\|NOTE\\|note\\|DEPRECATED\\|deprecated\\|BUG\\|bug\\|XXX\\)\\>[:]*\\)"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("feat" font-lock-function-call-face bold)
          ("FEAT" font-lock-function-call-face bold)
          ("TODO" warning bold)
          ("todo" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ("fixme" error bold)
          ("FIX" error bold)
          ("fix" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ("hack" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ("review" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ("note" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ("deprecated" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ("bug" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

(use-package zig-ts-mode)
(use-package markdown-ts-mode)
(use-package templ-ts-mode)

(use-package shell
  :config
  (evil-define-key 'normal shell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal shell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal shell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal shell-mode-map (kbd "C-l") 'windmove-right)
  )


(use-package eshell
  :config
  (evil-define-key 'normal eshell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-l") 'windmove-right)
  )

;; transpaency

(defvar +lsp--default-read-process-output-max nil)
(defvar +lsp--default-gcmh-high-cons-threshold nil)
(defvar +lsp--optimization-init-p nil)

(define-minor-mode lsp-optimization-mode
  "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
  :global t
  :init-value nil
  (if (not lsp-optimization-mode)
      (setq-default read-process-output-max +lsp--default-read-process-output-max
                    gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                    +lsp--optimization-init-p nil)
    ;; Only apply these settings once!
    (unless +lsp--optimization-init-p
      (setq +lsp--default-read-process-output-max (default-value 'read-process-output-max)
            +lsp--default-gcmh-high-cons-threshold (default-value 'gcmh-high-cons-threshold))
      (setq-default read-process-output-max (* 2(* 1024 1024)))
      ;; REVIEW LSP causes a lot of allocations, with or without the native JSON
      ;;        library, so we up the GC threshold to stave off GC-induced
      ;;        slowdowns/freezes. Doom uses `gcmh' to enforce its GC strategy,
      ;;        so we modify its variables rather than `gc-cons-threshold'
      ;;        directly.
      (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
      (when (bound-and-true-p gcmh-mode)
        (gcmh-set-high-threshold))
      (setq +lsp--optimization-init-p t))))

(use-package sharper)
(use-package csproj-mode)
(use-package dap-mode
  :commands dap-debug
  :hook (dap-mode . dap-tooltip-mode)
  :config
  (require 'dap-node)
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-edge)
  (require 'dap-netcore)
  (require 'dap-lldb)
  (require 'dap-cpptools))

(provide 'init)
