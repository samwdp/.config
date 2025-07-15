;;; init.el -*- lexical-binding: t; -*-
(setq gc-cons-threshold 300000000)
(setq read-process-output-max (* 3(* 1024 1024))) ;; 1mb
(set-default-coding-systems 'utf-8)
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
                      
;; Package management
(use-package straight
  :custom
  (straight-check-for-modifications nil))

(defun sp/new-frame ()
  (set-face-attribute 'default nil :font (font-spec :family "RecMonoLinear Nerd Font") :height 140 :weight 'normal)
  (set-face-attribute 'fixed-pitch nil :font (font-spec :family "RecMonoLinear Nerd Font") :height 140)
  (when IS-WINDOWS
    (set-frame-parameter (selected-frame) 'alpha '(98 . 98))
    (add-to-list 'default-frame-alist '(alpha . (98 . 98)))
    )
  (when IS-LINUX
    (set-frame-parameter (selected-frame) 'alpha-background 0.9 ))
  )

(defun unicode-fonts-setup-h (frame)
  "Run unicode-fonts-setup, then remove the hook."
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (require 'unicode-fonts)
      (sp/new-frame)
      (unicode-fonts-setup))))

(defun sp/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name (concat user-emacs-directory "README.org")))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'sp/org-babel-tangle-config)))

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


(defconst IS-LINUX   (eq system-type 'gnu/linux))
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))

(when IS-WINDOWS
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
(setq history-length 25)
(savehist-mode +1)
(save-place-mode 1)
(set-fringe-mode 10)
(display-battery-mode 1)
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
(setq use-dialog-box nil)
(global-auto-revert-mode 1)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode +1)
(set-face-attribute 'default nil :family "RecMonoLinear Nerd Font" :height 140 :weight 'medium)
(setq custom-theme-directory (concat user-emacs-directory "themes/"))
(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)
(global-display-fill-column-indicator-mode +1)

(global-set-key [remap lookup-definition] #'xref-find-definitions)
(global-set-key [remap lookup-reference] #'xref-find-references)
(global-set-key [remap sp/format-buffer] #'format-all-buffer)

(use-package org
  :after evil
  :config
  (when IS-WINDOWS
    (setq org-directory "c:/Users/sam/Documents/org")
    )
  (evil-define-key 'normal org-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal org-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal org-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal org-mode-map (kbd "C-l") 'windmove-right)
  ;; (setq org-export-with-broken-links t)
  )

(use-package toc-org
  :hook (org-mode . toc-org-mode)
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
  :after evil
  :hook (org-mode . evil-org-mode))

(use-package org-roam
  :after org
  :straight (org-roam :host github :repo "org-roam/org-roam")
  :config
    ;;; org-roam-db-sync-optimization.el

;;; Commentary:
  ;;
  ;; This Emacs Lisp file optimizes the Org-roam database synchronization process by introducing
  ;; a mechanism to skip unnecessary synchronization when the database file has not been modified
  ;; since the last update. The timestamp of the last Org-roam database update is stored in an
  ;; external file.

;;; Code:

  (defcustom org-roam-db-last-update-file "~/.emacs.d/org-roam-db-last-update-time"
    "File to store the timestamp of the last Org-roam database update."
    :type 'file
    :group 'org-roam)

  (defvar org-roam-db-last-update-time nil
    "Timestamp of the last Org-roam database update.")

  (defun org-roam-db-load-last-update-time ()
    "Load the timestamp of the last Org-roam database update from file.
If the file is not readable or does not exist, the timestamp remains nil."
    (when (file-readable-p org-roam-db-last-update-file)
      (setq org-roam-db-last-update-time
            (with-temp-buffer
              (insert-file-contents org-roam-db-last-update-file)
              (read (current-buffer))))))

  (defun org-roam-db-save-last-update-time ()
    "Save the timestamp of the last Org-roam database update to file."
    (with-temp-buffer
      (prin1 org-roam-db-last-update-time (current-buffer))
      (write-region (point-min) (point-max) org-roam-db-last-update-file)))

  (defun org-roam-db-update-time ()
    "Update the timestamp of the last Org-roam database update.
This function sets the timestamp to the current time and saves it to the external file."
    (setq org-roam-db-last-update-time (current-time))
    (org-roam-db-save-last-update-time))

  (defun org-roam-db-sync-advice (orig-fun &rest args)
    "Advice function for org-roam-db-sync to check if syncing is necessary.
This advice checks whether the Org-roam database file has been modified since the last update.
If the file has been modified or the last update time is nil, it calls the original function (`org-roam-db-sync`),
updates the timestamp, and saves it to the external file."
    (let ((db-file-modified-time (nth 5 (file-attributes org-roam-db-location))))
      (when (or (null org-roam-db-last-update-time)
                (time-less-p org-roam-db-last-update-time db-file-modified-time))
        ;; Call the original function to perform synchronization
        (apply orig-fun args)
        ;; Update and save the timestamp
        (org-roam-db-update-time))))

  (defun custom/org-roam-db-sync (&optional force)
    "Temporarily remove the sync advice and do org-roam-db-sync.
If FORCE is non-nil, force a rebuild of the cache from scratch."
    (interactive "P")
    (advice-remove 'org-roam-db-sync #'org-roam-db-sync-advice)
    (org-roam-db-sync force)
    (org-roam-db-update-time)
    (advice-add 'org-roam-db-sync :around #'org-roam-db-sync-advice))

;;; Initialization:

  ;; Load the last update time when Emacs starts
  (org-roam-db-load-last-update-time)

  ;; Advising org-roam-db-sync
  (advice-add 'org-roam-db-sync :around #'org-roam-db-sync-advice)

  ;; Save the last update time when Emacs is about to exit
  (add-hook 'kill-emacs-hook 'org-roam-db-save-last-update-time)


;;; org-roam-db-sync-optimization.el ends here
  (when IS-WINDOWS
    (setq org-roam-directory "c:/Users/sam/Documents/org/roam"))
  (org-roam-db-autosync-mode)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-capture-templates
        '(("n" "notes")
          ("nd" "default" plain "%?"
           :target (file+head "notes/${slug}.org" "#+title: ${title}\n")
           :create-file yes
           :unnarrowed t)
          ("nc" "coding" plain "%?"
           :target (file+head "notes/coding/${slug}.org" "#+title: ${title}\n")
           :create-file yes
           :unnarrowed t)
          ))
  )
(use-package org-roam-ui
  :after org-roam
  :hook (org-roam . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t))

(use-package undo-tree
  :after evil
  :custom (undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "var/undo-tree-hist/"))))
  :config
  (evil-global-set-key 'normal (kbd "u") 'undo-tree-undo)
  (evil-global-set-key 'normal (kbd "C-r") 'undo-tree-redo)
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo limits to avoid emacs prematurely truncating the undo
        ;; history and corrupting the tree. This is larger than the undo-fu
        ;; defaults because undo-tree trees consume exponentially more space,
        ;; and then some when `undo-tree-enable-undo-in-region' is involved. See
        ;; syl20bnr/spacemacs#12110
        undo-limit 800000           ; 800kb (default is 160kb)
        undo-strong-limit 12000000  ; 12mb  (default is 240kb)
        undo-outer-limit 128000000) ; 128mb (default is 24mb)
  :init (global-undo-tree-mode))

(use-package no-littering)
(use-package s)
(use-package gcmh
  :ensure t
  :config
  (gcmh-mode 1))

(use-package dtrt-indent)
(use-package smartparens)
(use-package posframe)
(use-package transient)

(use-package doom-themes
  :init
  (load-theme 'gruvbox-sp t))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package which-key
  :init (which-key-mode))

(use-package windmove
  :config
  (setq windmove-wrap-around t)
  )

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (fundamental-mode . hl-todo-mode)
         (org-mode . hl-todo-mode)
         (git-commit-mode . hl-todo-mode))
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo--regexp "\\(\\<\\(HOTFIX\\|hotfix\\|FIX\\|fix\\|FEAT\\|feat\\|TODO\\|todo\\|FIXME\\|fixme\\|HACK\\|hack\\|REVIEW\\|review\\|NOTE\\|note\\|DEPRECATED\\|deprecated\\|BUG\\|bug\\|XXX\\)\\>[:]*\\)"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("feat" font-lock-function-call-face bold)
          ("FEAT" font-lock-function-call-face bold)
          ("TODO" warning bold)
          ("todo" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("hotfix" error bold)
          ("HOTFIX" error bold)
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

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package golden-ratio
  :init
  (golden-ratio-mode +1))

(use-package unicode-fonts
  :init
  (if (display-graphic-p)
      (unicode-fonts-setup-h (selected-frame))
    (add-hook 'after-make-frame-functions 'unicode-fonts-setup-h)))

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

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package pulsar
  :init (pulsar-global-mode +1))

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
  (set-popup-rules!  '(("^\\*"  :slot 1 :vslot -1 :select t)
                       ("^\\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit)
                       ("^\\magit:" :slot 1 :vslot -1 :size +popup-shrink-to-fit)
                       ))
  )

(use-package adaptive-wrap)
(use-package adaptive-word-wrap-mode
 :straight (adaptive-word-wrap-mode :type git :host github :repo "samwdp/adaptive-word-wrap-mode")
 :hook (after-init . global-adaptive-word-wrap-mode))

(defvar sp/keys-keymap (make-keymap)
  "Keymap for my/keys-mode")

(define-minor-mode sp/keys-mode
  "Minor mode for my personal keybindings."
  :init-value t
  :global t
  :keymap sp/keys-keymap)

;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists
             `((sp/keys-mode . ,sp/keys-keymap)))

(define-key sp/keys-keymap (kbd "C-j") 'windmove-down)
(define-key sp/keys-keymap (kbd "C-h") 'windmove-left)
(define-key sp/keys-keymap (kbd "C-k") 'windmove-up)
(define-key sp/keys-keymap (kbd "C-l") 'windmove-right)

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
  (evil-global-set-key 'normal (kbd "C-u") 'evil-scroll-up)
  (evil-global-set-key 'normal (kbd "K") nil)
  (evil-global-set-key 'normal (kbd "J") nil)
  (evil-global-set-key 'normal (kbd "C-f") nil)
  (evil-global-set-key 'normal (kbd "C-j") 'windmove-down)
  (evil-global-set-key 'normal (kbd "C-k") 'windmove-up)
  (evil-global-set-key 'normal (kbd "C-h") 'windmove-left)
  (evil-global-set-key 'normal (kbd "C-l") 'windmove-right)
  (evil-global-set-key 'normal "-" 'dired-jump)
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

(use-package evil-multiedit
  :commands (evil-mc-make-cursor-here
             evil-mc-make-all-cursors
             evil-mc-undo-all-cursors
             evil-mc-pause-cursors
             evil-mc-resume-cursors
             evil-mc-make-and-goto-first-cursor
             evil-mc-make-and-goto-last-cursor
             evil-mc-make-cursor-in-visual-selection-beg
             evil-mc-make-cursor-in-visual-selection-end
             evil-mc-make-cursor-move-next-line
             evil-mc-make-cursor-move-prev-line
             evil-mc-make-cursor-at-pos
             evil-mc-has-cursors-p
             evil-mc-make-and-goto-next-cursor
             evil-mc-skip-and-goto-next-cursor
             evil-mc-make-and-goto-prev-cursor
             evil-mc-skip-and-goto-prev-cursor
             evil-mc-make-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-skip-and-goto-next-match
             evil-mc-make-and-goto-prev-match
             evil-mc-skip-and-goto-prev-match)
  :config
  (evil-mc-define-vars)
  (evil-mc-initialize-vars)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-pause-incompatible-modes)
  (add-hook 'evil-mc-before-cursors-created #'evil-mc-initialize-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-teardown-active-state)
  (add-hook 'evil-mc-after-cursors-deleted  #'evil-mc-resume-incompatible-modes)
  (advice-add #'evil-mc-initialize-hooks :override #'ignore)
  (advice-add #'evil-mc-teardown-hooks :override #'evil-mc-initialize-vars)
  (advice-add #'evil-mc-initialize-active-state :before #'turn-on-evil-mc-mode)
  (advice-add #'evil-mc-teardown-active-state :after #'turn-off-evil-mc-mode))

(use-package evil-mc
  :config
  ;; evil-multiedit
  (evil-define-key 'normal 'global
    (kbd "M-b")   #'evil-multiedit-match-symbol-and-next
    (kbd "M-B")   #'evil-multiedit-match-symbol-and-prev)
  (evil-define-key 'visual 'global
    "R"           #'evil-multiedit-match-all
    (kbd "M-b")   #'evil-multiedit-match-and-next
    (kbd "M-B")   #'evil-multiedit-match-and-prev)
  (evil-define-key '(visual normal) 'global
    (kbd "C-M-b") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit
    (evil-define-key 'multiedit 'global
      (kbd "M-b")   #'evil-multiedit-match-and-next
      (kbd "M-S-b") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev))

  ;; evil-mc
  (evil-define-key '(normal visual) 'global
    "gzm" #'evil-mc-make-all-cursors
    "gzu" #'evil-mc-undo-all-cursors
    "gzn" #'evil-mc-make-and-goto-next-cursor
    "gzp" #'evil-mc-make-and-goto-prev-cursor
    "gzN" #'evil-mc-make-and-goto-last-cursor
    "gzP" #'evil-mc-make-and-goto-first-cursor)
  (with-eval-after-load 'evil-mc
    (evil-define-key '(normal visual) evil-mc-key-map
      (kbd "C-n") #'evil-mc-make-and-goto-next-cursor
      (kbd "C-N") #'evil-mc-make-and-goto-last-cursor
      (kbd "C-p") #'evil-mc-make-and-goto-prev-cursor
      (kbd "C-P") #'evil-mc-make-and-goto-first-cursor)))

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
    "ar" '(gptel-rewrite :wk "[A]i [R]ewrite")
    "at" '(gptel-menu :wk "[A]i [R]ewrite")
    )
  (sp/leader-keys
    :keymaps 'normal
    ;; single use keymaps
    "." '(find-file :wk "find files")
    "SPC" '(consult-project-extra-find-other-window :wk "find files")
    "f" '(sp/format-buffer :wk "format buffer")
    "w" '(save-buffer :wk "save")
    ;; ai
    "a" '(:ignore t :wk "[A]I")
    "aa" '(gptel :wk "[A]I [A]sk")
    "at" '(gptel-menu :wk "[A]I [T]sk")
    "ae" '(gptel-send :wk "[A]I [E]sk")
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
    "db" '(evil-delete-buffer :wk "[D]elete [B]uffer")
    "dw" '(delete-window :wk "[D]elete [W]indow")
    "h" '(:ignore t :wk "[H]arpoon")
    "ha" '(harpoon-add-file :wk "[H]arpoon [A]dd")
    "he" '(harpoon-toggle-quick-menu :wk "[H]arpoon [E]dit")
    "hc" '(harpoon-clear :wk "[H]arpoon [C]lear")
    ;; git
    "g" '(:ignore t :wk "[G]it")
    "gs" '(magit-status :wk "[G]it [S]tatus")
    ;; instert
    "i" '(:ignore t :wk "[I]nsert")
    "is" '(consult-yasnippet :wk "[I]nsert [S]nippet")
    ;; open things
    "o" '(:ignore t :wk "[O]pen")
    "oe" '(project-eshell t :wk "[O]pen [E]shell")
    "ot" '(project-shell t :wk "[O]pen [T]erminal")
    ;; projects
    "p" '(:ignore t :wk "[P]erspective")
    "ps" '(persp-switch :wk "[P]erspective [S]witch")
    "pp" '(+popup/toggle :wk "[P]opup [T]oggle")
    "pn" '(+popup/other :wk "[P]opup [N]ext")
    ;; search
    "s" '(:ignore t :wk "[S]earch")
    "sd" '(consult-lsp-diagnostics :wk "[S]earch [D]iagnostics")
    "sg" '(consult-ripgrep :wk "[S]earch [G]rep")
    "ss" '(consult-lsp-symbols :wk "[S]earch [G]rep")
    )
  (general-define-key
   "C-f" '(project-switch-project :wk "switch project")
   "C-+" 'text-scale-increase
   (kbd "C--") 'text-scale-increase
   "C-M-n" 'harpoon-go-to-1
   "C-M-e" 'harpoon-go-to-2
   "C-M-o" 'harpoon-go-to-3
   "C-M-i" 'harpoon-go-to-4
   "C-M-=" 'harpoon-toggle-file
   "C-h" 'windmove-left
   "C-l" 'windmove-right
   "C-k" 'windmove-up
   "C-j" 'windmove-down))

(use-package drag-stuff
  :defer t
  :config
  (evil-global-set-key 'visual (kbd "J") (lambda (arg) (interactive "p") (drag-stuff-down arg)
                                           (if (bound-and-true-p lsp-mode)
                                               (lsp-format-region)
                                             (format-all-region-or-buffer))))
  (evil-global-set-key 'visual (kbd "K") (lambda (arg) (interactive "p") (drag-stuff-up arg)
                                           (if (bound-and-true-p lsp-mode)
                                               (lsp-format-region)
                                             (format-all-region-or-buffer))))
  :init
  (drag-stuff-global-mode +1))

;; projects
(use-package project
  :straight (:type built-in)
  :config
  (evil-global-set-key 'normal (kbd "C-f") 'project-switch-project)
  (evil-global-set-key 'normal (kbd "<f5>") 'project-compile)
  )

(use-package consult-project-extra
  :straight t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c C-p"))
  :config
  (setq persp-modestring-dividers '(" "))
  (setq persp-nil-name "main"
        persp-modestring-short t
        persp-set-last-persp-for-new-frames t)

  (persp-mode))

(use-package perspective-project-bridge
  :after perspective
  :config
  (defvar perspective-project-bridge-separator "/")
  (defvar perspective-project-bridge-depth 3)
  (defun my-persp-project-name-from-path (project-root)
    "Generate a perspective name from PROJECT-ROOT path."
    (let* ((parts (split-string (directory-file-name project-root) "[/\\]" t))
           (n (length parts)))
      ;; Always include up to 3 last parts: project, feature, branch
      (mapconcat #'identity (last parts (min perspective-project-bridge-depth n)) perspective-project-bridge-separator)))

  (defun perspective-project-bridge-find-perspective-for-buffer (buffer)
    "Find a project-specific perspective for BUFFER.
If no such perspective exists, a new one is created and the buffer is added to it."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and perspective-project-bridge-mode
                   (buffer-name buffer)
                   (project-current))
          (let* ((project-root (directory-file-name
                                (if (fboundp 'project-root)
                                    (project-root (project-current))
                                  (car (project-roots (project-current))))))
                 (name (my-persp-project-name-from-path project-root))
                 (persp (persp-new name)))
            (with-perspective (persp-name persp)
              (setq perspective-project-bridge-persp t)
              (persp-add-buffer buffer))
            persp)))))

  (add-hook 'perspective-project-bridge-mode-hook
            (lambda ()
              (if perspective-project-bridge-mode
                  (perspective-project-bridge-find-perspectives-for-all-buffers)
                (perspective-project-bridge-kill-perspectives))))

  (add-hook 'after-init-hook
            (lambda ()
              (perspective-project-bridge-mode 1))
            t))

(use-package gptel
  :config
  (require 'gptel-integrations)
  (setq gptel-use-curl nil)
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'o4-mini
        gptel-backend (gptel-make-gh-copilot "Copilot"))
  ;; Enable tool use
  (setq gptel-use-tools t)

  ;; Add a tool to gptel-tools
  (add-to-list 'gptel-tools
               (gptel-make-tool
                :name "read_url"
                :function (lambda (url) 
                            ;; function implementation
                            )
                :description "Fetch and read the contents of a URL"
                :args (list '(:name "url"
                                    :type string
                                    :description "The URL to read"))
                :category "web"))
  (gptel-make-tool
   :function (lambda (filepath)
               (with-temp-buffer
                 (insert-file-contents (expand-file-name filepath))
                 (buffer-string)))
   :name "read_file"
   :description "Read and display the contents of a file"
   :args (list '(:name "filepath"
                       :type string
                       :description "Path to the file to read. Supports relative paths and ~."))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type string
                       :description "The path to the directory to list"))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (directory)
               (mapconcat #'identity
                          (directory-files directory)
                          "\n"))
   :name "list_directory"
   :description "List the contents of a given directory"
   :args (list '(:name "directory"
                       :type string
                       :description "The path to the directory to list"))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (parent name)
               (condition-case nil
                   (progn
                     (make-directory (expand-file-name name parent) t)
                     (format "Directory %s created/verified in %s" name parent))
                 (error (format "Error creating directory %s in %s" name parent))))
   :name "make_directory"
   :description "Create a new directory with the given name in the specified parent directory"
   :args (list '(:name "parent"
                       :type string
                       :description "The parent directory where the new directory should be created, e.g. /tmp")
               '(:name "name"
                       :type string
                       :description "The name of the new directory to create, e.g. testdir"))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (path filename content)
               (let ((full-path (expand-file-name filename path)))
                 (with-temp-buffer
                   (insert content)
                   (write-file full-path))
                 (format "Created file %s in %s" filename path)))
   :name "create_file"
   :description "Create a new file with the specified content"
   :args (list '(:name "path"
                       :type string
                       :description "The directory where to create the file")
               '(:name "filename"
                       :type string
                       :description "The name of the file to create")
               '(:name "content"
                       :type string
                       :description "The content to write to the file"))
   :category "filesystem")
  (defun my-gptel--edit_file (file-path file-edits)
    "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
    (if (and file-path (not (string= file-path "")) file-edits)
        (with-current-buffer (get-buffer-create "*edit-file*")
          (erase-buffer)
          (insert-file-contents (expand-file-name file-path))
          (let ((inhibit-read-only t)
                (case-fold-search nil)
                (file-name (expand-file-name file-path))
                (edit-success nil))
            ;; apply changes
            (dolist (file-edit (seq-into file-edits 'list))
              (when-let* ((line-number (plist-get file-edit :line_number))
                         (old-string (plist-get file-edit :old_string))
                         (new-string (plist-get file-edit :new_string))
                         (is-valid-old-string (not (string= old-string ""))))
                (goto-char (point-min))
                (forward-line (1- line-number))
                (when (search-forward old-string nil t)
                  (replace-match new-string t t)
                  (setq edit-success t))))
            ;; return result to gptel
            (if edit-success
                (progn
                  ;; show diffs
                  (ediff-buffers (find-file-noselect file-name) (current-buffer))
                  (format "Successfully edited %s" file-name))
              (format "Failed to edited %s" file-name))))
      (format "Failed to edited %s" file-path)))

  (gptel-make-tool
   :function #'my-gptel--edit_file
   :name "edit_file"
   :description "Edit file with a list of edits, each edit contains a line-number,
  a old-string and a new-string, new-string will replace the old-string at the specified line."
   :args (list '(:name "file-path"
                       :type string
                       :description "The full path of the file to edit")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:line_number
                                      (:type integer :description "The line number of the file where edit starts.")
                                      :old_string
                                      (:type string :description "The old-string to be replaced.")
                                      :new_string
                                      (:type string :description "The new-string to replace old-string.")))
                       :description "The list of edits to apply on the file"))
   :category "filesystem")
  (gptel-make-tool
   :function (lambda (command &optional working_dir)
               (with-temp-message (format "Executing command: `%s`" command)
                 (let ((default-directory (if (and working_dir (not (string= working_dir "")))
                                              (expand-file-name working_dir)
                                            default-directory)))
                   (shell-command-to-string command))))
   :name "run_command"
   :description "Executes a shell command and returns the output as a string. IMPORTANT: This tool allows execution of arbitrary code; user confirmation will be required before any command is run."
   :args (list
          '(:name "command"
                  :type string
                  :description "The complete shell command to execute.")
          '(:name "working_dir"
                  :type string
                  :description "Optional: The directory in which to run the command. Defaults to the current directory if not specified."))
   :category "command"
   :confirm t
   :include t)

  (defun run_async_command (callback command)
    "Run COMMAND asynchronously and pass output to CALLBACK."
    (condition-case error
        (let ((buffer (generate-new-buffer " *async output*")))
          (with-temp-message (format "Running async command: %s" command)
            (async-shell-command command buffer nil))
          (let ((proc (get-buffer-process buffer)))
            (when proc
              (set-process-sentinel
               proc
               (lambda (process _event)
                 (unless (process-live-p process)
                   (with-current-buffer (process-buffer process)
                     (let ((output (buffer-substring-no-properties (point-min) (point-max))))
                       (kill-buffer (current-buffer))
                       (funcall callback output)))))))))
      (t
       ;; Handle any kind of error
       (funcall callback (format "An error occurred: %s" error)))))

  (gptel-make-tool
   :function #'run_async_command
   :name "run_async_command"
   :description "Run an async command."
   :args (list
          '(:name "command"
                  :type "string"
                  :description "Command to run."))
   :category "command"
   :async t
   :include t)
  )

(defvar mcp-filesystem-dirs nil
  "List of directories for the mcp filesystem backend.")
(when IS-WINDOWS
  (setq mcp-filesystem-dirs
        '("d:/work/foretracklite/develop/"
          "d:/projects/treesit-context-headerline/main/")
        )
  )

(when IS-LINUX
  (setq mcp-filesystem-dirs
        '("~/work/foretracklite/develop/")
        )
  )

(use-package mcp
  :ensure t
  :after gptel
  :custom (mcp-hub-servers
           `(("filesystem" . (:command "npx" :args ("-y"
                                                    "@modelcontextprotocol/server-filesystem"
                                                    ,@mcp-filesystem-dirs 
                                                    )))
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ("github" . (:command "docker"
                                   :args ("run" "-i" "--rm" "-e" "GITHUB_PERSONAL_ACCESS_TOKEN" "ghcr.io/github/github-mcp-server")
                                   :env (:GITHUB_PERSONAL_ACCESS_TOKEN github-pat-token)))))
  :config
  (require 'mcp-hub)
  (require 'secrets nil t)
  :hook (after-init . mcp-hub-start-all-server))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

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
  (setq consult-preview-key "M-,")
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  ;; (set-face-attribute 'consult-preview-file nil :slant 'normal)
  (set-face-attribute 'consult-highlight-match nil :slant 'normal)
  ;; (set-face-attribute 'consult-highlight-mark nil :slant 'normal)
  ;; (set-face-attribute 'consult-preview-mark nil :slant 'normal)
  (set-face-attribute 'consult-preview-insertion nil :slant 'normal)
  (set-face-attribute 'consult-narrow-indicator nil :slant 'normal)
  (set-face-attribute 'consult-async-running nil :slant 'normal)
  (set-face-attribute 'consult-async-finished nil :slant 'normal)
  (set-face-attribute 'consult-async-failed nil :slant 'normal)
  (set-face-attribute 'consult-async-split nil :slant 'normal)
  (set-face-attribute 'consult-help nil :slant 'normal)
  (set-face-attribute 'consult-key nil :slant 'normal)
  (set-face-attribute 'consult-line-number nil :slant 'normal)
  (set-face-attribute 'consult-file nil :slant 'normal)
  (set-face-attribute 'consult-grep-context nil :slant 'normal)
  (set-face-attribute 'consult-bookmark nil :slant 'normal)
  (set-face-attribute 'consult-buffer nil :slant 'normal)
  (set-face-attribute 'consult-line-number-prefix nil :slant 'normal)
  (set-face-attribute 'consult-line-number-wrapped nil :slant 'normal)
  (set-face-attribute 'consult-separator nil :slant 'normal)
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

(use-package embark
  :bind
  (("C-q" . embark-act)
   ("C-#" . embark-export)) ;; Bind C-q to embark-act for acting on results
  :config
  
  (evil-define-key 'normal collect-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal collect-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal collect-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal collect-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal embark-collect-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  ((embark-collect-mode . consult-preview-at-point-mode)))

(use-package corfu
  :bind (:map corfu-map
              ("TAB" . nil)
              ("M-p" . nil)
              ("M-n" . nil)
              ("<tab>" . nil))
  :config
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "C-y") #'corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-preselect 'insert)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-popupinfo-delay '(0.1 . 0.2))
  (corfu-auto-delay 0)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  (corfu-on-exact-match nil)
  (corfu-preselect 'first)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  )

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;;;###autoload
(defun lsp/switch-client (client)
  "Switch to another LSP server CLIENT for the current buffer."
  (interactive
   (progn
     (require 'lsp-mode)
     (list (completing-read
            "Select server: "
            (or (mapcar #'lsp--client-server-id
                        (lsp--filter-clients
                         (lambda (c)
                           (and (lsp--supports-buffer? c)
                                (lsp--server-binary-present? c)))))
                (user-error "No available LSP clients for %S" major-mode))))))
  (require 'lsp-mode)
  (let* ((client-sym (if (symbolp client) client (intern client)))
         (match (car (lsp--filter-clients
                      (lambda (c) (eq (lsp--client-server-id c) client-sym)))))
         (workspaces (lsp-workspaces)))
    (unless match
      (user-error "Couldn't find an LSP client named %S" client))
    (let ((old-priority (lsp--client-priority match)))
      (setf (lsp--client-priority match) 9999)
      (unwind-protect
          (if workspaces
              (lsp-workspace-restart
               (if (cdr workspaces)
                   (completing-read
                    "Select LSP workspace: "
                    (mapcar #'lsp--workspace-print workspaces)
                    nil t)
                 (car workspaces)))
            (lsp-mode +1))
        ;; Restore priority after initialization
        (add-hook
         'lsp-after-initialize-hook
         (lambda ()
           (setf (lsp--client-priority match) old-priority))
         nil 'local)))))
;; lsp
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

(use-package lsp-mode
  :straight (:host github :repo "emacs-lsp/lsp-mode")
  :hook ((typescript-ts-mode . lsp-deferred)
         (html-ts-mode . lsp-deferred)
         (go-ts-mode . lsp-deferred)
         (csharp-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (odin-ts-mode . lsp-deferred)
         (lsp-mode . lsp-optimization-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         )
  :commands lsp-deferred
  :custom
  (read-process-output-max (* 3(* 1024 1024)))
  (lsp-completion-provider :none)
  :init
  (setq lsp-keymap-prefic "C-c")
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-lens-enable nil
        lsp-signature-auto-activate nil
        lsp-signature-function 'lsp-signature-posframe)
  (setq lsp-headerline-breadcrumb-enable nil)
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
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
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
  (define-key lsp-mode-map [remap sp/format-buffer] #'lsp-format-buffer)
  (evil-define-key 'normal lsp-mode-map (kbd "SPC c a") 'lsp-execute-code-action)
  (evil-global-set-key 'normal (kbd "C-SPC") 'lsp-execute-code-action)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package lsp-ui
  :hook ((lsp-mode . lsp-ui-mode))
  :init
  ;; (evil-define-key 'normal lsp-ui-mode-map (kbd "K") 'lsp-ui-doc-glance)
  (evil-define-key 'normal lsp-ui-mode-map (kbd "TAB") 'lsp-ui-doc-focus-frame)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "<escape>") 'lsp-ui-doc-hide)
  (evil-define-key 'normal lsp-ui-doc-frame-mode-map (kbd "q") 'lsp-ui-doc-hide)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  (define-key lsp-mode-map [remap evil-lookup] #'lsp-ui-doc-glance)

  (define-key lsp-ui-peek-mode-map (kbd "j") #'lsp-ui-peek--select-next)
  (define-key lsp-ui-peek-mode-map (kbd "k") #'lsp-ui-peek--select-prev)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-next-file)
  (define-key lsp-ui-peek-mode-map (kbd "M-j") #'lsp-ui-peek--select-prev-file))



(use-package consult-lsp
  :defer t)

(use-package treemacs)

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))
(use-package lsp-treemacs-nerd-icons
  :after nerd-icons
  :straight (:host github :repo "Velnbur/lsp-treemacs-nerd-icons")
  :init (with-eval-after-load 'lsp-treemacs
          (require 'lsp-treemacs-nerd-icons))
  )
(use-package lsp-treemacs
  :custom (lsp-treemacs-theme "nerd-icons-ext"))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-n" . flycheck-next-error)
              ("C-p" . flycheck-previous-error))
  :custom
  (flycheck-display-errors-delay .3)
  (flycheck-checker-error-threshold 2000)
  )
(use-package consult-flycheck)

(use-package format-all)

(use-package yasnippet
  :init (yas-global-mode))

(use-package yasnippet-capf
  :after cape
  :straight (yasnippet-capf :fetcher github :repo "elken/yasnippet-capf")
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(use-package consult-yasnippet)

(use-package yasnippet-snippets)

(use-package competitive-programming-snippets)

(use-package dap-mode
  :commands dap-debug
  :hook (dap-mode . dap-tooltip-mode)
  :config
  
  (defvar my/golden-ratio-was-on t
    "Remember whether `golden-ratio-mode' was on before starting DAP.")

  (defun my/dap-disable-golden-ratio (&rest _)
    "Disable `golden-ratio-mode' when DAP session starts."
    (setq my/golden-ratio-was-on golden-ratio-mode)
    (when golden-ratio-mode
      (golden-ratio-mode -1)))

  (defun my/dap-restore-golden-ratio (&rest _)
    "Re-enable `golden-ratio-mode' if it was on before DAP."
    (when my/golden-ratio-was-on
      (golden-ratio-mode +1)))

  ;; Hook into DAP session start/end
  (with-eval-after-load 'dap-mode
    (add-hook 'dap-session-created-hook    #'my/dap-disable-golden-ratio)
    (add-hook 'dap-terminated-hook         #'my/dap-restore-golden-ratio)
    (add-hook 'dap-exited-hook             #'my/dap-restore-golden-ratio))
  (require 'dap-node)
  (require 'dap-chrome)
  (require 'dap-firefox)
  (require 'dap-edge)
  (require 'dap-netcore)
  (require 'dap-lldb)
  (require 'dap-cpptools))

(use-package magit
  :demand t
  :hook (magit-mode . (lambda ()
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-k") nil)
                        (evil-collection-define-key 'normal 'magit-mode-map (kbd "C-j") nil)
                        ))
  :config
  (when IS-WINDOWS
    (setq magit-git-executable "C:/Program Files/Git/mingw64/bin/git.exe")
    )
  (setq git-commit-major-mode 'git-commit-ts-mode)
  (evil-collection-magit-setup))

(use-package dired
  :straight (:type built-in)
  :config
  (setq dired-dwim-target t))

(use-package dirvish
  :config
  (dirvish-override-dired-mode)
  (setq ls-lisp-dirs-first t)
  (evil-define-key 'normal dired-mode-map (kbd "o") 'dired-create-empty-file)
  (evil-collection-define-key 'normal 'dired-mode-map (kbd "SPC") nil)
  (setq dirvish-attributes
        (append
         ;; The order of these attributes is insignificant, they are always
         ;; displayed in the same position.
         '(vc-state subtree-state nerd-icons collapse)
         ;; Other attributes are displayed in the order they appear in this list.
         '(git-msg file-size))
        dirvish-hide-details t))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package shell
  :config
  (evil-define-key 'normal shell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal shell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal shell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal shell-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))
  (evil-define-key 'normal eshell-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal eshell-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-k") 'windmove-up)
  (evil-define-key 'normal eshell-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal eshell-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal eshell-prompt-mode-map (kbd "C-l") 'windmove-right)
  )

(use-package eshell-z)
(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode)
  )
(use-package eshell-did-you-mean
  :config
  (eshell-did-you-mean-setup)
  )

(use-package harpoon
  :config
 (setq harpoon-project-package 'project) 
  )

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (setq treesit-auto-langs '(lua yaml c go gomod json markdown c-sharp javascript typescript tsx css html))
  (treesit-auto-add-to-auto-mode-alist '(lua yaml c go gomod json markdown c-sharp javascript typescript tsx css html))
  (global-treesit-auto-mode))

(use-package treesit
  :straight (:type built-in)
  :config
  (setq treesit-font-lock-level 4)
  ;; (add-to-list 'treesit-language-source-alist '(lua "https://github.com/tjdevries/tree-sitter-lua" "master" "src"))
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.0" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.0" "tree-sitter-markdown-inline/src"))
  (add-to-list 'treesit-language-source-alist '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
  (add-to-list 'treesit-language-source-alist '(gitcommit "https://github.com/gbprod/tree-sitter-gitcommit" "v0.3.3" "src"))
  (add-to-list 'treesit-language-source-alist '(zig "https://github.com/maxxnino/tree-sitter-zig"))
  )

(use-package treesit-context-overlay
  :straight (treesit-context-overlay :host github :repo "samwdp/treesit-context-overlay")
  :hook ((csharp-ts-mode . treesit-context-overlay-mode)
         (typescript-ts-mode . treesit-context-overlay-mode))
  :config
  (setq treesit-context-overlay-face "#bdae93"
        treesit-context-overlay-delimiter "=>")
  )

(use-package treesit-context-headerline
  :straight (treesit-context-headerline :host github :repo "samwdp/treesit-context-headerline")
  :hook ((csharp-ts-mode . treesit-context-headerline-mode)
         (typescript-ts-mode . treesit-context-headerline-mode))
  :config
  (setq treesit-context-headerline-separator '("nf-cod-chevron_right" . nerd-icons)))

(use-package lua-ts-mode
  :mode ("\\.lua\\'" . lua-ts-mode)
  :straight (:type built-in)
  )
(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  )
(use-package treesitter-context
  :straight (treesitter-context :host github :repo "zbelial/treesitter-context.el")
  :config
  (setq treesitter-context-idle-time 0.1)
  ) 
(use-package grip-mode)
(use-package ox-gfm)
(use-package evil-markdown
  :hook (markdown-ts-mode . evil-markdown-mode)
  :straight (evil-markdown :host github :repo "samwdp/evil-markdown")
  )

(use-package zig-ts-mode)
(use-package markdown-ts-mode)
(use-package templ-ts-mode)
(use-package sharper)
(use-package csproj-mode)
(use-package odin-ts-mode
  :straight (:host github :repo "Sampie159/odin-ts-mode")
  :mode ("\\.odin\\'" . odin-ts-mode))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cshtml?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.razor?\\'" . web-mode))
  (add-to-list 'web-mode-engines-alist '(("razor" . "\\.cshtml\\'")))
  )
(use-package git-commit-ts-mode
:mode "\\COMMIT_EDITMSG\\'")

(provide 'init)
