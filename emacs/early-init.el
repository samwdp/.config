;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(defvar envvars-env-file
  (expand-file-name "emacs-env.el" user-emacs-directory)
  "The location of your envvar file, generated by `envvars-generate-file'.")

(defvar envvars-deny
  '(;; Unix/shell state that shouldn't be persisted
    "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$"
    "^USER$" "^GIT_CONFIG"
    ;; X server, Wayland, or services' env that shouldn't be persisted
    "^\\(WAYLAND_\\)?DISPLAY$" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
    ;; Windows+WSL envvars that shouldn't be persisted
    "^WSL_INTEROP$"
    ;; XDG variables that are best not persisted.
    "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
    "^XDG_\\(VTNR$\\|SEAT$\\|BACKEND$\\|SESSION_\\)"
    ;; Socket envvars
    "SOCK$"
    ;; ssh and gpg variables
    "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$"
    "^GPG_AGENT_INFO$")
  "Environment variables to omit from envvar files.
Each string is a regexp, matched against variable names to omit.")

(defvar envvars-allow '()
  "Environment variables to include in envvar files.
This overrules `envvars-deny`. Each string is a regexp, matched against variable names.")

(defun envvars--should-include-var-p (var)
  "Return non-nil if VAR (a string) should be included based on allow/deny lists."
  (let ((deny (seq-some (lambda (re) (string-match-p re var)) envvars-deny))
        (allow (seq-some (lambda (re) (string-match-p re var)) envvars-allow)))
    (or allow (not deny))))

;;;###autoload
(defun envvars-generate-file (&optional file)
  "Generate environment variable file from current `process-environment'.
Write to FILE or `envvars-env-file'."
  (interactive)
  (let ((file (or file envvars-env-file)))
    (with-temp-file file
      (setq-local coding-system-for-write 'utf-8-unix)
      (insert
       ";; -*- mode: lisp-interaction; coding: utf-8-unix; -*-\n"
       ";; ---------------------------------------------------------------------------\n"
       ";; This file was auto-generated by `envvars-generate-file'. It contains a list of environment\n"
       ";; variables scraped from your shell environment.\n"
       ";;\n"
       ";; It is NOT safe to edit this file. Changes will be overwritten next time you\n"
       ";; run `envvars-generate-file'.\n"
       "\n(")
      (dolist (env process-environment)
        (let ((var (car (split-string env "="))))
          (when (envvars--should-include-var-p var)
            (insert (prin1-to-string env) "\n "))))
      (insert ")\n"))))

;;;###autoload
(defun envvars-load-file (&optional file)
  "Load environment variables from FILE (or `envvars-env-file') into Emacs."
  (interactive)
  (let ((file (or file envvars-env-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        ;; Skip comment lines
        (while (looking-at "^;")
          (forward-line 1))
        (let ((env-list (read (current-buffer))))
          (setq process-environment env-list)
          (setenv "PATH" (getenv "PATH"))
          (setq exec-path (split-string (getenv "PATH") path-separator t))
          t)))))

;;; envvars.el ends here
(unless (file-exists-p envvars-env-file)
  (envvars-generate-file))

(envvars-load-file)
(setenv "LSP_USE_PLISTS" "true")
