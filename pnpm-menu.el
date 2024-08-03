;;; pnpm-menu.el --- Transient menu for the PnPM package manager -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/pnpm-menu
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "28.1") (transient "0.7.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Transient menu for the PnPM package manager

;;; Code:

(require 'transient)
(require 'npmjs)

(defcustom pnpm-menu-inhibit-cache nil
  "Inhibition flag for caching pnpm menu commands.

When non-nil, disables the caching mechanism for pnpm commands.

By default, pnpm commands are cached to improve performance.
Setting this variable to t will inhibit this caching, causing
pnpm commands to be re-evaluated each time the menu is invoked.

This can be useful for development or debugging purposes where
the command set might change frequently."
  :group 'pnpm-menu
  :type 'boolean)

(defcustom pnpm-menu-pnpm-help-url "https://pnpm.io/filtering"
  "URL for the pnpm help documentation.

URL for the pnpm help documentation.

This URL is used to direct users to the official pnpm
documentation page for filtering commands.

It is utilized within the `pnpm-menu' transient prefix to provide
contextual help when managing dependencies with pnpm."
  :group 'pnpm-menu
  :type 'boolean)

(defcustom pnpm-menu-extra-command-props '(("start" :inapt-if-not
                                            (lambda ()
                                              (or
                                               (npmjs-get-package-json-script
                                                'start)
                                               (when-let
                                                   ((proj
                                                     (npmjs-get-project-root)))
                                                 (file-exists-p
                                                  (expand-file-name
                                                   "server.js"
                                                   proj))))))
                                           ("test" :inapt-if-not (lambda ()
                                                                   (npmjs-get-package-json-script
                                                                    'test)))
                                           ("run" :inapt-if-not
                                            npmjs-get-project-root)
                                           ("exec" :inapt-if-not
                                            npmjs-get-project-root)
                                           ("audit" :inapt-if-not
                                            npmjs-get-project-root)
                                           ("licenses" :inapt-if-not
                                            npmjs-get-project-root))
  "Transient properties for additional pnpm commands.

An alist of additional properties for pnpm commands in the menu.

Each element is a cons cell where the car is a string representing
the pnpm command, and the cdr is a plist of properties. The plist
can contain various keys to control the behavior and appearance
of the command in the menu."
  :group 'pnpm-menu
  :type
  '(alist
    :key-type (string :tag "Command")
    :value-type (plist
                 :key-type (choice
                            (const :if)
                            (const :if-not)
                            (const :if-non-nil)
                            (const :if-nil)
                            (const :if-mode)
                            (const :if-not-mode)
                            (const :if-derived)
                            (const :if-not-derived)
                            (const :inapt-face)
                            (const :inapt-if)
                            (const :inapt-if-not)
                            (const :inapt-if-non-nil)
                            (const :inapt-if-nil)
                            (const :inapt-if-mode)
                            (const :inapt-if-not-mode)
                            (const :inapt-if-derived)
                            (const :inapt-if-not-derived)
                            (symbol))
                 :value-type sexp)))

(defvar pnpm-menu-package-column [:class transient-column
                                  :setup-children
                                  (lambda (&rest _argsn)
                                    (mapcar
                                     (apply-partially #'transient-parse-suffix
                                                      (oref transient--prefix
                                                            command))
                                     (list '("-g" "global" "--global")
                                           `("." "package" "<pkg>"
                                             :prompt "Package "
                                             :class transient-option
                                             :multi-value t
                                             :reader
                                             ,(npmjs-make-reader nil
                                               #'pnpm-menu--pkg-get-dependencies-table
                                               t)))))])

(defvar pnpm-menu-command-prefix-props `(("rebuild" . ,pnpm-menu-package-column)
                                         ("remove" . ,pnpm-menu-package-column)
                                         ("update" . ,pnpm-menu-package-column)
                                         ("unlink" . ,pnpm-menu-package-column)))

(defconst pnpm-menu-argument-regex
  (rx (seq bol
           (zero-or-more " ")
           (opt (group
                 (group "-"
                        (any "a-z"))
                 ", "))
           (group "--"
                  (opt (group
                        (opt "[")
                        (group
                         (one-or-more
                          (any "a-z" "_|-"))
                         "-")
                        "]"))
                  (one-or-more
                   (any "a-z" "_-")))
           (opt (group
                 (any " =")
                 (group
                  (one-or-more
                   (not (any " "))))))))
  "Regular expression for matching pnpm command-line arguments.")


(defconst pnpm-menu-argument-group-title-regex
  (rx (seq bol
           (group
            (any "a-z")
            (one-or-more nonl))
           ":"
           (zero-or-more " ")
           eol))
  "Regular expression matching argument group titles.")

(defconst pnpm-menu--command-regex
  (rx (seq bol
           (zero-or-more " ")
           (group
            (opt (group
                  (group
                   (one-or-more
                    (any "a-z" "-")))
                  (opt (group ","))
                  " "))
            (group
             (one-or-more
              (any "a-z" "-"))))
           (group
            (or (seq " "
                     (one-or-more " "))
                eol))))
  "Regular expression for matching pnpm commands.")


(defun pnpm-menu--arg-description-at-point ()
  "Concatenate and return argument descriptions from the current point onward."
  (let* ((descr-parts
          (list (buffer-substring-no-properties (point)
                                                (line-end-position))))
         (col (current-column))
         (re (concat (make-string col ?\s) "\\([^\s][^\n]+\\)")))
    (forward-line 1)
    (while (looking-at re)
      (push (match-string-no-properties 1) descr-parts)
      (forward-line 1))
    (string-join (nreverse descr-parts) "\s")))


(defun pnpm-menu-read-dir-relative (&optional prompt &rest _)
  "Read a directory name with optional PROMPT and return its relative path.

Optional argument PROMPT is a string used to prompt the user for a directory.

Remaining arguments _ are ignored."
  (file-relative-name
   (file-local-name
    (expand-file-name
     (read-directory-name
      (or prompt "Directory: "))))
   default-directory))

(transient-define-argument pnpm-menu-level-argument ()
  "Define a transient argument for selecting a log level with specific choices."
  :class 'transient-switches
  :argument-format "--loglevel %s"
  :argument-regexp "\\(--loglevel[=\s]\\(?:debug\\|error\\|info\\|warn\\)\\)"
  :choices '("debug" "warn" "error" "info"))


(defvar pnpm-menu-recursive nil)

(transient-define-argument pnpm-menu-toggle-recursive ()
  "Toggle the variable `pnpm-menu-recursive' between true and false."
  :class 'transient-lisp-variable
  :description "Runs a command in every project of a workspace"
  :variable 'pnpm-menu-recursive
  :reader (lambda (&rest _)
            (setq pnpm-menu-recursive (not pnpm-menu-recursive)))
  :init-value (lambda (ob)
                (setf
                 (slot-value ob 'value)
                 pnpm-menu-recursive)))

(defvar eww-auto-rename-buffer)
(defun pnpm-menu--browse-url (url)
  "Open a URL in EWW and set up a custom quit keybinding.

Argument URL is the web address to be opened in the EWW browser."
  (require 'eww)
  (let* ((wnd (selected-window))
         (save-selected-window--state
          (internal--before-with-selected-window wnd)))
    (letrec
        ((cleanup-fn
          (lambda (&rest _)
            (remove-hook
             'eww-after-render-hook
             cleanup-fn)
            (let* ((quit-key (or (where-is-internal
                                  'quit-window
                                  special-mode-map
                                  t t t)
                                 `[,(string-to-char
                                     "q")]))
                   (map
                    (make-sparse-keymap))
                   (buff (current-buffer)))
              (define-key map quit-key
                          (lambda ()
                            (interactive)
                            (internal--after-with-selected-window
                             save-selected-window--state)
                            (when (and wnd
                                       (window-live-p wnd))
                              (select-window wnd))
                            (transient-resume)
                            (when (buffer-live-p buff)
                              (kill-buffer buff))))
              (use-local-map
               (make-composed-keymap
                map
                (current-local-map)))
              (ignore-errors
                (when (text-property-search-forward 'outline-level)
                  (scroll-up-command)
                  (recenter)))
              (message
               (substitute-command-keys
                "Type \\`q' to resume transient command.")))))
         (eww-auto-rename-buffer nil))
      (add-hook 'eww-after-render-hook cleanup-fn)
      (let ((other-wnd
             (let ((wind-target
                    (if (minibuffer-selected-window)
                        (with-minibuffer-selected-window
                          (let ((wind (selected-window)))
                            (or
                             (window-right wind)
                             (window-left wind)
                             (split-window-sensibly)
                             wind)))
                      (let ((wind (selected-window)))
                        (or
                         (window-right wind)
                         (window-left wind)
                         (split-window-sensibly)
                         wind)))))
               wind-target)))
        (select-window other-wnd)
        (eww url nil)))))


(defmacro pnpm-menu-define-command (name cmd description &rest body)
  "Define a transient command with NAME, CMD, DESCRIPTION, and BODY.

Argument NAME is the symbol for the command being defined.

Argument CMD is the string representing the pnpm command to run.

Argument DESCRIPTION is the string describing the command.

Remaining arguments BODY are the forms to be included in the command definition."
  (let ((sym (make-symbol "sym"))
        (help-output (make-symbol "help-output")))
    `(progn
       (let ((,sym)
             (,help-output))
        (transient-define-prefix ,name ()
          ,(concat (or description cmd) ".")
          :value (lambda ()
                   (let ((args
                          (unless (npmjs-get-project-root)
                           (list "--global"))))
                    (when pnpm-menu-recursive
                     (push "--recursive" args))
                    args))
          :show-help (lambda (&rest _)
                       (setq ,help-output
                        (or ,help-output (npmjs-parse-help-with-output
                                             (shell-command-to-string (format
                                                                       "pnpm help %s"
                                                                       ,cmd))
                                           (buffer-string))))
                       (let ((url (with-temp-buffer
                                    (insert ,help-output)
                                    (goto-char (point-max))
                                    (require 'ffap)
                                    (when (re-search-backward
                                           ffap-url-regexp
                                           nil t 1)
                                     (ffap-url-at-point)))))
                        (pnpm-menu--browse-url url)))
          [:description (lambda ()
                          (setq ,help-output
                           (or ,help-output
                            (npmjs-parse-help-with-output
                                (shell-command-to-string (format
                                                          "pnpm help %s"
                                                          ,cmd))
                              (buffer-string))))
                          (concat
                           (with-temp-buffer
                             (insert ,help-output)
                             (goto-char (point-min))
                             (re-search-forward "^[\s]*Options:" nil t 1)
                             (delete-region (match-beginning 0)
                              (point-max))
                             (while (zerop (forward-line -1))
                              (let ((pos (point)))
                               (save-excursion
                                 (goto-char (line-end-position))
                                 (when (> (current-column) 50)
                                  (fill-region-as-paragraph pos (point))))))
                             (buffer-string))
                           "\n"
                           (car (nth 0 ,sym))))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 0 ,sym))))]
          [:description (lambda ()
                          (car (nth 1 ,sym)))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 1 ,sym))))]
          [:description (lambda ()
                          (car (nth 2 ,sym)))
           :if (lambda ()
                 (car (nth 2 ,sym)))
           :class transient-column
           :setup-children
           (lambda (&rest _argsn)
             (mapcar
              (apply-partially #'transient-parse-suffix
               (oref transient--prefix command))
              (cdr (nth 2 ,sym))))]
          ,@body
          [[("RET" "Run" (lambda ()
                           (interactive)
                           (npmjs-run-compile
                            (npmjs-confirm-command
                             "pnpm"
                             ,cmd
                             (pnpm-menu--get-formatted-transient-args))
                            (concat
                             (npmjs--get-project-buffer-name)
                             "-pnpm"))))
            ("C-c C-a" "Show arguments" pnpm-menu-show-args)]]
          (interactive)
          (npmjs-nvm-with-current-node-version
           (setq ,help-output
            (or ,help-output (npmjs-parse-help-with-output
                                 (shell-command-to-string
                                  (concat "pnpm help " ,cmd))
                               (buffer-string))))
           (setq ,sym (or ,sym
                       (pnpm-menu--eval-command-args
                        ,cmd
                        ,help-output)))
           (transient-setup ',name)))
        ;; (autoload #',name "pnpm-menu" nil t)
        )
       (put ',name 'pnpm-command
        ,(concat "pnpm " cmd)))))

(defun pnpm-menu--global-package-completion-table ()
  "Provide completion for global npm packages."
  (let* ((alist (ignore-errors (pnpm-menu--global-packages)))
         (annotf (lambda (it)
                   (let ((value (cdr-safe (assoc it alist))))
                     (concat "@" (if (listp value)
                                     (string-join value " ")
                                   (or value "")))))))
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotf))
        (complete-with-action action alist str pred)))))

(defun pnpm-menu--global-packages ()
  "List global npm packages installed on the system."
  (condition-case nil
      (npmjs-pluck-depenencies
       (npmjs-json-parse-string
        (npmjs-with-temp-buffer
         (shell-command
          "pnpm ls --global --json"
          (current-buffer))
         (let ((beg)
               (end))
           (when (re-search-forward "{"
                                    nil t
                                    1)
             (setq beg (1- (point))))
           (goto-char (point-max))
           (when (re-search-backward "}"
                                     nil t
                                     1)
             (setq end (1+ (point))))
           (buffer-substring-no-properties
            beg
            end)))))
    (error nil)))

(defun pnpm-menu--pkg-get-dependencies-table ()
  "Retrieve global or local npm package dependencies table."
  (let* ((args
          (npmjs-get-arguments))
         (global-p (seq-intersection args '("--global" "-g"))))
    (if
        (or global-p (not (npmjs-get-project-root)))
        (pnpm-menu--global-package-completion-table)
      (npmjs-local-package-completion-table))))

(defun pnpm-menu--show-help (arg description &optional short specifier)
  "Display a help buffer with ARG, DESCRIPTION, and optional SHORT and SPECIFIER.

Argument ARG is the main string to be displayed in the help buffer.

Argument DESCRIPTION is the detailed text to be shown in the help buffer.

Optional argument SHORT is an additional short DESCRIPTION to be displayed.

Optional argument SPECIFIER is an extra string to be shown in the help buffer."
  (let* ((buffer (get-buffer-create
                  "*pnpm-menu-help*"))
         (orign-wnd (selected-window)))
    (with-current-buffer buffer
      (with-current-buffer-window
          buffer
          (cons 'display-buffer-at-bottom
                '((window-height . fit-window-to-buffer)))
          (lambda (window _value)
            (with-selected-window window
              (setq buffer-read-only t)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (setq truncate-lines nil)
                (when-let* ((quit-key (where-is-internal
                                       'quit-window
                                       special-mode-map
                                       t t t))
                            (map
                             (make-sparse-keymap))
                            (buff (current-buffer)))
                  (define-key map quit-key
                              (lambda ()
                                (interactive)
                                (when (and orign-wnd
                                           (window-live-p orign-wnd))
                                  (select-window orign-wnd)
                                  (transient-resume)
                                  (when (buffer-live-p buff)
                                    (kill-buffer buff)))))
                  (use-local-map
                   (make-composed-keymap
                    map
                    (current-local-map))))
                (save-excursion
                  (insert (propertize (substring-no-properties arg) 'face
                                      'font-lock-keyword-face))
                  (when short
                    (insert ", "
                            (propertize (substring-no-properties short) 'face
                                        'font-lock-keyword-face)))
                  (when specifier
                    (insert " " (propertize
                                 (or (substring-no-properties specifier) "")
                                 'face
                                 'font-lock-type-face)))
                  (insert "\n")
                  (when description
                    (let ((pos (point)))
                      (insert description)
                      (fill-region-as-paragraph pos (point)))))))
            (select-window window))))))

(defun pnpm-menu--get-formatted-transient-args ()
  "Format transient arguments for npmjs."
  (npmjs-format-args (reverse (npmjs-get-arguments))))


;;;###autoload (autoload 'pnpm-menu-show-args "pnpm-menu" nil t)
(transient-define-suffix pnpm-menu-show-args ()
  "Display formatted npm command with arguments."
  :transient t
  (interactive)
  (let ((name (or
               (get transient-current-command 'pnpm-command)
               (get transient-current-command 'npm-command)))
        (args
         (pnpm-menu--get-formatted-transient-args)))
    (npmjs-message
     (propertize (npmjs-make-command-name name args)
                 'face 'success))))

(defvar pnpm-menu-add-groupped-args nil)

;;;###autoload (autoload 'pnpm-menu-add "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu-add ()
  "Install Node.js packages using npm."
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
  :show-help (lambda ()
               (pnpm-menu--browse-url "https://pnpm.io/cli/add"))
  [:description
   "Installs a package and any packages that it depends on. By default, any new package is installed as a prod dependency"
   :class transient-column
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      '(("." "package" npmjs-install-pkg-argument)
        ("@t" "package@tag" npmjs-install-pkg-tag-argument)
        ("@v" "package@version" npmjs-install-pkg-version-argument)
        ("-d" "directory" npmjs-install-pkg-directory)
        ("-t" "Install tarball file" npmjs-install-pkg-tarbal-argument))))]
  [:description (lambda ()
                  (car (nth 0 pnpm-menu-add-groupped-args)))
   :class transient-column
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 0 pnpm-menu-add-groupped-args))))]
  [:description (lambda ()
                  (car (nth 1 pnpm-menu-add-groupped-args)))
   :class transient-column
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 1 pnpm-menu-add-groupped-args))))]
  [[("RET" "Run" (lambda ()
                   (interactive)
                   (npmjs-run-compile
                    (npmjs-confirm-command
                     "pnpm"
                     "add"
                     (pnpm-menu--get-formatted-transient-args))
                    (concat
                     (npmjs--get-project-buffer-name)
                     "-pnpm"))))
    ("C-c C-a" "Show arguments" pnpm-menu-show-args)]]
  (interactive)
  (npmjs-nvm-with-current-node-version
   (setq pnpm-menu-add-groupped-args (pnpm-menu--eval-command-args
                                      "add"
                                      (npmjs-parse-help-with-output
                                          (shell-command-to-string
                                           (concat "pnpm help add"))
                                        (buffer-string)))))
  (transient-setup 'pnpm-menu-add))

(put 'pnpm-menu-add 'pnpm-command "pnpm add")

(defvar pnpm-menu-run-args nil)

;;;###autoload (autoload 'pnpm-menu-run "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu-run ()
  "Runs a defined package script."
  :show-help (lambda ()
               (pnpm-menu--browse-url "https://pnpm.io/cli/run"))
  [[:description
    (lambda ()
      (if-let ((name (npmjs-project-display-name)))
          (format "Run script (%s)" name)
        "Run script"))
    :setup-children
    (lambda (&rest _args)
      (mapcar
       (apply-partially #'transient-parse-suffix
                        (oref
                         transient--prefix
                         command))
       npmjs-current-scripts))
    :class transient-column]]
  [:description
   (lambda nil
     (concat "Runs a defined package script" "
"
             (car
              (nth 0 pnpm-menu-run-args))))
   :class transient-column
   :setup-children
   (lambda
     (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr
       (nth 0 pnpm-menu-run-args))))]
  [:description
   (lambda nil
     (car
      (nth 1 pnpm-menu-run-args)))
   :class transient-column
   :setup-children
   (lambda
     (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr
       (nth 1 pnpm-menu-run-args))))]
  [[("RET" "Run"
     (lambda nil
       (interactive)
       (npmjs-run-compile
        (npmjs-confirm-command "pnpm" "run"
                               (pnpm-menu--get-formatted-transient-args))
        (concat
         (npmjs--get-project-buffer-name)
         "-pnpm"))))
    ("C-c C-a" "Show arguments" pnpm-menu-show-args)]]
  (interactive)
  (npmjs-nvm-with-current-node-version
   (setq npmjs-current-scripts (npmjs-get-scripts-suffixes))
   (setq pnpm-menu-run-args
         (pnpm-menu--eval-command-args "run"
                                       (npmjs-parse-help-with-output
                                           (shell-command-to-string
                                            (concat "pnpm help " "run"))
                                         (buffer-string))
                                       (mapcar #'car npmjs-current-scripts)))
   (transient-setup 'pnpm-menu-run)))

(put 'pnpm-menu-run 'pnpm-command
      "pnpm run")

(defun pnpm-menu--extract-choices-from-description (description)
  "Extract and return choices from DESCRIPTION string separated by |.

Argument DESCRIPTION is a string containing the choices separated by |."
  (when (string-match-p "\\([a-zA-Z0-9-]+|\\)+" description)
    (with-temp-buffer (save-excursion
                        (insert description))
                      (re-search-forward "\\([a-z0-9_-]+|\\)+" nil t 1)
                      (let ((beg (match-beginning 0)))
                        (split-string
                         (buffer-substring-no-properties beg
                                                         (progn
                                                           (skip-chars-forward
                                                            "a-z0-9_-")
                                                           (point)))
                         "|" t)))))

(defun pnpm-menu--parse-args ()
  "Parse command-line arguments into structured groups and commands."
  (let ((groups)
        (used-keys (list "q")))
    (let ((case-fold-search t))
      (while (progn (skip-chars-forward "\n")
                    (and
                     (not (looking-at pnpm-menu-argument-group-title-regex))
                     (zerop (forward-line 1)))))
      (while (progn (skip-chars-forward "\n")
                    (looking-at pnpm-menu-argument-group-title-regex))
        (when-let ((group-title (match-string-no-properties 1)))
          (forward-line 1)
          (let ((commands))
            (while (looking-at pnpm-menu-argument-regex)
              (let ((short (match-string-no-properties 2))
                    (argument  (match-string-no-properties 3))
                    (no-option (match-string-no-properties 4))
                    (bracket-value (match-string-no-properties 5))
                    (specifier (match-string-no-properties 7))
                    (descr)
                    (short-descr)
                    (arg-list))
                (goto-char (match-end 0))
                (skip-chars-forward "\s")
                (setq descr (pnpm-menu--arg-description-at-point))
                (unless (member argument '("--help"))
                  (when short
                    (push short used-keys)
                    (setq arg-list (list short argument)))
                  (setq short-descr (replace-regexp-in-string "^--" "" argument))
                  (cond ((and argument
                              no-option
                              bracket-value)
                         (let* ((arg-value (substring-no-properties
                                            argument
                                            (length
                                             (concat
                                              "--"
                                              no-option))))
                                (choices (mapcan
                                          (lambda (it)
                                            (list (concat "--" it arg-value)
                                                  (concat "--" arg-value)))
                                          (split-string bracket-value "|" t)))
                                (pl `(:choices ',choices
                                      :argument-format "%s"
                                      :argument-regexp ,(regexp-opt
                                                         choices)
                                      :show-help (lambda (&rest _)
                                                   (interactive)
                                                   (pnpm-menu--show-help
                                                    ,(string-trim
                                                      argument)
                                                    ,descr
                                                    ,short
                                                    ,specifier)))))
                           (push
                            (append (list (car choices))
                                    pl)
                            commands)))
                        ((and specifier
                              (string-match-p "\\([a-zA-Z0-9-]+|\\)+"
                                              descr))
                         (let* ((choices (pnpm-menu--extract-choices-from-description
                                          descr))
                                (pl `(:choices ',choices
                                      :argument-format ,(concat argument " %s")
                                      :argument-regexp ,(regexp-opt
                                                         choices)
                                      :show-help (lambda (&rest _)
                                                   (interactive)
                                                   (pnpm-menu--show-help
                                                    ,(string-trim
                                                      argument)
                                                    ,descr
                                                    ,short
                                                    ,specifier)))))
                           (push
                            (append (list (car choices))
                                    pl)
                            commands)))
                        (specifier
                         (setq argument (concat argument " "))
                         (let ((pl
                                (pcase specifier
                                  ((pred
                                    (string-match-p
                                     "<\\(path\\|dir\\|directory\\)>"))
                                   (list
                                    :prompt "Directory: "
                                    :class 'transient-option
                                    :reader (npmjs--compose
                                              (apply-partially
                                               #'format
                                               (replace-regexp-in-string
                                                "<[^>]+>"
                                                "%s"
                                                specifier))
                                              pnpm-menu-read-dir-relative)))
                                  (_ (list
                                      :prompt specifier
                                      :class 'transient-option
                                      :reader (npmjs--compose
                                                (apply-partially
                                                 #'format
                                                 (replace-regexp-in-string
                                                  "<[^>]+>"
                                                  "%s"
                                                  specifier))
                                                #'read-string))))))
                           (setq pl (plist-put pl
                                               :show-help
                                               (lambda (&rest _)
                                                 (interactive)
                                                 (pnpm-menu--show-help
                                                  (string-trim argument) descr
                                                  short
                                                  specifier))))
                           (push (append (list short-descr argument)
                                         pl)
                                 commands)))
                        (t
                         (push (list short-descr (or arg-list argument)
                                     :show-help (lambda (&rest _)
                                                  (interactive)
                                                  (pnpm-menu--show-help
                                                   (string-trim
                                                    argument)
                                                   descr
                                                   short
                                                   specifier)))
                               commands))))))
            (push (cons group-title (nreverse commands)) groups))))
      (setq groups (nreverse groups)))))

(defun pnpm-menu--eval-command-args (cmd output &optional used-keys key-prefix)
  "Evaluate command arguments from CMD and OUTPUT, generating structured results.

Argument CMD is the command to be evaluated.

Argument OUTPUT is the string output from the command.

Optional argument USED-KEYS is a list of keys that have already been used.

Optional argument KEY-PREFIX is a string prefix to be added to keys."
  (let ((groups (with-temp-buffer
                  (save-excursion
                    (insert output))
                  (pnpm-menu--parse-args)))
        (results)
        (used-keys (append used-keys '("q"))))
    (pcase-dolist (`(,group-name . ,cmds) groups)
      (when cmds
        (let ((items
               (npmjs-key-builder-generate-shortcuts
                cmds
                (pcase-lambda
                  (`(,alias ,cmd
                     . _))
                  (or alias cmd))
                (lambda (key def)
                  (push key used-keys)
                  (setq key (concat key-prefix key))
                  (push key used-keys)
                  (cond ((memq :argument-format def)
                         (append (list key)
                                 (npmjs-eval-infix
                                  (make-symbol (concat "pnpm-" cmd "-"
                                                       (car def)))
                                  def)))
                        ((equal (car def) "loglevel")
                         (list key
                               (car def)
                               'pnpm-menu-level-argument))
                        ((listp (cadr def))
                         (append (list (or (car (cadr def)) key)
                                       (car def)
                                       (cadr def))
                                 (seq-drop def 2)))
                        (t (append (list key) def))))
                used-keys)))
          (push (cons group-name items) results))))
    (nreverse results)))



(defun pnpm-menu--make-command-description (cmd descr)
  "Join CMD and DESCR with \": \" and return the resulting string.

Argument CMD is the command to be described.

Argument DESCR is the description of the command."
  (string-join (delq nil (list cmd descr)) ": "))



(defun pnpm-menu--parse-commands ()
  "Parse and organize PNPM commands and their descriptions into structured groups."
  (let ((groups)
        (usage)
        (used-keys '("q")))
    (let ((case-fold-search t))
      (while (progn (skip-chars-forward "\n")
                    (and
                     (not (looking-at pnpm-menu-argument-group-title-regex))
                     (zerop (forward-line 1)))))
      (setq usage (buffer-substring-no-properties (point-min) (point)))
      (while
          (progn
            (when (and (looking-at "\n")
                       (looking-back "\n" 0))
              (re-search-forward "[\n]" nil t 1))
            (looking-at pnpm-menu-argument-group-title-regex))
        (when-let ((group-title (match-string-no-properties 1)))
          (forward-line 1)
          (let ((commands)
                (case-fold-search nil))
            (while (looking-at pnpm-menu--command-regex)
              (let ((short (match-string-no-properties 3))
                    (long  (match-string-no-properties 5))
                    (comma (match-string-no-properties 4))
                    (descr-start (match-end 0))
                    (descr-end (line-end-position))
                    (descr-col)
                    (descr))
                (when (and short
                           (not comma))
                  (setq long (concat short " " long))
                  (setq short nil))
                (when (and short (= 1 (length short)))
                  (setq short (concat short short)))
                (when short
                  (push short used-keys))
                (goto-char descr-start)
                (setq descr-col (current-column))
                (while
                    (when (zerop (forward-line 1))
                      (progn (move-to-column descr-col)
                             (looking-back "[\n][\s]+" 0)))
                  (setq descr-end (line-end-position)))
                (setq descr (string-join
                             (split-string
                              (buffer-substring-no-properties descr-start
                                                              descr-end)
                              nil t)
                             " "))
                (goto-char (line-beginning-position))
                (push (list short long descr)
                      commands)))
            (push (cons group-title (nreverse commands)) groups))))
      (setq groups (nreverse groups))
      (cons usage
            groups))))

(defun pnpm-menu--add-commands-shortcuts (groups &optional used-keys)
  "Add keyboard shortcuts to command GROUPS, avoiding already used keys.

Argument GROUPS is a list of command groups, each containing a name and
commands.

Optional argument USED-KEYS is a list of keys that are already in use."
  (let ((used-keys (delete-dups (append (list "q" "un" "ii") used-keys)))
        (results))
    (pcase-dolist (`(,group-name . ,cmds) groups)
      (when cmds
        (let ((group-keys (delq nil
                                (mapcar #'car cmds)))
              (non-alised-cmds (seq-filter (npmjs--compose not car)
                                           cmds)))
          (setq used-keys (append used-keys group-keys))
          (npmjs-key-builder-generate-shortcuts
           non-alised-cmds
           (pcase-lambda
             (`(,_alias ,cmd
                . _))
             cmd)
           (lambda (key def)
             (push key used-keys)
             (setcar def key)
             def)
           used-keys)
          (push (cons group-name cmds) results))))
    (nreverse results)))

(defvar pnpm-menu-version nil)
(defvar pnpm-menu-cache (make-hash-table :test 'equal))
(defvar pnpm-menu-commands nil)
(defvar pnpm-menu-current-description nil)

(defun pnpm-menu--version ()
  "Return the version of pnpm by calling the \"pnpm -v\" command."
  (npmjs-nvm-with-current-node-version
   (npmjs-with-temp-buffer
    (let ((status (call-process "pnpm"  nil t nil "-v")))
      (when (zerop status)
        (string-trim (buffer-string)))))))

(defun pnpm-menu--setup ()
  "Initialize and cache PNPM commands and their descriptions."
  (setq pnpm-menu-version (pnpm-menu--version))
  (setq pnpm-menu-commands (unless pnpm-menu-inhibit-cache
                             (gethash pnpm-menu-version
                                      pnpm-menu-cache)))
  (unless pnpm-menu-commands
    (setq pnpm-menu-commands
          (npmjs-nvm-with-current-node-version
           (npmjs-with-temp-buffer
            (let ((status (call-process "pnpm"  nil t nil "help")))
              (when (zerop status)
                (goto-char (point-min))
                (pcase-let ((`(,usage . ,groups)
                             (pnpm-menu--parse-commands)))
                  (setq pnpm-menu-current-description usage)
                  (setq groups (pnpm-menu--add-commands-shortcuts groups))
                  (let ((results))
                    (pcase-dolist (`(,group-name . ,cmds) groups)
                      (let ((mapped-cmds
                             (mapcar
                              (pcase-lambda (`(,k ,cmd ,descr))
                                (let
                                    ((opt
                                      (list
                                       (string-join
                                        (delq nil
                                              (list cmd
                                                    descr))
                                        ": ")
                                       k))
                                     (extra-props
                                      (cdr
                                       (assoc-string
                                        cmd
                                        pnpm-menu-extra-command-props))))
                                  (pcase cmd
                                    ("add"
                                     (push 'pnpm-menu-add opt))
                                    ("run" (push 'pnpm-menu-run opt))
                                    ("--recursive"
                                     (push 'pnpm-menu-toggle-recursive opt))
                                    (_
                                     (let ((name
                                            (intern
                                             (concat "pnpm-menu-"
                                                     (replace-regexp-in-string
                                                      " " "-" cmd)
                                                     "-"
                                                     pnpm-menu-version)))
                                           (prefix-body
                                            (cdr
                                             (assoc-string
                                              cmd
                                              pnpm-menu-command-prefix-props))))
                                       (if prefix-body
                                           (eval
                                            `(pnpm-menu-define-command
                                              ,name
                                              ,cmd
                                              ,descr
                                              ,prefix-body)
                                            t)
                                         (eval `(pnpm-menu-define-command
                                                 ,name
                                                 ,cmd
                                                 ,descr)
                                               t))
                                       (push name opt)
                                       name)))
                                  (if extra-props
                                      (append (reverse opt) extra-props)
                                    (reverse opt))))
                              cmds)))
                        (when mapped-cmds
                          (push (cons group-name mapped-cmds) results))))
                    (nreverse results)))))))))
  (puthash pnpm-menu-version pnpm-menu-commands pnpm-menu-cache))

;;;###autoload (autoload 'pnpm-menu-dispatch "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu ()
  "Manage dependencies with pnpm."
  :show-help (lambda (&rest _)
               (pnpm-menu--browse-url pnpm-menu-pnpm-help-url))
  [:description (lambda ()
                  (concat pnpm-menu-current-description
                          "\n"
                          (car (nth 0 pnpm-menu-commands))))
   :class transient-column
   :if (lambda ()
         (cdr (nth 0 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 0 pnpm-menu-commands))))]
  [:description (lambda ()
                  (car (nth 1 pnpm-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 1 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 1 pnpm-menu-commands))))]
  [:description (lambda ()
                  (car (nth 2 pnpm-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 2 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 2 pnpm-menu-commands))))]
  [:description (lambda ()
                  (car (nth 3 pnpm-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 3 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 3 pnpm-menu-commands))))]
  [:description (lambda ()
                  (car (nth 4 pnpm-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 4 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 4 pnpm-menu-commands))))]
  [:description (lambda ()
                  (car (nth 5 pnpm-menu-commands)))
   :class transient-column
   :if (lambda ()
         (cdr (nth 5 pnpm-menu-commands)))
   :setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       (oref transient--prefix command))
      (cdr (nth 5 pnpm-menu-commands))))]
  (interactive)
  (pnpm-menu--setup)
  (transient-setup #'pnpm-menu))



(provide 'pnpm-menu)
;;; pnpm-menu.el ends here