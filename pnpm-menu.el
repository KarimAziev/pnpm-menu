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

(defmacro pnpm-menu-define-command (name cmd description &rest body)
  "Define a transient command with NAME, CMD, DESCRIPTION, and BODY.

Argument NAME is the symbol for the command being defined.

Argument CMD is the string representing the pnpm command to run.

Argument DESCRIPTION is the string describing the command.

Remaining arguments BODY are the forms to be included in the command definition."
  `(progn
     (let ((sym))
      (transient-define-prefix ,name ()
        ,(concat (or description cmd) ".")
        :value (lambda ()
                 (unless (npmjs-get-project-root)
                  (list "--global")))
        :show-help (lambda (&rest _)
                     (pnpm-menu--show-help ,cmd
                      (npmjs-nvm-with-current-node-version
                       (npmjs-parse-help-with-output
                           (shell-command-to-string (format "pnpm help %s" ,cmd))
                         (goto-char (point-min))
                         (buffer-substring-no-properties
                          (point-min)
                          (if (re-search-forward "^[\s]*Options:" nil t 1)
                              (match-beginning 0)
                            (point-max)))))))
        [:description (lambda ()
                        (concat ,description "\n"
                         (car (nth 0 sym))))
         :class transient-column
         :setup-children
         (lambda (&rest _argsn)
           (mapcar
            (apply-partially #'transient-parse-suffix
             (oref transient--prefix command))
            (cdr (nth 0 sym))))]
        [:description (lambda ()
                        (car (nth 1 sym)))
         :class transient-column
         :setup-children
         (lambda (&rest _argsn)
           (mapcar
            (apply-partially #'transient-parse-suffix
             (oref transient--prefix command))
            (cdr (nth 1 sym))))]
        [:description (lambda ()
                        (car (nth 2 sym)))
         :if (lambda ()
               (car (nth 2 sym)))
         :class transient-column
         :setup-children
         (lambda (&rest _argsn)
           (mapcar
            (apply-partially #'transient-parse-suffix
             (oref transient--prefix command))
            (cdr (nth 2 sym))))]
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
         (setq sym (pnpm-menu--eval-command-args
                    ,cmd)))
        (transient-setup this-command)))
     (put ',name 'pnpm-command
      ,(concat "pnpm " cmd))))


;; (defvar pnpm-menu-installed-package-multi-reader (npmjs-make-reader nil
;;                                                                     #'pnpm-menu--pkg-get-dependencies-table
;;                                                                     t))

(pnpm-menu-define-command pnpm-menu-import "import" "Generates a pnpm-lock.yaml from an npm package-lock.json (or npm-shrinkwrap.json) file")
(pnpm-menu-define-command pnpm-menu-install "install" "Install all dependencies for a project")
(pnpm-menu-define-command pnpm-menu-install-test "install-test" "Runs a pnpm install followed immediately by a pnpm test")
(pnpm-menu-define-command pnpm-menu-link "link" "Connect the local project to another one")
(pnpm-menu-define-command pnpm-menu-prune "prune" "Removes extraneous packages")
(pnpm-menu-define-command pnpm-menu-rebuild "rebuild" "Rebuild a package"
                          [:class transient-column
                           :setup-children
                           (lambda (&rest _argsn)
                             (mapcar
                              (apply-partially #'transient-parse-suffix
                                               (oref transient--prefix command))
                              (list '("-g" "global" "--global")
                                    `("." "package" "<pkg>"
                                      :prompt "Package "
                                      :class transient-option
                                      :multi-value t
                                      :reader
                                      ,(npmjs-make-reader nil
                                        #'pnpm-menu--pkg-get-dependencies-table
                                        t)))))])
(pnpm-menu-define-command pnpm-menu-remove "remove"
                          "Removes packages from node_modules and from the project's package.json"
                          [:class transient-column
                           :setup-children
                           (lambda (&rest _argsn)
                             (mapcar
                              (apply-partially #'transient-parse-suffix
                                               (oref transient--prefix command))
                              (list '("-g" "global" "--global")
                                    (append
                                     `("." "package" "<pkg>"
                                       :prompt "Package "
                                       :class transient-option
                                       :multi-value t
                                       :reader
                                       ,(npmjs-make-reader nil
                                         #'pnpm-menu--pkg-get-dependencies-table
                                         t))))))])
(pnpm-menu-define-command pnpm-menu-unlink "unlink"
                          "Unlinks a package. Like yarn unlink but pnpm re-installs the dependency after removing the external link"
                          [:class transient-column
                           :setup-children
                           (lambda (&rest _argsn)
                             (mapcar
                              (apply-partially #'transient-parse-suffix
                                               (oref transient--prefix command))
                              (list '("-g" "global" "--global")
                                    `("." "package" "<pkg>"
                                      :prompt "Package "
                                      :class transient-option
                                      :reader
                                      ,(npmjs-make-reader nil
                                        #'pnpm-menu--pkg-get-dependencies-table)))))])
(pnpm-menu-define-command pnpm-menu-update "update"
                          "Updates packages to their latest version based on the specified range"
                          [:class transient-column
                           :setup-children
                           (lambda (&rest _argsn)
                             (mapcar
                              (apply-partially #'transient-parse-suffix
                                               (oref transient--prefix command))
                              (list
                               `("." "package" "<pkg>"
                                 :prompt "Package "
                                 :class transient-option
                                 :reader
                                 ,(npmjs-make-reader nil
                                   #'pnpm-menu--pkg-get-dependencies-table)))))])

(pnpm-menu-define-command pnpm-menu-audit "audit" "Checks for known security issues with the installed packages")

(pnpm-menu-define-command pnpm-menu-licenses "licenses" "Check licenses in consumed packages")
(pnpm-menu-define-command pnpm-menu-list "list" "Print all the versions of packages that are installed, as well as their dependencies, in a tree-structure")
(pnpm-menu-define-command pnpm-menu-outdated "outdated" "Check for outdated packages")
(pnpm-menu-define-command pnpm-menu-exec "exec" "Executes a shell command in scope of a project")
(pnpm-menu-define-command pnpm-menu-start "start" "Runs an arbitrary command specified in the package's \"start\" property of its \"scripts\" object")
(pnpm-menu-define-command pnpm-menu-test "test" "Runs a package's \"test\" script, if one was provided")
(pnpm-menu-define-command pnpm-menu-cat-file "cat-file" "Prints the contents of a file based on the hash value stored in the index file")
(pnpm-menu-define-command pnpm-menu-cat-index "cat-index" "Prints the index file of a specific package from the store")
(pnpm-menu-define-command pnpm-menu-find-hash "find-hash" "Experimental! Lists the packages that include the file with the specified hash.")
(pnpm-menu-define-command pnpm-menu-pack "pack" "")
(pnpm-menu-define-command pnpm-menu-publish "publish" "Publishes a package to the registry")
(pnpm-menu-define-command pnpm-menu-root "root" "")
(pnpm-menu-define-command pnpm-menu-store-add "store add"
                          "Adds new packages to the pnpm store directly. Does not modify any projects or files outside the store")
(pnpm-menu-define-command pnpm-menu-store-path "store path" "Prints the path to the active store directory")
(pnpm-menu-define-command pnpm-menu-store-prune "store prune" "Removes unreferenced (extraneous, orphan) packages from the store")
(pnpm-menu-define-command pnpm-menu-store-status "store status" "Checks for modified packages in the store")

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


(defvar pnpm-menu-add-groupped-args nil)


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

;;;###autoload (autoload 'pnpm-menu-add "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu-add ()
  "Install Node.js packages using npm."
  :value (lambda ()
           (unless (npmjs-get-project-root)
             (list "--global")))
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
                                      "add")))
  (transient-setup this-command))

(put 'pnpm-menu-add 'pnpm-command
     "pnpm add")

(defvar pnpm-menu-run-args nil)

;;;###autoload (autoload 'pnpm-menu-run "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu-run ()
  "Runs a defined package script."
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
                                       (mapcar #'car npmjs-current-scripts)))
   (transient-setup this-command)))

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
  (let ((group-title-regex "^\\([a-z][^\n]+\\):$")
        (command-regex
         "^[ ]*\\(\\(-[a-z]\\),[ ]\\)?\\(--\\(\\[?\\([a-z_|-]+-\\)\\]\\)?[a-z_-]+\\)\\([ =]\\([^ ]+\\)\\)?")
        (groups)
        (used-keys (list "q")))
    (let ((case-fold-search t))
      (while (progn (skip-chars-forward "\n")
                    (and
                     (not (looking-at group-title-regex))
                     (zerop (forward-line 1)))))
      (while (progn (skip-chars-forward "\n")
                    (looking-at group-title-regex))
        (when-let ((group-title (match-string-no-properties 1)))
          (forward-line 1)
          (let ((commands))
            (while (looking-at command-regex)
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
                           (setq pl (plist-put pl :show-help (lambda (&rest _)
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

(defun pnpm-menu--eval-command-args (cmd &optional used-keys key-prefix)
  "Evaluate command arguments and return structured groups and commands.

Argument CMD is the command for which help is to be evaluated.

Optional argument USED-KEYS is a list of keys that have already been used.

Optional argument KEY-PREFIX is a string prefix to be added to keys."
  (let ((groups (npmjs-nvm-with-current-node-version
                 (npmjs-parse-help-with-output
                     (shell-command-to-string
                      (concat "pnpm help " cmd))
                   (pnpm-menu--parse-args))))
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

;;;###autoload (autoload 'pnpm-menu "pnpm-menu" nil t)
(transient-define-prefix pnpm-menu ()
  "Manage dependencies, review them, run scripts, and handle the pnpm store."
  ["Manage your dependencies"
   ("ad"
    "add: Installs a package and any packages that it depends on. By default, any new package is installed as a prod dependency"
    pnpm-menu-add)
   ("im"
    "import: Generates a pnpm-lock.yaml from an npm package-lock.json (or npm-shrinkwrap.json) file"
    pnpm-menu-import)
   ("ii" "install: Install all dependencies for a project" pnpm-menu-install)
   ("it" "install-test: Runs a pnpm install followed immediately by a pnpm test"
    pnpm-menu-install-test)
   ("ln" "link: Connect the local project to another one" pnpm-menu-link)
   ("pr" "prune: Removes extraneous packages" pnpm-menu-prune)
   ("rb" "rebuild: Rebuild a package" pnpm-menu-rebuild)
   ("rm"
    "remove: Removes packages from node_modules and from the project's package.json"
    pnpm-menu-remove)
   ("un"
    "unlink: Unlinks a package. Like yarn unlink but pnpm re-installs the dependency after removing the external link"
    pnpm-menu-unlink)
   ("up"
    "update: Updates packages to their latest version based on the specified range"
    pnpm-menu-update)]
  ["Review your dependencies"
   ("au" "audit: Checks for known security issues with the installed packages"
    pnpm-menu-audit)
   ("lc" "licenses: Check licenses in consumed packages" pnpm-menu-licenses)
   ("ls"
    "list: Print all the versions of packages that are installed, as well as their dependencies, in a tree-structure"
    pnpm-menu-list)
   ("ou" "outdated: Check for outdated packages" pnpm-menu-outdated)]
  ["Run your scripts"
   ("ex" "exec: Executes a shell command in scope of a project" pnpm-menu-exec)
   ("ru" "run: Runs a defined package script" pnpm-menu-run)
   ("st"
    "start: Runs an arbitrary command specified in the package's \"start\" property of its \"scripts\" object"
    pnpm-menu-start :inapt-if-not (lambda ()
                                    (or
                                     (npmjs-get-package-json-script 'start)
                                     (when-let ((proj (npmjs-get-project-root)))
                                       (file-exists-p (expand-file-name
                                                       "server.js" proj))))))
   ("te" "test: Runs a package's \"test\" script, if one was provided"
    pnpm-menu-test
    :inapt-if-not (lambda ()
                    (npmjs-get-package-json-script 'test)))]
  ["Other"
   ("ot"
    "cat-file: Prints the contents of a file based on the hash value stored in the index file"
    pnpm-menu-cat-file)
   ("ci" "cat-index: Prints the index file of a specific package from the store"
    pnpm-menu-cat-index)
   ("fh"
    "find-hash: Experimental! Lists the packages that include the file with the specified hash."
    pnpm-menu-find-hash)
   ("pa" "pack" pnpm-menu-pack)
   ("pu" "publish: Publishes a package to the registry" pnpm-menu-publish)
   ("ro" "root" pnpm-menu-root)]
  ["Manage your store"
   ("sa"
    "store add: Adds new packages to the pnpm store directly. Does not modify any projects or files outside the store"
    pnpm-menu-store-add)
   ("sp" "store path: Prints the path to the active store directory"
    pnpm-menu-store-path)
   ("sn"
    "store prune: Removes unreferenced (extraneous, orphan) packages from the store"
    pnpm-menu-store-prune)
   ("ss" "store status: Checks for modified packages in the store"
    pnpm-menu-store-status)])



(provide 'pnpm-menu)
;;; pnpm-menu.el ends here