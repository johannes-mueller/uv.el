;;; uv.el --- An interface for the uv python package manager -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/uv.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1") (tomlparse "1.0.0"))

;;; Commentary:

;; The python package manager uv `https://docs.astral.sh/uv/' is becoming increasingly
;; popular as it cleans up the jungle of different tools that were necessary to manage a
;; python project with all its dependencies.  Moreover it is refreshingly fast.

;; This package attempts to be a user friendly transient based user interface to a
;; subset of uv functionality that is useful when developing python projects.  It does
;; not – at least not from the beginning – cover all the commands with all their
;; switches.  Many of them are only needed when building docker containers or in CI/CD
;; pipelines.  This package aims primarily to support you as a developer to advance your
;; project and its dependencies and tools.  So it focuses on your tasks as a developer,
;; not so much as a DevOps.

;;; Code:

(require 'transient)
(require 'ansi-color)
(require 'tomlparse)
(require 'project)
(require 'subr-x)
(require 'python)

(defclass uv--transient-multiswitch (transient-argument)
  ((scope :initarg :scope))
  "A `transient-argument' to select from a list of mutually non exclusive items.")

(defconst uv-commands (mapcar #'symbol-name
  '(run
    init
    add
    remove
    version
    sync
    lock
    export
    tree
    tool
    python
    pip
    venv
    build
    publish
    cache
    self
    generate-shell-completion)))

(defun uv-show-command-help (command &rest subcommands)
  "Show the help text for uv COMMAND and maybe SUBCOMMANDS."
  (interactive (list (completing-read "UV command: " uv-commands nil t)))
  (let ((buf-name "*UV Help*"))
    (with-help-window buf-name
      (apply #'call-process "uv" nil buf-name nil "help" command subcommands))))

(defun uv-show-uv-help ()
  "Show help for uv."
  (let ((buf-name "*UV Help*"))
    (with-help-window buf-name
      (call-process "uv" nil buf-name nil "help"))))

(defvar uv--run-history (make-hash-table :test 'equal)
  "A hash-table to store the history of uv runs for each project.")

(defvar uv--history nil
  "An internal variable that serves as a symbol for minibuffer history.")

(defvar uv--last-run-args (make-hash-table :test 'equal))

(defvar uv--tool-run-history (make-hash-table :test 'equal)
  "A hash-table to store the history of uv tool runs for each project.")

(defvar uv--last-tool-run-args (make-hash-table :test 'equal))

(defvar uv--projects-last-venv nil)

(defvar uv--after-run-hook nil
  "An internal hook for uv command functions.

This is only for uv command functions to execute something after the uv
command has been finished.  Please don't set it manually outside uv
command functions.")

(defvar uv--run-fail-hook nil)

(defvar uv--special-hook nil
  "Variable to schedule venv creation after uv init finished.")


(defun uv--schedule-special-hook (hook-function)
  "Schedule execuion of HOOK-FUNCTION after uv process."
  (setq uv--special-hook hook-function))

(defun uv--perform-special-hook ()
  "Perform the venv creation."
  (when uv--special-hook
    (funcall uv--special-hook))
  (setq uv--special-hook nil))

(defun uv--cancel-special-hook ()
  "Cancel the special hook."
  (setq uv--special-hook nil))

(defun uv-init-cmd (directory &optional args no-venv)
  "Perform the `uv init' command in DIRECTORY with ARGS.

Only to be used directly when the default arguments of `uv init' are
suitable.  Use `uv-init' instead.

A venv is created unless NO-VENV is non-nil."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create project in: " default-directory))))
         (no-venv (cl-find "no-venv" (transient-args transient-current-command) :test 'equal))
         (args (cl-remove "no-venv" (transient-args transient-current-command) :test 'equal)))
     (append (list directory) (list args) (list no-venv))))
  (let ((args (append (uv--quote-string-transient-args args) (list directory))))
    (uv--schedule-special-hook
     (lambda ()
       (unless no-venv
         (let ((default-directory directory))
           (ignore (process-lines "uv" "venv"))))
       (dired directory)))
    (uv--do-command (concat "uv init " (string-join args " ")))))

 ;;;###autoload (autoload 'uv-init "uv" nil t)
(transient-define-prefix uv-init ()
  "Initialize python project using `uv init'"
  :show-help (lambda (_) (uv-show-command-help "init"))
  [["Options"
    ("n" "Name" "--name=" :prompt "Project name: ")
    ("d" "Description" "--description=" :prompt "Project description: ")
    ("l" "lib – create a lib rather than an app" "--lib")
    ("v" "do not initialize a git repository" "--vcs none")
    ("p" "setup to build a python package" "--package")
    ("a" "automatically determine author info" "--author-from=auto" )
    (uv--select-build-backend)
    ("r" "No README.md" "--no-readme")
    ("V" "Do not create a `.python-version` file for the project." "--no-pin-python")
    ("w" "Avoid discovering a workspace and create a standalone project." "--no-workspace")
    ("-nv" "Do not create a venv" "no-venv")]
   ["Python options"
    (uv--select-python-version)]]
  ["Init"
   ("RET" "Ask for target directory" uv-init-cmd)])

(defconst uv--build-backends '("hatch" "flit" "pdm" "setuptools" "maturin" "scikit")
  "The available python build-backends.")

(transient-define-argument uv--select-build-backend ()
  "Selector for the build-backend to be used in the project."
  :key "b"
  :description "Initialize the build-backend for the project"
  :class transient-switches
  :argument-format "--build-backend %s"
  :argument-regexp "--build-backend \\(.*\\)"
  :choices uv--build-backends)

(transient-define-argument uv--select-python-version ()
  "Selector for the python version"
  :key "-p"
  :argument "--python "
  :description "Python version"
  :class transient-option
  :reader (lambda (prompt initial _history)
            (let ((completion-styles '(basic)))
              (completing-read prompt (uv--sorted-python-version-completions) initial t))))

(defun uv--sorted-python-version-completions ()
  "Determine and sort the available python versions."
  (let ((python-versions (uv--available-python-versions)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (display-sort-function . ,#'identity))
        (complete-with-action action python-versions string pred)))))

(defun uv--available-python-versions ()
  "Determine the available python versions."
  (sort (delete-dups
         (mapcar (lambda (python) (gethash "version" python))
                 (json-parse-string
                  (car (process-lines "uv" "python" "list" "--output-format=json")))))
        #'uv--python-version>))

(defun uv--python-version> (version-1 version-2)
  "Compare python VERSION-1 with VERSION-2 to sort latest release.

Prereleases are put lastlast."
  (let ((prerelease-1 (uv--prerelease version-1))
        (prerelease-2 (uv--prerelease version-2)))
    (cond ((and (not prerelease-1) (not prerelease-2))
           (uv--python-version-release> (uv--parse-python-version version-1)
                                (uv--parse-python-version version-2)))
          ((and (not prerelease-1) prerelease-2) t)
          ((and prerelease-1 (not prerelease-2)) nil)
          (t (let ((kind-1 (car prerelease-1))
                   (kind-2 (car prerelease-2))
                   (num-1 (cdr prerelease-1))
                   (num-2 (cdr prerelease-2)))
               (cond ((equal kind-1 kind-2) (string< num-1 num-2))
                     (t (string> kind-1 kind-2))))))))

(defun uv--parse-python-version (version)
  "Parse a python VERSION string and return a list `(major minor patch)'."
  (save-match-data
    (when (string-match "\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)\\.\\([[:digit:]]+\\)" version)
      (list (string-to-number (match-string 1 version))
            (string-to-number (match-string 2 version))
            (string-to-number (match-string 2 version))))))

(defun uv--python-version-release> (version-1 version-2)
  "Compare parsed python versions VERSION-1 and VERSION-2."
  (cond
   ((not (and version-1 version-2)) nil)
   ((eq (car version-1) (car version-2)) (uv--python-version-release> (cdr version-1) (cdr version-2)))
   (t (> (car version-1) (car version-2)))))

(defun uv--prerelease (version)
  "Determine whether VERSION is a python prerelease version."
  (save-match-data
    (when (string-match "\\(a\\|b\\|rc\\)\\(.+\\)" version)
      (cons (match-string 1 version) (match-string 2 version)))))

(defun uv-venv-cmd (&optional args)
  "Perform the `uv venv' command with ARGS.

Only to be used directly when the default arguments of `uv venv' are
suitable.  Use `uv-venv' instead."
  (interactive (list (transient-args transient-current-command)))
  (uv--do-command (string-trim-right (concat "uv venv " (string-join args " ")))))

 ;;;###autoload (autoload 'uv-venv "uv" nil t)
(transient-define-prefix uv-venv ()
  "Create a virtual environment."
  :show-help (lambda (_) (uv-show-command-help "venv"))
  ["Options"
   (uv--select-python-version)
   ("s" "Seed environment" "--seed")]
  ["venv"
  ("RET" "Create the venv" uv-venv-cmd)])

(defun uv--read-project-data ()
  "Read the `pyproject.toml' file of the project's root if it exists."
  (when-let* ((root (uv--project-root))
              (pyproject-file (concat (file-name-as-directory root) "pyproject.toml")))
    (when (file-exists-p pyproject-file)
      (tomlparse-file pyproject-file))))

(defun uv--known-dependency-groups ()
  "Determine the projects known dependency-groups from pyproject.toml."
  (when-let* ((pyproject-data (uv--read-project-data))
              (ht (gethash "dependency-groups" pyproject-data)))
    (hash-table-keys ht)))

(defun uv--known-extras ()
  "Determine the projects known extras from pyproject.toml."
  (when-let* ((pyproject-data (uv--read-project-data))
              (project-entry (gethash "project" pyproject-data))
              (ht (gethash "optional-dependencies" project-entry)))
      (hash-table-keys ht)))

(defun uv--known-dependencies ()
  "Determine the projects known extras from pyproject.toml."
  (when-let* ((pyproject-data (uv--read-project-data))
              (project-entry (gethash "project" pyproject-data)))
    (append
     (pcase (uv--group-arg (transient-args transient-current-command))
       (`(group . ,group) (gethash group (gethash "dependency-groups" pyproject-data)))
       (`(extra . ,extra) (gethash extra (gethash "optional-dependencies" project-entry)))
       (_ (gethash "dependencies" project-entry)))
     nil)))


 ;;;###autoload (autoload 'uv-add "uv" nil t)
(transient-define-prefix uv-add ()
  "Add dependencies to the project"
  :show-help (lambda (_) (uv-show-command-help "add"))
  ["Options"
   ("d" "Into development dependency group" "--dev")
   ("g" "Into a specified dependency group" "--group "
    :prompt "Choose group: "
    :class transient-option
    :reader (lambda (prompt initial _history)
              (completing-read prompt (uv--known-dependency-groups) initial nil)))
   ("o" "To a specified extra" "--optional "
    :prompt "Choose extra: "
    :class transient-option
    :reader (lambda (prompt initial _history)
              (completing-read prompt (uv--known-extras) initial nil)))
   ("e" "Specify extras (comma separated)" "--extra=")
   ("a" "Sync into active virtual environment." "--active")
   ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
   ("f" "Sync without updating `uv.lock'" "--frozen")]
  ["add"
   ("RET" "Add dependency" uv-add-cmd)])

 ;;;###autoload (autoload 'uv-remove "uv" nil t)
(transient-define-prefix uv-remove ()
  "Remove dependencies from the project"
  :show-help (lambda (_) (uv-show-command-help "remove"))
  ["Options"
   ("d" "From development dependency group" "--dev")
   ("g" "From a specified depencency group" "--group "
    :prompt "Choose group: "
    :class transient-option
    :reader (lambda (prompt initial _history)
              (completing-read prompt (uv--known-dependency-groups) initial nil)))
   ("o" "From a specified extra" "--optional "
    :prompt "Choose extra: "
    :class transient-option
    :reader (lambda (prompt initial _history)
              (completing-read prompt (uv--known-extras) initial nil)))
   ("a" "Sync into active virtual environment." "--active")
   ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
   ("f" "Sync without updating `uv.lock'" "--frozen")]
  ["remove"
   ("RET" "Remove dependency" uv-remove-cmd)])

(defun uv-add-cmd (package &optional args)
  "Perform the `uv add' command to add PACKAGE with ARGS.

Only to be used directly when the default arguments of `uv add' are
suitable.  Use `uv-add' instead."
  (interactive
   (let ((package (read-string "Package name: ")))
     (append (list package) (when transient-current-command (list (transient-args transient-current-command))))))
  (let ((args (when args (concat (string-join (uv--spread-comma-separated-args args "--extra=") " ") " "))))
    (uv--do-command (concat "uv add " args  package))))

(defun uv-remove-cmd (package &optional args)
  "Perform the `uv remove' command to remove PACKAGE with ARGS.

Only to be used directly when the default arguments of `uv remove' are
suitable.  Use `uv-remove' instead."
  (interactive
   (let ((package (completing-read "Remove package: " (uv--known-dependencies) nil t)))
     (append (list package) (when transient-current-command (list (transient-args transient-current-command))))))
    (uv--do-command (format "uv remove %s \"%s\"" (string-join args " ") package)))

(defun uv-sync-cmd (&optional args)
  "Perform the `uv sync' command with ARGS.

Only to be used directly when the default arguments of `uv sync' are
suitable.  Use `uv-sync' instead."
  (interactive
   (when transient-current-command
     (list (transient-args transient-current-command))))
  (uv--do-command (concat "uv sync " (string-join args " ")))
  (uv-activate-venv))

;;;###autoload
(defun uv-sync-background ()
  "Run `uv sync' silently in background."
  (interactive)
  (let ((uv--run-fail-hook (lambda (buf _msg) (display-buffer buf))))
    (cl-flet ((display-buffer #'ignore))
      (save-window-excursion (uv--do-command "uv sync")))))

(defun uv--spread-comma-separated-args (args argument)
  "Spread comma separated list after ARGUMENT in ARGS into separated arguments.

Example:
--extra=foo, bar => --extra=foo --extra=bar"
  (mapcar (lambda (arg)
            (if (string-prefix-p argument arg)
                (let ((items (string-split (substring arg (length argument)) "[, \t]+")))
                  (concat argument (string-join items (concat " " argument))))
              arg))
          args))

(defun uv--find-multiswitch-suffix (argument)
  "Find the `uv--transient-multiswitch' with the argument ARGUMENT."
  (cl-find-if (lambda (obj)
                (and (eq (eieio-object-class obj) 'uv--transient-multiswitch)
                     (equal (oref obj argument) argument)))
              transient--suffixes))

(defun uv--all-ticks-toggle (argument)
  "Select or deselect all options of the multiswitch ARGUMENT."
  (let* ((suffix (uv--find-multiswitch-suffix argument))
         (candidates (funcall (oref suffix choices))))
    (if (equal (oref suffix value) candidates)
        (oset suffix value nil)
      (oset suffix value candidates))))

(transient-define-argument uv--extra-multiswitch ()
  "Selector for package extras."
  :key "e"
  :argument "--extra "
  :description "extras to be used for installation"
  :multi-value t
  :class 'uv--transient-multiswitch
  :prompt "extra: "
  :choices 'uv--known-extras)

(transient-define-suffix uv--select-or-deselect-all-extras ()
  :transient 'transient--do-stay
  (interactive)
  (uv--all-ticks-toggle "--extra "))

(transient-define-argument uv--group-multiswitch ()
  "Selector for dependency groups."
  :key "g"
  :argument "--group "
  :description "dependency-groups to be used for installation"
  :multi-value t
  :class 'uv--transient-multiswitch
  :prompt "group: "
  :choices 'uv--known-dependency-groups)

(transient-define-suffix uv--select-or-deselect-all-groups ()
  :transient 'transient--do-stay
  (interactive)
  (uv--all-ticks-toggle "--group "))

(transient-define-suffix uv--toggle-dev-group ()
  :transient 'transient--do-stay
  (interactive)
  (let* ((suffix (uv--find-multiswitch-suffix "--group "))
         (selection (oref suffix value))
         (new-selection
          (if (member "dev" selection)
              (delete "dev" selection)
            (push "dev" selection))))
    (oset suffix value new-selection)))

(defmacro uv--transient-define-group (name &rest groups)
  "Temporary hack."
  (declare (indent defun))
  `(if (macrop 'transient-define-group)
       (transient-define-group ,name ,@groups)
     (defconst ,name ,@groups)))

(uv--transient-define-group uv--dependency-options
  ["Dependency options"
   (uv--extra-multiswitch)
   ("E" "(de)select all extras" uv--select-or-deselect-all-extras)
   (uv--group-multiswitch)
   ("G" "(de)select all groups" uv--select-or-deselect-all-groups)
   ("d" "with dev dependency group" uv--toggle-dev-group)
   ])

(uv--transient-define-group uv--cache-options
  ["Cache options"
   ("nc" "Do not use the cache" "--no-cache")
   ("r" "Refresh all cached data" "--refresh")
   ("R" "Refresh given package" "--refresh-package "
    :prompt "Refresh package: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (read-string prompt initial history)))])

(uv--transient-define-group uv--resolver-options
  ["Resolver options"
   ("U" "Allow package upgrades" "--upgrade")
   ("P" "Allow upgrade for given package" "--package-upgrade "
    :prompt "Allow upgrade for package: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (read-string prompt initial history)))
   ("s" "Resolution strategy" "--resolution "
    :class transient-switches
    :argument-format "--resolution %s"
    :argument-regexp "--resolution \\(.*\\)"
    :choices ("highest" "lowest" "lowest-direct"))])

 ;;;###autoload (autoload 'uv-sync "uv" nil t)
(transient-define-prefix uv-sync ()
  "Update the project's environment"
  :show-help (lambda (_) (uv-show-command-help "sync"))
  [uv--dependency-options
   ["Sync options"
    ("ne" "Install editable dependencies non-editable." "--non-editable")
    ("i" "Do not remove extraneous packages." "--inexact")
    ("a" "Sync into active virtual environment." "--active")
    ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
    ("f" "Sync without updating `uv.lock'" "--frozen")]
   uv--cache-options
   uv--resolver-options]
  ["sync"
   ("RET" "Run uv sync" uv-sync-cmd)])

(defun uv--run-candidates ()
  "Determine candidate commands for `uv run'."
  (let ((default-directory (uv--project-root)))
    (append (file-expand-wildcards "*.py")
            (string-split
             (shell-command-to-string
              (uv--devcontainer-advise-command "uv run 2> /dev/null | sed -n 's/^- //p'"))))))

(defun uv--project-run-command-history ()
  "Retrieve the run command history of the current project."
  (gethash (project-current) uv--run-history))

(defun uv--add-run-command-to-history (cmd)
  "Add the command CMD to the run command history of the current project."
  (let ((history (remove cmd (gethash (project-current) uv--run-history))))
    (puthash (project-current) (push cmd history) uv--run-history)))

(defun uv--project-last-run-args ()
  "Retrieve the argument list of the current project's last `uv run' command."
  (gethash (project-current) uv--last-run-args))

(defun uv--put-project-last-run-args (args)
  "Remember ARGS as the argument list of the current project's last `uv run'."
  (puthash (project-current) args uv--last-run-args))

(defun uv-run-cmd (command &optional args)
  "Perform the `uv run' command to run COMMAND with ARGS.

Only to be used directly when the default arguments of `uv run' are
suitable.  Use `uv-run' instead."
  (interactive
   (let* ((uv--history (uv--project-run-command-history))
          (command (completing-read "Command: " (uv--run-candidates) nil nil nil '(uv--history . 0)))
          (args (or (and transient-current-command (transient-args transient-current-command)) '())))
     (append (list command) (list args))))
  (let ((terminal (seq-position args "terminal"))
        (switches (string-join (remove "terminal" args) " ")))
    (uv--do-command-maybe-terminal (format "uv run %s -- %s" switches command) terminal)
    (uv--put-project-last-run-args args)
    (uv--add-run-command-to-history command)))

;;;###autoload
(defun uv-repeat-run ()
  "Repeat the last `uv run' command with its arguments."
  (interactive)
  (if-let* ((last-cmd (car (gethash (project-current) uv--run-history))))
      (uv-run-cmd last-cmd (uv--project-last-run-args))
    (message "Nothing to repeat")))

 ;;;###autoload (autoload 'uv-run "uv" nil t)
(transient-define-prefix uv-run ()
  "Run a command or script"
  :show-help (lambda (_) (uv-show-command-help "run"))
  :value #'uv--project-last-run-args
  [uv--dependency-options
   ["Run options"
    ("m" "Run as a module" "--module")
    ("ne" "Install editable dependencys non-editable." "--non-editable")
    ("x" "Remove extraneous packages" "--exact")
    ("i" "Run in an isolated virtual environment" "--isolated")
    ("a" "Run in the active virtual environment." "--active")
    ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
    ("f" "Sync without updating `uv.lock'" "--frozen")
    ("ns" "Do not sync the virtual environement" "--no-sync")
    ("T" "Interactive: run in an ansi-term" "terminal")]]
  ["run"
   ("RET" "Run" uv-run-cmd)])

(defun uv--project-tool-run-command-history ()
  "Retrieve the tool run command history of the current project."
  (gethash (project-current) uv--tool-run-history))

(defun uv--add-tool-run-command-to-history (cmd)
  "Add the command CMD to the tool run command history of the current project."
  (let ((history (remove cmd (gethash (project-current) uv--tool-run-history))))
    (puthash (project-current) (push cmd history) uv--tool-run-history)))

(defun uv--project-last-tool-run-args ()
  "Retrieve the argument list of the current project's last `uv tool run' command."
  (gethash (project-current) uv--last-tool-run-args))

(defun uv--put-project-last-tool-run-args (args)
  "Remember ARGS as the argument list of the current project's last `uv tool run'."
  (puthash (project-current) args uv--last-tool-run-args))

(defun uv-tool-run-cmd (tool &optional args)
  "Perform the `uv tool' TOOL to run COMMAND with ARGS.

Only to be used directly when the default arguments of `uv sync' are
suitable.  Use `uv-sync' instead."
  (interactive
   (let* ((uv--history (uv--project-tool-run-command-history))
          (tool (read-string "Run tool: " (car uv--history) '(uv--history . 1)))
          (args (or (and transient-current-command (transient-args transient-current-command)) '())))
     (append (list tool) (list args))))
   (let ((terminal (seq-position args "terminal"))
         (switches (string-join (remove "terminal" args) " ")))
     (uv--do-command-maybe-terminal (format "uv tool run %s %s" switches tool) terminal)
     (uv--put-project-last-tool-run-args args)
     (uv--add-tool-run-command-to-history tool)))

;;;###autoload
(defun uv-repeat-tool-run ()
  "Repeat the last `uv tool run' command with its arguments."
  (interactive)
  (if-let* ((last-cmd (car (gethash (project-current) uv--tool-run-history))))
      (uv-tool-run-cmd last-cmd (uv--project-last-tool-run-args))
    (message "Nothing to repeat")))

;;;###autoload (autoload 'uv-tool-run "uv" nil t)
(transient-define-prefix uv-tool-run ()
  "Run a tool by `uv tool run'"
  :show-help (lambda (_) (uv-show-command-help "tool" "run"))
  :value #'uv--project-last-tool-run-args
  [["Options"
    ("f" "Use a the given package to provide the command" "--from "
     :prompt "From package: "
     :class transient-option
     :reader (lambda (prompt initial history)
               (read-string prompt initial history)))
    ("w" "Run with the given packages installed" "--with "
     :prompt "With packages (comma separated): "
     :class transient-option
     :reader (lambda (prompt initial history)
               (read-string prompt initial history)))
    ("T" "Terminal: run in an ansi-term window rather than compile/comint" "terminal")]
   uv--cache-options
   uv--resolver-options]
  ["tool run"
   ("RET" "tool run" uv-tool-run-cmd)])

;;;###autoload
(defun uv-cache-clean ()
  "Clean the uv cache."
  (interactive)
  (uv--do-command "uv cache clean"))

;;;###autoload
(defun uv-cache-prune ()
  "Prune the uv cache."
  (interactive)
  (uv--do-command "uv cache prune"))

(defun uv--quote-string-transient-args (args)
  "Preprocess transient ARGS to append them to a uv command."
  (mapcar (lambda (arg)
            (save-match-data
              (if (string-match "\\(--.+=\\)\\(.*\\)" arg)
                  (concat (match-string 1 arg)
                          (shell-quote-argument (match-string 2 arg)))
                arg)))
          args))



 ;;;###autoload (autoload 'uv-lock "uv" nil t)
(transient-define-prefix uv-lock ()
  "Update the project's lockfile"
  :show-help (lambda (_) (uv-show-command-help "lock"))
  [["Options"
    ("c" "Check if the lockfile is up-to-date." "--check")
    ("C" "Assert that a `uv.lock` exists without checking if it is up-to-date." "--check-exists")
    ("d" "Perform a dry run, without writing the lockfile." "--dry-run")]
   ["Resolver options"
    ("u" "Allow package upgrades, ignoring pinned versions" "--upgrade")
    ("U" "Allow upgrades for a specific package, ignoring pinned versions." "--package-upgrade "
     :prompt "Allow upgrade for: "
     :class transient-option
     :reader (lambda (prompt _initial _history)
               (completing-read prompt (uv--known-locked-packages) nil t)))]]
  ["lock" ("RET" "Lock dependencies" uv-lock-cmd)])

(defun uv-lock-cmd (&optional args)
  "Perform the `uv lock' command with ARGS.

Only to be used directly when the default arguments of `uv lock' are
suitable.  Use `uv-lock' instead."
  (interactive
   (when transient-current-command
     (list (transient-args transient-current-command))))
  (uv--do-command (concat "uv lock " (string-join args " "))))

(defun uv--known-locked-packages ()
  "Determine the project's locked dependency packages."
  (string-split (uv--command-stdout-to-string "uv export --no-hashes --no-emit-project --no-header --no-annotate --all-extras")))

 ;;;###autoload (autoload 'uv "uv" nil t)
(transient-define-prefix uv ()
  :show-help (lambda (_) (uv-show-uv-help))
  ["Commands:"
   ("i" "init – Initialize a project" uv-init)
   ("v" "venv – Create a virtual environment" uv-venv)
   ("a" "add – Add dependencies to the project" uv-add)
   ("r" "remove – Remove dependencies from the project" uv-remove)
   ("s" "sync – Update the project's environment" uv-sync)
   ("l" "lock – Update the project's lockfile" uv-lock)
   ("t" "tool run – run a python tool" uv-tool-run)
   ("R" "run – Run a command or script" uv-run)])

(defun uv--do-command (command)
  "Perform COMMAND in a compint compile buffer in the project's root dir."
  (when-let* ((workdir (uv--project-root))
              (command (split-string-shell-command (string-trim command)))
              (proc-name (uv--process-name command))
              (buf (uv--process-get-buffer-if-available proc-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join command " "))
        (insert "\n"))
      (let ((args (uv--quote-string-transient-args (cdr command)))
            (default-directory workdir))
        (apply #'make-comint-in-buffer proc-name buf (car command) nil args))
      (set-process-sentinel (get-buffer-process buf) #'uv--process-sentinel))
    buf))

(defun uv--process-name (command)
  "Determine name for upcoming process according top COMMAND."
  (pcase command
    (`("uv" "run" . ,_) "uv run")
    (`("uv" "tool" "run" . ,_) "uv tool run")
    (_ "uv process")))

(defun uv--process-get-buffer-if-available (proc-name)
  "Return the buffer of PROC-NAME if it is available or create it."
  (let* ((buffer-name (format "*%s*" proc-name))
         (buf (get-buffer-create buffer-name)))
    (when (or (not (process-live-p (get-buffer-process buf)))
              (y-or-n-p (format "A process `%s' is already running.  Kill it?" proc-name)))
      buf)))

(defun uv--process-sentinel (process event)
  "Perform scheduled stuff after a uv PROCESS finished with EVENT."
  (message "%s %s" (process-name process) (string-trim event))
  (if (string-suffix-p "finished\n" event)
      (uv--perform-success-hooks)
    (uv--perform-failure-hooks)))

(defun uv--perform-success-hooks ()
  "Perform the scheduled stuff after successful uv process."
  (uv--perform-special-hook)
  (when uv--after-run-hook (funcall uv--after-run-hook)))

(defun uv--perform-failure-hooks ()
  "Perform the scheduled stuff after failed uv process."
  (uv--cancel-special-hook)
  (when uv--run-fail-hook (funcall uv--run-fail-hook)))

(defun uv--do-command-maybe-terminal (command terminal)
  "Perform COMMAND either as compile or if TERMINAL is non nil in `ansi-term'."
  (let ((default-directory (uv--project-root)))
    (if terminal
        (ansi-term command)
      (temp-buffer-window-show (uv--do-command command)))))

(defun uv--group-arg (args)
  "Extract dependency groups and extras from transient ARGS."
  (pcase args
    ((app (seq-find (lambda (cand) (string-prefix-p "--group " cand)))
          (and (pred stringp) group))
     `(group . ,(substring group (length "--group "))))
    ((app (seq-find (lambda (cand) (string-prefix-p "--optional " cand)))
          (and (pred stringp) extra))
     `(extra . ,(substring extra (length "--optional "))))
    ((pred (transient-arg-value "--dev"))
     '(group . "dev"))))

(defun uv-activate-venv ()
  "Detect and activate the venv for the current project."
  (interactive)
  (let* ((candidate-venv (expand-file-name (concat (uv--project-root) ".venv")))
         (candidate-bin (concat (file-name-as-directory candidate-venv) "bin")))
    (if (file-directory-p candidate-bin)
        (let ((current-path (or (plist-get uv--projects-last-venv :path)
                                (getenv "PATH")))
              (virtual-env (getenv "VIRTUAL_ENV")))
          (unless (equal virtual-env candidate-venv)
            (setenv "VIRTUAL_ENV" candidate-venv)
            (setenv "PYTHONHOME" nil)
            (setenv "PATH" (string-join (cons candidate-bin (remove candidate-bin (string-split current-path ":"))) ":"))
            (setq python-shell-virtualenv-root candidate-venv)
            (setq uv--projects-last-venv
                  `(:path ,current-path
                    :venv ,virtual-env
                    :python-home ,(getenv "PYTHONHOME")))
            candidate-venv))
      (uv-deactivate-venv))))

(defun uv-deactivate-venv ()
  "Deactivate the current venv."
  (setq python-shell-virtualenv-root nil)
  (setenv "VIRTUAL_ENV" nil)
  (setenv "PYTHONHOME" (plist-get uv--projects-last-venv :python-home))
  (when-let* ((path (plist-get uv--projects-last-venv :path)))
    (setenv "PATH" path))
  (setq uv--projects-last-venv nil))

(cl-defmethod transient-infix-read ((obj uv--transient-multiswitch))
  "Implement function `transient-infix-read' for OBJ."
  (let* ((prompt (oref obj prompt))
         (choices (oref obj choices))
         (selected-item (completing-read prompt (funcall choices)))
         (selection (oref obj value)))
    (if (member selected-item selection)
        (delete selected-item selection)
      (push selected-item selection))))

(cl-defmethod transient-format-value ((obj uv--transient-multiswitch))
  "Implement function `transient-format-value' for OBJ.
The list of selected items is formatted in a way to present it to the user.
OJB is just the self reference."
  (let* ((enabled-items (oref obj value))
         (choices (funcall (oref obj choices)))
         (needs-shortening (> (length choices) 5)))
    (when needs-shortening
      (setq choices (seq-filter (lambda (item) (member item enabled-items)) choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (item)
                  (if (member item enabled-items)
                      (propertize item 'face 'transient-value)
                    (propertize item 'face 'transient-inactive-value)))
                choices
                (propertize ", " 'face 'transient-inactive-value))
     (when needs-shortening (propertize (if enabled-items ", ..." "...") 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-value ((obj uv--transient-multiswitch))
  "Join the selected multiswitch options of OBJ to a set of command line switches."
  (let ((choices (oref obj value))
        (argument (oref obj argument)))
    (when choices
      (concat argument (string-join choices (concat " " argument))))))

(defun uv--project-root ()
  "Save determination of the project root with `default-directory' as default."
  (if-let* ((pc (project-current)))
      (file-name-as-directory (project-root pc))
    default-directory))

(defun uv--command-stdout-to-string (command-line)
  "Execute COMMAND-LINE and return its stdout while discarding stderr."
  (let ((cmd-list (string-split command-line)))
    (with-temp-buffer
     (apply #'call-process (car cmd-list) nil `(,(current-buffer) nil) nil (cdr cmd-list))
     (buffer-string))))

(defun uv--devcontainer-advise-command (command)
  "Prepend devcontainer forwarder to COMMAND if needed."
  (funcall (or (symbol-function 'devcontainer-advise-command) #'identity) command))

(provide 'uv)

;;; uv.el ends here
