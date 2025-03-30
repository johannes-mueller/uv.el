;;; uv --- An interface for the uv python package manager -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/uv.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

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
(require 'toml)
(require 'project)


(defclass uv--transient-multiswitch (transient-argument)
  ((scope :initarg :scope))
  "A `transient-argument' to select from a list of mutually non exclusive items.")


(defun uv-init-cmd (directory &optional args)
  "Perform the `uv init' command in DIRECTORY with ARGS.

Only to be used directly when the default arguments of `uv init' are
suitable.  Use `uv-init' instead."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create project in: ")))))
     (append (list directory) (list (transient-args transient-current-command)))))
  (let ((args (append (uv--quote-string-transient-args args) (list directory))))
    (uv--do-command (concat "uv init " (string-join args " ")))
                                        ;(project-switch-project directory)
                                        ;(project-dired)
    ))

(defun uv-init-here-cmd (&optional args)
  "Perform the `uv init' command in the current directory with ARGS.

Only to be used directly when the default arguments of `uv init' are
suitable.  Use `uv-init' instead."
  (interactive
   (list (transient-args transient-current-command))
  (uv-init default-directory args)))

(defconst uv--python-group
  ["Python options"
   (uv--select-python-version)]
  "Transient group for general python options.")

 ;;;###autoload (autoload 'uv-init "uv" nil t)
(transient-define-prefix uv-init ()
  "Initialize python project using `uv init'"
  :show-help (lambda (obj) (uv--show-help "init"))
  [["Options"
    ("n" "Name" "--name=" :prompt "Project name: ")
    ("d" "Description" "--description=" :prompt "Project description: ")
    ("l" "lib – create a lib rather than an app" "--lib")
    ("v" "do not initalize a git repository" "--vcs none")
    ("p" "setup to build a python package" "--package")
    (uv--select-build-backend)
    ("r" "No README.md" "--no-readme")
    ("V" "Do not create a `.python-version` file for the project." "--no-pin-python")
    ("w" "Avoid discovering a workspace and create a standalone project." "--no-workspace")]
   uv--python-group]
  ["Init"
   ("M-RET" "Ask for target directory" uv-init-cmd)
   ("RET" "Initialize in current directory" uv-init-here-cmd)])

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
  :argument "--python"
  :description "Python version"
  :class transient-option
  :reader (lambda (prompt initial history)
            (let ((completion-styles '(basic)))
              (completing-read prompt (uv--sorted-python-version-completions) initial t))))


(defun uv--sorted-python-version-completions ()
  (let ((python-versions (uv--available-python-versions)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          `(metadata (display-sort-function . ,#'identity))
        (complete-with-action action python-versions string pred)))))


(defun uv--available-python-versions ()
  "Determine and sort the available python versions."
  (sort (delete-dups
         (mapcar (lambda (python) (gethash "version" python))
                 (json-parse-string
                  (shell-command-to-string "uv python list --output-format=json"))))
        #'uv--python-version>))

(defun uv--python-version> (version-1 version-2)
  "Compare python VERSION-1 with VERSION-2 to sort latest release first but prereleases last."
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
  "Parse a python VERSION string and return a list '(`major' `minor' `patch')"
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
  :show-help (lambda (obj) (uv--show-help "venv"))
  ["Options"
   (uv--select-python-version)
   ("s" "Seed environment" "--seed")]
  ["venv"
  ("RET" "Create the venv" uv-venv)])


(defun uv--known-dependency-groups ()
  "Determine the projects known dependency-groups from pyproject.toml."
  (let* ((pyproject-file (concat (file-name-as-directory (project-root (project-current))) "pyproject.toml"))
         (pyproject-data (toml:read-from-file pyproject-file)))
    (mapcar 'car (alist-get "dependency-groups" pyproject-data nil nil #'equal))))


(defun uv--known-extras ()
  "Determine the projects known extras from pyproject.toml."
  (let* ((pyproject-file (concat (file-name-as-directory (project-root (project-current))) "pyproject.toml"))
         (pyproject-data (toml:read-from-file pyproject-file))
         (project-entry (alist-get "project" pyproject-data nil nil #'equal)))
    (mapcar 'car (alist-get "optional-dependencies" project-entry nil nil #'equal))))


(defun uv--known-dependencies ()
  "Determine the projects known extras from pyproject.toml."
  (let* ((pyproject-file (concat (file-name-as-directory (project-root (project-current))) "pyproject.toml"))
         (pyproject-data (toml:read-from-file pyproject-file))
         (project-entry (alist-get "project" pyproject-data nil nil #'equal)))
    (pcase (uv--group-arg (transient-args transient-current-command))
      ((and (pred stringp) group)
       (alist-get group
                  (alist-get "dependency-groups" pyproject-data nil nil #'equal) nil nil #'equal))
      (_ (alist-get "dependencies" project-entry nil nil #'equal)))))


(defconst uv--dependency-group
  ["Options"   ("d" "Into development dependency group" "--dev")
   ("g" "Into a specified depencency group" "--group "
    :prompt "Choose group: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (completing-read prompt (uv--known-dependency-groups) initial nil)))
   ("o" "Add the dependency to a specified extra" "--optional "
    :prompt "Choose extra: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (completing-read prompt (uv--known-extras) initial nil)))]
  "Transient group to add and remove python dependencies.")

 ;;;###autoload (autoload 'uv-add "uv" nil t)
(transient-define-prefix uv-add ()
  "Add dependencies to the project"
  :show-help (lambda (obj) (uv--show-help "add"))
  uv--dependency-group
  ["add"
   ("RET" "Add dependency" uv-add-cmd)])

 ;;;###autoload (autoload 'uv-remove "uv" nil t)
(transient-define-prefix uv-remove ()
  "Remove dependencies from the project"
  :show-help (lambda (obj) (uv--show-help "remove"))
  uv--dependency-group
  ["remove"
   ("RET" "Remove dependency" uv-remove-cmd)])

(defun uv-add-cmd (package &optional args)
  "Perform the `uv add' command to add PACKAGE with ARGS.

Only to be used directly when the default arguments of `uv add' are
suitable.  Use `uv-add' instead."
  (interactive
   (let ((package (read-string "Package name: ")))
     (append (list package) (when transient-current-command (list (transient-args transient-current-command))))))
  (let ((args (when args (concat (string-join args " ") " "))))
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
  (uv--do-command (concat "uv sync " (string-join args " "))))


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


(defconst uv--dependency-options
  ["Dependency options"
   (uv--extra-multiswitch)
   ("E" "(de)select all extras" uv--select-or-deselect-all-extras)
   (uv--group-multiswitch)
   ("G" "(de)select all groups" uv--select-or-deselect-all-groups)
   ("d" "with dev dependency group" uv--toggle-dev-group)
   ])

 ;;;###autoload (autoload 'uv-sync "uv" nil t)
(transient-define-prefix uv-sync ()
  "Update the project's environment"
  :show-help (lambda (obj) (uv--show-help "sync"))
  [uv--dependency-options
   ["Sync options"
    ("ne" "Install editable dependencys non-editable." "--non-editable")
    ("i" "Do not remove extraneous packages." "--inexact")
    ("a" "Sync into active virtual environment." "--active")
    ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
    ("f" "Sync without updating `uv.lock'" "--frozen")]]
  ["sync"
   ("RET" "Run uv sync" uv-sync-cmd)])

(defun uv--run-candidates ()
  "Determine candidate commands for `uv run'."
  (string-split (shell-command-to-string "uv run | sed -n 's/^- //p'")))

(defun uv-run-cmd (command &optional args)
  "Perform the `uv run' command to run COMMAND with ARGS.

Only to be used directly when the default arguments of `uv sync' are
suitable.  Use `uv-sync' instead."
(interactive
   (let ((command (completing-read "Command: " (uv--run-candidates))))
     (append (list command)
             (when transient-current-command (list (transient-args transient-current-command))))))
  (let ((args (when args (concat (string-join args " ") " "))))
    (uv--do-command (concat "uv run " args command))))

 ;;;###autoload (autoload 'uv-run "uv" nil t)
(transient-define-prefix uv-run ()
  "Run a command or script"
  :show-help (lambda (obj) (uv--show-help "run"))
  [uv--dependency-options
   ["Run options"
    ("m" "Run as a module" "--module")
    ("ne" "Install editable dependencys non-editable." "--non-editable")
    ("x" "Remove extraneous packages" "--exact")
    ("i" "Run in an isolated virtual environment" "--isolated")
    ("a" "Run in the active virtual environment." "--active")
    ("l" "Assert that `uv.lock' will remain unchanged." "--locked")
    ("f" "Sync without updating `uv.lock'" "--frozen")
    ("ns" "Do not sync the virtual environement" "--no-sync")]]
  ["run"
   ("RET" "Run" uv-run-cmd)])



(defun uv-tool-run-cmd (tool &optional args)
  "Perform the `uv tool' command to run COMMAND with ARGS.

Only to be used directly when the default arguments of `uv sync' are
suitable.  Use `uv-sync' instead."
(interactive
 (let ((tool (read-string "Run tool: ")))
     (append (list tool)
             (when transient-current-command (list (transient-args transient-current-command))))))
  (let ((args (when args (concat (string-join args " ") " "))))
    (ansi-term (concat "uv tool run " args tool))))


 ;;;###autoload (autoload 'uv-tool-run "uv" nil t)
(transient-define-prefix uv-tool-run ()
  "Run a tool by `uv tool run'"
  :show-help (lambda (obj (uv--show-help "tool run")))
  ["Options"
   ("f" "Use a the given package to provide the command" "--from "
    :prompt "From package: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (read-string prompt initial history)))
   ("w" "Run with the given packages installed" "--with "
    :prompt "With packages (comma separated): "
    :class transient-option
    :reader (lambda (prompt initial history)
               (read-string prompt initial history)))]
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

(defun uv--show-help (command)
  "Show the help text for uv COMMAND."
  (let ((buffer (get-buffer-create (format "*uv help %s*" command))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (shell-command-to-string (concat "uv help " command)))
      (compilation-mode))
    (display-buffer buffer)))


 ;;;###autoload (autoload 'uv-lock "uv" nil t)
(transient-define-prefix uv-lock ()
  "Update the project's lockfile"
  :show-help (lambda (obj) (uv--show-help "lock"))
  [["Options"
    ("c" "Check if the lockfile is up-to-date." "--check")
    ("C" "Assert that a `uv.lock` exists without checking if it is up-to-date." "--check-exists")
    ("d" "Perform a dry run, without writing the lockfile." "--dry-run")]
   ["Resolver options"
    ("u" "Allow package upgrades, ignoring pinned versions" "--upgrade")
    ("U" "Allow upgrades for a specific package, ignoring pinned versions." "--package-upgrade "
     :prompt "Allow upgrade for: "
     :class transient-option
     :reader (lambda (prompt initial history)
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
  (string-split (uv--shell-command-stdout-to-string "uv export --no-hashes --no-emit-project --no-header --all-extras")))


 ;;;###autoload (autoload 'uv "uv" nil t)
(transient-define-prefix uv ()
  :show-help (lambda (obj) (uv--show-help ""))
  ["Commands:"
   ("i" "init – Initialize a project" uv-init)
   ("v" "venv – Create a virtual environment" uv-venv)
   ("a" "add – Add dependencies to the project" uv-add)
   ("r" "remove – Remove dependencies from the project" uv-remove)
   ("s" "sync – Update the project's environment" uv-sync)
   ("l" "lock – Update the project's lockfile" uv-lock)
   ("t" "tool run – run a python tool" uv-tool-run)
   ("R" "run – Run a command or script" uv-run)])

(defun uv--do-command (cmd)
  "Performs the command CMD in a compint compile buffer in the project's root dir."
  (let ((default-directory (project-root (project-current))))
    (compile cmd t)))

(defun uv--group-arg (args)
  "Extract dependency groups and extras from transient ARGS."
  (pcase args
    ((app (seq-find (lambda (cand) (string-prefix-p "--group " cand)))
          (and (pred stringp) group))
     (substring group (length "--group ")))
    ((app (seq-find (lambda (cand) (string-prefix-p "--optional " cand)))
          (and (pred stringp) extra))
     (substring extra (length "--optional ")))
    ((pred (transient-arg-value "--dev")) "dev")))

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
  (let ((enabled-items (oref obj value))
        (choices (oref obj choices)))
    (concat
     (propertize "[" 'face 'transient-inactive-value)
     (mapconcat (lambda (item)
                  (if (member item enabled-items)
                      (propertize item 'face 'transient-value)
                    (propertize item 'face 'transient-inactive-value)))
                (funcall choices)
                (propertize ", " 'face 'transient-inactive-value))
     (propertize "]" 'face 'transient-inactive-value))))

(cl-defmethod transient-infix-value ((obj uv--transient-multiswitch))
  "Join the selected multiswitch options of OBJ to a set of command line switches."
  (let ((choices (oref obj value))
        (argument (oref obj argument)))
    (when choices
      (concat argument (string-join choices (concat " " argument))))))


(defun uv--shell-command-stdout-to-string (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (let ((stderr (get-buffer-create " *temp*")))
        (unwind-protect
            (shell-command command t stderr)
          (kill-buffer stderr))))))

(provide 'uv)

;;; ub.el ends here
