;;; uv --- An interface for the uv python package manager -*- lexical-binding: t; -*-

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/uv.el
;; Version: 0.1.0
;; License: GPLv3
;; SPDX-License-Identifier: GPL-3.0-only
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; To be documented

;;; Code:

(require 'transient)
(require 'ansi-color)
(require 'toml)
(require 'project)

(defun uv-init (directory &optional args)
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create project in: ")))))
     (append (list directory) (list (transient-args transient-current-command)))))
  (let ((args (append (uv--quote-string-transient-args args) (list directory))))
    (uv--do-command (concat "uv init " (string-join args " ")))))

(defun uv-init-here (&optional args)
  (interactive
   (list (transient-args transient-current-command))
  (uv-init default-directory args)))

(transient-define-prefix uv-init-menu ()
  "Initialize python project using `uv init'"
  :show-help (lambda (obj) (uv--show-help "init"))
  ["Options"
   ("n" "Name" "--name=" :prompt "Project name: ")
   ("d" "Description" "--description=" :prompt "Project description: ")
   ("l" "lib – create a lib rather than an app" "--lib")
   (uv--select-python-version)
   ("r" "No README.md" "--no-readme")
   ]
  ["Init"
   ("M-RET" "Ask for target directory" uv-init)
   ("RET" "Initialize in current directory" uv-init-here)])

(transient-define-infix uv--select-python-version ()
  "Selector for the python version"
  :key "p"
  :argument "--python "
  :description "Python version"
  :class transient-option
  :reader (lambda (prompt initial history)
            (completing-read prompt (uv-available-python-versions) initial t)))

(defun uv-venv (&optional args)
  (interactive (list (transient-args transient-current-command)))
  (uv--do-command (string-trim-right (concat "uv venv " (string-join args " ")))))

(defun uv-available-python-versions ()
  (sort (mapcar (lambda (python) (gethash "version" python))
                (json-parse-string
                 (shell-command-to-string "uv python list --output-format=json")))
        #'uv--compare-python-versions))

(defun uv--prerelease (version)
  (save-match-data
    (when (string-match "\\(a\\|b\\|rc\\)\\(.+\\)" version)
      (cons (match-string 1 version) (match-string 2 version)))))

(defun uv--compare-python-versions (version-1 version-2)
  (let ((prerelease-1 (uv--prerelease version-1))
        (prerelease-2 (uv--prerelease version-2)))
    (cond ((and (not prerelease-1) (not prerelease-2)) (string> version-1 version-2))
          ((and (not prerelease-1) prerelease-2) t)
          ((and prerelease-1 (not prerelease-2)) nil)
          (t (let ((kind-1 (car prerelease-1))
                   (kind-2 (car prerelease-2))
                   (num-1 (cdr prerelease-1))
                   (num-2 (cdr prerelease-2)))
               (cond ((equal kind-1 kind-2) (string< num-1 num-2))
                     (t (string> kind-1 kind-2))))))))

(transient-define-prefix uv-venv-menu ()
  "Create a virtual environment"
  :show-help (lambda (obj) (uv--show-help "venv"))
  ["Options"
   (uv--select-python-version)
   ("s" "Seed environment" "--seed")]
  ["venv"
  ("RET" "Create the venv" uv-venv)])


(defun uv-add (package &optional args)
  (interactive
   (let ((package (read-string "Package name: ")))
     (append (list package) (list (transient-args transient-current-command)))))
  (let ((args (when args (concat (string-join args " ") " "))))
    (uv--do-command (concat "uv add " args  package))))


(defun uv--known-dependency-groups ()
  (let* ((pyproject-file (concat (file-name-as-directory (project-root (project-current))) "pyproject.toml"))
         (pyproject-data (toml:read-from-file pyproject-file)))
    (mapcar 'car
            (cdr (seq-find
                  (lambda (group-kv) (string= (car group-kv) "dependency-groups"))
                  pyproject-data)))))


(transient-define-prefix uv-add-menu ()
  "Add dependencies to the project"
  :show-help (lambda (obj) (uv--show-help "add"))
  ["Options"
   ("d" "Into development dependency group" "--dev")
   ("g" "Into a specified depencency group" "--group "
    :prompt "Choose group: "
    :class transient-option
    :reader (lambda (prompt initial history)
              (completing-read prompt (uv--known-dependency-groups) initial nil)))]
  ["add"
   ("RET" "Add dependency" uv-add)])


(defun uv-sync (&optional args)
  (interactive
   (when transient-current-command
     (list (transient-args transient-current-command))))
  (uv--do-command (concat "uv sync " (string-join args " "))))


(transient-define-prefix uv-sync-menu ()
  "Update the project's environment"
  :show-help (lambda (obj) (uv--show-help "sync"))
  ["Options"
   ("a" "Sync all extras" "--all-extras")]
  ["sync"
   ("RET" "Run uv sync" uv-sync)])


(defun uv--quote-string-transient-args (args)
  (mapcar (lambda (arg)
            (save-match-data
              (if (string-match "\\(--.+=\\)\\(.*\\)" arg)
                  (concat (match-string 1 arg)
                          (shell-quote-argument (match-string 2 arg)))
                arg)))
          args))

(defun uv--show-help (command)
  (let ((buffer (get-buffer-create (format "*uv help %s*" command))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (shell-command-to-string (concat "uv help " command)))
      (compilation-mode))
    (display-buffer buffer)))

(transient-define-prefix uv-menu ()
  :show-help (lambda (obj) (uv--show-help ""))
  ["Commands:"
   ("i" "init – Initialize a project" uv-init-menu)
   ("v" "venv – Create a virtual environment" uv-venv-menu)
   ("a" "add – Add dependencies to the project" uv-add-menu)
   ("s" "sync – Update the project's environment" uv-sync-menu)])


(defun uv--do-command (cmd)
  (let ((default-directory (project-root (project-current))))
    (compile cmd t)))

(provide 'uv)
