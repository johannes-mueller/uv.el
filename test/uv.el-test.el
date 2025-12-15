;;; uv.el-test.el --- An interface for the uv python package manager -*- lexical-binding: t; -*-

;;; Code:

(require 'mocker)
(require 'uv)
(require 'cl-lib)

(defvar uv--test-cmd-result-event-string "finished\n")
(defvar uv--test-project "/foo/bar/project")
(defvar uv--test-process-name "uv process")

(defmacro expect-process-call (command &rest body)
  "Expect (compile COMMAND) while executing BODY."
  (declare (indent 1))
  `(let* ((stdout-buf (get-buffer-create "*uv process*"))
          (uv-args (cdr ,command)))
     (with-temp-buffer
       (mocker-let ((project-current () ((:output-generator (lambda ()
                                                              (when uv--test-project
                                                                (cons 'project uv--test-project))))))
                    (project-root (project) ((:input-matcher 'always
                                              :output-generator (lambda (project) (cdr project))
                                              :min-occur 0)))
                    (uv--process-get-buffer-if-available (name) ((:input `(,uv--test-process-name) :output stdout-buf)))
                    (make-comint-in-buffer (proc-name buf cmd startfile &rest args)
                                           ((:input (append `(,uv--test-process-name ,stdout-buf "uv" nil) uv-args))))
                    (get-buffer-process (buf) ((:input `(,stdout-buf) :output 'proc)))
                    (set-process-sentinel (proc sentinel) ((:input '(proc uv--process-sentinel))))
                    (process-name (proc) ((:input '(proc) :output "uv")))
                    (message (format-string proc event) ((:input `("%s %s" "uv" ,(string-trim uv--test-cmd-result-event-string))))))
         ,@body
         (uv--process-sentinel 'proc uv--test-cmd-result-event-string)))))

(ert-deftest uv-get-buffer-no-conflict ()
  (mocker-let ((get-buffer-create (proc-name) ((:input '("*uv cmd*") :output 'the-buffer)))
               (get-buffer-process (buffer) ((:input '(the-buffer) :output 'the-process)))
               (process-live-p (process) ((:input '(the-process) :output nil))))
    (should (equal (uv--process-get-buffer-if-available "uv cmd") 'the-buffer))))

(ert-deftest uv-get-buffer-with-conflict-kill ()
  (mocker-let ((get-buffer-create (proc-name) ((:input '("*uv cmd*") :output 'the-buffer)))
               (get-buffer-process (buffer) ((:input '(the-buffer) :output 'the-process)))
               (process-live-p (process) ((:input '(the-process) :output 'process-live)))
               (y-or-n-p (prompt) ((:input '("A process `uv cmd' is already running.  Kill it?") :output t))))
    (should (equal (uv--process-get-buffer-if-available "uv cmd") 'the-buffer))))

(ert-deftest uv-get-buffer-with-conflict-resign ()
  (mocker-let ((get-buffer-create (proc-name) ((:input '("*uv cmd*") :output 'the-buffer)))
               (get-buffer-process (buffer) ((:input '(the-buffer) :output 'the-process)))
               (process-live-p (process) ((:input '(the-process) :output 'process-live)))
               (y-or-n-p (prompt) ((:input '("A process `uv cmd' is already running.  Kill it?") :output nil))))
    (should-not (uv--process-get-buffer-if-available "uv cmd"))))

(ert-deftest uv-init-no-args-current-project ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-process-call '("uv" "init" "foo-bar")
      (uv-init-cmd "foo-bar"))))

(ert-deftest uv-init-no-args-no-project-current ()
  (let ((uv--test-project nil))
    (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
                 (dired (dir) ((:input '("foo-bar")))))
      (expect-process-call '("uv" "init" "foo-bar")
        (uv-init-cmd "foo-bar")))))

(ert-deftest uv-init-transient-name-no-masks ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-process-call '("uv" "init" "--name=FooBar" "foo-bar")
      (uv-init-cmd "foo-bar" '("--name=FooBar")))))

(ert-deftest uv-init-transient-name-with-masks ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-process-call '("uv" "init" "--name=My\\ Project" "foo-bar")
      (uv-init-cmd "foo-bar" '("--name=My Project")))))

(ert-deftest uv-init-transient-python-version ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-process-call '("uv" "init" "--python" "3.12" "foo-bar")
      (uv-init-cmd "foo-bar" '("--python 3.12")))))

(ert-deftest uv-init-transient-no-readme ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-process-call '("uv" "init" "--no-readme" "foo-bar")
      (uv-init-cmd "foo-bar" '("--no-readme")))))

(ert-deftest uv-init-failure ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv") :occur 0)))
               (dired (dir) ((:input '("foo-bar") :occur 0))))
    (let ((uv--test-cmd-result-event-string "failure\n"))
      (expect-process-call '("uv" "init" "foo-bar")
        (uv-init-cmd "foo-bar")))))


(ert-deftest uv-available-python-versions-sorted ()
  (mocker-let ((process-lines (cmd &rest args) ((:input '("uv" "python" "list" "--output-format=json")
                                           :output '("[{\"version\": \"3.7.9\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}]")))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1" "3.7.9")))))

(ert-deftest uv-available-python-versions-sorted-non-unique ()
  (mocker-let ((process-lines (cmd &rest args) ((:input '("uv" "python" "list" "--output-format=json")
                                           :output '("[{\"version\": \"3.13.2\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}]")))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1")))))

(ert-deftest uv-available-python-versions-unsorted ()
  (mocker-let ((process-lines (cmd &rest args) ((:input '("uv" "python" "list" "--output-format=json")
                                           :output '("[{\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}, {\"version\": \"3.14.0b3\"}, {\"version\": \"3.14.0rc1\"}, {\"version\": \"3.12.9\"}, {\"version\": \"3.14.0a4\"}, {\"version\": \"3.14.0a5\"}, {\"version\": \"3.7.9\"}]")))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1" "3.12.9" "3.7.9" "3.14.0rc1" "3.14.0b3" "3.14.0a4" "3.14.0a5")))))

;; (ert-deftest uv-available-python-versions-minor-releases ()
;;   (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json" t);                                                        :output "[{\"version\": \"3.13.1\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.12.1\"}, {\"version\": \"3.12.2\"}]"))))
;;     (should (equal (uv--available-python-versions) '("3.13" "3.13.1" "3.13.2" "3.12" "3.12.1" "3.12.2")))))

(ert-deftest uv-venv-plain ()
  (expect-process-call '("uv" "venv")
    (uv-venv-cmd)))

(ert-deftest uv-venv-seed ()
  (expect-process-call '("uv" "venv" "--seed")
    (uv-venv-cmd '("--seed"))))

(ert-deftest uv-cmd-buffer-read-only ()
  (expect-process-call '("uv" "venv")
    (uv-venv-cmd))
  (with-current-buffer "*uv process*"
    (should buffer-read-only)))

(ert-deftest uv-cmd-buffer-process-name ()
  (expect-process-call '("uv" "venv")
    (uv-venv-cmd))
  (with-current-buffer "*uv process*"
    (should (equal list-buffers-directory "/foo/bar/project/"))))

(ert-deftest uv-add-one ()
  (expect-process-call '("uv" "add" "pandas")
    (uv-add-cmd "pandas")))

(ert-deftest uv-add-dev ()
  (expect-process-call '("uv" "add" "--dev" "pytest")
    (uv-add-cmd "pytest" '("--dev"))))

(ert-deftest uv-add-extra-two-only-comma ()
  (expect-process-call '("uv" "add" "--extra=excel" "--extra=hdf5" "pandas")
    (uv-add-cmd "pandas" '("--extra=excel,hdf5"))))

(ert-deftest uv-add-extra-two-comma-ws ()
  (expect-process-call '("uv" "add" "--extra=excel" "--extra=hdf5" "pandas")
    (uv-add-cmd "pandas" '("--extra=excel, hdf5"))))

(ert-deftest uv-add-extra-two-only-ws ()
  (expect-process-call '("uv" "add" "--extra=excel" "--extra=hdf5" "pandas")
    (uv-add-cmd "pandas" '("--extra=excel hdf5"))))

(ert-deftest uv-add-extra-two-ws-comma ()
  (expect-process-call '("uv" "add" "--extra=excel" "--extra=hdf5" "pandas")
    (uv-add-cmd "pandas" '("--extra=excel ,hdf5"))))

(defun alist-to-hash-table (alist)
  "Turn ALIST into a hash table."
  (let ((hash-table (make-hash-table :test 'equal)))
    (dolist (elt alist)
      (puthash (car elt)
               (if (listp (cdr elt))
                   (alist-to-hash-table (cdr elt))
                 (cdr elt))
               hash-table))
    hash-table))

(defun equal-set (list-1 list-2)
  "Check if LIST-1 and LIST-2 a represent the equal set."
  (eq (cl-set-difference list-1 list-2 :test 'equal) nil))

(ert-deftest known-groups-no-project ()
  (mocker-let ((project-current () ((:output nil))))
    (should-not (uv--known-dependency-groups))))

(ert-deftest known-groups-no-pyproject ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output nil))))
      (should-not (uv--known-dependency-groups)))))

(ert-deftest known-groups-groups ()
  (let ((native-comp-enable-subr-trampolines nil))
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project" ("version" . "0.1.0"))
                                                   ("dependency-groups" ("my-extra" . ["scipy"]) ("dev" . ["pytest>=8.3.5"]))))))))
    (should (equal-set (uv--known-dependency-groups) '("my-extra" "dev"))))))

(ert-deftest known-groups-no-groups ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table '(("project" ("version" . "0.1.0"))))))))
      (should (eq (uv--known-dependency-groups) nil)))))


(ert-deftest known-extras-no-project ()
  (mocker-let ((project-current () ((:output nil))))
    (should-not (uv--known-extras))))

(ert-deftest known-extras-extras ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("optional-dependencies"
                                                       ("my-extra" . ["scipy"])
                                                       ("dev" . ["pytest>=8.3.5"])))))))))
      (should (equal-set (uv--known-extras) '("my-extra" "dev"))))))

(ert-deftest known-extras-no-extras ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project" ("version" . "0.1.0"))))))))
      (should (equal (uv--known-extras) nil)))))


(ert-deftest known-dependencies-no-project ()
  (mocker-let ((project-current () ((:output nil))))
    (should-not (uv--known-dependencies))))

(ert-deftest known-dependencies-dependencies-plain ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("dependencies" . ["scipy" "pytest>=8.3.5"]))))))))
      (should (equal (uv--known-dependencies) '("scipy" "pytest>=8.3.5"))))))


(ert-deftest known-dependencies-no-dependencies ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project" ("version" . "0.1.0"))))))))
      (should (equal (uv--known-dependencies) nil)))))


(ert-deftest known-dependencies-dependencies-without-group ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("dependencies" . ["numpy" "scipy"]))
                                                     ("dependency-groups"
                                                      ("dev" . ["pytest" "freezegun"])
                                                      ("docs" . ["sphinx" "nbsphinx"]))))))))
      (should (equal (uv--known-dependencies) '("numpy" "scipy"))))))


(ert-deftest known-dependencies-dependencies-docs-group ()
  (let ((transient-current-command 'uv-remove-menu)
        (native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("dependencies" . ["numpy" "scipy"]))
                                                     ("dependency-groups"
                                                      ("dev" . ["pytest" "freezegun"])
                                                      ("docs" . ["sphinx" "nbsphinx"])))))))
                 (transient-args (transient-cmd) ((:input '(uv-remove-menu)
                                                   :output '("--group docs")))))
      (should (equal (uv--known-dependencies) '("sphinx" "nbsphinx"))))))

(ert-deftest known-dependencies-dependencies-dev ()
  (let ((transient-current-command 'uv-remove-menu)
        (native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("dependencies" . ["numpy" "scipy"]))
                                                     ("dependency-groups"
                                                      ("dev" . ["pytest>=8.3.5" "freezegun==1.2.3"])
                                                      ("docs" . ["sphinx" "nbsphinx"])))))))
                 (transient-args (transient-cmd) ((:input '(uv-remove-menu)
                                                   :output '("--dev")))))
      (should (equal (uv--known-dependencies) '("pytest>=8.3.5" "freezegun==1.2.3"))))))


(ert-deftest known-dependencies-dependencies-extra ()
  (let ((transient-current-command 'uv-remove-menu)
        (native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
                 (file-exists-p (file) ((:input '("/foo/bar/project/pyproject.toml") :output t)))
                 (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                          :output (alist-to-hash-table
                                                   '(("project"
                                                      ("version" . "0.1.0")
                                                      ("dependencies" . ["numpy"])
                                                      ("optional-dependencies"
                                                       ("my-extra" . ["scipy"])
                                                       ("dev" . ["pytest>=8.3.5"]))))))))
                 (transient-args (transient-cmd) ((:input '(uv-remove-menu)
                                                   :output '("--optional my-extra")))))
      (should (equal (uv--known-dependencies) '("scipy"))))))

(defmacro expect-run-process-call (command &rest body)
  "Expect tool COMMAND call while executing BODY."
  (declare (indent 1))
  `(let ((uv--test-process-name "uv run"))
     (expect-process-call ,command ,@body)))

(ert-deftest uv-run-command-compile ()
  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-run-cmd "foo-command")))

(ert-deftest uv-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv run  -- foo-command")))))
    (uv-run-cmd "foo-command" '("terminal"))))

(ert-deftest uv-run-command-history-one-project ()
  (clrhash uv--run-history)

  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command"))))

  (expect-run-process-call '("uv" "run" "--" "other-command")
    (uv-run-cmd "other-command")
    (should (equal (uv--project-run-command-history) '("other-command" "foo-command"))))

  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command" "other-command")))))

(ert-deftest uv-run-command-history-two-projects ()
  (clrhash uv--run-history)

  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command"))))

  (let ((uv--test-project "/some/other/project"))
    (expect-run-process-call '("uv" "run" "--" "foo-command")
      (should-not (uv--project-run-command-history))
      (uv-run-cmd "foo-command")
      (should (equal (uv--project-run-command-history) '("foo-command")))))

  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command")))
    (uv-repeat-run)
    (should (equal (uv--project-run-command-history) '("foo-command")))))

(ert-deftest uv-run-command-repeat-one-project ()
  (clrhash uv--run-history)
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project")))))
    (should-not (uv--project-run-command-history)))
  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-run-cmd "foo-command"))
  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-repeat-run))
  (expect-run-process-call '("uv" "run" "--" "other-command")
    (uv-run-cmd "other-command"))
  (expect-run-process-call '("uv" "run" "--" "other-command")
    (uv-repeat-run))
  (expect-run-process-call '("uv" "run" "--module" "--" "foo-command")
    (uv-run-cmd "foo-command" '("--module")))
  (expect-run-process-call '("uv" "run" "--module" "--" "foo-command")
    (uv-repeat-run))
  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-run-cmd "foo-command"))
  (expect-run-process-call '("uv" "run" "--" "foo-command")
    (uv-repeat-run)))

(defmacro expect-tool-run-process-call (command &rest body)
  "Expect tool COMMAND call while executing BODY."
  (declare (indent 1))
  `(let ((uv--test-process-name "uv tool run"))
     (expect-process-call ,command ,@body)))

(ert-deftest uv-tool-run-command-compile ()
  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-tool-run-cmd "foo-command")))

(ert-deftest uv-tool-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv tool run  foo-command")))))
    (uv-tool-run-cmd "foo-command" '("terminal"))))

(ert-deftest uv-tool-run-command-history-one-project ()
  (clrhash uv--tool-run-history)

  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command"))))

  (expect-tool-run-process-call '("uv" "tool" "run" "other-command")
    (uv-tool-run-cmd "other-command")
    (should (equal (uv--project-tool-run-command-history) '("other-command" "foo-command"))))

  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command" "other-command")))))

(ert-deftest uv-tool-run-command-history-two-projects ()
  (clrhash uv--tool-run-history)

  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command"))))

  (let ((uv--test-project "/some/other/project"))
    (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
      (should-not (uv--project-tool-run-command-history))
      (uv-tool-run-cmd "foo-command")
      (should (equal (uv--project-tool-run-command-history) '("foo-command")))))

  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command")))
    (uv-repeat-tool-run)
    (should (equal (uv--project-tool-run-command-history) '("foo-command")))))

(ert-deftest uv-tool-run-command-repeat-one-project ()
  (clrhash uv--tool-run-history)
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project")))))
    (should-not (uv--project-tool-run-command-history)))
  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-tool-run-cmd "foo-command"))
  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-repeat-tool-run))
  (expect-tool-run-process-call '("uv" "tool" "run" "other-command")
    (uv-tool-run-cmd "other-command"))
  (expect-tool-run-process-call '("uv" "tool" "run" "other-command")
    (uv-repeat-tool-run))
  (expect-tool-run-process-call '("uv" "tool" "run" "--module" "foo-command")
    (uv-tool-run-cmd "foo-command" '("--module")))
  (expect-tool-run-process-call '("uv" "tool" "run" "--module" "foo-command")
    (uv-repeat-tool-run))
  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-tool-run-cmd "foo-command"))
  (expect-tool-run-process-call '("uv" "tool" "run" "foo-command")
    (uv-repeat-tool-run)))

(ert-deftest uv-group-arg-empty ()
  (should (eq (uv--group-arg '()) nil)))

(ert-deftest uv-group-arg-no-group ()
  (should (eq (uv--group-arg '("--foo bar")) nil)))

(ert-deftest uv-group-arg-group ()
  (should (equal (uv--group-arg '("--foo bar" "--group foo")) `(group . "foo"))))

(ert-deftest uv-group-arg-extra ()
  (should (equal (uv--group-arg '("--foo bar" "--optional foo")) `(extra . "foo"))))

(ert-deftest uv-group-arg-dev ()
  (should (equal (uv--group-arg '("--dev")) `(group . "dev"))))


(ert-deftest known-locked-packages-empty ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((current-buffer () ((:output "*some buffer*")))
                 (call-process (cmd &rest args)
                               ((:input '("uv" nil ("*some buffer*" nil) nil "export" "--no-hashes" "--no-emit-project" "--no-header" "--no-annotate" "--all-extras"))))
                 (buffer-string () ((:output ""))))
      (should (eq (uv--known-locked-packages) nil)))))


(ert-deftest known-locked-packages-non-empty ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((current-buffer () ((:output "*some buffer*")))
                 (call-process (cmd &rest args)
                               ((:input '("uv" nil ("*some buffer*" nil) nil "export" "--no-hashes" "--no-emit-project" "--no-header" "--no-annotate" "--all-extras"))))
                 (buffer-string () ((:output "numpy==2.2.4
pandas==2.2.3
python-dateutil==2.9.0.post0
"))))
    (should (equal (uv--known-locked-packages) '("numpy==2.2.4" "pandas==2.2.3" "python-dateutil==2.9.0.post0"))))))


(ert-deftest uv--run-candidates-no-python-script ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv run 2> /dev/null | sed -n 's/^- //p'")
                                                :output "foo\nbar\n")))
               (file-expand-wildcards (pattern) ((:input '("*.py") :output nil))))
    (should (equal (uv--run-candidates) '("foo" "bar")))))


(ert-deftest uv--run-candidates-with-python-script ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv run 2> /dev/null | sed -n 's/^- //p'")
                                                :output "foo\nbar\n")))
               (file-expand-wildcards (pattern) ((:input '("*.py")
                                                  :output '("hello.py" "goodbye.py")))))
    (should (equal (uv--run-candidates) '("hello.py" "goodbye.py" "foo" "bar")))))

(ert-deftest uv--run-candidates-devcontainer ()
  (mocker-let ((devcontainer-advise-command (cmd) ((:input '("uv run 2> /dev/null | sed -n 's/^- //p'")
                                                    :output "devcontainer exec --workspace-folder . uv run 2> /dev/null | sed -n 's/^- //p'")))
               (shell-command-to-string (cmd) ((:input '("devcontainer exec --workspace-folder . uv run 2> /dev/null | sed -n 's/^- //p'")
                                                :output "foo\nbar\n")))
               (file-expand-wildcards (pattern) ((:input '("*.py") :output nil))))
    (should (equal (uv--run-candidates) '("foo" "bar")))))

(ert-deftest uv--activate-venv-no-venv-available ()
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv/bin")
                                             :output nil)))
               (uv-deactivate-venv () ((:output nil))))
    (should-not (uv-activate-venv))))

(ert-deftest uv--activate-venv-venv-available ()
  (setq uv--projects-last-venv nil)
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv/bin")
                                             :output t)))
               (getenv (var) ((:input '("PATH") :output "/original/path:/usr/bin")
                              (:input '("VIRTUAL_ENV") :output nil)
                              (:input '("PYTHONHOME") :output nil)))
               (setenv (var value) ((:input '("VIRTUAL_ENV" "/foo/bar/project/.venv"))
                                    (:input '("PYTHONHOME" nil))
                                    (:input '("PATH" "/foo/bar/project/.venv/bin:/original/path:/usr/bin")))))
    (should (uv-activate-venv))
    (should (equal python-shell-virtualenv-root "/foo/bar/project/.venv"))))

(ert-deftest uv--activate-venv-unify-path ()
  (setq uv--projects-last-venv nil)
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv/bin")
                                             :output t)))
               (getenv (var) ((:input '("PATH") :output "/original/path:/foo/bar/project/.venv/bin:/usr/bin")
                              (:input '("VIRTUAL_ENV") :output nil)
                              (:input '("PYTHONHOME") :output nil)))
               (setenv (var value) ((:input '("VIRTUAL_ENV" "/foo/bar/project/.venv"))
                                    (:input '("PYTHONHOME" nil))
                                    (:input '("PATH" "/foo/bar/project/.venv/bin:/original/path:/usr/bin")))))
    (should (uv-activate-venv))))


(ert-deftest uv--activate-venv-venv-expand-path ()
  (setq uv--projects-last-venv nil)
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:input '() :output (cons 'project "~/project"))))
                 (project-root (project) ((:input '((project . "~/project"))
                                           :output "~/project")))
                 (expand-file-name (name) ((:input '("~/project/.venv")
                                            :output "/home/me/project/.venv")))
                 (file-directory-p (venvdir) ((:input '("/home/me/project/.venv/bin")
                                               :output t)))
                 (getenv (var) ((:input '("PATH") :output "/some/path:/usr/bin")
                                (:input '("VIRTUAL_ENV") :output nil)
                                (:input '("PYTHONHOME") :output nil)))
                 (setenv (var value) ((:input '("VIRTUAL_ENV" "/home/me/project/.venv"))
                                      (:input '("PYTHONHOME" nil))
                                      (:input '("PATH" "/home/me/project/.venv/bin:/some/path:/usr/bin")))))
      (should (uv-activate-venv))
      (should (equal python-shell-virtualenv-root "/home/me/project/.venv")))))


(ert-deftest uv--activate-change-venv-expand-path ()
  (setq uv--projects-last-venv nil)
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:input '() :output (cons 'project "~/project-one") :occur 1)
                                      (:input '() :output (cons 'project "~/project-two") :occur 1)))
                 (project-root (project) ((:input '((project . "~/project-one"))
                                           :output "~/project-one")
                                          (:input '((project . "~/project-two"))
                                           :output "~/project-two")))
                 (expand-file-name (name) ((:input '("~/project-one/.venv")
                                            :output "/home/me/project-one/.venv")
                                           (:input '("~/project-two/.venv")
                                            :output "/home/me/project-two/.venv")))
                 (file-directory-p (venvdir) ((:input '("/home/me/project-one/.venv/bin")
                                               :output t)
                                              (:input '("/home/me/project-two/.venv/bin")
                                               :output t)))
                 (getenv (var) ((:input '("PATH") :output "/some/path:/usr/bin")
                                (:input '("VIRTUAL_ENV") :output nil)
                                (:input '("PYTHONHOME") :output nil)
                                (:input '("VIRTUAL_ENV") :output "/home/me/project-one/.venv")
                                (:input '("PYTHONHOME") :output nil)))
                 (setenv (var value) ((:input '("VIRTUAL_ENV" "/home/me/project-one/.venv"))
                                      (:input '("PYTHONHOME" nil))
                                      (:input '("PATH" "/home/me/project-one/.venv/bin:/some/path:/usr/bin"))
                                      (:input '("VIRTUAL_ENV" "/home/me/project-two/.venv"))
                                      (:input '("PYTHONHOME" nil))
                                      (:input '("PATH" "/home/me/project-two/.venv/bin:/some/path:/usr/bin")))))
      (should (uv-activate-venv))
      (should (equal python-shell-virtualenv-root "/home/me/project-one/.venv"))
      (should (uv-activate-venv))
      (should (equal python-shell-virtualenv-root "/home/me/project-two/.venv"))
      )))


(ert-deftest uv--activate-and-deactivate-venv ()
  (setq uv--projects-last-venv nil)
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv/bin")
                                             :output t)))
               (getenv (var) ((:input '("PATH") :output "/original/path:/usr/bin")
                              (:input '("VIRTUAL_ENV") :output "/some/other/venv")
                              (:input '("PYTHONHOME") :output "/some/pythonhome")))
               (setenv (var value) ((:input '("VIRTUAL_ENV" "/foo/bar/project/.venv"))
                                    (:input '("PYTHONHOME" nil))
                                    (:input '("PATH" "/foo/bar/project/.venv/bin:/original/path:/usr/bin"))
                                    (:input '("VIRTUAL_ENV" nil))
                                    (:input '("PYTHONHOME" "/some/pythonhome"))
                                    (:input '("PATH" "/original/path:/usr/bin")))))
    (should (uv-activate-venv))
    (uv-deactivate-venv)
    (should-not python-shell-virtualenv-root)))


(provide 'uv.el-test.el)

;;; uv.el-test.el ends here
