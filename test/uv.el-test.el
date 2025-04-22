(require 'mocker)
(require 'uv)
(require 'cl-lib)

(defmacro expect-compile (command &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (mocker-let ((compile (cmd comint) ((:input (list ,command t) :output (current-buffer)))))
       (setq compilation-finish-functions "uuu")
       ,@body
       (when compilation-finish-functions (funcall (car compilation-finish-functions) ,(current-buffer) "msg")))))

(ert-deftest uv-init-no-args-current-project ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init foo-bar"
      (uv-init-cmd "foo-bar"))))

(ert-deftest uv-init-no-args-no-project-current ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (project-current () ((:output nil)))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init foo-bar"
      (uv-init-cmd "foo-bar"))))

(ert-deftest uv-init-transient-name-no-masks ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init --name=FooBar foo-bar"
      (uv-init-cmd "foo-bar" '("--name=FooBar")))))

(ert-deftest uv-init-transient-name-with-masks ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init --name=My\\ Project foo-bar"
      (uv-init-cmd "foo-bar" '("--name=My Project")))))

(ert-deftest uv-init-transient-python-version ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init --python 3.12 foo-bar"
      (uv-init-cmd "foo-bar" '("--python 3.12")))))

(ert-deftest uv-init-transient-no-readme ()
  (mocker-let ((process-lines (cmd args) ((:input '("uv" "venv"))))
               (dired (dir) ((:input '("foo-bar")))))
    (expect-compile "uv init --no-readme foo-bar"
      (uv-init-cmd "foo-bar" '("--no-readme")))))

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
  (expect-compile "uv venv"
    (uv-venv-cmd)))

(ert-deftest uv-venv-seed ()
  (expect-compile "uv venv --seed"
    (uv-venv-cmd '("--seed"))))

(ert-deftest uv-add-one ()
  (expect-compile "uv add pandas"
    (uv-add-cmd "pandas")))

(ert-deftest uv-add-dev ()
  (expect-compile "uv add --dev pytest"
    (uv-add-cmd "pytest" '("--dev"))))

(ert-deftest uv-add-extra-two-only-comma ()
  (expect-compile "uv add --extra=excel --extra=hdf5 pandas"
    (uv-add-cmd "pandas" '("--extra=excel,hdf5"))))

(ert-deftest uv-add-extra-two-comma-ws ()
  (expect-compile "uv add --extra=excel --extra=hdf5 pandas"
    (uv-add-cmd "pandas" '("--extra=excel, hdf5"))))

(ert-deftest uv-add-extra-two-only-ws ()
  (expect-compile "uv add --extra=excel --extra=hdf5 pandas"
    (uv-add-cmd "pandas" '("--extra=excel hdf5"))))

(ert-deftest uv-add-extra-two-ws-comma ()
  (expect-compile "uv add --extra=excel --extra=hdf5 pandas"
    (uv-add-cmd "pandas" '("--extra=excel ,hdf5"))))

(defun alist-to-hash-table (alist)
  (let ((hash-table (make-hash-table :test 'equal)))
    (mapcar (lambda (elt)
              (puthash (car elt) (if (listp (cdr elt)) (alist-to-hash-table (cdr elt))
                                   (cdr elt))
                       hash-table)
              )
            alist)
    hash-table))

(defun equal-set (list-1 list-2)
  (eq (cl-set-difference list-1 list-2 :test 'equal) nil))

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


(ert-deftest uv-run-command-compile ()
  (mocker-let ((compile (cmd comint) ((:input '("uv run  -- foo-command" t)))))
    (uv-run-cmd "foo-command")))

(ert-deftest uv-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv run  -- foo-command")))))
    (uv-run-cmd "foo-command" '("terminal"))))

(ert-deftest uv-run-command-history-one-project ()
  (clrhash uv--run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv run  -- foo-command" t))
                                      (:input '("uv run  -- other-command" t))
                                      (:input '("uv run  -- foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command")))
    (uv-run-cmd "other-command")
    (should (equal (uv--project-run-command-history) '("other-command" "foo-command")))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command" "other-command")))))

(ert-deftest uv-run-command-history-two-projects ()
  (clrhash uv--run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv run  -- foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command"))))
  (mocker-let ((compile (cmd comint) ((:input '("uv run  -- bar-command" t))))
               (project-current () ((:input '() :output (cons 'project "/bar/bar/project"))))
               (project-root (project) ((:input '((project . "/bar/bar/project"))
                                         :output "/bar/bar/project"))))
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "bar-command")
    (should (equal (uv--project-run-command-history) '("bar-command"))))
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project")))))
    (should (equal (uv--project-run-command-history) '("foo-command")))))

(ert-deftest uv-run-command-repeat-one-project ()
  (clrhash uv--run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv run  -- foo-command" t) :occur 2)
                                      (:input '("uv run  -- other-command" t) :occur 2)
                                      (:input '("uv run --module -- foo-command" t) :occur 2)
                                      (:input '("uv run  -- foo-command" t) :occur 2)))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (uv-run-cmd "foo-command")
    (uv-repeat-run)
    (uv-run-cmd "other-command")
    (uv-repeat-run)
    (uv-run-cmd "foo-command" '("--module"))
    (uv-repeat-run)
    (uv-run-cmd "foo-command")
    (uv-repeat-run)))

(ert-deftest uv-tool-run-command-compile ()
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run  foo-command" t)))))
    (uv-tool-run-cmd "foo-command")))

(ert-deftest uv-tool-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv tool run  foo-command")))))
    (uv-tool-run-cmd "foo-command" '("terminal"))))

(ert-deftest uv-tool-run-command-history-one-project ()
  (clrhash uv--tool-run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run  foo-command" t))
                                      (:input '("uv tool run  other-command" t))
                                      (:input '("uv tool run  foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command")))
    (uv-tool-run-cmd "other-command")
    (should (equal (uv--project-tool-run-command-history) '("other-command" "foo-command")))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command" "other-command")))))

(ert-deftest uv-tool-run-command-history-two-projects ()
  (clrhash uv--tool-run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run  foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command"))))
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run  bar-command" t))))
               (project-current () ((:input '() :output (cons 'project "/bar/bar/project"))))
               (project-root (project) ((:input '((project . "/bar/bar/project"))
                                         :output "/bar/bar/project"))))
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "bar-command")
    (should (equal (uv--project-tool-run-command-history) '("bar-command"))))
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project")))))
    (should (equal (uv--project-tool-run-command-history) '("foo-command")))))

(ert-deftest uv-tool-run-command-repeat-one-project ()
  (clrhash uv--tool-run-history)
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run  foo-command" t) :occur 2)
                                      (:input '("uv tool run  other-command" t) :occur 2)
                                      (:input '("uv tool run --module foo-command" t) :occur 2)
                                      (:input '("uv tool run  foo-command" t) :occur 2)))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (uv-tool-run-cmd "foo-command")
    (uv-repeat-tool-run)
    (uv-tool-run-cmd "other-command")
    (uv-repeat-tool-run)
    (uv-tool-run-cmd "foo-command" '("--module"))
    (uv-repeat-tool-run)
    (uv-tool-run-cmd "foo-command")
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
  (mocker-let ((uv--command-stdout-to-string (cmd &rest args) ((:input '("uv" "export" "--no-hashes" "--no-emit-project" "--no-header" "--no-annotate" "--all-extras")
                                                     :output ""))))
    (should (eq (uv--known-locked-packages) nil))))


(ert-deftest known-locked-packages-non-empty ()
  (mocker-let ((uv--command-stdout-to-string (cmd &rest args) ((:input '("uv" "export" "--no-hashes" "--no-emit-project" "--no-header" "--no-annotate" "--all-extras")
                                                     :output "numpy==2.2.4
pandas==2.2.3
python-dateutil==2.9.0.post0
"))))
    (should (equal (uv--known-locked-packages) '("numpy==2.2.4" "pandas==2.2.3" "python-dateutil==2.9.0.post0")))))


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


(ert-deftest uv--activate-venv-no-venv-available ()
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv")
                                             :output nil))))
    (should-not (uv-activate-venv))
    (should-not (uv-deactivate-venv))))

(ert-deftest uv--activate-venv-venv-available ()
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv")
                                             :output t)))
               (getenv (var) ((:input '("PATH") :output "/original/path:/usr/bin")
                              (:input '("VIRTUAL_ENV") :output nil)
                              (:input '("PYTHONHOME") :output nil)))
               (setenv (var value) ((:input '("VIRTUAL_ENV" "/foo/bar/project/.venv"))
                                    (:input '("PYTHONHOME" nil))
                                    (:input '("PATH" "/foo/bar/project/.venv/bin:/original/path:/usr/bin")))))
    (should (uv-activate-venv))
    (should (equal python-shell-virtualenv-root "/foo/bar/project/.venv"))))

(ert-deftest uv--activate-venv-venv-expand-path ()
  (let ((native-comp-enable-subr-trampolines nil))
    (mocker-let ((project-current () ((:input '() :output (cons 'project "~/project"))))
                 (project-root (project) ((:input '((project . "~/project"))
                                           :output "~/project")))
                 (expand-file-name (name) ((:input '("~/project/.venv")
                                            :output "/home/me/project/.venv")))
                 (file-directory-p (venvdir) ((:input '("/home/me/project/.venv")
                                               :output t)))
                 (getenv (var) ((:input '("PATH") :output "/some/path:/usr/bin")
                                (:input '("VIRTUAL_ENV") :output nil)
                                (:input '("PYTHONHOME") :output nil)))
                 (setenv (var value) ((:input '("VIRTUAL_ENV" "/home/me/project/.venv"))
                                      (:input '("PYTHONHOME" nil))
                                      (:input '("PATH" "/home/me/project/.venv/bin:/some/path:/usr/bin")))))
      (should (uv-activate-venv))
      (should (equal python-shell-virtualenv-root "/home/me/project/.venv")))))


(ert-deftest uv--activate-and-deactivate-venv ()
  (mocker-let ((project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (file-directory-p (venvdir) ((:input '("/foo/bar/project/.venv")
                                             :output t)))
               (getenv (var) ((:input '("PATH") :output "/original/path:/usr/bin")
                              (:input '("VIRTUAL_ENV") :output "/some/other/venv")
                              (:input '("PYTHONHOME") :output "/some/pythonhome")))
               (setenv (var value) ((:input '("VIRTUAL_ENV" "/foo/bar/project/.venv"))
                                    (:input '("PYTHONHOME" nil))
                                    (:input '("PATH" "/foo/bar/project/.venv/bin:/original/path:/usr/bin"))
                                    (:input '("VIRTUAL_ENV" "/some/other/venv"))
                                    (:input '("PYTHONHOME" "/some/pythonhome"))
                                    (:input '("PATH" "/original/path:/usr/bin")))))
    (should (uv-activate-venv))
    (should (uv-deactivate-venv))
    (should (equal python-shell-virtualenv-root "/some/other/venv"))))
