(require 'mocker)
(require 'uv)
(require 'cl-lib)


(ert-deftest uv-init-no-args ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init foo-bar" t))))
               (call-interactively (fun) ((:input '(project-dired)))))
    (uv-init-cmd "foo-bar")
    (should (equal project-current-directory-override "foo-bar"))))

(ert-deftest uv-init-transient-name-no-masks ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --name=FooBar foo-bar" t)))))
    (uv-init-cmd "foo-bar" '("--name=FooBar"))))

(ert-deftest uv-init-transient-name-with-masks ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --name=My\\ Project foo-bar" t)))))
    (uv-init-cmd "foo-bar" '("--name=My Project"))))

(ert-deftest uv-init-transient-python-version ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --python 3.12 foo-bar" t)))))
    (uv-init-cmd "foo-bar" '("--python 3.12"))))

(ert-deftest uv-init-transient-no-readme ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --no-readme foo-bar" t)))))
    (uv-init-cmd "foo-bar" '("--no-readme"))))

(ert-deftest uv-available-python-versions-sorted ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json")
                                                :output "[{\"version\": \"3.7.9\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}]"))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1" "3.7.9")))))

(ert-deftest uv-available-python-versions-sorted-non-unique ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json")
                                                :output "[{\"version\": \"3.13.2\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}]"))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1")))))


(ert-deftest uv-available-python-versions-unsorted ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json")
                                                :output "[{\"version\": \"3.13.2\"}, {\"version\": \"3.13.1\"}, {\"version\": \"3.14.0b3\"}, {\"version\": \"3.14.0rc1\"}, {\"version\": \"3.12.9\"}, {\"version\": \"3.14.0a4\"}, {\"version\": \"3.14.0a5\"}, {\"version\": \"3.7.9\"}]"))))
    (should (equal (uv--available-python-versions) '("3.13.2" "3.13.1" "3.12.9" "3.7.9" "3.14.0rc1" "3.14.0b3" "3.14.0a4" "3.14.0a5")))))

;; (ert-deftest uv-available-python-versions-minor-releases ()
;;   (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json" t);                                                        :output "[{\"version\": \"3.13.1\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.12.1\"}, {\"version\": \"3.12.2\"}]"))))
;;     (should (equal (uv--available-python-versions) '("3.13" "3.13.1" "3.13.2" "3.12" "3.12.1" "3.12.2")))))


(ert-deftest uv-venv-plain ()
  (mocker-let ((compile (cmd comint) ((:input '("uv venv" t)))))
    (uv-venv-cmd)))

(ert-deftest uv-venv-seed ()
  (mocker-let ((compile (cmd comint) ((:input '("uv venv --seed" t)))))
    (uv-venv-cmd '("--seed"))))


(ert-deftest uv-add-one ()
  (mocker-let ((compile (cmd comint) ((:input '("uv add pandas" t)))))
    (uv-add-cmd "pandas")))


(ert-deftest uv-add-dev ()
  (mocker-let ((compile (cmd comint) ((:input '("uv add --dev pytest" t)))))
    (uv-add-cmd "pytest" '("--dev"))))


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
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project" ("version" . "0.1.0"))
                                                   ("dependency-groups" ("my-extra" . ["scipy"]) ("dev" . ["pytest>=8.3.5"]))))))))
    (should (equal-set (uv--known-dependency-groups) '("my-extra" "dev")))))


(ert-deftest known-groups-no-groups ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                        :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table '(("project" ("version" . "0.1.0"))))))))
    (should (eq (uv--known-dependency-groups) nil))))


(ert-deftest known-extras-extras ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project"
                                                    ("version" . "0.1.0")
                                                    ("optional-dependencies"
                                                     ("my-extra" . ["scipy"])
                                                     ("dev" . ["pytest>=8.3.5"])))))))))
    (should (equal-set (uv--known-extras) '("my-extra" "dev")))))


(ert-deftest known-extras-no-extras ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                        :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project" ("version" . "0.1.0"))))))))
    (should (equal (uv--known-extras) nil))))


(ert-deftest known-dependencies-dependencies-plain ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project"
                                                    ("version" . "0.1.0")
                                                    ("dependencies" . ["scipy" "pytest>=8.3.5"]))))))))
    (should (equal (uv--known-dependencies) '("scipy" "pytest>=8.3.5")))))


(ert-deftest known-dependencies-no-dependencies ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                        :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project" ("version" . "0.1.0"))))))))
    (should (equal (uv--known-dependencies) nil))))


(ert-deftest known-dependencies-dependencies-without-group ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (tomlparse-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                        :output (alist-to-hash-table
                                                 '(("project"
                                                    ("version" . "0.1.0")
                                                    ("dependencies" . ["numpy" "scipy"]))
                                                   ("dependency-groups"
                                                    ("dev" . ["pytest" "freezegun"])
                                                    ("docs" . ["sphinx" "nbsphinx"]))))))))
    (should (equal (uv--known-dependencies) '("numpy" "scipy")))))


(ert-deftest known-dependencies-dependencies-docs-group ()
  (let ((transient-current-command 'uv-remove-menu))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
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
  (let ((transient-current-command 'uv-remove-menu))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
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
  (let ((transient-current-command 'uv-remove-menu))
    (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
                 (project-root (project) ((:input '((project . "/foo/bar/project"))
                                           :output "/foo/bar/project")))
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
  (mocker-let ((compile (cmd comint) ((:input '("uv run foo-command" t)))))
    (uv-run-cmd "foo-command")))

(ert-deftest uv-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv run foo-command")))))
    (uv-run-cmd "foo-command" 'interactive)))

(ert-deftest uv-run-command-history ()
  (mocker-let ((compile (cmd comint) ((:input '("uv run foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-run-command-history))
    (uv-run-cmd "foo-command")
    (should (equal (uv--project-run-command-history) '("foo-command")))))

(ert-deftest uv-tool-run-command-compile ()
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run foo-command" t)))))
    (uv-tool-run-cmd "foo-command")))

(ert-deftest uv-tool-run-command-ansi-term ()
  (mocker-let ((ansi-term (cmd) ((:input '("uv tool run foo-command")))))
    (uv-tool-run-cmd "foo-command" 'interactive)))

(ert-deftest uv-tool-run-command-history ()
  (mocker-let ((compile (cmd comint) ((:input '("uv tool run foo-command" t))))
               (project-current () ((:input '() :output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project"))))
    (should-not (uv--project-tool-run-command-history))
    (uv-tool-run-cmd "foo-command")
    (should (equal (uv--project-tool-run-command-history) '("foo-command")))))

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
  (mocker-let ((uv--shell-command-stdout-to-string (cmd) ((:input '("uv export --no-hashes --no-emit-project --no-header --all-extras")
                                                :output ""))))
    (should (eq (uv--known-locked-packages) nil))))


(ert-deftest known-locked-packages-non-empty ()
  (mocker-let ((uv--shell-command-stdout-to-string (cmd)
                                                   ((:input '("uv export --no-hashes --no-emit-project --no-header --all-extras")
                                                     :output "numpy==2.2.4
pandas==2.2.3
python-dateutil==2.9.0.post0
"))))
    (should (equal (uv--known-locked-packages) '("numpy==2.2.4" "pandas==2.2.3" "python-dateutil==2.9.0.post0")))))
