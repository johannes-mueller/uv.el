(require 'mocker)
(require 'uv)


(ert-deftest uv-init-no-args ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init foo-bar" t)))))
    (uv-init "foo-bar")))

(ert-deftest uv-init-transient-name-no-masks ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --name=FooBar foo-bar" t)))))
    (uv-init "foo-bar" '("--name=FooBar"))))

(ert-deftest uv-init-transient-name-with-masks ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --name=My\\ Project foo-bar" t)))))
    (uv-init "foo-bar" '("--name=My Project"))))

(ert-deftest uv-init-transient-python-version ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --python 3.12 foo-bar" t)))))
    (uv-init "foo-bar" '("--python 3.12"))))

(ert-deftest uv-init-transient-no-readme ()
  (mocker-let ((compile (cmd comint) ((:input '("uv init --no-readme foo-bar" t)))))
    (uv-init "foo-bar" '("--no-readme"))))

(ert-deftest uv-available-python-versions-sorted ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json")                                               :output "[{\"version\": \"3.12\"}, {\"version\": \"3.13\"}, {\"version\": \"3.11\"}]"))))
    (should (equal (uv-available-python-versions) '("3.13" "3.12" "3.11")))))

(ert-deftest uv-available-python-versions-unsorted ()
  (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json")                                               :output "[{\"version\": \"3.13\"}, {\"version\": \"3.14.0b3\"}, {\"version\": \"3.14.0rc1\"}, {\"version\": \"3.12\"}, {\"version\": \"3.14.0a4\"}, {\"version\": \"3.14.0a5\"}]"))))
    (should (equal (uv-available-python-versions) '("3.13" "3.12" "3.14.0rc1" "3.14.0b3" "3.14.0a4" "3.14.0a5")))))

;; (ert-deftest uv-available-python-versions-minor-releases ()
;;   (mocker-let ((shell-command-to-string (cmd) ((:input '("uv python list --output-format=json" t);                                                        :output "[{\"version\": \"3.13.1\"}, {\"version\": \"3.13.2\"}, {\"version\": \"3.12.1\"}, {\"version\": \"3.12.2\"}]"))))
;;     (should (equal (uv-available-python-versions) '("3.13" "3.13.1" "3.13.2" "3.12" "3.12.1" "3.12.2")))))


(ert-deftest uv-venv-plain ()
  (mocker-let ((compile (cmd comint) ((:input '("uv venv" t)))))
    (uv-venv)))

(ert-deftest uv-venv-seed ()
  (mocker-let ((compile (cmd comint) ((:input '("uv venv --seed" t)))))
    (uv-venv '("--seed"))))


(ert-deftest uv-add-one ()
  (mocker-let ((compile (cmd comint) ((:input '("uv add pandas" t)))))
    (uv-add "pandas")))


(ert-deftest uv-add-dev ()
  (mocker-let ((compile (cmd comint) ((:input '("uv add --dev pytest" t)))))
    (uv-add "pytest" '("--dev"))))


(ert-deftest known-groups-no-groups ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                         :output "/foo/bar/project")))
               (toml:read-from-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                             :output '(("project" ("version" . "0.1.0"))
                                                       ("dependency-groups" ("my-extra" "scipy") ("dev" "pytest>=8.3.5")))))))
    (should (equal (uv--known-dependency-groups) '("my-extra" "dev")))))


(ert-deftest known-groups-groups ()
  (mocker-let ((project-current () ((:output (cons 'project "/foo/bar/project"))))
               (project-root (project) ((:input '((project . "/foo/bar/project"))
                                        :output "/foo/bar/project")))
               (toml:read-from-file (file) ((:input '("/foo/bar/project/pyproject.toml")
                                             :output '(("project" ("version" . "0.1.0")))))))
    (should (equal (uv--known-dependency-groups) nil))))
