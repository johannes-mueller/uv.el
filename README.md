[![Tests](https://github.com/johannes-mueller/uv.el/actions/workflows/test.yml/badge.svg)](https://github.com/johannes-mueller/uv.el/actions/workflows/test.yml)

# uv.el

An interface for the uv python package manager for emacs


## Synopsis

This package providas a transient based user interface for the [uv package
manager](https://github.com/astral-sh/uv) package manager for Python.


## Status

Just started development, need to collect practival experiences if it actually
works and add features.


## Motivation

The python package manager [uv](https://docs.astral.sh/uv/) is becoming
increasingly popular as it cleans up the jungle of different tools that were
necessary to manage a python project with all its dependencies.  Moreover it is
refreshingly fast.

This package attempts to be a user friendly transient based user interface to a
subset of uv functionality that is useful when developing python projects.  It
does not – at least not from the beginning – cover all the commands with all
their switches.  Many of them are only needed when building docker containers
or in CI/CD pipelines.  This package aims primarily to support you as a
developer to advance your project and its dependencies and tools.  So it
focuses on your tasks as a developer, not so much as a DevOps.


## Installation

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

```elisp
(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))
```

Then you can try `M-x uv` for the top menu. There are also commands for each
sub menu directly, they are `uv-init`, `uv-add`, etc.  That is needed to read
the `pyproject.toml` file to get information about the python project in order
to propose completions.

You will also need the Tree sitter grammar for TOML files in order for the
package to read your `pyproject.toml` file. In order to install that along with
the package use the following snippet.

```elisp
(require 'treesit)

(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el")
  :init
  (add-to-list 'treesit-language-source-alist '(toml "https://github.com/tree-sitter-grammars/tree-sitter-toml"))
  (unless (treesit-language-available-p 'toml)
    (treesit-install-language-grammar 'toml)))
```



## Future plans

This package does not – and probably never will – provide a complete interfact
to th `uv` command line tool (see [Motivation](#motivation)).  But even the
feature subset that is useful for developing tasks, rather than for DevOps
tasks is far from complete.  I have to admit, that I don't understand really
all the user stories of every single command line switch.  That's why I am not
sure how to get the user interface right for certain features.

If you miss a certain feature, feel free to come up with a proposal in the
[issue tracker](https://github.com/johannes-mueller/uv.el/issues).
