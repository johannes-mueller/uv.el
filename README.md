[![Tests](https://github.com/johannes-mueller/uv.el/actions/workflows/test.yml/badge.svg)](https://github.com/johannes-mueller/uv.el/actions/workflows/test.yml)

# uv.el

An interface for the uv python package manager for emacs


## Synopsis

This package providas a transient based user interface for the [uv package
manager](https://github.com/astral-sh/uv) package manager for Python.


## Status

Just started development, need to collect practival experiences if it actually
works and add features.


## Installation

At the moment the most convenient method to install it is using
[straight.el](https://github.com/raxod502/straight.el). Put the following lines
into your startup file.

``` elisp
(use-package uv
  :straight (uv :type git :host github :repo "johannes-mueller/uv.el"))
```

Then you can try `M-x uv-menu`.
