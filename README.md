# pyimport

*Manage Python imports from Emacs!*

[![Build Status](https://travis-ci.org/Wilfred/pyimport.svg?branch=master)](https://travis-ci.org/Wilfred/pyimport)
[![Coverage Status](https://coveralls.io/repos/github/Wilfred/pyimport/badge.svg?branch=master)](https://coveralls.io/github/Wilfred/pyimport?branch=master)

Installation: add pyimport.el to your
`load-path`. [MELPA coming soon](https://github.com/melpa/melpa/pull/4009).

## Unused Imports

Requires pyflakes to be installed.

![screenshot](remove_unused.gif)

Run `M-x pyimport-remove-unused`.

This requires `pyflakes` to be on `PATH`. Alternatively, set
`pyimport-pyflakes-path`.

## Insert Missing Imports

![screenshot](insert_missing.gif)

Place point on the missing variable, then run
`M-x pyimport-insert-missing`.

This assumes that you have another Python buffer that contains an
example of importing the variable.

I like to bind this to a key:

```emacs-lisp
(define-key python-mode-map (kbd "C-c C-i") #'pyimport-insert-missing)
```

