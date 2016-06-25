(require 'f)

(defvar pyimport--test-path
  (f-parent (f-this-file)))

(defvar pyimport--root-path
  (f-parent pyimport--test-path))

(require 'ert)
(require 'pyimport (f-expand "pyimport" pyimport--root-path))
