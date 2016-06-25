(require 'ert)
(require 'pyimport)

(ert-deftest var-extraction ()
  "Ensure we parse pyflakes output for older pyflakes versions."
  (should
   (equal
    (pyimport--extract-unused-var "'foo' imported but unused")
    "foo")))

(ert-deftest var-extraction-new ()
  "Ensure we parse pyflakes output for recent pyflakes versions."
  (should
   (equal
    (pyimport--extract-unused-var "'foo.bar' imported but unused")
    "bar")))

(ert-deftest remove-import-case-sensitive ()
  "Ensure we remove imports case-sensitively"
  (with-temp-buffer
    (insert "import cPickle as pickle")
    (pyimport--remove-import 1 "pickle")
    (should
     (equal (buffer-string) ""))))

(ert-deftest remove-on-line-first ()
  "We should remove the first occurrence, if present."
  (with-temp-buffer
    (insert "foo bar baz bar")
    (pyimport--remove-on-line "bar")
    (should
     (equal (buffer-string) "foo  baz bar"))))

