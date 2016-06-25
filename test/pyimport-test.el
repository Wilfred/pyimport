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

