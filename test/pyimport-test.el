(require 'ert)
(require 'pyimport)

(ert-deftest var-extraction ()
  "Ensure we parse pyflakes output for different pyflakes versions."
  (should
   (equal
    (pyimport--extract-unused-var "'foo' imported but unused")
    "foo"))
  )

