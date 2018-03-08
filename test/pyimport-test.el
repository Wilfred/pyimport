(require 'ert)
(require 'pyimport)
(require 'shut-up)

(ert-deftest pyimport-var-extraction ()
  "Ensure we parse pyflakes output for older pyflakes versions."
  (should
   (equal
    (pyimport--extract-unused-var "'foo' imported but unused")
    "foo")))

(ert-deftest pyimport-var-extraction-new ()
  "Ensure we parse pyflakes output for recent pyflakes versions."
  (should
   (equal
    (pyimport--extract-unused-var "'foo.bar' imported but unused")
    "bar")))

(ert-deftest pyimport-remove-import-case-sensitive ()
  "Ensure we remove imports case-sensitively"
  (with-temp-buffer
    (insert "import cPickle as pickle")
    (pyimport--remove-import 1 "pickle")
    (should
     (equal (buffer-string) ""))))

(ert-deftest pyimport-remove-import-extra-whitespace ()
  "Ensure we remove imports correctly even when there's extra whitespace."
  (with-temp-buffer
    (insert "from   foo  import   bar")
    (pyimport--remove-import 1 "bar")
    (should
     (equal (buffer-string) ""))))

(ert-deftest pyimport-remove-import-as ()
  "Ensure we remove imports correctly when there are aliases."
  (with-temp-buffer
    (insert "from foo import bar as newname")
    (pyimport--remove-import 1 "newname")
    (should
     (equal (buffer-string) ""))))

(ert-deftest pyimport-remove-import-as-alias ()
  "Ensure we remove imports correctly when there are aliases."
  (with-temp-buffer
    (insert "import foo.bar as bar")
    (pyimport--remove-import 1 "bar")
    (should
     (equal (buffer-string) ""))))

(ert-deftest pyimport-remove-on-line-first ()
  "We should remove the first occurrence, if present."
  (with-temp-buffer
    (insert "foo bar baz bar")
    (pyimport--remove-on-line "bar")
    (should
     (equal (buffer-string) "foo  baz bar"))))

(ert-deftest pyimport-import-lines ()
  (with-temp-buffer
    (insert "from foo import bar\n"
            "import baz\n"
            "y = 1\n"
            "import quz.zox")
    (should (equal
             (pyimport--import-lines (current-buffer))
             '("from foo import bar"
               "import baz"
               "import quz.zox")))))

(ert-deftest pyimport-import-lines-correct-buffer ()
  "Ensure we extract lines from the buffer passed in."
  (let ((buf (get-buffer-create "my-buffer")))
    (with-current-buffer buf
      (insert "import foo")
      
      (with-temp-buffer
        (insert "import bar")
        (let ((lines (pyimport--import-lines buf)))
          (should (equal lines '("import foo")))
          (should (equal (get-text-property 0 'pyimport-path (-first-item lines))
                         "my-buffer")))))))

(ert-deftest pyimport-extract-unused-var ()
  (should
   (equal
    (pyimport--extract-unused-var "'foo' imported but unused")
    "foo"))
  (should
   (equal
    (pyimport--extract-unused-var "'foo.bar' imported but unused")
    "bar"))
  (should
   (equal
    (pyimport--extract-unused-var "'foo.bar as xxx' imported but unused")
    "xxx")))

(ert-deftest pyimport-same-module ()
  (should
   (pyimport--same-module
    "from foo import x"
    "from foo import y, z"))
  (should
   (not (pyimport--same-module
         "from foo import x"
         "from foo.bar import x"))))

(ert-deftest pyimport-buffers-in-mode ()
  (let ((buf1 (get-buffer-create "buf1"))
        (buf2 (get-buffer-create "buf2")))
    (shut-up
      (with-current-buffer buf1
        (python-mode))
      (with-current-buffer buf2
        (python-mode)))

    (let ((result (pyimport--buffers-in-mode 'python-mode)))
      (should
       (equal (list buf1 buf2)
              (--sort (string< (buffer-name it) (buffer-name other))
                      result))))
    
    (kill-buffer buf1)
    (kill-buffer buf2)))

(ert-deftest pyimport-for-each-line ()
  (let (result-lines)
    (with-temp-buffer
      (insert "a\nb\nc\nd")
      (pyimport--for-each-line
        (push (pyimport--current-line) result-lines)))

    (setq result-lines (nreverse result-lines))
    (should
     (equal result-lines '("a" "b" "c" "d")))))

(ert-deftest pyimport-insert-import-simple ()
  "Test inserting an import."
  (with-temp-buffer
    (pyimport--insert-import "from foo import x")
    (should
     (equal (buffer-string)
            "from foo import x\n"))))

(ert-deftest pyimport-insert-import-with-existing ()
  "Test inserting an import when we already have imports from that module."
  (with-temp-buffer
    (insert "from foo import x\n")
    (pyimport--insert-import "from foo import y")
    (should
     (equal (buffer-string)
            "from foo import x, y\n"))))

(ert-deftest pyimport-insert-import-duplicate ()
  "Test inserting an import when we already have that symbol imported"
  (with-temp-buffer
    (insert "from foo import x\n")
    (pyimport--insert-import "from foo import x")
    (should
     (equal (buffer-string)
            "from foo import x\n"))))

(ert-deftest pyimport-insert-import-module-docstring ()
  "Test inserting an import when the module starts with a docstring."
  (with-temp-buffer
    (insert "\"\"\"hello world.\nfoo bar.\n\n\"\"\"\n\nfrom bar import y")
    (pyimport--insert-import "from foo import x")
    (should
     (equal (buffer-string)
            "\"\"\"hello world.\nfoo bar.\n\n\"\"\"\n\nfrom foo import x\nfrom bar import y"))))

(ert-deftest pyimport-extract-simple-import ()
  (should
   (equal
    (pyimport--extract-simple-import "from foo import bar, xxx" "bar")
    "from foo import bar"))
  (should
   (equal
    (pyimport--extract-simple-import "from foo import bar,xxx" "xxx")
    "from foo import xxx"))
  (should
   (equal
    (pyimport--extract-simple-import "from foo import xxx as bar, yyy" "bar")
    "from foo import xxx as bar"))
  (should
   (equal
    (pyimport--extract-simple-import "import foo" "foo")
    "import foo"))
  (should
   (equal
    (pyimport--extract-simple-import "import foo as bar" "foo")
    "import foo"))
  (should
   (equal
    (pyimport--extract-simple-import "import foo as bar" "bar")
    "import foo as bar")))

(ert-deftest pyimport-buffer-lines ()
  "We should ignore multiline strings, as they may not contain valid imports."
  (let (lines)
    (with-temp-buffer
      (insert "x = \"\"\"\nfrom foo\n\"\"\"\n")
      (setq lines (pyimport--buffer-lines (current-buffer))))
    (should (not
             (-contains-p lines "from foo")))))
