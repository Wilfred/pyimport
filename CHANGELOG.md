## v1.1

* Fixed a bug with `pyimport-insert-missing` where it reported the
  wrong buffer where it found the import.
* `pyimport-remove-unused` now works on all buffers, even if they're
  unsaved or not visiting a file.
* `pyimport-insert-missing` now steps over shebangs when choosing
  where to insert new imports.
* Fixed an issue with `pyimport-insert-missing` using the contents of
  multiline strings that looked like imports.

## v1.0

Initial release
