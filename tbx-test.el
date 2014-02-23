(require 'tbx "~/Projects/tbx/tbx.el")

(defun tbx-run-tests ()
  (interactive)
  (tbx-import-file "~/Projects/tbx/test-file.tbx"))
