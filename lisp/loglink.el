;;; loglink.el --- Adds links to a file on a remote file system translated to the local filesystem

;;; Commentary:

;;; Code:
(defun loglink ()
  "Scans a buffer for matching file paths."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "opt/platform\\(/\\w+\\)+\\.\\w+:\\w+" nil t)
    (make-button (match-beginning 0) (match-end 0) 'action 'translate-and-find)))

(defun translate-and-find (button)
  "Opens file to line number when BUTTON is accessed."
  (let ((parts (split-string (button-label button) ":")))
    (find-file
     (mapconcat 'identity
                (append '("~" "Sites" "platform")
                        (cdr (split-string (car parts) "/")))
                "/")
     )
    (goto-char (point-min))
    (forward-line (1- (string-to-number (car (cdr parts)))))))

(provide 'loglink)
;;; loglink.el ends here
