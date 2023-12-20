;;; defun-elisp.el --- init-elisp.el functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;###autoload
(defun parentheses/byte-compile-current-buffer ()
  "Compile current buffer if it's Emacs-Lisp code."
  (interactive)
  (when (eq major-mode 'emacs-lisp-mode)
    (byte-compile-file buffer-file-name)))

;;; defun-elisp.el ends here
