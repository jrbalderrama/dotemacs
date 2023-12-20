;;; defun-compl.el --- init-completion.el functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local ido-temp-list nil)

(require 'filecache)
(require 'recentf)

;;;###autoload
(defun parentheses/ido-switch-buffer-same-mode ()
  "Change to a buffer of the same current mode."
  (interactive)
  (let ((the-mode major-mode)
        (current-buffer (current-buffer)))
    (switch-to-buffer
     (ido-completing-read
      (format "Buffers of %s: " the-mode)
      (save-excursion
        (delq nil (mapcar
                   (lambda (buf)
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (and (eq major-mode the-mode)
                              (not (eq current-buffer buf))
                              (buffer-name buf)))))
                   (buffer-list))))))))

;;;###autoload
(defun parentheses/ido-find-file-filecache (file)
  "Use ido to interactively open FILE from file cache."
  (interactive
   (list (ido-file-filecache--read-choices "File: " (mapcar
                                                     (lambda (x)
                                                       (car x))
                                                     file-cache-alist))))
  (let ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file (if (= (length record) 2)
               (car (cdr record))
             (ido-file-filecache--read-choices (format "Find %s in: " file) (cdr record)))))))

(defun ido-file-filecache--read-choices (prompt choices)
  "Internal function to use with 'ido-file-cache-find-file'.

The PROMPT is selected from mini-buffer and CHOICES is a list of alternatives."
  (let ((ido-make-buffer-list-hook
         (lambda ()
           (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(eval-when-compile (require 'cl))

;;;###autoload
(defun parentheses/ido-find-file-recentf ()
  "Find a recent file using Ido."
  (interactive)

  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x) x))
                    recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Recent file: "
                                        filename-list nil t)))
    (when filename
      (find-file (cdr (assoc filename file-assoc-list))))))

;;; defun-compl.el ends here
