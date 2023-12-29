;;; defun-default.el --- init-default.el functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;;###autoload
(defun parentheses/org-file-register (filename &optional user-org-directory)
  "Get a path of a 'org-mode' file FILENAME with from the USER-ORG-DIRECTORY.

The USER-ORG-DIRECTORY is set to ~/Org as default."
  (unless user-org-directory
    (setq
     user-org-directory
     (expand-file-name "Org" (file-name-as-directory
                              (getenv "HOME")))))
  (expand-file-name filename (file-name-as-directory
                          user-org-directory)))

;;;###autoload
(defun parentheses/kill-buffer (kill-buffer &rest args)
  "Do not kill the `*scratch*' buffer when KILL-BUFFER is invoked.

Override the `kill-buffer' command burying the *scratch* buffer
instead of killing it.  The advice does not modify the command or
the ARGS."
  (require 'advice)
  (let ((buffer-to-kill (ad-get-argument args 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      (apply kill-buffer args))))

(defun parentheses/battery-connected-p ()
  "True only when is AC connected."
  (require 'battery nil t)
  (string-equal (cdr (assoc ?L (funcall battery-status-function))) "Battery"))

;;;###autoload
(defun parentheses/display-on-battery ()
  "Activate widgets when the AC is disconnected."
  (let ((display-on-battery-p (if (parentheses/battery-connected-p) t -1)))
    (display-battery-mode display-on-battery-p)))

(defun parentheses/fullscreen-p ()
  "True only when Emacs is in full screen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

;;;###autoload
(defun parentheses/display-on-fullscreen ()
  "Activate widgets on full screen.

TODO: - display-time-mail-directory
      - zoneinfo-style-world-list"
  (setq display-time-24hr-format t
        display-time-default-load-average nil)
  (let ((display-on-fullscreen-p (if (parentheses/fullscreen-p) t -1)))
    (display-time-mode display-on-fullscreen-p)))

;;;###autoload
(defun parentheses/kill-completion-buffer ()
  "Remove/kill completion buffer when done."
  (let ((buffer "*Completions*"))
    (and (get-buffer buffer)
         (kill-buffer buffer))))

(defun parentheses/remove-highlight-changes ()
  "Remove the highlight face on buffer displaying modifications."
  (highlight-changes-remove-highlight
   (point-min) (point-max)))

;;;###autoload
(defun parentheses/update-autoloads-in-current-directory (&optional file)
  "Update autoloads for files in the directory containing this FILE."
  (let* ((base (file-truename
                (file-name-directory
                 (symbol-file 'parentheses/update-autoloads-in-current-directory 'defun))))
         (generated-autoload-file (expand-file-name "loaddefs.el" base)))
    (cd base)
    (if file
        (update-file-autoloads file)
      (update-directory-autoloads base))))

;;;###autoload
(defun parentheses/iedit-narrow-to-defun (arg)
  "Start iedit using \\[narrow-to-defun] to limit its scope.

When an ARG is used call default iedit-mode."
  (interactive "P")
  (require 'iedit)
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

;; (defun parentheses/buffer-face-mode-upsidedown ()
;;   "Set font to a variable width (proportional) fonts in current buffer."
;;   (interactive)
;;   (require 'face-remap)
;;   (setq buffer-face-mode-face '(:family "UpsideDown" :height 150))
;;   (buffer-face-mode))

;;;###autoload
(defun parentheses/yank-primary ()
  "Pull from PRIMARY selection (same as middle mouse click)."
  (interactive)
  (insert (gui-get-primary-selection)))

;;; defun-default.el ends here
