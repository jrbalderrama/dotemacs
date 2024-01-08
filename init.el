;;; init.el --- Emacs init configuration -*- lexical-binding: t -*-
;;; Commentary:
;;
;;  M-x eval-buffer to restart this file without exiting Emacs
;;  C-h v describe variable
;;  C-h f describe function
;;  C-h m describe mode (including key bindings)
;;  C-h b show all key bindings
;;  C-h k describe a given key binding
;;
;;  Use C-x 8 <> for "ñ" "¿" "¡" (?` and !` in latex)
;;
;;; Code:

(setq
 ;; activate debug while loading
 debug-on-error t)

;; define directories and files
(defconst user-data-directory
  (expand-file-name "data"
                    (file-name-as-directory
                     user-emacs-directory))
  "User data directory.")

(defconst user-cache-directory
  (expand-file-name "cache" (file-name-as-directory
                             user-emacs-directory))
  "Emacs user's cache directory.")

(defconst user-backup-directory
  (expand-file-name "backup" (file-name-as-directory
                              user-cache-directory))
  "Emacs backup directory.")

(defconst user-auto-save-directory
  (expand-file-name "auto-save" (file-name-as-directory
                                 user-cache-directory))
  "Emacs auto-save directory.")

(defconst user-elisp-directory
  (expand-file-name "elisp" (file-name-as-directory
                             user-emacs-directory))
  "User Emacs Lisp source code directory.")

;; (defconst user-elisp-autoload-file
;;   (expand-file-name "loaddefs.el" (file-name-as-directory
;;                                    user-elisp-directory))
;;   "User self-managed auto-load file.")

;; (defconst user-defun-default-file
;;   (expand-file-name "defun-default.el" (file-name-as-directory
;;                                         user-elisp-directory))
;;   "User defun file containing basic global utilities.")

;; creating user-defined directories
(dolist (directory (list
                    user-data-directory
                    user-cache-directory
                    user-backup-directory
                    user-auto-save-directory))
  (unless (file-directory-p directory)
    (make-directory directory)))

;; add directories to the load path
(dolist (directory (list
                    user-elisp-directory))
  (when (file-directory-p directory)
    (add-to-list 'load-path directory)))

;; (defvar parentheses/packages
;;   '(use-package) ;;  el-get
;;   "List of packages to install during boot.")
;;
;; (eval-when-compile ;progn
;;   ;; conf/init package manager only for installing use-package
;;   (require 'package)
;;
;;   ;; for emacs <26.2 in order to fetcf packages
;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;   (setq package-enable-at-startup nil)
;;   (add-to-list 'package-archives
;;                '("melpa" . "https://melpa.org/packages/") t)
;;
;;   (package-initialize)
;;
;;   ;; fetch the list of packages available
;;   (unless package-archive-contents
;;     (package-refresh-contents))
;;
;;   (dolist (package parentheses/packages)
;;     (unless (package-installed-p package)
;;       (package-install package)))
;;
;;   (require 'use-package)
;;   (setq use-package-always-ensure t))

(eval-when-compile
  (setq straight-use-package-by-default t)
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(use-package el-patch
  :straight t)

;; Emacs base configuration
(require 'init-default)
;; completion configuration
(require 'init-compl)
;; programming configuration
(require 'init-prog)


;;; registers :: C-x r j ?
;;
;; emacs init file
(set-register ?e `(file . ,user-init-file))
(set-register ?p `(file . "~/.emacs.d/elisp/init-prog.el"))
(set-register ?a `(file . ,(parentheses/org-file-register "Audio.org")))
(set-register ?f `(file . ,(parentheses/org-file-register "Food.org")))
(set-register ?l `(file . ,(parentheses/org-file-register "Logbook.org")))
(set-register ?n `(file . ,(parentheses/org-file-register "Notes.org")))
(set-register ?t `(file . ,(parentheses/org-file-register "Tasks.org")))
;; (set-register ?x '(file . "/path/file"))
;; (set-register ?x (cons 'file "/path/file"))


;; Load theme after all configuration is done
(use-package zenburn-theme
  :config
  (load-theme 'zenburn :no-confirm))

;; turn off debugging
(setq debug-on-error nil)

;; Local Variables:
;; no-byte-compile: t
;; coding: utf-8
;; End:
;;; init.el ends here
