;;; init-compl.el --- Completion configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq
 ;; append to ignore extensions
 completion-ignored-extensions
 (cons "*.asc_archive" completion-ignored-extensions))

;; recent files booking
(recentf-mode t)

(use-package recentf
  :init
  (setq
   ;; set recentf save file
   recentf-save-file
   (expand-file-name "recentf" user-cache-directory))
  :config
  (setq
   recentf-auto-cleanup 'never
   recentf-max-saved-items 100
   recentf-exclude
   (append recentf-exclude
           '("\\.emacs.d/el-get/"
             "\\.emacs.d/elpa/"
             "\\.emacs.d/cache/"))))

(with-eval-after-load "filecache"
  ;; set filecache directory
  (file-cache-add-directory user-elisp-directory)
  ;; add `user-init-file' to filecache
  (file-cache-add-file user-init-file))

(use-package ido
  :init
  (setq ido-save-directory-list-file
        (expand-file-name "ido.last" (file-name-as-directory
                                      user-cache-directory)))
  :config
  (setq
   ;;
   ido-auto-merge-delay-time 5
   ;; ignore case sensitive search
   ido-case-fold  t
   ;;
   ido-confirm-unique-completion t
   ;; creates buffer if name does not exist
   ido-create-new-buffer 'always
   ;;
   ido-enable-last-directory-history t
   ;;
   ido-enable-flex-matching t
   ;; next works with variable `completion-ignored-extensions'
   ido-ignore-extensions t
   ;; disable ido faces to see flx
   ido-use-faces nil
   ;;
   ido-use-filename-at-point nil
   ;;
   ido-use-url-at-point nil
   ;;
   ido-ignore-files
   (append ido-ignore-files
           ;; emacs related
           '(".elc" "ido.last")
           ;; python related
           '(".pyc" ".python-version" ".egg-info" ".lock")
           ;; latex related
           '(".aux" ".log" ".toc"))
   ;;
   ido-ignore-directories
   (append ido-ignore-directories '("\\`.git" "\\`.svn" "\\`.hg" "dist"))
   ;;
   ido-ignore-extensions t

   ;;
   ido-file-extensions-order
   '(".org" ".py" ".el" ".tex" ".yaml" ".sh")
   ;; (setq
   ;;  ido-ignore-buffers '("^ "
   ;;                       "*Messages*" "Async Shell Command" "Compile-Log"
   ;;                       "Buffer List" "*Slack Log \*" "*Calendar*" name))
   ;;
   ido-ignore-buffers
   (list (rx (or (and bos " ")
                 (and bos
                      (or "*elpy-rpc*"
                          "*Compile-Log*"
                          "*Completions*"
                          "*Org-toodledo-log*"
                          "*Shell Command Output*"
                          "*vc-diff*")
                      eos)
                 (and bos
                      (or "*epc con "))))))
  (ido-mode t)
  (ido-everywhere t)

  (load "~/.emacs.d/elisp/defun-compl.el")
  :bind (;; change to buffer of the same mode
         ("C-c b" . 'parentheses/ido-switch-buffer-same-mode)
         ;; integrate 'file-cache' with ido
         ("C-c f" . 'parentheses/ido-find-file-filecache)
         ;; integrate 'recentf' with ido
         ("C-x C-r" . 'parentheses/ido-find-file-recentf)))

;;
;; (use-package ido-ubiquitous
;;   :config
;;   (setq ido-ubiquitous-mode t))
;;
;; (use-package flx-ido
;;   :config
;;   (flx-ido-mode t))
;;
;; (use-package ido-sort-mtime
;;   :config
;;   (ido-sort-mtime-mode t)))

;; (require 'ido-at-point)
;; (ido-at-point-mode)

(use-package smex
  :after ido
  :commands smex-initialize
  :init
  (smex-initialize)
  :config
  (setq smex-save-file
        (expand-file-name "smex-items" (file-name-as-directory
                                        user-cache-directory)))
  :bind (;; bind smex to Emacs
         ("M-x" . smex)
         ;; smex only major mode commands
         ("M-X" . smex-major-mode-commands)
         ;; This is the old 'M-x'
         ("C-c C-c M-x" . execute-extended-command)))

(provide 'init-compl)

;;; init-compl.el ends here
